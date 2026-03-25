//! Preprocessor macros, conditionals and includes are all tied very much together. Here, almost everything gets evaluated.

use std::borrow::Cow;

use camino::{Utf8Path, Utf8PathBuf};
use dt_analyzer::macros::MacroDefinition;
use dt_diagnostic::{Diagnostic, DiagnosticCollector, Severity};
use dt_parser::{
    ast::{self, AstNode, AstNodeOrToken, AstToken},
    lexer::TokenKind,
    TextRange,
};
use rustc_hash::FxHashMap;

use crate::salsa::{
    db::BaseDb,
    includes::IncludeDirs,
    macros::env::{MacroEnv, MacroEnvMut},
};

#[salsa::tracked]
pub struct PpEvalFileResult<'db> {
    #[tracked]
    #[returns(ref)]
    pub toplevels: Vec<PpEvalFileResultToplevel<'db>>,

    #[tracked]
    pub env_after: MacroEnv<'db>,

    #[tracked]
    #[returns(ref)]
    pub diagnostics: Vec<Diagnostic>,
}

#[salsa::tracked]
pub struct PpEvalFileResultToplevel<'db> {
    pub text_range: TextRange,
    #[tracked]
    #[no_eq]
    pub inner: PpEvalFileResultToplevelInner<'db>,
    #[tracked]
    pub env_after: MacroEnv<'db>,
}

#[derive(Clone)]
pub enum PpEvalFileResultToplevelInner<'db> {
    Include {
        file: crate::salsa::file::File,
        result: PpEvalFileResult<'db>,
        /// Whether the include is a preprocessor (`#include`) or a DTS (`/include/`) include.
        is_preprocessor: bool,
    },
    MacroDefinition {
        /// Fetch the definition from [`PpEvalFileResultToplevel::env_after`].
        name: String,
    },
    Undef,
    // TODO: conditionals should be evaluated, right?
    Conditional,
}

/// Lists possible file paths under `parent_path` or `include_dirs`, depending on [`relative`](Self::relative).
pub fn possible_include_paths_utf8<'a, P: AsRef<Utf8Path>>(
    relative: bool,
    path: &'a str,
    parent_path: &'a Utf8Path,
    include_dirs: &'a [P],
) -> impl Iterator<Item = Utf8PathBuf> + use<'a, P> {
    relative
        .then_some(parent_path)
        .into_iter()
        .chain(include_dirs.iter().map(AsRef::as_ref))
        .map(move |base_path| base_path.join(path))
}

fn get_pp_dir_args<'inp>(input: &'inp str, directive: &str) -> &'inp str {
    debug_assert!(input.starts_with('#'));
    let s = input
        .get(1..)
        .expect("lexer safe")
        .trim_start_matches([' ', '\t']);

    debug_assert!(s.starts_with(directive));
    let s = s
        .get(directive.len()..)
        .expect("lexer safe")
        .trim_start_matches([' ', '\t']);
    s
}
#[cfg(test)]
#[test]
fn test_pp_dir_args() {
    assert_eq!(get_pp_dir_args("# foo bar baz", "foo"), "bar baz");
}

fn subslice_offset(this: &str, inner: &str) -> Option<usize> {
    let self_beg = this.as_ptr() as usize;
    let inner = inner.as_ptr() as usize;
    if inner < self_beg || inner > self_beg.wrapping_add(this.len()) {
        None
    } else {
        Some(inner.wrapping_sub(self_beg))
    }
}
#[cfg(test)]
#[test]
fn test_subslice_offset() {
    let s = "foo";
    assert_eq!(subslice_offset(s, s.to_owned().as_str()), None);
    assert_eq!(subslice_offset(s, &s[0..]), Some(0));
    assert_eq!(subslice_offset(s, &s[1..]), Some(1));
}

fn get_pp_dir_arg_string<'inp>(
    input: &'inp str,
    directive: &str,
    text_range: TextRange,
    diag: &impl DiagnosticCollector,
) -> Option<(bool, &'inp str)> {
    // TODO: real evaluation and macro substitution

    let args = get_pp_dir_args(input, directive);

    let (relative, split) = match args.as_bytes().first() {
        Some(b'<') => (false, args.get(1..).expect("safe").split_once('>')),
        Some(b'"') => (true, args.get(1..).expect("safe").split_once('"')),
        None => {
            diag.emit(Diagnostic::new(
                text_range,
                Cow::Borrowed("Expected an argument"),
                Severity::Error,
            ));
            return None;
        }
        _ => {
            diag.emit(Diagnostic::new(
                text_range,
                Cow::Borrowed("Unexpected character in argument, expected `\"` or `<`."),
                Severity::Error,
            ));
            return None;
        }
    };

    let Some((s, rest)) = split else {
        diag.emit(Diagnostic::new(
            text_range,
            Cow::Borrowed("Missing string terminator"),
            Severity::Error,
        ));
        return None;
    };

    if !rest.is_empty() {
        if let Some(offset) = subslice_offset(input, rest) {
            diag.emit(Diagnostic::new(
                TextRange {
                    start: text_range.start + offset,
                    end: text_range.end,
                },
                Cow::Borrowed("Unexpected characters after include string"),
                Severity::Warn,
            ));
        }
    }

    Some((relative, s))
}

// TODO: not only toplevel.. :(

/// Walks toplevel items of a file, evaluating necessary preprocessor directives and macros.
///
/// Returns `None` if the file doesn't exist.
///
/// # Algorithm
///
/// - Keep a [`MacroEnv`].
/// - For each toplevel item:
///   - Update the macro environment as appropriate.
///   - On an include, recurse `eval_outline` because we have to know the next macro env.
///   - On a conditional, evalute it and choose the correct branch, if any.
///
/// # Panics
///
/// Will panic if [`IncludeDirs`] hasn't been defined.
#[salsa::tracked]
pub fn preprocessor_eval_file<'db>(
    db: &'db dyn BaseDb,
    file: super::file::File,
    parent_env: Option<MacroEnv<'db>>,
) -> Option<PpEvalFileResult<'db>> {
    let mut env = MacroEnvMut {
        parent: parent_env,
        own_map: FxHashMap::default(),
    };
    let mut toplevels = Vec::new();

    let parse = super::parse_file(db, file)?;
    let file_ast = parse.parse(db).source_file();

    let mut diagnostics = Vec::new();
    let diag = parking_lot::Mutex::new(&mut diagnostics);

    // Path of the current file's parent directory.
    let parent_path = file.path(db).parent()?;
    let include_dirs = IncludeDirs::get(db).include_dirs(db);
    let files = db.get_files();

    for item in file_ast.items() {
        let text_range = item.syntax().text_range();

        let inner = match item {
            ast::ToplevelItem::Node(dt_node) => {
                // TODO: recurse!!!
                continue;
            }
            ast::ToplevelItem::Directive(dir) => {
                let mut iter = dir.syntax().child_tokens();

                // Skip until DtIncludeDirective is found, continue items if it isn't found
                if !iter.any(|tok| tok.green.kind == TokenKind::DtIncludeDirective) {
                    continue;
                }

                let Some(string_tok) = iter.find(|tok| tok.green.kind == TokenKind::String) else {
                    continue;
                };
                let path = match dt_analyzer::string::interpret_escaped_string(string_tok.text()) {
                    Ok(path) => path,
                    Err(err) => {
                        diag.emit(Diagnostic::new(
                            string_tok.text_range(),
                            Cow::Owned(err.to_string()),
                            Severity::Error,
                        ));
                        continue;
                    }
                };

                let Some(file) =
                    possible_include_paths_utf8(true, &path, parent_path, include_dirs)
                        .map(|path| files.get_file(db, &path))
                        .find(|file| file.is_readable_file(db))
                else {
                    diag.emit(Diagnostic::new(
                        text_range,
                        Cow::Borrowed("Couldn't find file to include"),
                        Severity::Error,
                    ));
                    continue;
                };

                let result =
                    preprocessor_eval_file(db, file, Some(env.into_salsa_tracked_struct(db)))
                        .expect("The file should exist, its existence is confirmed above");

                env = MacroEnvMut {
                    parent: Some(result.env_after(db)),
                    own_map: FxHashMap::default(),
                };

                PpEvalFileResultToplevelInner::Include {
                    is_preprocessor: false,
                    file,
                    result,
                }
            }
            // TODO: conditionals!
            ast::ToplevelItem::PreprocessorConditional(preprocessor_conditional) => continue,
            ast::ToplevelItem::PreprocessorDirective(dir) => match dir.kind() {
                TokenKind::PragmaDirective => {
                    // TODO: implement #pragma

                    diag.emit(Diagnostic::new(
                        text_range,
                        Cow::Borrowed("`#pragma` is unimplemented"),
                        Severity::Error,
                    ));
                    continue;
                }
                TokenKind::DefineDirective => {
                    let parsed = match MacroDefinition::parse(dir.syntax().text()) {
                        Ok(inc) => inc,
                        Err(err) => {
                            diag.emit(Diagnostic::new(
                                if let Some(local_range) = err.text_range() {
                                    local_range.offset(dir.syntax().text_offset)
                                } else {
                                    dir.syntax().text_range()
                                },
                                Cow::Owned(err.to_string()),
                                Severity::Error,
                            ));
                            continue;
                        }
                    };

                    let name_clone = parsed.name.clone();
                    env.own_map.insert(parsed.name.clone(), Some(parsed));

                    PpEvalFileResultToplevelInner::MacroDefinition { name: name_clone }
                }
                TokenKind::UndefDirective => {
                    let input = dir.syntax().text();
                    let args = get_pp_dir_args(input, "undef");

                    if args.contains(' ') {
                        diag.emit(Diagnostic::new(
                            dir.syntax().text_range(),
                            Cow::Borrowed("Arguments to `#undef` should be just a macro name"),
                            Severity::Error,
                        ));
                        continue;
                    }

                    env.own_map.insert(args.to_owned(), None);

                    PpEvalFileResultToplevelInner::Undef
                }
                TokenKind::IncludeDirective => {
                    // TODO: real evaluation and macro substitution
                    let input = dir.syntax().text();

                    let Some((relative, path)) =
                        get_pp_dir_arg_string(input, "include", text_range, &diag)
                    else {
                        continue;
                    };

                    let Some(file) =
                        possible_include_paths_utf8(relative, path, parent_path, include_dirs)
                            .map(|path| files.get_file(db, &path))
                            .find(|file| file.is_readable_file(db))
                    else {
                        diag.emit(Diagnostic::new(
                            text_range,
                            Cow::Borrowed("Couldn't find file to include"),
                            Severity::Error,
                        ));
                        continue;
                    };

                    let result =
                        preprocessor_eval_file(db, file, Some(env.into_salsa_tracked_struct(db)))
                            .expect("The file should exist, its existence is confirmed above");

                    env = MacroEnvMut {
                        parent: Some(result.env_after(db)),
                        own_map: FxHashMap::default(),
                    };

                    PpEvalFileResultToplevelInner::Include {
                        is_preprocessor: true,
                        file,
                        result,
                    }
                }
                TokenKind::ErrorDirective => {
                    let input = dir.syntax().text();
                    let args = get_pp_dir_args(input, "error");

                    diag.emit(Diagnostic::new(
                        text_range,
                        // TODO: remove debug thing
                        Cow::Owned(format!(
                            "`#error`: {args}, defined({args})={}",
                            env.get_macro(db, args).is_some()
                        )),
                        Severity::Error,
                    ));
                    continue;
                }
                _ => continue,
            },
        };

        toplevels.push(PpEvalFileResultToplevel::new(
            db,
            text_range,
            inner,
            env.to_salsa_tracked_struct_mut(db),
        ));
    }

    super::tag_diagnostics(
        &mut diagnostics,
        concat!(module_path!(), "::preprocessor_eval_file"),
    );

    Some(PpEvalFileResult::new(
        db,
        toplevels,
        env.into_salsa_tracked_struct(db),
        diagnostics,
    ))
}
