//! Preprocessor macros, conditionals and includes are all tied very much together, which means they must be evaluated in order.

use std::borrow::Cow;

use camino::{Utf8Path, Utf8PathBuf};
use dt_analyzer::macros::MacroDefinition;
use dt_diagnostic::{
    Diagnostic, DiagnosticCollector, MultiSpan, OffsetDiagnosticCollector, Severity,
};
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

fn get_pp_directive_args(
    input: &str,
    directive: &str,
    text_range: TextRange,
) -> (String, TextRange) {
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

    let start_offset = subslice_offset(input, s).unwrap();

    // Newlines are escaped outside comments. Let's undo that to make the text parseable.
    let s = s.replace("\\\n", " \n");

    debug_assert_eq!(text_range.start + input.len(), text_range.end);
    (
        s,
        TextRange {
            start: text_range.start + start_offset,
            end: text_range.end,
        },
    )
}
#[cfg(test)]
#[test]
fn test_get_pp_directive_args() {
    let input = "#include a";
    assert_eq!(
        get_pp_directive_args(
            input,
            "include",
            TextRange {
                start: 0,
                end: input.len()
            }
        ),
        (
            "a".to_owned(),
            TextRange {
                start: "#include ".len(),
                end: input.len()
            }
        )
    );
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

fn get_pp_directive_arg_string(
    input: &str,
    directive: &str,
    text_range: TextRange,
    diag: &impl DiagnosticCollector,
) -> Option<(bool, String)> {
    // TODO: macro substitution in #include

    let (args, args_text_range) = get_pp_directive_args(input, directive, text_range);

    let (relative, split) = match args.as_bytes().first() {
        Some(b'<') => (false, args.get(1..).expect("safe").split_once('>')),
        Some(b'"') => (true, args.get(1..).expect("safe").split_once('"')),
        None => {
            diag.emit(Diagnostic::new(
                args_text_range,
                Cow::Borrowed("Expected an argument"),
                Severity::Error,
            ));
            return None;
        }
        _ => {
            diag.emit(Diagnostic::new(
                args_text_range,
                Cow::Borrowed("Unexpected character in argument, expected `\"` or `<`."),
                Severity::Error,
            ));
            return None;
        }
    };

    let Some((s, rest)) = split else {
        diag.emit(Diagnostic::new(
            args_text_range,
            Cow::Borrowed("Missing string terminator"),
            Severity::Error,
        ));
        return None;
    };

    if !rest.is_empty() {
        diag.emit(Diagnostic::new(
            args_text_range,
            Cow::Borrowed("Unexpected characters after include string"),
            Severity::Warn,
        ));
    }

    Some((relative, s.to_owned()))
}

fn pp_cond_directive_eval(
    db: &dyn BaseDb,
    env: &mut MacroEnvMut,
    directive: &ast::PreprocessorDirective,
    diag: &impl DiagnosticCollector,
) -> Option<bool> {
    let input = directive.syntax().text();

    let directive_text_range = directive.syntax().text_range();

    let Some(directive_name) = directive.syntax().green.kind.preprocessor_directive_name() else {
        diag.emit(Diagnostic::new(
            directive_text_range,
            Cow::Borrowed("Internal compiler error: preprocessor directive should have a name"),
            Severity::Warn,
        ));
        return None;
    };

    let (mut condition, mut condition_text_range) =
        get_pp_directive_args(input, directive_name, directive_text_range);

    if directive_name == "ifdef" || directive_name == "elifdef" {
        // TODO: add a test for this and preprocessor directives in general
        condition.insert_str(0, "defined(");
        condition.push(')');

        // TODO: proper trmaps...
        condition_text_range =
            condition_text_range.offset_wrapping_signed(-"defined(".len().cast_signed());
        condition_text_range.end -= 1;
    } else if directive_name == "ifndef" || directive_name == "elifndef" {
        condition.insert_str(0, "!defined(");
        condition.push(')');

        condition_text_range =
            condition_text_range.offset_wrapping_signed(-"!defined(".len().cast_signed());
        condition_text_range.end -= 1;
    } else if directive_name == "else" {
        if !condition.is_empty() {
            diag.emit(Diagnostic::new(
                condition_text_range,
                Cow::Borrowed("Extra arguments to `#else`"),
                Severity::Warn,
            ));
        }

        // Else branch is always enabled
        return Some(true);
    }

    let parse = dt_parser::parser::Entrypoint::PreprocessorConditional.parse(&condition);

    let offset_diag = OffsetDiagnosticCollector {
        inner: diag,
        offset: condition_text_range.start,
    };

    if !parse.lex_errors.is_empty() || !parse.errors.is_empty() {
        let earliest_lex_error_range = parse.lex_errors.first().map(|e| e.text_range);
        for lex_error in &parse.lex_errors {
            offset_diag.emit(Diagnostic::new(
                lex_error.text_range,
                format!("{} [lex error]", lex_error.inner).into(),
                Severity::Error,
            ));
        }

        for error in &parse.errors {
            if let Some(earliest_lex_error_range) = earliest_lex_error_range {
                if error.primary_span.start >= earliest_lex_error_range.start {
                    break;
                }
            }

            let mut diagnostic = Diagnostic {
                span: MultiSpan {
                    primary_spans: vec![error.primary_span],
                    span_labels: error.span_labels.clone(),
                },
                msg: error.message.clone(),
                severity: Severity::Error,
            };
            super::tag_diagnostic(&mut diagnostic, "dt-tools(syntax-error)");
            offset_diag.emit(diagnostic);
        }
        return None;
    }

    let expr_ast = parse.red_node().child_nodes().find_map(ast::Expr::cast)?;

    let val = super::expr_eval::eval(db, env, expr_ast, &offset_diag)?;

    Some(val != 0)
}

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
///   - On a conditional, evalute the conditions of the branches and choose the correct branch, if any.
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

    for item in file_ast.items() {
        handle_toplevel_item(
            db,
            &mut env,
            &diag,
            parent_path,
            include_dirs,
            &mut toplevels,
            item,
        );
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

fn handle_toplevel_item<'db>(
    db: &'db dyn BaseDb,
    env: &mut MacroEnvMut<'db>,
    diag: &impl DiagnosticCollector,
    parent_path: &Utf8Path,
    include_dirs: &[Utf8PathBuf],
    toplevels: &mut Vec<PpEvalFileResultToplevel<'db>>,
    item: ast::ToplevelItem,
) {
    let files = db.get_files();

    let text_range = item.syntax().text_range();

    let inner: PpEvalFileResultToplevelInner = match item {
        ast::ToplevelItem::Node(dt_node) => {
            // TODO: recurse!!!
            return;
        }
        ast::ToplevelItem::Directive(dir) => {
            let mut iter = dir.syntax().child_tokens();

            // Skip until DtIncludeDirective is found, continue items if it isn't found
            if !iter.any(|tok| tok.green.kind == TokenKind::DtIncludeDirective) {
                return;
            }

            let Some(string_tok) = iter.find(|tok| tok.green.kind == TokenKind::String) else {
                return;
            };

            let path = match dt_analyzer::string::interpret_escaped_string(string_tok.text()) {
                Ok(path) => path,
                Err(err) => {
                    diag.emit(Diagnostic::new(
                        string_tok.text_range(),
                        Cow::Owned(err.to_string()),
                        Severity::Error,
                    ));
                    return;
                }
            };

            let Some(file) = possible_include_paths_utf8(true, &path, parent_path, include_dirs)
                .map(|path| files.get_file(db, &path))
                .find(|file| file.is_readable_file(db))
            else {
                diag.emit(Diagnostic::new(
                    text_range,
                    Cow::Owned(format!(
                        "Couldn't find file to include from include_dirs={include_dirs:?}"
                    )),
                    Severity::Error,
                ));
                return;
            };

            let result = preprocessor_eval_file(
                db,
                file,
                Some(std::mem::take(env).into_salsa_tracked_struct(db)),
            )
            .expect("The file should exist, its existence is confirmed above");

            *env = MacroEnvMut {
                parent: Some(result.env_after(db)),
                own_map: FxHashMap::default(),
            };

            PpEvalFileResultToplevelInner::Include {
                is_preprocessor: false,
                file,
                result,
            }
        }
        ast::ToplevelItem::PreprocessorConditional(preprocessor_conditional) => {
            let Some((_dir, branch)) =
                preprocessor_conditional.branches().find(|(dir, _branch)| {
                    pp_cond_directive_eval(db, env, dir, &diag).is_some_and(|val| val)
                })
            else {
                return;
            };

            for item in branch.items() {
                handle_toplevel_item(db, env, diag, parent_path, include_dirs, toplevels, item);
            }
            return;
        }
        ast::ToplevelItem::PreprocessorDirective(dir) => match dir.kind() {
            TokenKind::PragmaDirective => {
                // TODO: implement #pragma

                diag.emit(Diagnostic::new(
                    text_range,
                    Cow::Borrowed("`#pragma` is unimplemented"),
                    Severity::Error,
                ));
                return;
            }
            TokenKind::DefineDirective => {
                let parsed = match MacroDefinition::parse(dir.syntax().text()) {
                    Ok(inc) => inc,
                    Err(err) => {
                        diag.emit(Diagnostic::new(
                            if let Some(local_range) = err.text_range() {
                                local_range.offset(text_range.start)
                            } else {
                                text_range
                            },
                            Cow::Owned(err.to_string()),
                            Severity::Error,
                        ));
                        return;
                    }
                };

                let name_clone = parsed.name.clone();
                env.own_map.insert(parsed.name.clone(), Some(parsed));

                PpEvalFileResultToplevelInner::MacroDefinition { name: name_clone }
            }
            TokenKind::UndefDirective => {
                let input = dir.syntax().text();
                let (args, args_text_range) = get_pp_directive_args(input, "undef", text_range);

                if args.contains(' ') {
                    diag.emit(Diagnostic::new(
                        args_text_range,
                        Cow::Borrowed("Arguments to `#undef` should be just a macro name"),
                        Severity::Error,
                    ));
                    return;
                }

                env.own_map.insert(args, None);

                PpEvalFileResultToplevelInner::Undef
            }
            TokenKind::IncludeDirective => {
                // TODO: real evaluation and macro substitution
                let input = dir.syntax().text();

                let Some((relative, path)) =
                    get_pp_directive_arg_string(input, "include", text_range, &diag)
                else {
                    return;
                };

                let Some(file) =
                    possible_include_paths_utf8(relative, &path, parent_path, include_dirs)
                        .map(|path| files.get_file(db, &path))
                        .find(|file| file.is_readable_file(db))
                else {
                    diag.emit(Diagnostic::new(
                        text_range,
                        Cow::Owned(format!(
                            "Couldn't find file to include from include_dirs={include_dirs:?}"
                        )),
                        Severity::Error,
                    ));
                    return;
                };

                let result = preprocessor_eval_file(
                    db,
                    file,
                    Some(std::mem::take(env).into_salsa_tracked_struct(db)),
                )
                .expect("The file should exist, its existence is confirmed above");

                *env = MacroEnvMut {
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
                let (args, _args_text_range) = get_pp_directive_args(input, "error", text_range);

                diag.emit(Diagnostic::new(
                    text_range,
                    // TODO: remove debug thing
                    Cow::Owned(format!(
                        "`#error`: {args:?}, defined {args:?}={}",
                        env.get_macro(db, &args).is_some()
                    )),
                    Severity::Error,
                ));
                return;
            }
            _ => return,
        },
    };

    // TODO: PERF: Salsa tracked? currently this breaks all Salsa tracking...
    env.flatten_ancestors(db);

    toplevels.push(PpEvalFileResultToplevel::new(
        db,
        text_range,
        inner,
        env.to_salsa_tracked_struct_mut(db),
    ));
}
