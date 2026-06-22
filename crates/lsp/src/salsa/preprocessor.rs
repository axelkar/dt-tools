//! Preprocessor macros, conditionals and includes are all tied very much together, which means they must be evaluated in order.
//!
//! This module also handles [`ast::DtNode`] processing and MIR emission.
// TODO: split to multiple modules!
// TODO: handle recursive macros?

use std::{borrow::Cow, sync::Arc};

use camino::{Utf8Path, Utf8PathBuf};
use dt_tools_analyzer::macros::MacroDefinition;
use dt_tools_diagnostic::{Diagnostic, DiagnosticCollector, Severity, Span};
use dt_tools_parser::{
    TextRange,
    ast::{
        self, AstNode, AstNodeOrToken, AstToken, HasDtPhandle, HasLabels, HasMacroInvocation,
        HasName, HasUnitAddress,
    },
    cst::RedNode,
    lexer::TokenKind,
    parser::Entrypoint,
};

use crate::salsa::{
    db::BaseDb,
    emit_parse_errors,
    expr_eval::{interpret_escaped_char_tok, parse_int_tok},
    file::File,
    includes::IncludeDirs,
    macros::{
        MacroCtx,
        env::{TrackedMapEnv, TrackedMapEnvMut},
        substitute_macro_tok,
    },
    mir::{
        Mir, MirCell, MirDefinition, MirDefinitionValue, MirNodeData, MirPhandleTarget,
        MirPropertyData, MirProvenance, MirValue, UnresolvedExtension,
    },
};

#[salsa::tracked]
pub struct PpEvalFileResult<'db> {
    /// Macro and label environment after processing this file.
    #[tracked]
    pub env_after: TrackedMapEnv<'db>,

    #[tracked]
    #[returns(ref)]
    pub diagnostics: Vec<Diagnostic<File>>,

    #[tracked]
    #[returns(ref)]
    pub processed_files: Vec<File>,

    #[tracked]
    #[returns(ref)]
    pub includes: Vec<(File, Span<File>)>,

    #[tracked]
    #[returns(ref)]
    pub mir: Mir,

    /// Whether this file is part of a plugin/overlay (`/plugin/;`).
    pub is_overlay: bool,
}

/// Lists possible file paths under `parent_dir_path` or `include_dirs`, depending on [`relative`](Self::relative).
pub fn possible_include_paths_utf8<'a, P: AsRef<Utf8Path>>(
    relative: bool,
    path: &'a str,
    parent_dir_path: &'a Utf8Path,
    include_dirs: &'a [P],
) -> impl Iterator<Item = Utf8PathBuf> + use<'a, P> {
    relative
        .then_some(parent_dir_path)
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
    diag: &impl DiagnosticCollector<File>,
    spanner: &mut impl FnMut(TextRange) -> Span<File>,
) -> Option<(bool, String)> {
    // TODO: macro substitution in #include

    let (args, args_text_range) = get_pp_directive_args(input, directive, text_range);

    let (relative, split) = match args.as_bytes().first() {
        Some(b'<') => (false, args.get(1..).expect("safe").split_once('>')),
        Some(b'"') => (true, args.get(1..).expect("safe").split_once('"')),
        None => {
            diag.emit(Diagnostic::new(
                spanner(args_text_range),
                Cow::Borrowed("Expected an argument"),
                Severity::Error,
            ));
            return None;
        }
        _ => {
            diag.emit(Diagnostic::new(
                spanner(args_text_range),
                Cow::Borrowed("Unexpected character in argument, expected `\"` or `<`."),
                Severity::Error,
            ));
            return None;
        }
    };

    let Some((s, rest)) = split else {
        diag.emit(Diagnostic::new(
            spanner(args_text_range),
            Cow::Borrowed("Missing string terminator"),
            Severity::Error,
        ));
        return None;
    };

    if !rest.is_empty() {
        diag.emit(Diagnostic::new(
            spanner(args_text_range),
            Cow::Borrowed("Unexpected characters after include string"),
            Severity::Warn,
        ));
    }

    Some((relative, s.to_owned()))
}

fn pp_cond_directive_eval(
    db: &dyn BaseDb,
    env: &mut TrackedMapEnvMut,
    file: File,
    directive: &ast::PreprocessorDirective,
    diag: &impl DiagnosticCollector<File>,
) -> Option<bool> {
    let input = directive.syntax().text();

    let directive_text_range = directive.syntax().text_range();

    let Some(directive_name) = directive.syntax().green.kind.preprocessor_directive_name() else {
        diag.emit(Diagnostic::new(
            directive_text_range.within_file(file),
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
                condition_text_range.within_file(file),
                Cow::Borrowed("Extra arguments to `#else`"),
                Severity::Warn,
            ));
        }

        // Else branch is always enabled
        return Some(true);
    }

    let parse = dt_tools_parser::parser::Entrypoint::PreprocessorConditional.parse(&condition);

    emit_parse_errors(&parse, &diag, &mut |text_range| {
        text_range
            .offset(condition_text_range.start)
            .within_file(file)
    });

    let expr_ast = RedNode::new_offset(Arc::new(parse.green_node), condition_text_range.start)
        .child_nodes()
        .find_map(ast::Expr::cast)?;

    let val = super::expr_eval::eval(db, env, expr_ast, &diag, &mut |tr| tr.within_file(file))?;

    Some(val != 0)
}

pub fn preprocessor_eval_root_file(
    db: &dyn BaseDb,
    root_file: File,
) -> Option<PpEvalFileResult<'_>> {
    // Detect /plugin/; in the root file.
    let is_overlay = crate::salsa::parse_file(db, root_file).is_some_and(|p| {
        p.parse(db).source_file().directives().any(|dir| {
            dir.syntax()
                .child_tokens()
                .any(|tok| tok.green.kind == TokenKind::PluginDirective)
        })
    });

    preprocessor_eval_file(db, root_file, None, is_overlay)
}

// TODO: rename this function as it actually returns MIR and does everything in "one pass"
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
    file: File,
    parent_env: Option<TrackedMapEnv<'db>>,
    is_overlay: bool,
) -> Option<PpEvalFileResult<'db>> {
    let span = profiling::tracy_client::span!("lsp::salsa::preprocessor::preprocessor_eval_file");
    span.emit_text(file.path(db).as_str());

    let mut env = TrackedMapEnvMut::from_parent(parent_env);

    let parse = super::parse_file(db, file)?;
    let file_ast = parse.parse(db).source_file();

    let mut diagnostics = Vec::new();
    let diag = parking_lot::Mutex::new(&mut diagnostics);

    // Path of the current file's parent directory.
    let parent_dir_path = file.path(db).parent()?;
    let include_dirs = IncludeDirs::get(db).include_dirs(db);

    let mut processed_files = Vec::new();
    let mut includes = Vec::new();
    let mut mir = Mir::default();
    let mut ctx = IntraFileCtx {
        db,
        file,
        env: &mut env,
        diag: &diag,
        mir: &mut mir,
        is_overlay,
    };

    // TODO: split into phases with includes and after includes.

    for item in file_ast.items() {
        handle_toplevel_item(
            &mut ctx,
            parent_dir_path,
            include_dirs,
            &mut processed_files,
            &mut includes,
            item,
        );
    }

    super::tag_diagnostics(
        &mut diagnostics,
        concat!(module_path!(), "::preprocessor_eval_file"),
    );

    Some(PpEvalFileResult::new(
        db,
        env.into_immut(db),
        diagnostics,
        processed_files,
        includes,
        mir,
        is_overlay,
    ))
}

struct IntraFileCtx<'a, 'db, D: DiagnosticCollector<File>> {
    db: &'db dyn BaseDb,
    file: File,
    env: &'a mut TrackedMapEnvMut<'db>,
    diag: &'a D,
    mir: &'a mut Mir,
    is_overlay: bool,
}

#[expect(clippy::too_many_lines, reason = "hard to make this shorter")]
fn handle_toplevel_item(
    ctx: &mut IntraFileCtx<'_, '_, impl DiagnosticCollector<File>>,
    parent_dir_path: &Utf8Path,
    include_dirs: &[Utf8PathBuf],
    processed_files: &mut Vec<File>,
    includes: &mut Vec<(File, Span<File>)>,
    item: ast::ToplevelItem,
) {
    let files = ctx.db.get_files();

    let text_range = item.syntax_item().text_range();

    match item {
        ast::ToplevelItem::Node(dt_node) => {
            process_dt_node(ctx, "", &dt_node);
        }
        ast::ToplevelItem::Directive(dir) => {
            handle_directive(
                ctx,
                processed_files,
                includes,
                "",
                &dir,
                include_dirs,
                parent_dir_path,
                files,
            );
        }
        ast::ToplevelItem::PreprocessorConditional(preprocessor_conditional) => {
            let Some((_dir, branch)) =
                preprocessor_conditional.branches().find(|(dir, _branch)| {
                    pp_cond_directive_eval(ctx.db, ctx.env, ctx.file, dir, &ctx.diag)
                        .is_some_and(|val| val)
                })
            else {
                return;
            };
            for item in branch.items() {
                handle_toplevel_item(
                    ctx,
                    parent_dir_path,
                    include_dirs,
                    processed_files,
                    includes,
                    item,
                );
            }
        }
        ast::ToplevelItem::PreprocessorDirective(dir) => match dir.kind() {
            TokenKind::PragmaDirective => {
                // TODO: implement #pragma
                ctx.diag.emit(Diagnostic::new(
                    text_range.within_file(ctx.file),
                    Cow::Borrowed("`#pragma` is unimplemented"),
                    Severity::Error,
                ));
            }
            TokenKind::DefineDirective => match MacroDefinition::parse(dir.syntax().text()) {
                Ok(parsed) => ctx.env.insert_macro(parsed),
                Err(err) => {
                    ctx.diag.emit(Diagnostic::new(
                        if let Some(local_range) = err.text_range() {
                            local_range.offset(text_range.start)
                        } else {
                            text_range
                        }
                        .within_file(ctx.file),
                        Cow::Owned(err.to_string()),
                        Severity::Error,
                    ));
                }
            },
            TokenKind::UndefDirective => {
                let input = dir.syntax().text();
                let (args, args_text_range) = get_pp_directive_args(input, "undef", text_range);
                if args.contains(' ') {
                    ctx.diag.emit(Diagnostic::new(
                        args_text_range.within_file(ctx.file),
                        Cow::Borrowed("Arguments to `#undef` should be just a macro name"),
                        Severity::Error,
                    ));
                } else {
                    ctx.env.own_macro_map.insert(args, None);
                }
            }
            TokenKind::IncludeDirective => {
                // TODO: real evaluation and macro substitution
                let input = dir.syntax().text();
                let Some((relative, include_path)) = get_pp_directive_arg_string(
                    input,
                    "include",
                    text_range,
                    &ctx.diag,
                    &mut |tr| tr.within_file(ctx.file),
                ) else {
                    return;
                };

                let Some(include_file) = possible_include_paths_utf8(
                    relative,
                    &include_path,
                    parent_dir_path,
                    include_dirs,
                )
                .map(|path| files.get_file(ctx.db, &path))
                .find(|file| file.is_readable_file(ctx.db)) else {
                    ctx.diag.emit(Diagnostic::new(
                        text_range.within_file(ctx.file),
                        Cow::Owned(format!("Couldn't find file to include: {include_path}")),
                        Severity::Error,
                    ));
                    return;
                };

                processed_files.push(include_file);
                includes.push((include_file, text_range.within_file(ctx.file)));

                let result = preprocessor_eval_file(
                    ctx.db,
                    include_file,
                    Some(std::mem::take(ctx.env).into_immut(ctx.db)),
                    ctx.is_overlay,
                )
                .expect("The file should exist, its existence is confirmed above");

                *ctx.env = result.env_after(ctx.db).to_mut();
                ctx.mir.merge(result.mir(ctx.db));
                result
                    .diagnostics(ctx.db)
                    .iter()
                    .for_each(|diagnostic| ctx.diag.emit(diagnostic.clone()));
                processed_files.extend_from_slice(result.processed_files(ctx.db));
                includes.extend_from_slice(result.includes(ctx.db));

                // TODO: PERF: Salsa tracked? currently this breaks all Salsa tracking...
                ctx.env.flatten_ancestors(ctx.db);
            }
            TokenKind::ErrorDirective => {
                let input = dir.syntax().text();
                let (args, _args_text_range) = get_pp_directive_args(input, "error", text_range);

                // TODO: remove debug thing?
                ctx.diag.emit(Diagnostic::new(
                    text_range.within_file(ctx.file),
                    Cow::Owned(format!(
                        "`#error`: {args:?}, defined {args:?}={}",
                        ctx.env.get_macro(ctx.db, &args).is_some()
                    )),
                    Severity::Error,
                ));
            }
            _ => {}
        },
    }
}

/// Handle a DTS directive: `/include/`, `/delete-node/`, `/delete-property/`.
fn handle_directive(
    ctx: &mut IntraFileCtx<'_, '_, impl DiagnosticCollector<File>>,
    processed_files: &mut Vec<File>,
    includes: &mut Vec<(File, Span<File>)>,
    path_prefix: &str,
    dir: &ast::Directive,
    include_dirs: &[Utf8PathBuf],
    parent_dir_path: &Utf8Path,
    files: &crate::salsa::file::Files,
) {
    let tokens: Vec<_> = dir.syntax().child_tokens().collect();
    let text_range = dir.syntax().text_range();

    if tokens
        .iter()
        .any(|tok| tok.green.kind == TokenKind::DtIncludeDirective)
    {
        // /include/ "path"
        let Some(string_tok) = tokens
            .iter()
            .find(|tok| tok.green.kind == TokenKind::String)
        else {
            return;
        };

        let include_path =
            match dt_tools_analyzer::string::interpret_escaped_string(string_tok.text()) {
                Ok(path) => path,
                Err(err) => {
                    ctx.diag.emit(Diagnostic::new(
                        string_tok.text_range().within_file(ctx.file),
                        Cow::Owned(format!("Failed to parse string: {err}")),
                        Severity::Error,
                    ));
                    return;
                }
            };

        let Some(include_file) =
            possible_include_paths_utf8(true, &include_path, parent_dir_path, include_dirs)
                .map(|path| files.get_file(ctx.db, &path))
                .find(|f| f.is_readable_file(ctx.db))
        else {
            ctx.diag.emit(Diagnostic::new(
                text_range.within_file(ctx.file),
                Cow::Owned(format!("Couldn't find file to include: {include_path}")),
                Severity::Error,
            ));
            return;
        };

        processed_files.push(include_file);
        includes.push((include_file, text_range.within_file(ctx.file)));

        let result = preprocessor_eval_file(
            ctx.db,
            include_file,
            Some(std::mem::take(ctx.env).into_immut(ctx.db)),
            ctx.is_overlay,
        )
        .expect("file exists");

        *ctx.env = result.env_after(ctx.db).to_mut();
        ctx.mir.merge(result.mir(ctx.db));
        result
            .diagnostics(ctx.db)
            .iter()
            .for_each(|diagnostic| ctx.diag.emit(diagnostic.clone()));
        processed_files.extend_from_slice(result.processed_files(ctx.db));
        includes.extend_from_slice(result.includes(ctx.db));

        // TODO: PERF: Salsa tracked? currently this breaks all Salsa tracking...
        ctx.env.flatten_ancestors(ctx.db);
    } else {
        emit_delete_directive(ctx, path_prefix, dir);
    }
}

/// Build a node/property path. Handles the root node `/` and avoids `//`.
fn build_path(parent: &str, name: &str) -> String {
    if name == "/" {
        "/".to_owned()
    } else if parent == "/" {
        format!("/{name}")
    } else {
        format!("{parent}/{name}")
    }
}

fn resolve_name_or_macro<'db, Ast: HasName + HasMacroInvocation>(
    db: &'db dyn BaseDb,
    env: &mut TrackedMapEnvMut<'db>,
    diag: &impl DiagnosticCollector<File>,
    spanner: &mut impl FnMut(TextRange) -> Span<File>,
    ast: &Ast,
) -> Option<String> {
    if let Some(name_ast) = ast.name() {
        Some(
            substitute_macro_tok(db, env, diag, spanner, &MacroCtx::Implicit(&name_ast))
                .map(|(_span, _trmaps, expanded)| expanded)
                .unwrap_or(name_ast.syntax().text().as_str().to_owned()),
        )
    } else if let Some(invoc) = ast.macro_invocation() {
        substitute_macro_tok(db, env, diag, spanner, &MacroCtx::Explicit(&invoc))
            .map(|(_span, _trmaps, expanded)| expanded)
    } else {
        None
    }
}

fn get_name_and_unit_addr<'db, Ast: HasName + HasMacroInvocation + HasUnitAddress>(
    db: &'db dyn BaseDb,
    env: &mut TrackedMapEnvMut<'db>,
    diag: &impl DiagnosticCollector<File>,
    spanner: &mut impl FnMut(TextRange) -> Span<File>,
    ast: &Ast,
) -> Option<String> {
    let without_unit_addr = resolve_name_or_macro(db, env, diag, spanner, ast);

    let unit_addr = ast
        .unit_address()
        .and_then(|ast| resolve_name_or_macro(db, env, diag, spanner, &ast));

    without_unit_addr.map(|without_unit_addr| {
        if let Some(unit_addr) = unit_addr {
            format!("{without_unit_addr}@{unit_addr}")
        } else {
            without_unit_addr
        }
    })
}

/// Process an [`ast::DtNode`] and its subtree into flat [`MirDefinition`]s.
fn process_dt_node(
    ctx: &mut IntraFileCtx<'_, '_, impl DiagnosticCollector<File>>,
    path_prefix: &str,
    dt_node: &ast::DtNode,
) {
    let provenance = MirProvenance {
        file: ctx.file,
        text_range: dt_node.syntax().text_range(),
    };

    // Handle extensions: &label { } or &{/path} { }
    if let Some(phandle) = dt_node.extension_name() {
        let Some(target) = convert_phandle(
            ctx.db,
            ctx.env,
            ctx.diag,
            &mut |tr| tr.within_file(ctx.file),
            &phandle,
        ) else {
            return;
        };

        let target_path = match &target {
            MirPhandleTarget::Label(name) => ctx
                .env
                .get_label(ctx.db, name)
                .map(std::borrow::ToOwned::to_owned),
            MirPhandleTarget::Path(path) => Some(path.clone()),
        };
        if let Some(ref target_path) = target_path {
            let labels = collect_labels(ctx, dt_node, target_path);

            // Emit the node definition.
            ctx.mir.definitions.push(MirDefinition {
                path: target_path.clone(),
                value: MirDefinitionValue::Node(MirNodeData { labels }),
                provenance,
            });

            process_dt_node_body(ctx, target_path, dt_node);
        } else if ctx.is_overlay {
            // Overlay; we can leave the extension unresolved
            let mut body_mir = Mir::default();
            process_dt_node_body(
                &mut IntraFileCtx {
                    db: ctx.db,
                    file: ctx.file,
                    env: ctx.env,
                    diag: ctx.diag,
                    mir: &mut body_mir,
                    is_overlay: ctx.is_overlay,
                },
                "",
                dt_node,
            );

            // Propagate any nested unresolved extensions.
            ctx.mir
                .unresolved_extensions
                .extend(body_mir.unresolved_extensions);

            if let MirPhandleTarget::Label(label) = target {
                ctx.mir.unresolved_extensions.push(UnresolvedExtension {
                    label,
                    body: body_mir.definitions,
                    provenance: provenance.clone(),
                });
            }
        } else {
            // DTC wants extensions to be resolved from items above/before the extensions in
            // non-overlay mode.

            // Process so diagnostics are emitted
            let mut body_mir = Mir::default();
            process_dt_node_body(
                &mut IntraFileCtx {
                    db: ctx.db,
                    file: ctx.file,
                    env: ctx.env,
                    diag: ctx.diag,
                    mir: &mut body_mir,
                    is_overlay: ctx.is_overlay,
                },
                "",
                dt_node,
            );

            ctx.diag.emit(Diagnostic::new(
                phandle.syntax().text_range().within_file(ctx.file),
                Cow::Owned(format!("Label not found: {target}")),
                Severity::Error,
            ));
        }
        return;
    }

    let name = get_name_and_unit_addr(
        ctx.db,
        ctx.env,
        ctx.diag,
        &mut |tr| tr.within_file(ctx.file),
        dt_node,
    )
    .or_else(|| {
        // Root node
        dt_node
            .syntax()
            .child_tokens()
            .any(|tok| tok.green.kind == TokenKind::Slash)
            .then(String::new)
    });

    if let Some(name) = name {
        // Build the node's full path.
        let node_path = build_path(path_prefix, &name);

        let labels = collect_labels(ctx, dt_node, &node_path);

        // Emit the node definition.
        ctx.mir.definitions.push(MirDefinition {
            path: node_path.clone(),
            value: MirDefinitionValue::Node(MirNodeData { labels }),
            provenance,
        });

        // Process the node body: subnodes and properties.
        process_dt_node_body(ctx, &node_path, dt_node);
    } else {
        // Name is missing, but process so diagnostics are emitted
        let mut body_mir = Mir::default();
        process_dt_node_body(
            &mut IntraFileCtx {
                db: ctx.db,
                file: ctx.file,
                env: ctx.env,
                diag: ctx.diag,
                mir: &mut body_mir,
                is_overlay: ctx.is_overlay,
            },
            "",
            dt_node,
        );
    }
}

// Collect labels defined on an [`ast::DtNode`].
fn collect_labels(
    ctx: &mut IntraFileCtx<'_, '_, impl DiagnosticCollector<File>>,
    dt_node: &ast::DtNode,
    node_path: &str,
) -> Vec<String> {
    let mut labels: Vec<String> = Vec::new();
    for label_ast in dt_node.labels() {
        if let Some(label_name) = resolve_name_or_macro(
            ctx.db,
            ctx.env,
            ctx.diag,
            &mut |tr| tr.within_file(ctx.file),
            &label_ast,
        ) {
            // Check for duplicate labels (DTC: no duplicates globally).
            if ctx.env.get_label(ctx.db, &label_name).is_some() {
                // TODO: MultiSpan & cross-file diagnostics
                ctx.diag.emit(Diagnostic::new(
                    label_ast.syntax().text_range().within_file(ctx.file),
                    Cow::Owned(format!("Duplicate label `{label_name}`")),
                    Severity::Error,
                ));
            } else {
                ctx.env
                    .own_label_map
                    .insert(label_name.clone(), Some(node_path.to_owned()));
                labels.push(label_name);
            }
        }
    }
    labels
}

/// Process the body of an [`ast::DtNode`]: subnodes and properties.
fn process_dt_node_body(
    ctx: &mut IntraFileCtx<'_, '_, impl DiagnosticCollector<File>>,
    parent_node_path: &str,
    dt_node: &ast::DtNode,
) {
    // TODO: preprocessor conditionals in nodes

    for item in dt_node.node_items() {
        match item {
            ast::NodeItem::DtProperty(prop) => {
                if let Some(def) = process_dt_property(ctx, parent_node_path, &prop) {
                    ctx.mir.definitions.push(def);
                }
            }
            ast::NodeItem::DtNode(subnode) => {
                process_dt_node(ctx, parent_node_path, &subnode);
            }
            ast::NodeItem::Directive(dir) => {
                emit_delete_directive(ctx, parent_node_path, &dir);
            }
        }
    }
}

/// Emit [`MirDefinitionValue::DeletedNode`] or [`MirDefinitionValue::DeletedProperty`] from a directive.
fn emit_delete_directive(
    ctx: &mut IntraFileCtx<'_, '_, impl DiagnosticCollector<File>>,
    path_prefix: &str,
    dir: &ast::Directive,
) {
    let kind = dir.kind();
    let Some(args) = dir.arguments() else { return };

    let text_range = dir.syntax().text_range();
    let provenance = MirProvenance {
        file: ctx.file,
        text_range,
    };

    if kind == Some(TokenKind::DeleteNodeDirective) {
        let target_path = if let Some(name_tok) = args.name() {
            build_path(path_prefix, name_tok.syntax().text().as_str())
        } else if let Some(phandle) = args.dt_phandle() {
            let Some(target) = convert_phandle(
                ctx.db,
                ctx.env,
                ctx.diag,
                &mut |tr| tr.within_file(ctx.file),
                &phandle,
            ) else {
                return;
            };

            match &target {
                MirPhandleTarget::Label(name) => {
                    if let Some(path) = ctx.env.get_label(ctx.db, name) {
                        path.to_owned()
                    } else {
                        // Couldn't resolve it.
                        if ctx.is_overlay {
                            // TODO: handle unresolved delete-node!
                        } else {
                            ctx.diag.emit(Diagnostic::new(
                                text_range.within_file(ctx.file),
                                Cow::Owned(format!("Label not found: {name}")),
                                Severity::Error,
                            ));
                        }
                        return;
                    }
                }
                MirPhandleTarget::Path(path) => path.clone(),
            }
        } else {
            return;
        };

        ctx.mir.definitions.push(MirDefinition {
            path: target_path,
            value: MirDefinitionValue::DeletedNode,
            provenance,
        });
    } else if kind == Some(TokenKind::DeletePropertyDirective)
        && let Some(name_tok) = args.name()
    {
        let target_path = build_path(path_prefix, name_tok.syntax().text().as_str());

        ctx.mir.definitions.push(MirDefinition {
            path: target_path,
            value: MirDefinitionValue::DeletedProperty,
            provenance,
        });
    }
}

/// Convert a [`ast::DtProperty`] into a [`MirDefinition`], if valid.
///
/// Returns `None` if the property has no name or can't be processed.
fn process_dt_property(
    ctx: &mut IntraFileCtx<'_, '_, impl DiagnosticCollector<File>>,
    parent_node_path: &str,
    prop: &ast::DtProperty,
) -> Option<MirDefinition> {
    let mut values = Vec::new();
    for value_ast in prop.values() {
        if let Some(value) = convert_prop_value(
            ctx.db,
            ctx.env,
            ctx.diag,
            &mut |tr| tr.within_file(ctx.file),
            &value_ast,
        ) {
            values.push(value);
        }
    }

    let name = get_name_and_unit_addr(
        ctx.db,
        ctx.env,
        ctx.diag,
        &mut |tr| tr.within_file(ctx.file),
        prop,
    )?;
    let path = build_path(parent_node_path, &name);

    let text_range = prop.syntax().text_range();
    let provenance = MirProvenance {
        file: ctx.file,
        text_range,
    };

    Some(MirDefinition {
        path,
        value: MirDefinitionValue::Property(MirPropertyData { values }),
        provenance,
    })
}

/// Convert an AST property value to a [`MirValue`].
fn convert_prop_value<'db>(
    db: &'db dyn BaseDb,
    env: &mut TrackedMapEnvMut<'db>,
    diag: &impl DiagnosticCollector<File>,
    spanner: &mut impl FnMut(TextRange) -> Span<File>,
    value: &ast::PropValue,
) -> Option<MirValue> {
    match value {
        ast::PropValue::String(tok) => {
            match dt_tools_analyzer::string::interpret_escaped_string(tok.text()) {
                Ok(path) => Some(MirValue::String(path)),
                Err(err) => {
                    diag.emit(Diagnostic::new(
                        spanner(tok.text_range()),
                        Cow::Owned(format!("Failed to parse string: {err}")),
                        Severity::Error,
                    ));
                    None
                }
            }
        }
        ast::PropValue::CellList(cell_list) => {
            let cells: Vec<MirCell> = cell_list
                .cells()
                .filter_map(|cell| convert_cell(db, env, diag, spanner, &cell))
                .collect();
            Some(MirValue::CellList(cells))
        }
        ast::PropValue::Bytestring(tok) => {
            let mut bytes = Vec::new();
            let mut first_nibble = None;
            for ch in tok.text().chars() {
                if ch == ']' {
                    break;
                }
                #[expect(
                    clippy::cast_possible_truncation,
                    reason = "to_digit returns values 0-15"
                )]
                if let Some(nibble) = ch.to_digit(16) {
                    let nibble = nibble as u8;
                    if let Some(first_nibble) = first_nibble.take() {
                        bytes.push(nibble + (first_nibble << 4));
                    } else {
                        first_nibble = Some(nibble);
                    }
                }
            }

            if first_nibble.is_some() {
                diag.emit(Diagnostic::new(
                    spanner(tok.text_range()),
                    Cow::Borrowed("Bytestring is missing a hex digit"),
                    Severity::Error,
                ));
                return None;
            }

            Some(MirValue::Bytestring(bytes))
        }
        ast::PropValue::Phandle(phandle) => {
            let target = convert_phandle(db, env, diag, spanner, phandle)?;
            Some(MirValue::Phandle(target))
        }
        ast::PropValue::Macro(macro_inv) => resolve_macro_to_value(
            db,
            env,
            diag,
            spanner,
            &MacroCtx::Explicit(macro_inv),
            Entrypoint::PropValues,
            convert_prop_value,
        ),
    }
}

/// Convert an AST cell to a [`MirCell`].
fn convert_cell<'db>(
    db: &'db dyn BaseDb,
    env: &mut TrackedMapEnvMut<'db>,
    diag: &impl DiagnosticCollector<File>,
    spanner: &mut impl FnMut(TextRange) -> Span<File>,
    cell: &ast::Cell,
) -> Option<MirCell> {
    match cell {
        ast::Cell::Number(tok) => parse_int_tok::<u32>(
            tok,
            diag,
            spanner,
            u32::from_str_radix,
            "32-bit unsigned integer",
        )
        .map(MirCell::U32),
        ast::Cell::Char(tok) => {
            let val = interpret_escaped_char_tok(tok, diag, spanner)?;

            Some(MirCell::U32(val as u32))
        }
        ast::Cell::Phandle(phandle) => Some(MirCell::Phandle(convert_phandle(
            db, env, diag, spanner, phandle,
        )?)),
        ast::Cell::Macro(macro_inv) => resolve_macro_to_value(
            db,
            env,
            diag,
            spanner,
            &MacroCtx::Explicit(macro_inv),
            Entrypoint::Cells,
            convert_cell,
        ),
        ast::Cell::DtExpr(dt_expr) => {
            let expr = dt_expr.expr()?;
            let num = super::expr_eval::eval(db, env, expr, diag, spanner)?;
            if let Ok(num) = u32::try_from(num) {
                Some(MirCell::U32(num))
            } else if let Ok(num) = i32::try_from(num) {
                // Encoded using two's complement
                Some(MirCell::U32(num.cast_unsigned()))
            } else {
                let cmp = if num < 0 { "small" } else { "large" };

                diag.emit(Diagnostic::new(
                    spanner(cell.syntax_item().text_range()),
                    Cow::Owned(format!(
                        "number {num} too {cmp} to fit in 32-bit signed integer (using two's complement) or 32-bit unsigned integer"
                    )),
                    Severity::Error,
                ));
                None
            }
        } // TODO: exprs / DtExpr. Remember to error when it's over u32::MAX or under u32::MIN
    }
}

/// Resolves and substitutes a macro and reparses the result.
///
/// Returns `None` if the macro doesn't exist or there is some other error.
fn resolve_macro_to_value<
    'db,
    AstType: AstNodeOrToken,
    MirType,
    D: DiagnosticCollector<File>,
    Spanner: FnMut(TextRange) -> Span<File>,
>(
    db: &'db dyn BaseDb,
    env: &mut TrackedMapEnvMut<'db>,
    diag: &D,
    spanner: &mut Spanner,
    macro_ctx: &MacroCtx,
    entrypoint: Entrypoint,
    convert: impl FnOnce(
        &'db dyn BaseDb,
        &mut TrackedMapEnvMut<'db>,
        &D,
        &mut Spanner,
        &AstType,
    ) -> Option<MirType>,
) -> Option<MirType> {
    let (span, _trmaps, expanded) = substitute_macro_tok(db, env, diag, spanner, macro_ctx)?;

    let parse = entrypoint.parse(&expanded);

    // TODO: use trmaps to map error ranges back to the original macro invocation site.
    // TODO: multi-file errors!!

    emit_parse_errors(&parse, &diag, spanner);

    let red_node = parse.red_node();
    let Some(ast) = AstType::cast_node(red_node.clone())
        .or_else(|| red_node.children().find_map(AstType::cast_either))
    else {
        diag.emit(Diagnostic::new(
            span,
            Cow::Owned(format!(
                "Internal compiler error: Couldn't find {} as child of parse's root node, found node kinds {:?} and token kinds {:?}",
                std::any::type_name::<AstType>(),
                parse.red_node().child_nodes().map(|red| red.green.kind).collect::<Vec<_>>(),
                parse.red_node().child_tokens().map(|red| red.green.kind).collect::<Vec<_>>()
            )),
            Severity::Error,
        ));
        return None;
    };

    convert(db, env, diag, spanner, &ast)
}

/// Convert an AST phandle to a [`MirPhandleTarget`].
fn convert_phandle<'db>(
    db: &'db dyn BaseDb,
    env: &mut TrackedMapEnvMut<'db>,
    diag: &impl DiagnosticCollector<File>,
    spanner: &mut impl FnMut(TextRange) -> Span<File>,
    phandle: &ast::DtPhandle,
) -> Option<MirPhandleTarget> {
    use dt_tools_parser::parser::Entrypoint;

    if let Some(macro_inv) = phandle.macro_invocation() {
        // If the phandle has a macro invocation (e.g. `&MACRO(...)`), resolve it.
        resolve_macro_to_value(
            db,
            env,
            diag,
            spanner,
            &MacroCtx::Explicit(&macro_inv),
            Entrypoint::ReferenceNoamp,
            convert_phandle,
        )
    } else if phandle.is_path() {
        // &{/path/to/node}
        // NOTE: very naive, just strips the prefix/suffix from the source text.
        // A proper implementation would use the CST structure.
        let raw = phandle.syntax().green.text();
        let inner = raw.strip_prefix("&{")?.strip_suffix('}')?;

        Some(MirPhandleTarget::Path(inner.to_owned()))
    } else {
        // No explicit macro invocation, but the name itself might still be a macro
        // (e.g. `#define UART_1 soc/uart` and `&UART_1`).
        let name_ast = phandle.name()?;

        // DTC wants extensions to be resolved from items above/before the extensions in
        // non-overlay mode, but phandles are fine in any order.

        Some(
            resolve_macro_to_value(
                db,
                env,
                diag,
                spanner,
                &MacroCtx::Implicit(&name_ast),
                Entrypoint::ReferenceNoamp,
                convert_phandle,
            )
            .unwrap_or(MirPhandleTarget::Label(name_ast.syntax().text().to_owned())),
        )
    }
}

#[cfg(test)]
#[expect(
    clippy::needless_raw_string_hashes,
    reason = "expect-test auto update adds r#"
)]
mod tests {
    use crate::salsa::db::BaseDb;
    use expect_test::{Expect, expect};

    use super::*;

    /// Run the preprocessor on virtual files and snapshot MIR + diagnostics.
    ///
    /// The root file is named "/main.dts".
    #[expect(clippy::needless_pass_by_value, reason = "ergonomics")]
    fn check_mir(root_file_contents: &str, other_files: &[(&str, &str)], expect: Expect) {
        let db = crate::salsa::db::BaseDatabase::default();
        IncludeDirs::new(&db, vec![]);

        let root_file = db.get_files().add_virtual(
            &db,
            Utf8PathBuf::from("/main.dts"),
            root_file_contents.to_owned(),
        );

        for &(path, contents) in other_files {
            db.get_files()
                .add_virtual(&db, Utf8PathBuf::from(path), contents.to_owned());
        }

        let (diags, _included_files) = crate::salsa::compute_diagnostics(&db, root_file);

        let result =
            preprocessor_eval_root_file(&db, root_file).expect("Should be a readable file");
        let mir = result.mir(&db);

        let mut out = mir.fmt_for_test(&db);
        if !diags.is_empty() {
            out.push_str("\n--- errors ---\n");
            for d in diags {
                use std::fmt::Write;
                let range = d
                    .span
                    .primary_spans
                    .first()
                    .expect("Should have at least one primary span")
                    .text_range;
                let _ = writeln!(out, "{:?} {range}: {}", d.severity, d.msg);
            }
        }
        expect.assert_eq(&out);
    }

    #[test]
    fn mir_empty() {
        check_mir(
            r#"
/dts-v1/;
"#,
            &[],
            expect![""],
        );
    }

    #[test]
    fn mir_empty_root() {
        check_mir(
            r#"
/dts-v1/;
/ {};
"#,
            &[],
            expect![[r#"
                node   / /main.dts 11..16
            "#]],
        );
    }

    #[test]
    fn mir_properties() {
        check_mir(
            r#"
/dts-v1/;
/ { foo = "bar"; baz = <1 2 3>; qux = [ab cd]; };
"#,
            &[],
            expect![[r#"
                node   / /main.dts 11..60
                property = CellList([U32(1), U32(2), U32(3)]) /baz /main.dts 28..42
                property = String("bar") /foo /main.dts 15..27
                property = Bytestring([171, 205]) /qux /main.dts 43..57
            "#]],
        );
    }

    #[test]
    fn mir_label() {
        check_mir(
            r#"
/dts-v1/;
/ { LBL: node {}; };
"#,
            &[],
            expect![[r#"
                node   / /main.dts 11..31
                node labels=[LBL] /node /main.dts 15..28
            "#]],
        );
    }

    #[test]
    fn mir_extension() {
        check_mir(
            r#"
/dts-v1/;
/ { LBL: node {}; };
&LBL { prop = <1>; };
"#,
            &[],
            expect![[r#"
                node   / /main.dts 11..31
                node labels=[LBL] /node /main.dts 15..28
                node   /node /main.dts 32..53
                property = CellList([U32(1)]) /node/prop /main.dts 39..50
            "#]],
        );
    }

    #[test]
    fn mir_extension_unresolved() {
        check_mir(
            r#"
/dts-v1/;
/plugin/;
&UNKNOWN { prop = <1>; };
"#,
            &[],
            expect![[r#"
                --- unresolved ---
                  label=UNKNOWN (1 definitions)
            "#]],
        );
    }

    #[test]
    fn mir_include_preprocessor() {
        check_mir(
            r#"
/dts-v1/;
#include "inc.dtsi"
/ { main_prop = <1>; };
"#,
            &[("/inc.dtsi", r#"/ { inc_prop = <2>; };"#)],
            expect![[r#"
                node   / /inc.dtsi 0..22
                node   / /main.dts 31..54
                property = CellList([U32(2)]) /inc_prop /inc.dtsi 4..19
                property = CellList([U32(1)]) /main_prop /main.dts 35..51
            "#]],
        );
    }

    #[test]
    fn mir_include_preprocessor_error() {
        check_mir(
            r#"
/dts-v1/;
#include "inc.dtsi"
/ { main_prop = <1>; };
"#,
            &[("/inc.dtsi", r#"/ { inc_prop = <BOGUS>; };"#)],
            expect![[r#"
                node   / /inc.dtsi 0..26
                node   / /main.dts 31..54
                property = CellList([]) /inc_prop /inc.dtsi 4..23
                property = CellList([U32(1)]) /main_prop /main.dts 35..51

                --- errors ---
                Error 16..21: Macro `BOGUS` is not defined [dt_tools_lsp::salsa::preprocessor::preprocessor_eval_file] [dt_tools_lsp::salsa::preprocessor::preprocessor_eval_file]
            "#]],
        );
    }

    #[test]
    fn mir_delete_node_by_name() {
        check_mir(
            r#"
/dts-v1/;
/ { foo {}; /delete-node/ foo; };
"#,
            &[],
            expect![[r#"
                node   / /main.dts 11..44
                node   /foo /main.dts 15..22
                delete-node /foo /main.dts 23..41
            "#]],
        );
    }

    #[test]
    fn mir_delete_node_by_label() {
        check_mir(
            r#"
/dts-v1/;
/ { foo: bar {}; /delete-node/ &foo; };
"#,
            &[],
            expect![[r#"
                node   / /main.dts 11..50
                node labels=[foo] /bar /main.dts 15..27
                delete-node /bar /main.dts 28..47
            "#]],
        );
    }

    #[test]
    fn mir_delete_node_by_path() {
        check_mir(
            r#"
/dts-v1/;
/ { foo {}; /delete-node/ &{/foo}; };
"#,
            &[],
            expect![[r#"
                node   / /main.dts 11..48
                node   /foo /main.dts 15..22
                delete-node /foo /main.dts 23..45
            "#]],
        );
    }

    #[test]
    fn mir_delete_property() {
        check_mir(
            r#"
/dts-v1/;
/ { foo = <1>; /delete-property/ foo; };
"#,
            &[],
            expect![[r#"
                node   / /main.dts 11..51
                property = CellList([U32(1)]) /foo /main.dts 15..25
                delete-property /foo /main.dts 26..48
            "#]],
        );
    }

    #[test]
    fn mir_conditional_ifdef() {
        check_mir(
            r#"
/dts-v1/;
#define FLAG
#ifdef FLAG
/ { yes = <1>; };
#else
/ { no = <2>; };
#endif
"#,
            &[],
            expect![[r#"
                node   / /main.dts 36..53
                property = CellList([U32(1)]) /yes /main.dts 40..50
            "#]],
        );
    }

    #[test]
    fn mir_macro() {
        check_mir(
            r#"
/dts-v1/;
#define VAL 42
#define VAL2 "example"

/ { prop = <VAL>, VAL2; };
"#,
            &[],
            expect![[r#"
                node   / /main.dts 50..76
                property = CellList([U32(42)]), String("example") /prop /main.dts 54..73
            "#]],
        );
    }

    #[test]
    fn mir_macro_label() {
        check_mir(
            r#"
/dts-v1/;
#define MACRO FOO

/ { MACRO: foo {}; };
/ { prop = <&FOO>; };
"#,
            &[],
            expect![[r#"
                node   / /main.dts 30..51
                node   / /main.dts 52..73
                node labels=[FOO] /foo /main.dts 34..48
                property = CellList([Phandle(Label("FOO"))]) /prop /main.dts 56..70
            "#]],
        );
    }

    #[test]
    fn mir_macro_reference() {
        check_mir(
            r#"
/dts-v1/;
#define MACRO FOO

/ { FOO: foo {}; };
/ { prop = <&MACRO>; };
"#,
            &[],
            expect![[r#"
                node   / /main.dts 30..49
                node   / /main.dts 50..73
                node labels=[FOO] /foo /main.dts 34..46
                property = CellList([Phandle(Label("FOO"))]) /prop /main.dts 54..70
            "#]],
        );
    }

    #[test]
    fn mir_macro_node() {
        check_mir(
            r#"
/dts-v1/;
#define FOO SUBSTITUTED

/ { FOO@bar {}; };
/ { bar@FOO {}; };
"#,
            &[],
            expect![[r#"
                node   / /main.dts 36..54
                node   / /main.dts 55..73
                node   /SUBSTITUTED@bar /main.dts 40..51
                node   /bar@SUBSTITUTED /main.dts 59..70
            "#]],
        );
    }

    #[test]
    fn mir_undefined_macro() {
        check_mir(
            r#"
/dts-v1/;
/ { prop = <VAL>; prop2 = VAL; };
"#,
            &[],
            expect![[r#"
                node   / /main.dts 11..44
                property = CellList([]) /prop /main.dts 15..28
                property =  /prop2 /main.dts 29..41

                --- errors ---
                Error 23..26: Macro `VAL` is not defined [dt_tools_lsp::salsa::preprocessor::preprocessor_eval_file]
                Error 37..40: Macro `VAL` is not defined [dt_tools_lsp::salsa::preprocessor::preprocessor_eval_file]
            "#]],
        );
    }

    #[test]
    fn mir_undefined_label_extension() {
        check_mir(
            r#"
/dts-v1/;
&BOGUS { };
"#,
            &[],
            expect![[r#"

                --- errors ---
                Error 11..17: Label not found: &BOGUS [dt_tools_lsp::salsa::preprocessor::preprocessor_eval_file]
            "#]],
        );
    }

    #[test]
    fn mir_undefined_label_phandle() {
        check_mir(
            r#"
/dts-v1/;
/ { foo = &BOGUS; };
"#,
            &[],
            expect![[r#"
                node   / /main.dts 11..31
                property = Phandle(Label("BOGUS")) /foo /main.dts 15..28

                --- errors ---
                Error 15..28: Label not found: BOGUS [dt_tools_lsp::salsa::check_mir_post]
            "#]],
        );
    }

    #[test]
    fn mir_duplicate_label() {
        check_mir(
            r#"
/dts-v1/;
/ { foo: bar {}; };
/ { foo: baz {}; };
"#,
            &[],
            expect![[r#"
                node   / /main.dts 11..30
                node   / /main.dts 31..50
                node labels=[foo] /bar /main.dts 15..27
                node   /baz /main.dts 35..47

                --- errors ---
                Error 35..39: Duplicate label `foo` [dt_tools_lsp::salsa::preprocessor::preprocessor_eval_file]
            "#]],
        );
    }

    #[test]
    fn mir_num_out_of_bounds() {
        check_mir(
            r#"
/dts-v1/;
/ { prop = <(1 << 32)>; };
"#,
            &[],
            expect![[r#"
                node   / /main.dts 11..37
                property = CellList([]) /prop /main.dts 15..34

                --- errors ---
                Error 23..32: number 4294967296 too large to fit in 32-bit signed integer (using two's complement) or 32-bit unsigned integer [dt_tools_lsp::salsa::preprocessor::preprocessor_eval_file]
            "#]],
        );
    }

    #[test]
    fn mir_num_negative() {
        check_mir(
            r#"
/dts-v1/;
/ { prop = <-1 (-1)>; };
"#,
            &[],
            expect![[r#"
                node   / /main.dts 11..35
                property = CellList([U32(1), U32(4294967295)]) /prop /main.dts 15..32

                --- errors ---
                Error 23..24: Expected cell or ‘>’, but found ‘-’ [dt-tools(syntax-error)]
            "#]],
        );
    }

    #[test]
    fn mir_char() {
        check_mir(
            r#"
/dts-v1/;
/ { prop = <'\0' 'a'>; };
"#,
            &[],
            expect![[r#"
                node   / /main.dts 11..36
                property = CellList([U32(0), U32(97)]) /prop /main.dts 15..33
            "#]],
        );
    }
}
