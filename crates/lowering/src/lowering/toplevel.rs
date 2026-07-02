use std::borrow::Cow;

use camino::{Utf8Path, Utf8PathBuf};
use dt_tools_analyzer::macros::MacroDefinition;
use dt_tools_diagnostic::{Diagnostic, DiagnosticCollector, Severity, Span};
use dt_tools_parser::{
    ast,
    ast::{AstNode, AstNodeOrToken, AstToken, HasDtPhandle, HasName},
    lexer::TokenKind,
};

use super::{
    IntraFileCtx, lower_phandle,
    preprocessor::{get_pp_directive_args, get_pp_include_arg},
};
use crate::{
    file::File,
    lowering::{
        dt_node::{build_path, process_dt_node},
        lower_file,
        preprocessor::pp_cond_directive_eval,
    },
    mir::{MirDefinition, MirDefinitionValue, MirPhandleTarget, MirProvenance},
};

/// Lists candidate file paths for `#include`/`/include/`.
fn possible_include_paths_utf8<'a, P: AsRef<Utf8Path>>(
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

/// Handles [`ast::ToplevelItem`]s in the lowering process.
#[expect(clippy::too_many_lines, reason = "hard to make this shorter")]
pub(crate) fn handle_toplevel_item(
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
                Ok(parsed) => ctx
                    .env
                    .insert_macro(parsed, text_range.within_file(ctx.file)),
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
                let Some((relative, include_path)) =
                    get_pp_include_arg(input, "include", text_range, &ctx.diag, &mut |tr| {
                        tr.within_file(ctx.file)
                    })
                else {
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

                let result = lower_file(
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
                        ctx.env.get_macro_def(ctx.db, &args).is_some()
                    )),
                    Severity::Error,
                ));
            }
            _ => {}
        },
    }
}

/// Handles an [`ast::DtsDirective`] like `/include/`, `/delete-node/` or `/delete-property/`.
pub(crate) fn handle_directive(
    ctx: &mut IntraFileCtx<'_, '_, impl DiagnosticCollector<File>>,
    processed_files: &mut Vec<File>,
    includes: &mut Vec<(File, Span<File>)>,
    path_prefix: &str,
    dir: &ast::DtsDirective,
    include_dirs: &[Utf8PathBuf],
    parent_dir_path: &Utf8Path,
    files: &crate::file::Files,
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

        let result = lower_file(
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

/// Optionally emit [`MirDefinitionValue::DeletedNode`] or [`MirDefinitionValue::DeletedProperty`] from an [`ast::DtsDirective`].
pub(crate) fn emit_delete_directive(
    ctx: &mut IntraFileCtx<'_, '_, impl DiagnosticCollector<File>>,
    path_prefix: &str,
    dir: &ast::DtsDirective,
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
            let Some(target) = lower_phandle(
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
                    if let Some(path) = ctx.env.get_label_path(ctx.db, name) {
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
