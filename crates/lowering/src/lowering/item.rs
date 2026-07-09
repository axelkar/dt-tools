use std::borrow::Cow;

use camino::{Utf8Path, Utf8PathBuf};
use dt_tools_analyzer::macros::MacroDefinition;
use dt_tools_diagnostic::Severity;
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
    diag::Diag,
    lowering::{
        dt_node::{build_path, get_name_and_unit_addr, lower_dt_node},
        dt_property::{interpret_escaped_string_tok, lower_dt_property},
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

/// Lowers an [`ast::Item`] to [`MirDefinition`]s.
pub(crate) fn lower_item(
    ctx: &mut IntraFileCtx<'_, '_>,
    diag: &mut Diag<'_, '_>,
    parent_node_path: &str,
    item: ast::Item,
) {
    let files = ctx.db.get_files();

    let text_range = item.syntax_item().text_range();

    match item {
        ast::Item::DtNode(dt_node) => {
            lower_dt_node(ctx, diag, parent_node_path, &dt_node);
        }
        ast::Item::DtProperty(dt_property) => {
            let _ = lower_dt_property(ctx, diag, parent_node_path, &dt_property);
        }
        ast::Item::DtsDirective(dir) => {
            handle_dts_directive(ctx, diag, parent_node_path, &dir, files);
        }
        ast::Item::PreprocessorConditional(cond) => {
            let Some(branch) = handle_preprocessor_conditional(ctx, diag, &cond) else {
                return;
            };
            for item in branch.items() {
                lower_item(ctx, diag, parent_node_path, item);
            }
        }
        ast::Item::PreprocessorDirective(dir) => {
            handle_preprocessor_directive(ctx, diag, parent_node_path, files, text_range, &dir);
        }
    }
}

pub(crate) fn handle_preprocessor_conditional(
    ctx: &mut IntraFileCtx<'_, '_>,
    diag: &mut Diag<'_, '_>,
    cond: &ast::PreprocessorConditional,
) -> Option<ast::PreprocessorBranch> {
    for (dir, branch) in cond.branches() {
        let condition = pp_cond_directive_eval(ctx.db, ctx.env, &dir, diag).ok()?;
        if condition {
            return Some(branch);
        }
    }
    None
}

/// Handles an [`ast::PreprocessorDirective`] like `#define`.
pub(crate) fn handle_preprocessor_directive(
    ctx: &mut IntraFileCtx<'_, '_>,
    diag: &mut Diag<'_, '_>,
    parent_node_path: &str,
    files: &crate::file::Files,
    text_range: dt_tools_parser::TextRange,
    dir: &ast::PreprocessorDirective,
) {
    match dir.kind() {
        TokenKind::PragmaDirective => {
            // TODO: implement #pragma
            diag.emit(
                text_range,
                Cow::Borrowed("`#pragma` is unimplemented"),
                Severity::Error,
            );
        }
        TokenKind::DefineDirective => match MacroDefinition::parse(dir.syntax().text()) {
            Ok(parsed) => ctx.env.insert_macro(parsed, diag.resolve(text_range)),
            Err(err) => {
                diag.emit(
                    if let Some(local_range) = err.text_range() {
                        local_range.offset(text_range.start)
                    } else {
                        text_range
                    },
                    Cow::Owned(err.to_string()),
                    Severity::Error,
                );
            }
        },
        TokenKind::UndefDirective => {
            let input = dir.syntax().text();
            let (args, args_text_range) = get_pp_directive_args(input, "undef", text_range);
            if args.contains(' ') {
                diag.emit(
                    args_text_range,
                    Cow::Borrowed("Arguments to `#undef` should be just a macro name"),
                    Severity::Error,
                );
            } else {
                ctx.env.own_macro_map.insert(args, None);
            }
        }
        TokenKind::IncludeDirective => {
            // TODO: real evaluation and macro substitution
            let input = dir.syntax().text();
            let Some((relative, include_path)) =
                get_pp_include_arg(input, "include", text_range, diag)
            else {
                return;
            };

            let Some(include_file) =
                find_file_to_include(ctx, diag, files, text_range, relative, &include_path)
            else {
                return;
            };

            ctx.processed_files.push(include_file);
            ctx.includes.push((include_file, diag.resolve(text_range)));

            let result = lower_file(
                ctx.db,
                include_file,
                Some(std::mem::take(ctx.env).into_immut(ctx.db)),
                ctx.is_overlay(),
                parent_node_path.to_owned(),
            )
            .expect("The file should exist, its existence is confirmed above");

            *ctx.env = result.env_after(ctx.db).to_mut();
            ctx.mir.merge(result.mir(ctx.db));
            result
                .diagnostics(ctx.db)
                .iter()
                .for_each(|diagnostic| diag.push(diagnostic.clone()));
            ctx.processed_files
                .extend_from_slice(result.processed_files(ctx.db));
            ctx.includes.extend_from_slice(result.includes(ctx.db));

            // TODO: PERF: Salsa tracked? currently this breaks all Salsa tracking...
            ctx.env.flatten_ancestors(ctx.db);
        }
        TokenKind::ErrorDirective => {
            let input = dir.syntax().text();
            let (args, _args_text_range) = get_pp_directive_args(input, "error", text_range);

            // TODO: remove debug thing?
            diag.emit(
                text_range,
                Cow::Owned(format!(
                    "`#error`: {args:?}, defined {args:?}={}",
                    ctx.env.get_macro_def(ctx.db, &args).is_some()
                )),
                Severity::Error,
            );
        }
        _ => {}
    }
}

/// Wraps [`possible_include_paths_utf8`], finding a readable file and handling errors.
fn find_file_to_include(
    ctx: &mut IntraFileCtx<'_, '_>,
    diag: &mut Diag<'_, '_>,
    files: &crate::file::Files,
    text_range: dt_tools_parser::TextRange,
    relative: bool,
    include_path: &str,
) -> Option<crate::file::File> {
    if let Some(include_file) = possible_include_paths_utf8(
        relative,
        include_path,
        ctx.parent_dir_path,
        ctx.include_dirs,
    )
    .map(|path| files.get_file(ctx.db, &path))
    .find(|file| file.is_readable_file(ctx.db))
    {
        Some(include_file)
    } else {
        diag.emit(
            text_range,
            Cow::Owned(format!("Couldn't find file to include: {include_path}")),
            Severity::Error,
        );
        None
    }
}

/// Handles an [`ast::DtsDirective`] like `/include/`, `/delete-node/` or `/delete-property/`.
pub(crate) fn handle_dts_directive(
    ctx: &mut IntraFileCtx<'_, '_>,
    diag: &mut Diag<'_, '_>,
    parent_node_path: &str,
    dir: &ast::DtsDirective,
    files: &crate::file::Files,
) {
    let kind = dir.kind();

    if kind == Some(TokenKind::DtIncludeDirective) {
        // /include/ "path"
        let Some(string_tok) = dir
            .syntax()
            .child_tokens()
            .find(|tok| tok.green.kind == TokenKind::String)
        else {
            return;
        };

        let Ok(include_path) = interpret_escaped_string_tok(&string_tok, diag) else {
            return;
        };

        // Always true for /include/
        let relative = true;

        let Some(include_file) = find_file_to_include(
            ctx,
            diag,
            files,
            dir.syntax().text_range(),
            relative,
            &include_path,
        ) else {
            return;
        };

        ctx.processed_files.push(include_file);
        ctx.includes
            .push((include_file, diag.resolve(dir.syntax().text_range())));

        let result = lower_file(
            ctx.db,
            include_file,
            Some(std::mem::take(ctx.env).into_immut(ctx.db)),
            ctx.is_overlay(),
            parent_node_path.to_owned(),
        )
        .expect("file exists");

        *ctx.env = result.env_after(ctx.db).to_mut();
        ctx.mir.merge(result.mir(ctx.db));
        result
            .diagnostics(ctx.db)
            .iter()
            .for_each(|diagnostic| diag.push(diagnostic.clone()));
        ctx.processed_files
            .extend_from_slice(result.processed_files(ctx.db));
        ctx.includes.extend_from_slice(result.includes(ctx.db));

        // TODO: PERF: Salsa tracked? currently this breaks all Salsa tracking...
        ctx.env.flatten_ancestors(ctx.db);
    } else if kind == Some(TokenKind::V1Directive) {
        ctx.mir.definitions.push(MirDefinition {
            path: parent_node_path.to_owned(),
            value: MirDefinitionValue::V1Directive,
            provenance: MirProvenance {
                span: diag.resolve(dir.syntax().text_range()),
            },
        });
    } else if kind == Some(TokenKind::PluginDirective) {
        ctx.mir.definitions.push(MirDefinition {
            path: parent_node_path.to_owned(),
            value: MirDefinitionValue::PluginDirective,
            provenance: MirProvenance {
                span: diag.resolve(dir.syntax().text_range()),
            },
        });
    } else {
        emit_delete_directive(ctx, diag, parent_node_path, dir);
    }
}

/// Optionally emit [`MirDefinitionValue::DeletedNode`] or [`MirDefinitionValue::DeletedProperty`] from an [`ast::DtsDirective`].
pub(crate) fn emit_delete_directive(
    ctx: &mut IntraFileCtx<'_, '_>,
    diag: &mut Diag<'_, '_>,
    parent_node_path: &str,
    dir: &ast::DtsDirective,
) {
    let kind = dir.kind();
    let Some(args) = dir.arguments() else { return };

    let text_range = dir.syntax().text_range();
    let provenance = MirProvenance {
        span: diag.resolve(text_range),
    };

    if kind == Some(TokenKind::DeleteNodeDirective) {
        let target_path = if let Ok(name) = get_name_and_unit_addr(ctx.db, ctx.env, diag, &args) {
            build_path(parent_node_path, &name)
        } else if let Some(phandle) = args.dt_phandle() {
            let Ok(target) = lower_phandle(ctx.db, ctx.env, diag, &phandle) else {
                return;
            };

            match resolve_phandle(ctx, diag, &target, &phandle) {
                Ok(value) => value,
                Err(()) if ctx.is_overlay() => {
                    // TODO: handle unresolved delete-node!
                    return;
                }
                Err(()) => return,
            }
        } else {
            return;
        };

        // Clear the labels
        for def in ctx.mir.iter_live_defs_under(&target_path) {
            if let MirDefinitionValue::Node(data) = &def.value {
                for label in &data.labels {
                    ctx.env.own_label_map.insert(label.clone(), None);
                }
            }
        }

        ctx.mir.definitions.push(MirDefinition {
            path: target_path,
            value: MirDefinitionValue::DeletedNode,
            provenance,
        });
    } else if kind == Some(TokenKind::DeletePropertyDirective)
        && let Some(name_tok) = args.name()
    {
        let target_path = build_path(parent_node_path, name_tok.syntax().text().as_str());

        ctx.mir.definitions.push(MirDefinition {
            path: target_path,
            value: MirDefinitionValue::DeletedProperty,
            provenance,
        });
    }
}

/// Resolves a [`MirPhandleTarget`] to a path.
///
/// Returns `Err(())` if it can't be resolved. If in an overlay, an error will not be emitted.
pub(crate) fn resolve_phandle(
    ctx: &mut IntraFileCtx<'_, '_>,
    diag: &mut Diag<'_, '_>,
    target: &MirPhandleTarget,
    phandle: &ast::DtPhandle,
) -> Result<String, ()> {
    match &target {
        MirPhandleTarget::Label(name) => {
            if let Some(path) = ctx.env.get_label_path(ctx.db, name) {
                Ok(path.to_owned())
            } else {
                // Couldn't resolve it.
                if ctx.is_overlay() {
                    // TODO: handle unresolved delete-node!
                } else {
                    diag.emit(
                        phandle.syntax().text_range(),
                        Cow::Owned(format!("Label not found: {name}")),
                        Severity::Error,
                    );
                }
                Err(())
            }
        }
        MirPhandleTarget::Path(path) => {
            if ctx.mir.contains_node(path) {
                Ok(path.clone())
            } else {
                if ctx.is_overlay() {
                    // TODO: handle unresolved delete-node!
                } else {
                    diag.emit(
                        phandle.syntax().text_range(),
                        Cow::Owned(format!("Node at path not found: {path}")),
                        Severity::Error,
                    );
                }
                Err(())
            }
        }
    }
}

#[cfg(test)]
#[expect(
    clippy::needless_raw_string_hashes,
    reason = "expect-test auto update adds r#"
)]
mod tests {
    use expect_test::expect;

    use crate::lowering::tests::check_mir;

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
                dts-v1  /main.dts L2:1-L2:10
                node   / /inc.dtsi L1:1-L1:23
                node   / /main.dts L4:1-L4:24
                property = CellList(Bits32([Number(2)])) /inc_prop /inc.dtsi L1:5-L1:20
                property = CellList(Bits32([Number(1)])) /main_prop /main.dts L4:5-L4:21
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
                dts-v1  /main.dts L2:1-L2:10
                node   / /inc.dtsi L1:1-L1:27
                node   / /main.dts L4:1-L4:24
                property = CellList(Bits32([])) /inc_prop /inc.dtsi L1:5-L1:24
                property = CellList(Bits32([Number(1)])) /main_prop /main.dts L4:5-L4:21

                --- errors ---
                Error L1:17-L1:22: Macro `BOGUS` is not defined
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
                dts-v1  /main.dts L2:1-L2:10
                node   / /main.dts L3:1-L3:34
                node   /foo /main.dts L3:5-L3:12
                delete-node /foo /main.dts L3:13-L3:31
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
                dts-v1  /main.dts L2:1-L2:10
                node   / /main.dts L3:1-L3:40
                node labels=[foo] /bar /main.dts L3:5-L3:17
                delete-node /bar /main.dts L3:18-L3:37
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
                dts-v1  /main.dts L2:1-L2:10
                node   / /main.dts L3:1-L3:38
                node   /foo /main.dts L3:5-L3:12
                delete-node /foo /main.dts L3:13-L3:35
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
                dts-v1  /main.dts L2:1-L2:10
                node   / /main.dts L3:1-L3:41
                property = CellList(Bits32([Number(1)])) /foo /main.dts L3:5-L3:15
                delete-property /foo /main.dts L3:16-L3:38
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
                dts-v1  /main.dts L2:1-L2:10
                node   / /main.dts L5:1-L5:18
                property = CellList(Bits32([Number(1)])) /yes /main.dts L5:5-L5:15
            "#]],
        );
    }

    #[test]
    fn mir_conditional_inside_node() {
        check_mir(
            r#"
/dts-v1/;
#define FLAG

/ {
#ifdef FLAG
    yes = <1>;
#else
    no = <2>;
#endif
};
"#,
            &[],
            expect![[r#"
                dts-v1  /main.dts L2:1-L2:10
                node   / /main.dts L5:1-L11:3
                property = CellList(Bits32([Number(1)])) /yes /main.dts L7:5-L7:15
            "#]],
        );
    }

    #[test]
    fn mir_conditional_inside_cell() {
        check_mir(
            r#"
/dts-v1/;
#define FLAG

/ {
    prop = <
#ifdef FLAG
    1
#else
    0
#endif
    >;
};
"#,
            &[],
            expect![[r#"
                dts-v1  /main.dts L2:1-L2:10
                node   / /main.dts L5:1-L13:3
                property = CellList(Bits32([Number(1)])) /prop /main.dts L6:5-L12:7
            "#]],
        );
    }

    #[test]
    fn mir_conditional_inside_prop_value() {
        check_mir(
            r#"
/dts-v1/;
#define FLAG

/ {
    prop =
#ifdef FLAG
    <1>
#else
    <0>
#endif
    ;
};
"#,
            &[],
            expect![[r#"
                dts-v1  /main.dts L2:1-L2:10
                node   / /main.dts L5:1-L13:3
                property = CellList(Bits32([Number(1)])) /prop /main.dts L6:5-L12:6
            "#]],
        );
    }

    #[test]
    fn mir_include_inside_node() {
        check_mir(
            r#"
/dts-v1/;
/ {
#include "inc.dtsi"
};
"#,
            &[("/inc.dtsi", r#"inc_prop = <2>;"#)],
            expect![[r#"
                dts-v1  /main.dts L2:1-L2:10
                node   / /main.dts L3:1-L5:3
                property = CellList(Bits32([Number(2)])) /inc_prop /inc.dtsi L1:1-L1:16
            "#]],
        );
    }
}
