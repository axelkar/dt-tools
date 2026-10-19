use std::borrow::Cow;

use dt_tools_diagnostic::Severity;
use dt_tools_parser::{
    TextRange, ast,
    ast::{AstNode, AstToken, HasDtPhandle, HasName},
    lexer::TokenKind,
};

use super::{LowerCtx, lower_phandle};
use crate::{
    diag::Diag,
    expand::ExpandedText,
    lowering::{
        dt_node::{build_path, get_name_and_unit_addr, lower_dt_node},
        dt_property::lower_dt_property,
    },
    mir::{MirDefinition, MirDefinitionValue, MirPhandleTarget, MirProvenance},
};

/// Lowers an [`ast::Item`] to [`MirDefinition`]s.
pub(crate) fn lower_item(
    ctx: &mut LowerCtx<'_>,
    diag: &mut Diag<'_>,
    parent_node_path: &str,
    item: ast::Item,
) {
    profiling::scope!(
        "lowering::lower_item",
        match &item {
            ast::Item::DtNode(_) => "DtNode",
            ast::Item::DtProperty(_) => "DtProperty",
            ast::Item::DtsDirective(_) => "DtsDirective",
            ast::Item::PreprocessorConditional(_) => "PreprocessorConditional",
            ast::Item::PreprocessorDirective(_) => "PreprocessorDirective",
        }
    );

    match item {
        ast::Item::DtNode(dt_node) => {
            lower_dt_node(ctx, diag, parent_node_path, &dt_node);
        }
        ast::Item::DtProperty(dt_property) => {
            let _ = lower_dt_property(ctx, diag, parent_node_path, &dt_property);
        }
        ast::Item::DtsDirective(dir) => {
            handle_dts_directive(ctx, diag, parent_node_path, &dir);
        }
        ast::Item::PreprocessorConditional(cond) => {
            unsupported_preprocessor(diag, cond.syntax().text_range());
        }
        ast::Item::PreprocessorDirective(dir) => {
            unsupported_preprocessor(diag, dir.syntax().text_range());
        }
    }
}

/// Reports preprocessor syntax that only raw `/include/` content can contain.
pub(crate) fn unsupported_preprocessor(diag: &mut Diag<'_>, range: TextRange) {
    diag.emit(
        range,
        "Preprocessor directives in `/include/` files are not processed",
        Severity::Error,
    );
}

/// Handles an [`ast::DtsDirective`] like `/delete-node/` or `/delete-property/`.
pub(crate) fn handle_dts_directive(
    ctx: &mut LowerCtx<'_>,
    diag: &mut Diag<'_>,
    parent_node_path: &str,
    dir: &ast::DtsDirective,
) {
    let kind = dir.kind();

    if kind == Some(TokenKind::DtIncludeDirective) {
        // Preprocessing's second pass splices every well-formed `/include/`.
        diag.emit(
            dir.syntax().text_range(),
            "Unresolved `/include/` directive",
            Severity::Error,
        );
    } else if kind == Some(TokenKind::V1Directive) {
        ctx.mir.definitions.push(MirDefinition {
            path: parent_node_path.to_owned(),
            value: MirDefinitionValue::V1Directive,
            provenance: MirProvenance(dir.syntax().text_range().within_file(ExpandedText)),
        });
    } else if kind == Some(TokenKind::PluginDirective) {
        ctx.mir.definitions.push(MirDefinition {
            path: parent_node_path.to_owned(),
            value: MirDefinitionValue::PluginDirective,
            provenance: MirProvenance(dir.syntax().text_range().within_file(ExpandedText)),
        });
    } else {
        emit_delete_directive(ctx, diag, parent_node_path, dir);
    }
}

/// Optionally emit [`MirDefinitionValue::DeletedNode`] or [`MirDefinitionValue::DeletedProperty`] from an [`ast::DtsDirective`].
pub(crate) fn emit_delete_directive(
    ctx: &mut LowerCtx<'_>,
    diag: &mut Diag<'_>,
    parent_node_path: &str,
    dir: &ast::DtsDirective,
) {
    let kind = dir.kind();
    let Some(args) = dir.arguments() else { return };

    let text_range = dir.syntax().text_range();
    let provenance = MirProvenance(text_range.within_file(ExpandedText));

    if kind == Some(TokenKind::DeleteNodeDirective) {
        let target_path = if let Ok(name) = get_name_and_unit_addr(diag, &args) {
            build_path(parent_node_path, &name)
        } else if let Some(phandle) = args.dt_phandle() {
            let Ok(target) = lower_phandle(diag, &phandle) else {
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
                    ctx.labels.insert(label.clone(), None);
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
    ctx: &mut LowerCtx<'_>,
    diag: &mut Diag<'_>,
    target: &MirPhandleTarget,
    phandle: &ast::DtPhandle,
) -> Result<String, ()> {
    match &target {
        MirPhandleTarget::Label(name) => {
            if let Some(path) = ctx
                .labels
                .get(name)
                .and_then(Option::as_ref)
                .map(|label| &label.path)
            {
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
        MirPhandleTarget::LabelPath { label, path } => {
            let Some(base) = ctx
                .labels
                .get(label)
                .and_then(Option::as_ref)
                .map(|label| label.path.clone())
            else {
                if ctx.is_overlay() {
                    // TODO: handle unresolved delete-node!
                } else {
                    diag.emit(
                        phandle.syntax().text_range(),
                        Cow::Owned(format!("Label not found: {label}")),
                        Severity::Error,
                    );
                }
                return Err(());
            };

            let target = build_path(&base, path);

            if ctx.mir.contains_node(&target) {
                Ok(target)
            } else {
                if ctx.is_overlay() {
                    // TODO: handle unresolved delete-node!
                } else {
                    diag.emit(
                        phandle.syntax().text_range(),
                        Cow::Owned(format!("Node at path not found: {target}")),
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
                dts-v1 /main.dts L2:1-L2:10
                node / /inc.dtsi L1:1-L1:23
                property /inc_prop = <2>; /inc.dtsi L1:5-L1:20
                node / /main.dts L4:1-L4:24
                property /main_prop = <1>; /main.dts L4:5-L4:21
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
                dts-v1 /main.dts L2:1-L2:10
                node / /inc.dtsi L1:1-L1:27
                property /inc_prop = <>; /inc.dtsi L1:5-L1:24
                node / /main.dts L4:1-L4:24
                property /main_prop = <1>; /main.dts L4:5-L4:21

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
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L3:1-L3:34
                node /foo /main.dts L3:5-L3:12
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
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L3:1-L3:40
                node /bar labels=[foo] /main.dts L3:5-L3:17
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
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L3:1-L3:38
                node /foo /main.dts L3:5-L3:12
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
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L3:1-L3:41
                property /foo = <1>; /main.dts L3:5-L3:15
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
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L5:1-L5:18
                property /yes = <1>; /main.dts L5:5-L5:15
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
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L5:1-L11:3
                property /yes = <1>; /main.dts L7:5-L7:15
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
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L5:1-L13:3
                property /prop = <1>; /main.dts L6:5-L12:7
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
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L5:1-L13:3
                property /prop = <1>; /main.dts L6:5-L12:6
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
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L3:1-L5:3
                property /inc_prop = <2>; /inc.dtsi L1:1-L1:16
            "#]],
        );
    }

    /// A label defined in an include is visible after the include.
    #[test]
    fn mir_include_label() {
        check_mir(
            r#"
/dts-v1/;
#include "inc.dtsi"
/ { prop = <&LBL>; };
"#,
            &[("/inc.dtsi", r#"/ { LBL: node {}; };"#)],
            expect![[r#"
                dts-v1 /main.dts L2:1-L2:10
                node / /inc.dtsi L1:1-L1:21
                node /node labels=[LBL] /inc.dtsi L1:5-L1:18
                node / /main.dts L4:1-L4:22
                property /prop = <&LBL>; /main.dts L4:5-L4:19
            "#]],
        );
    }

    /// Raw `/include/` content is not preprocessed, so directives in it are diagnosed.
    #[test]
    fn mir_include_raw_directive() {
        check_mir(
            r#"
/dts-v1/;
/include/ "inc.dtsi"
"#,
            &[("/inc.dtsi", "#define X 1\n/ { prop = <X>; };\n")],
            expect![[r#"
                dts-v1 /main.dts L2:1-L2:10
                node / /inc.dtsi L2:1-L2:19
                property /prop = <>; /inc.dtsi L2:5-L2:16

                --- errors ---
                Error L1:1-L1:12: Preprocessor directives in `/include/` files are not processed
                Error L2:13: Macro `X` is not defined
            "#]],
        );
    }
}
