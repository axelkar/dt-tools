use std::borrow::Cow;

use dt_tools_diagnostic::{Diagnostic, MultiSpan, Severity, SpanLabel};
use dt_tools_parser::{
    TextRange,
    ast::{
        self, AstNode, AstToken, HasDtPhandle, HasLabels, HasMacroInvocation, HasName,
        HasUnitAddress,
    },
    lexer::TokenKind,
};

use super::{Label, LowerCtx, lower_phandle, undefined_macro};
use crate::{
    diag::Diag,
    lowering::item::{lower_item, resolve_phandle},
    mir::{Mir, MirDefinition, MirDefinitionValue, MirNodeData, UnresolvedExtension},
    preprocess::PreprocessedText,
};

/// Lowers an [`ast::DtNode`] and its subtree to flat [`MirDefinition`]s.
pub(crate) fn lower_dt_node(
    ctx: &mut LowerCtx<'_>,
    diag: &mut Diag<'_>,
    parent_node_path: &str,
    dt_node: &ast::DtNode,
) {
    let provenance = dt_node.syntax().text_range().within_file(PreprocessedText);
    let name_range = definition_name_range(dt_node);
    let name_span = name_range.within_file(PreprocessedText);
    let omit_if_no_ref = dt_node.omit_if_no_ref();

    let lower_resolved = |ctx: &mut _, diag: &mut _, provenance, node_path: &String| {
        let labels = collect_labels(ctx, diag, dt_node, node_path);

        // Emit the node definition.
        ctx.mir.definitions.push(MirDefinition {
            path: node_path.clone(),
            value: MirDefinitionValue::Node(MirNodeData {
                labels,
                omit_if_no_ref,
            }),
            provenance,
            name_span,
        });

        // Process the node body: subnodes and properties.
        lower_dt_node_body(ctx, diag, node_path, dt_node);
    };

    // Handle extensions: &label { } or &{/path} { }
    if let Some(phandle) = dt_node.dt_phandle() {
        let Ok(target) = lower_phandle(diag, &phandle) else {
            return;
        };

        if let Ok(ref target_path) = resolve_phandle(ctx, diag, &target, &phandle) {
            lower_resolved(ctx, diag, provenance, target_path);
        } else {
            let mut body_mir = Mir::default();
            lower_dt_node_body_replace_mir(ctx, diag, &mut body_mir, "/", dt_node);

            if ctx.is_overlay() {
                // Overlay; we can leave the extension unresolved

                // Propagate any nested unresolved extensions.
                ctx.mir
                    .unresolved_extensions
                    .extend(body_mir.unresolved_extensions);

                ctx.mir.unresolved_extensions.push(UnresolvedExtension {
                    target,
                    body: body_mir.definitions,
                    provenance,
                    phandle_span: phandle.syntax().text_range().within_file(PreprocessedText),
                });
            }
        }
    } else {
        let name = if let Ok(name) = get_name_and_unit_addr(diag, dt_node) {
            Some(name)
        } else if dt_node
            .syntax()
            .child_tokens()
            .any(|tok| tok.green.kind == TokenKind::Slash)
        {
            Some("/".to_owned())
        } else {
            None
        };

        if let Some(name) = name {
            let root_node = name == "/";

            if parent_node_path.is_empty() && !root_node {
                diag.emit(
                    name_range,
                    "Subnode must be defined inside a node",
                    Severity::Error,
                );
                return;
            }

            if root_node && !parent_node_path.is_empty() {
                diag.emit(
                    name_range,
                    "Root node (`/`) must be defined outside other nodes",
                    Severity::Error,
                );
                return;
            }

            // Build the node's full path.
            let node_path = build_path(parent_node_path, &name);

            lower_resolved(ctx, diag, provenance, &node_path);
        } else {
            // Name is missing, but process so diagnostics are emitted
            let mut body_mir = Mir::default();
            lower_dt_node_body_replace_mir(ctx, diag, &mut body_mir, "/", dt_node);
        }
    }
}

/// Collects labels defined on an [`ast::DtNode`].
pub(crate) fn collect_labels(
    ctx: &mut LowerCtx<'_>,
    diag: &mut Diag<'_>,
    dt_node: &ast::DtNode,
    node_path: &str,
) -> Vec<String> {
    let mut labels: Vec<String> = Vec::new();
    for label_ast in dt_node.labels() {
        if let Ok(label_name) = resolve_name_or_macro(diag, &label_ast) {
            // Check for duplicate labels (dtc: no duplicates globally).
            if let Some(Some(old_label)) = ctx.labels.get(&label_name)
                && old_label.path != node_path
            {
                let span = label_ast
                    .syntax()
                    .text_range()
                    .within_file(PreprocessedText);
                diag.push(Diagnostic {
                    span: MultiSpan::from(span).with_span_label(SpanLabel {
                        span: old_label.span,
                        msg: Cow::Owned(format!(
                            "Previous definition of label `{label_name}` here"
                        )),
                    }),
                    msg: Cow::Owned(format!("Duplicate label `{label_name}`")),
                    severity: Severity::Warn,
                });
            } else {
                let span = label_ast
                    .syntax()
                    .text_range()
                    .within_file(PreprocessedText);
                ctx.labels.insert(
                    label_name.clone(),
                    Some(Label {
                        path: node_path.to_owned(),
                        span,
                    }),
                );
                labels.push(label_name);
            }
        }
    }
    labels
}

/// [`lower_dt_node_body`] but with [`IntraFileCtx::mir`] replaced.
pub(crate) fn lower_dt_node_body_replace_mir(
    ctx: &mut LowerCtx<'_>,
    diag: &mut Diag<'_>,
    body_mir: &mut Mir,
    parent_node_path: &str,
    dt_node: &ast::DtNode,
) {
    lower_dt_node_body(
        &mut LowerCtx {
            mir: body_mir,
            labels: &mut *ctx.labels,
        },
        diag,
        parent_node_path,
        dt_node,
    );
}

/// Lowers the body of an [`ast::DtNode`].
pub(crate) fn lower_dt_node_body(
    ctx: &mut LowerCtx<'_>,
    diag: &mut Diag<'_>,
    parent_node_path: &str,
    dt_node: &ast::DtNode,
) {
    for item in dt_node.items() {
        lower_item(ctx, diag, parent_node_path, item);
    }
}

/// Returns the span of a node's name and unit address, or its extension target.
fn definition_name_range(dt_node: &ast::DtNode) -> TextRange {
    if let Some(phandle) = dt_node.dt_phandle() {
        return phandle.syntax().text_range();
    }

    let Some(name) = dt_node.name() else {
        // The root node's name is a slash token.
        return dt_node
            .syntax()
            .child_tokens()
            .find(|token| token.green.kind == TokenKind::Slash)
            .map_or_else(|| dt_node.syntax().text_range(), |token| token.text_range());
    };

    let mut range = name.syntax().text_range();
    if let Some(unit_address) = dt_node.unit_address() {
        range = range.to(unit_address.syntax().text_range());
    }
    range
}

/// Builds a node/property path. Handles the root node `/` and avoids `//`.
pub(crate) fn build_path(parent: &str, name: &str) -> String {
    if name == "/" {
        "/".to_owned()
    } else if parent == "/" {
        format!("/{name}")
    } else {
        format!("{parent}/{name}")
    }
}

/// Resolves a name, diagnosing a macro invocation left in the preprocessed text.
pub(crate) fn resolve_name_or_macro<Ast: HasName + HasMacroInvocation>(
    diag: &mut Diag<'_>,
    ast: &Ast,
) -> Result<String, ()> {
    if let Some(name_ast) = ast.name() {
        return Ok(name_ast.syntax().text().as_str().to_owned());
    }

    if let Some(invoc) = ast.macro_invocation() {
        undefined_macro(diag, &invoc);
    }

    Err(())
}

/// Resolves and concatenates together the name and unit address of a compatible AST node.
pub(crate) fn get_name_and_unit_addr<Ast: HasName + HasMacroInvocation + HasUnitAddress>(
    diag: &mut Diag<'_>,
    ast: &Ast,
) -> Result<String, ()> {
    let without_unit_addr = resolve_name_or_macro(diag, ast);

    let unit_addr = ast
        .unit_address()
        .map(|ast| resolve_name_or_macro(diag, &ast))
        .transpose()?;

    without_unit_addr.map(|without_unit_addr| {
        if let Some(unit_addr) = unit_addr {
            format!("{without_unit_addr}@{unit_addr}")
        } else {
            without_unit_addr
        }
    })
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
    fn mir_label() {
        check_mir(
            r#"
/dts-v1/;
/ { LBL: node {}; };
"#,
            &[],
            expect![[r#"
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L3:1-L3:21
                node /node labels=[LBL] /main.dts L3:5-L3:18
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
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L3:1-L3:21
                node /node labels=[LBL] /main.dts L3:5-L3:18
                node /node /main.dts L4:1-L4:22
                property /node/prop = <1>; /main.dts L4:8-L4:19
            "#]],
        );
    }

    /// `&{/}` extends the root node.
    #[test]
    fn mir_extension_root_path() {
        check_mir(
            r#"
/dts-v1/;
/ { foo {}; };
&{/} { prop = <1>; };
"#,
            &[],
            expect![[r#"
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L3:1-L3:15
                node /foo /main.dts L3:5-L3:12
                node / /main.dts L4:1-L4:22
                property /prop = <1>; /main.dts L4:8-L4:19
            "#]],
        );
    }

    /// `&{label/path}` extends a node below the labeled node.
    #[test]
    fn mir_extension_label_path() {
        check_mir(
            r#"
/dts-v1/;
/ { LBL: node { sub { }; }; };
&{LBL/sub} { prop = <1>; };
"#,
            &[],
            expect![[r#"
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L3:1-L3:31
                node /node labels=[LBL] /main.dts L3:5-L3:28
                node /node/sub /main.dts L3:17-L3:25
                node /node/sub /main.dts L4:1-L4:28
                property /node/sub/prop = <1>; /main.dts L4:14-L4:25
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
                dts-v1 /main.dts L2:1-L2:10
                plugin /main.dts L3:1-L3:10
                --- unresolved extensions ---
                  target=&UNKNOWN (1 definitions)
            "#]],
        );
    }

    #[test]
    fn mir_extension_unresolved_path() {
        check_mir(
            r#"
/dts-v1/;
/plugin/;
&{/UNKNOWN} { prop = <1>; };
"#,
            &[],
            expect![[r#"
                dts-v1 /main.dts L2:1-L2:10
                plugin /main.dts L3:1-L3:10
                --- unresolved extensions ---
                  target=&{/UNKNOWN} (1 definitions)
            "#]],
        );
    }

    #[test]
    fn mir_extension_unresolved_label_path() {
        check_mir(
            r#"
/dts-v1/;
/plugin/;
/ { LBL: node {}; };
&{LBL/missing} { prop = <1>; };
"#,
            &[],
            expect![[r#"
                dts-v1 /main.dts L2:1-L2:10
                plugin /main.dts L3:1-L3:10
                node / /main.dts L4:1-L4:21
                node /node labels=[LBL] /main.dts L4:5-L4:18
                --- unresolved extensions ---
                  target=&{LBL/missing} (1 definitions)
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
                dts-v1 /main.dts L2:1-L2:10

                --- errors ---
                Error L3:1-L3:7: Label not found: BOGUS
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
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L3:1-L3:20
                node /bar labels=[foo] /main.dts L3:5-L3:17
                node / /main.dts L4:1-L4:20
                node /baz /main.dts L4:5-L4:17

                --- errors ---
                Warn L4:5-L4:9: Duplicate label `foo`
            "#]],
        );
    }

    #[test]
    fn mir_omit_if_no_ref() {
        check_mir(
            r#"
/dts-v1/;
/ { /omit-if-no-ref/ foo {}; };
"#,
            &[],
            expect![[r#"
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L3:1-L3:32
                node /foo [omit-if-no-ref] /main.dts L3:5-L3:29
            "#]],
        );
    }

    #[test]
    fn mir_err_subnode_outside_node() {
        check_mir(
            r#"
/dts-v1/;
/ { };

#if 1
foo {};
#endif
bar {};
"#,
            &[],
            expect![[r#"
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L3:1-L3:7

                --- errors ---
                Error L6:1-L6:4: Subnode must be defined inside a node
                Error L8:1-L8:4: Subnode must be defined inside a node
            "#]],
        );
    }

    #[test]
    fn mir_err_root_node_inside_node() {
        check_mir(
            r#"
/dts-v1/;
/ {
    foo {
        / { };
    };
};
"#,
            &[],
            expect![[r#"
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L3:1-L7:3
                node /foo /main.dts L4:5-L6:7

                --- errors ---
                Error L5:9: Root node (`/`) must be defined outside other nodes
            "#]],
        );
    }
}
