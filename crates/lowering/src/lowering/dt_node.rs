use std::borrow::Cow;

use dt_tools_analyzer::macros::SubstitutedBody;
use dt_tools_diagnostic::{Diagnostic, DiagnosticCollector, MultiSpan, Severity, Span, SpanLabel};
use dt_tools_parser::{
    TextRange,
    ast::{
        self, AstNode, AstToken, HasDtPhandle, HasLabels, HasMacroInvocation, HasName,
        HasUnitAddress,
    },
    lexer::TokenKind,
    parser::Entrypoint,
};

use super::{IntraFileCtx, lower_phandle};
use crate::{
    db::BaseDb,
    file::File,
    lowering::item::resolve_phandle,
    macros::{MacroCtx, env::TrackedMapEnvMut, substitute_macro_tok},
    mir::{
        Mir, MirDefinition, MirDefinitionValue, MirNodeData, MirPhandleTarget, MirProvenance,
        UnresolvedExtension,
    },
};
use crate::{emit_parse_errors, lowering::item::lower_item};

/// Lowers an [`ast::DtNode`] and its subtree to flat [`MirDefinition`]s.
pub(crate) fn lower_dt_node(
    ctx: &mut IntraFileCtx<'_, '_, impl DiagnosticCollector<File>>,
    parent_node_path: &str,
    dt_node: &ast::DtNode,
) {
    let provenance = MirProvenance {
        file: ctx.file,
        text_range: dt_node.syntax().text_range(),
    };
    let omit_if_no_ref = dt_node.omit_if_no_ref();

    let lower_resolved = |ctx: &mut _, provenance, node_path: &String| {
        let labels = collect_labels(ctx, dt_node, node_path);

        // Emit the node definition.
        ctx.mir.definitions.push(MirDefinition {
            path: node_path.clone(),
            value: MirDefinitionValue::Node(MirNodeData {
                labels,
                omit_if_no_ref,
            }),
            provenance,
        });

        // Process the node body: subnodes and properties.
        lower_dt_node_body(ctx, node_path, dt_node);
    };

    // Handle extensions: &label { } or &{/path} { }
    if let Some(phandle) = dt_node.dt_phandle() {
        let Ok(target) = lower_phandle(
            ctx.db,
            ctx.env,
            ctx.diag,
            &mut |tr| tr.within_file(ctx.file),
            &phandle,
        ) else {
            return;
        };

        if let Ok(ref target_path) = resolve_phandle(ctx, &target, &phandle) {
            lower_resolved(ctx, provenance, target_path);
        } else {
            let mut body_mir = Mir::default();
            lower_dt_node_body_replace_mir(ctx, &mut body_mir, "/", dt_node);

            if ctx.is_overlay() {
                // Overlay; we can leave the extension unresolved

                // Propagate any nested unresolved extensions.
                ctx.mir
                    .unresolved_extensions
                    .extend(body_mir.unresolved_extensions);

                if let MirPhandleTarget::Label(label) = target {
                    ctx.mir.unresolved_extensions.push(UnresolvedExtension {
                        label,
                        body: body_mir.definitions,
                        provenance,
                    });
                }
            }
        }
    } else {
        let name = if let Ok(name) = get_name_and_unit_addr(
            ctx.db,
            ctx.env,
            ctx.diag,
            &mut |tr| tr.within_file(ctx.file),
            dt_node,
        ) {
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
                ctx.diag.emit(Diagnostic::new(
                    dt_node.syntax().text_range().within_file(ctx.file),
                    "Subnode must be defined inside a node".into(),
                    Severity::Error,
                ));
                return;
            }

            if root_node && !parent_node_path.is_empty() {
                ctx.diag.emit(Diagnostic::new(
                    dt_node.syntax().text_range().within_file(ctx.file),
                    "Root node (`/`) must be defined outside other nodes".into(),
                    Severity::Error,
                ));
                return;
            }

            // Build the node's full path.
            let node_path = build_path(parent_node_path, &name);

            lower_resolved(ctx, provenance, &node_path);
        } else {
            // Name is missing, but process so diagnostics are emitted
            let mut body_mir = Mir::default();
            lower_dt_node_body_replace_mir(ctx, &mut body_mir, "/", dt_node);
        }
    }
}

/// Collects labels defined on an [`ast::DtNode`].
pub(crate) fn collect_labels(
    ctx: &mut IntraFileCtx<'_, '_, impl DiagnosticCollector<File>>,
    dt_node: &ast::DtNode,
    node_path: &str,
) -> Vec<String> {
    let mut labels: Vec<String> = Vec::new();
    for label_ast in dt_node.labels() {
        if let Ok(label_name) = resolve_name_or_macro(
            ctx.db,
            ctx.env,
            ctx.diag,
            &mut |tr| tr.within_file(ctx.file),
            &label_ast,
        ) {
            // Check for duplicate labels (dtc: no duplicates globally).
            if let Some((old_path, span)) = ctx.env.get_label(ctx.db, &label_name)
                && old_path != node_path
            {
                ctx.diag.emit(Diagnostic {
                    span: MultiSpan {
                        primary_spans: vec![label_ast.syntax().text_range().within_file(ctx.file)],
                        span_labels: vec![SpanLabel {
                            span: *span,
                            msg: Cow::Owned(format!(
                                "Previous definition of label `{label_name}` here"
                            )),
                        }],
                    },
                    msg: Cow::Owned(format!("Duplicate label `{label_name}`")),
                    severity: Severity::Warn,
                });
            } else {
                ctx.env.own_label_map.insert(
                    label_name.clone(),
                    Some((
                        node_path.to_owned(),
                        label_ast.syntax().text_range().within_file(ctx.file),
                    )),
                );
                labels.push(label_name);
            }
        }
    }
    labels
}

/// [`lower_dt_node_body`] but with [`IntraFileCtx::mir`] replaced.
pub(crate) fn lower_dt_node_body_replace_mir(
    ctx: &mut IntraFileCtx<'_, '_, impl DiagnosticCollector<File>>,
    body_mir: &mut Mir,
    parent_node_path: &str,
    dt_node: &ast::DtNode,
) {
    let is_overlay = ctx.is_overlay();
    lower_dt_node_body(
        &mut IntraFileCtx {
            db: ctx.db,
            file: ctx.file,
            env: ctx.env,
            diag: ctx.diag,
            mir: body_mir,
            parent_is_overlay: is_overlay,
            parent_dir_path: ctx.parent_dir_path,
            include_dirs: ctx.include_dirs,
            processed_files: ctx.processed_files,
            includes: ctx.includes,
        },
        parent_node_path,
        dt_node,
    );
}

/// Lowers the body of an [`ast::DtNode`].
pub(crate) fn lower_dt_node_body(
    ctx: &mut IntraFileCtx<'_, '_, impl DiagnosticCollector<File>>,
    parent_node_path: &str,
    dt_node: &ast::DtNode,
) {
    // TODO: preprocessor conditionals in nodes

    for item in dt_node.items() {
        lower_item(ctx, parent_node_path, item);
    }
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

/// Resolves a macro-substitutable name into a concrete name.
pub(crate) fn resolve_name_or_macro<'db, Ast: HasName + HasMacroInvocation>(
    db: &'db dyn BaseDb,
    env: &mut TrackedMapEnvMut<'db>,
    diag: &impl DiagnosticCollector<File>,
    spanner: &mut impl FnMut(TextRange) -> Span<File>,
    ast: &Ast,
) -> Result<String, ()> {
    let (
        _span,
        SubstitutedBody {
            source_mappings: _,
            substituted_text,
        },
    ) = if let Some(name_ast) = ast.name() {
        match substitute_macro_tok(db, env, diag, spanner, &MacroCtx::Implicit(&name_ast))? {
            Some(val) => val,
            None => return Ok(name_ast.syntax().text().as_str().to_owned()),
        }
    } else if let Some(invoc) = ast.macro_invocation() {
        substitute_macro_tok(db, env, diag, spanner, &MacroCtx::Explicit(&invoc))?
            .expect("resolve_macro_to_value should not return Ok(None) with an explicit macro")
    } else {
        return Err(());
    };

    let parse = Entrypoint::Name.parse(&substituted_text);

    // TODO: trmaps -> spanner
    emit_parse_errors(&parse, diag, spanner);

    let ast = ast::EntryName::cast(parse.red_node())
        .expect("Entrypoint::Name should parse to ast::EntryName");

    resolve_name_or_macro(db, env, diag, spanner, &ast)
}

/// Resolves and concatenates together the name and unit address of a compatible AST node.
pub(crate) fn get_name_and_unit_addr<'db, Ast: HasName + HasMacroInvocation + HasUnitAddress>(
    db: &'db dyn BaseDb,
    env: &mut TrackedMapEnvMut<'db>,
    diag: &impl DiagnosticCollector<File>,
    spanner: &mut impl FnMut(TextRange) -> Span<File>,
    ast: &Ast,
) -> Result<String, ()> {
    let without_unit_addr = resolve_name_or_macro(db, env, diag, spanner, ast);

    let unit_addr = ast
        .unit_address()
        .map(|ast| resolve_name_or_macro(db, env, diag, spanner, &ast))
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
                dts-v1  /main.dts 1..10
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
                dts-v1  /main.dts 1..10
                node   / /main.dts 11..31
                node labels=[LBL] /node /main.dts 15..28
                node   /node /main.dts 32..53
                property = CellList(Bits32([Number(1)])) /node/prop /main.dts 39..50
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
                dts-v1  /main.dts 1..10
                plugin  /main.dts 11..20
                --- unresolved ---
                  label=UNKNOWN (1 definitions)
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
                dts-v1  /main.dts 1..10

                --- errors ---
                Error 11..17: Label not found: BOGUS
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
                dts-v1  /main.dts 1..10
                node   / /main.dts 11..30
                node   / /main.dts 31..50
                node labels=[foo] /bar /main.dts 15..27
                node   /baz /main.dts 35..47

                --- errors ---
                Warn 35..39: Duplicate label `foo`
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
                dts-v1  /main.dts 1..10
                node   / /main.dts 11..42
                /omit-if-no-ref/ node /foo /main.dts 15..39
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
                dts-v1  /main.dts 1..10
                node   / /main.dts 11..17

                --- errors ---
                Error 25..32: Subnode must be defined inside a node
                Error 40..47: Subnode must be defined inside a node
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
                dts-v1  /main.dts 1..10
                node   / /main.dts 11..49
                node   /foo /main.dts 19..46

                --- errors ---
                Error 33..39: Root node (`/`) must be defined outside other nodes
            "#]],
        );
    }
}
