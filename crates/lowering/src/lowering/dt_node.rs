use std::borrow::Cow;

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

use super::{IntraFileCtx, dt_property::lower_dt_property, lower_phandle};
use crate::emit_parse_errors;
use crate::{
    db::BaseDb,
    file::File,
    lowering::toplevel::{emit_delete_directive, resolve_phandle},
    macros::{MacroCtx, env::TrackedMapEnvMut, substitute_macro_tok},
    mir::{
        Mir, MirDefinition, MirDefinitionValue, MirNodeData, MirPhandleTarget, MirProvenance,
        UnresolvedExtension,
    },
};

/// Lowers an [`ast::DtNode`] and its subtree to flat [`MirDefinition`]s.
pub(crate) fn lower_dt_node(
    ctx: &mut IntraFileCtx<'_, '_, impl DiagnosticCollector<File>>,
    path_prefix: &str,
    dt_node: &ast::DtNode,
) {
    let provenance = MirProvenance {
        file: ctx.file,
        text_range: dt_node.syntax().text_range(),
    };

    let lower_resolved = |ctx: &mut _, provenance, node_path: &String| {
        let labels = collect_labels(ctx, dt_node, node_path);

        // Emit the node definition.
        ctx.mir.definitions.push(MirDefinition {
            path: node_path.clone(),
            value: MirDefinitionValue::Node(MirNodeData { labels }),
            provenance,
        });

        // Process the node body: subnodes and properties.
        lower_dt_node_body(ctx, node_path, dt_node);
    };

    // Handle extensions: &label { } or &{/path} { }
    if let Some(phandle) = dt_node.dt_phandle() {
        let Some(target) = lower_phandle(
            ctx.db,
            ctx.env,
            ctx.diag,
            &mut |tr| tr.within_file(ctx.file),
            &phandle,
        ) else {
            return;
        };

        if let Some(ref target_path) = resolve_phandle(ctx, &target, &phandle) {
            lower_resolved(ctx, provenance, target_path);
        } else {
            let mut body_mir = Mir::default();
            lower_dt_node_body_replace_mir(ctx, &mut body_mir, "", dt_node);

            if ctx.is_overlay {
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

            lower_resolved(ctx, provenance, &node_path);
        } else {
            // Name is missing, but process so diagnostics are emitted
            let mut body_mir = Mir::default();
            lower_dt_node_body_replace_mir(ctx, &mut body_mir, "", dt_node);
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
        if let Some(label_name) = resolve_name_or_macro(
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
    lower_dt_node_body(
        &mut IntraFileCtx {
            db: ctx.db,
            file: ctx.file,
            env: ctx.env,
            diag: ctx.diag,
            mir: body_mir,
            is_overlay: ctx.is_overlay,
        },
        parent_node_path,
        dt_node,
    );
}

/// Processes the body of an [`ast::DtNode`].
pub(crate) fn lower_dt_node_body(
    ctx: &mut IntraFileCtx<'_, '_, impl DiagnosticCollector<File>>,
    parent_node_path: &str,
    dt_node: &ast::DtNode,
) {
    // TODO: preprocessor conditionals in nodes

    for item in dt_node.node_items() {
        match item {
            ast::NodeItem::DtProperty(prop) => {
                if let Some(def) = lower_dt_property(ctx, parent_node_path, &prop) {
                    ctx.mir.definitions.push(def);
                }
            }
            ast::NodeItem::DtNode(subnode) => {
                lower_dt_node(ctx, parent_node_path, &subnode);
            }
            ast::NodeItem::Directive(dir) => {
                emit_delete_directive(ctx, parent_node_path, &dir);
            }
        }
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
) -> Option<String> {
    let (_span, _trmaps, expanded) = if let Some(name_ast) = ast.name() {
        match substitute_macro_tok(db, env, diag, spanner, &MacroCtx::Implicit(&name_ast)) {
            Some(val) => val,
            None => return Some(name_ast.syntax().text().as_str().to_owned()),
        }
    } else if let Some(invoc) = ast.macro_invocation() {
        substitute_macro_tok(db, env, diag, spanner, &MacroCtx::Explicit(&invoc))?
    } else {
        return None;
    };

    let parse = Entrypoint::Name.parse(&expanded);

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
