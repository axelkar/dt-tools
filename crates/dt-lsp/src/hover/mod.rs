use crate::{offset_to_position, position_to_offset};
use dt_parser::{
    ast::{self, AstNode, HasIdent},
    match_ast, SourceId,
};
use itertools::Itertools;
use tower_lsp::lsp_types::*;

// ## DynMap:
// Key<ast::Fn, FunctionId>
//
// ## rust markup
// {if mod path}
// ```rust
// {mod path}
// ```
//
// {fi}
// ```rust
// {sema desc e.g. fn main()}
// ```
// {if doc}
// ___
// {doc}
// {fi}

fn node_definition(node: &ast::DtNode, src: &str) -> Option<String> {
    let path = node.path(src).join("/");

    // TODO: get documentation by `compatible`
    Some(format!("node `/{path}`"))
}

pub async fn hover(state: &crate::Backend, params: HoverParams) -> Option<Hover> {
    let params = params.text_document_position_params;
    let uri = params.text_document.uri.to_string();
    let source_id = SourceId::from(uri.to_string());
    let document = state.document_map.get(&source_id)?;

    let cst = document.cst.clone()?;
    let offset = position_to_offset(params.position, &document.text)?;

    // TODO: try prev offset?
    // TODO: Check for nodes above e.g. quit the search directly on an error node
    let token = cst.token_at_offset(offset)?.clone();

    if token.green.kind.is_trivia() {
        return None;
    }
    if token.parent.green.kind.is_error() {
        state
            .client
            .show_message(MessageType::ERROR, "parent node is error")
            .await;
        return None;
    }
    if token.green.kind.is_error() {
        state
            .client
            .show_message(MessageType::ERROR, "token is error")
            .await;
        return None;
    }

    let rope = &document.text;
    let src: String = rope.into();
    let analyzed = document.analyzed.as_ref()?;

    // definition
    {
        let parent = token.parent.clone();

        if let Some(ident) = ast::Ident::cast(parent) {
            let parent = ident.syntax().parent.clone()?;
            match_ast! {
                match parent {
                    ast::DtProperty(_it) => {
                        // call doc and sema db
                        Some(("property".to_owned(), None))
                    },
                    ast::DtNode(node) => {
                        if node.is_extension() {
                            let label_name = node.ident()?.text(&src)?;
                            let Some(label_defs) = analyzed.labels.get(label_name) else {
                                state.client.show_message(MessageType::WARNING, "Unknown label").await;
                                return None
                            };
                            let label = label_defs.last()?; // TODO: show all label defs
                            Some((format!("{}\n---\nlabel `{label_name}`", node_definition(&label.node_ast, &src)?), None))
                        } else {
                            // call doc and sema db
                            Some(("node".into(), None))
                        }
                    },
                    ast::DtLabel(_it) => {
                        Some(("label".into(), None))
                    },
                    ast::DtPhandle(phandle) => {
                        if phandle.is_path() {
                            let _path = token.text(&src);
                            None // TODO
                        } else {
                            let label_name = token.text(&src)?;
                            let Some(label_defs) = analyzed.labels.get(label_name) else {
                                state.client.show_message(MessageType::WARNING, "Unknown label").await;
                                return None
                            };
                            let label = label_defs.last()?; // TODO: show all label defs
                            Some((format!("{}\n---\nlabel `{label_name}`", node_definition(&label.node_ast, &src)?), None))
                        }
                    },
                    _ => None
                }
            }
        } else {
            None
        }
    }
    // TODO: literals
    .map(|(markdown, span)| {
        Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: markdown
            }),
            range: span.and_then(|span: &dt_parser::TextRange| {
                // TODO: proper Span type with file IDs and more
                Some(Range::new(
                    offset_to_position(span.start, rope)?,
                    offset_to_position(span.end, rope)?
                ))
            })
        }
    })
}
