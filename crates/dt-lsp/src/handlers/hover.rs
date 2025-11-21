use crate::{offset_to_position, position_to_offset};
use dt_parser::{
    ast::{self, AstNode},
    cst::NodeKind,
    lexer::TokenKind,
    match_ast, SourceId,
};
use itertools::Itertools;
use tower_lsp_server::lsp_types::{
    Hover, HoverContents, HoverParams, MarkupContent, MarkupKind, MessageType, Range,
};

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

fn _node_definition(node: &ast::DtNode, src: &str) -> String {
    let path = node.path(src).join("/");

    // TODO: get documentation by `compatible`
    format!("node `/{path}`")
}

pub async fn hover(state: &crate::Backend, params: HoverParams) -> Option<Hover> {
    let params = params.text_document_position_params;
    let uri = params.text_document.uri.to_string();
    tracing::info!(?uri);
    let source_id = SourceId::from(uri.clone());
    let document = state.state.document_map.get(&source_id)?;

    let cst = document.file.as_ref()?.syntax();
    let offset = position_to_offset(params.position, &document.text)?;

    // TODO: try prev offset?
    // TODO: Check for nodes above e.g. quit the search directly on an error node
    let token = cst.token_at_offset(offset)?.clone();
    tracing::debug!(kind = ?token.green.kind, "Found token");

    if token.green.kind.is_trivia() {
        return None;
    }
    if token.parent.green.kind == NodeKind::ParseError {
        state
            .client
            .show_message(MessageType::ERROR, "parent node is error")
            .await;
        return None;
    }
    if token.green.kind == TokenKind::Unrecognized {
        state
            .client
            .show_message(MessageType::ERROR, "token is unrecognized")
            .await;
        return None;
    }

    let rope = &document.text;

    // definition
    {
        let parent = token.parent.clone();
        tracing::debug!("analyzed, direct parent #1 kind = {:?}", parent.green.kind);

        // FIXME: NameRef, NameDef
        // TODO: search for node at offset directly
        if let Some(name) = ast::NameRef::cast(parent) {
            let parent = name.syntax().parent.clone()?;
            match_ast! {
                match parent {
                    ast::DtProperty(_it) => {
                        // call doc and sema db
                        Some(("property".to_owned(), None))
                    },
                    ast::DtNode(_node) => {
                        // call doc and sema db
                        Some(("node".into(), None))
                    },
                    ast::DtLabel(_it) => {
                        Some(("label".into(), None))
                    },
                    ast::DtPhandle(phandle) => {
                        tracing::debug!("Is phandle");

                        // TODO: Something like Either<&RedNode, Arc<RedNode>> so I don't have to
                        // clone here

                        let node = phandle.syntax().parent.as_ref().unwrap().clone();
                        if let Some(_node) = ast::DtNode::cast(node) {}

                        if phandle.is_path() {
                            let _path = token.text();
                            None // TODO
                        } else {
                            let label_name = token.text();
                            // TODO: to_string is a HACK
                            //let label_defs = analyzed.exported_labels.get(&label_name.to_string())?;
                            //let label = label_defs.last()?; // TODO: show all label defs
                            //Some((format!("{}\n---\nlabel `{label_name}`", node_definition(&label.node_ast, &src)), None))
                            Some((format!("aaa\n---\nlabel `{label_name}`"), None))
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
                value: markdown,
            }),
            range: span.and_then(|span: &dt_parser::TextRange| {
                // TODO: proper Span type with file IDs and more
                Some(Range::new(
                    offset_to_position(span.start, rope)?,
                    offset_to_position(span.end, rope)?,
                ))
            }),
        }
    })
}
