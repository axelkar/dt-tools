use crate::position_to_offset;
use dt_parser::{
    ast::{self, AstNode},
    cst::NodeKind,
    lexer::TokenKind,
    match_ast, SourceId,
};
use tower_lsp_server::{
    lsp_types::{
        GotoDefinitionParams, GotoDefinitionResponse, Location, MessageType, Position, Range, Uri,
    },
    UriExt,
};

pub async fn goto_definition(
    state: &crate::Backend,
    params: GotoDefinitionParams,
) -> Option<GotoDefinitionResponse> {
    let params = params.text_document_position_params;
    let uri = params.text_document.uri.to_string();
    tracing::info!(?uri);
    let source_id = SourceId::from(uri.clone());
    let document = state.state.document_map.get(&source_id)?;

    let cst = document.file.as_ref()?.syntax();
    let offset = position_to_offset(params.position, &document.text)?;

    if let Some((include_path, _)) = document.included_paths.as_ref().and_then(|v| {
        v.iter()
            .find(|(_, text_range)| text_range.byte_range().contains(&offset))
    }) {
        let uri = Uri::from_file_path(include_path).expect("Included path should be absolute");
        return Some(GotoDefinitionResponse::Scalar(Location::new(
            uri,
            Range {
                start: Position::new(0, 0),
                end: Position::new(0, 0),
            },
        )));
    }

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
                        None
                    },
                    ast::DtNode(_node) => {
                        // call doc and sema db
                        None
                    },
                    ast::DtLabel(_it) => {
                        None
                    },
                    ast::DtPhandle(phandle) => {
                        tracing::debug!("Is phandle");

                        // TODO: Something like Either<&RedNode, Arc<RedNode>> so I don't have to
                        // clone here

                        let node = phandle.syntax().parent.as_ref().unwrap().clone();
                        if let Some(_node) = ast::DtNode::cast(node) {}

                        None
                    },
                    _ => None
                }
            }
        } else {
            None
        }
    }
}
