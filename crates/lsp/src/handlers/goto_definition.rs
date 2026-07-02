use dt_tools_lowering::{db::BaseDb, includes::document_deps};
use dt_tools_parser::{
    ast::{self, AstNode},
    cst::NodeKind,
    lexer::TokenKind,
    match_ast,
};
use tower_lsp_server::ls_types::{
    GotoDefinitionParams, GotoDefinitionResponse, Location, MessageType, Position, Range,
};

use crate::{lsp_utils::position_to_offset, uri_to_path};

// TODO: go to phandle, macro definition or other definitions of the same node/property
pub async fn goto_definition(
    state: &crate::Backend,
    params: GotoDefinitionParams,
) -> Option<GotoDefinitionResponse> {
    let params = params.text_document_position_params;
    let uri = params.text_document.uri;
    let path = uri_to_path(&uri).expect("Invalid document URI");
    tracing::info!(?path);

    let db = state.session.db.lock().clone();
    let db = &db;
    let file = db.get_files().get_file(db, &path);

    let Some(rope) = dt_tools_lowering::rope(db, file) else {
        return None;
    };
    let parse = dt_tools_lowering::parse_file(db, file)?;
    let file_ast = parse.parse(db).source_file();
    let cst = file_ast.syntax();

    let offset = position_to_offset(params.position, rope)?;

    let Ok(document_deps) = document_deps(db, file) else {
        return None;
    };

    if let Some((_, file)) = document_deps
        .included_files(db)
        .iter()
        .find(|(text_range, _)| text_range.byte_range().contains(&offset))
    {
        return Some(GotoDefinitionResponse::Scalar(Location::new(
            crate::lsp_utils::file_uri(db, *file),
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
