use dashmap::DashMap;
use dt_analyzer::{analyze_cst, FileDefinition};
use dt_parser::{
    ast::{self, AstNode},
    cst::RedNode,
    SourceId,
};
use ropey::Rope;
use std::net::{Ipv4Addr, SocketAddr};
use std::sync::Arc;
use tokio::net::TcpListener;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

mod hover;

#[derive(Debug)]
pub struct Document {
    pub text: Rope,
    pub cst: Option<Arc<RedNode>>,
    pub analyzed: Option<FileDefinition>,
}

#[derive(Debug)]
struct Backend {
    client: Client,
    document_map: DashMap<SourceId, Document>,
}

type Result<T, E = tower_lsp::jsonrpc::Error> = std::result::Result<T, E>;

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: "dt-lsp".to_owned(),
                version: Some(env!("CARGO_PKG_VERSION").to_owned()),
            }),
            capabilities: ServerCapabilities {
                //position_encoding: (),
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                //selection_range_provider: (),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                //completion_provider: (),
                //signature_help_provider: (),
                // definition_provider: (), // TODO: refer to kernel Documentation/bindings ?!?
                // implementation_provider: (), // TODO: for labels
                // references_provider: (), // TODO: for labels
                //document_highlight_provider: (),
                //document_symbol_provider: (),
                //code_action_provider: (),
                //code_lens_provider: (),
                //document_formatting_provider: (),
                //document_on_type_formatting_provider: (),
                // rename_provider: (), // TODO: for labels
                //document_link_provider: (),
                //color_provider: (),
                //folding_range_provider: (),
                //declaration_provider: (),
                //execute_command_provider: (),
                //linked_editing_range_provider: (),
                //inline_value_provider: (),
                //inlay_hint_provider: (),
                //diagnostic_provider: (),
                ..Default::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file opened!")
            .await;
        self.on_change(
            params.text_document.uri,
            params.text_document.text,
            params.text_document.version,
        )
        .await
    }
    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file changed!")
            .await;
        assert_eq!(
            params.content_changes[0].range, None,
            "asked for TextDocumentSyncKind::FULL"
        );
        self.on_change(
            params.text_document.uri,
            std::mem::take(&mut params.content_changes[0].text),
            params.text_document.version,
        )
        .await
    }
    async fn did_save(&self, _: DidSaveTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file saved!")
            .await;
    }
    async fn did_close(&self, _: DidCloseTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file closed!")
            .await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        Ok(hover::hover(self, params).await)
    }
}

impl Backend {
    async fn on_change(&self, uri: Url, text: String, version: i32) {
        let rope = ropey::Rope::from_str(&text);
        let source_id = SourceId::from(uri.to_string());
        self.document_map.insert(
            source_id.clone(),
            Document {
                text: rope.clone(),
                cst: None,
                analyzed: None,
            },
        );
        let cst = dt_parser::parse(&text);

        let mut diagnostics = Vec::new();

        if let Some(cst) = cst {
            let ast = ast::Document::cast(cst.clone()).unwrap();
            diagnostics.extend(dt_lint::default_lint(&ast, &text).iter().flat_map(|lint| {
                lint.span
                    .primary_spans
                    .iter()
                    .filter_map(|span| {
                        let range = Range::new(
                            offset_to_position(span.start, &rope)?,
                            offset_to_position(span.end, &rope)?,
                        );
                        Some(Diagnostic::new(
                            range,
                            Some(match lint.severity {
                                dt_lint::LintSeverity::Warn => DiagnosticSeverity::WARNING,
                                dt_lint::LintSeverity::Error => DiagnosticSeverity::ERROR,
                            }),
                            None,
                            None,
                            format!("{} [{}]", lint.msg, lint.id),
                            None,
                            None,
                        ))
                    })
                    .chain(lint.span.span_labels.iter().filter_map(|(span, hint)| {
                        let range = Range::new(
                            offset_to_position(span.start, &rope)?,
                            offset_to_position(span.end, &rope)?,
                        );
                        Some(Diagnostic::new(
                            range,
                            Some(DiagnosticSeverity::HINT),
                            None,
                            None,
                            format!("{} [{}.hint]", hint, lint.id),
                            None,
                            None,
                        ))
                    }))
            }));

            let analyzed = analyze_cst(cst.clone(), &text);
            if analyzed.is_none() {
                // TODO: only analyze or show this message on save
                self.client
                    .show_message(MessageType::ERROR, "failed to analyze document")
                    .await;
            } else {
                self.client
                    .show_message(MessageType::INFO, "analyzed document!")
                    .await;
            }
            self.document_map.insert(
                source_id.clone(),
                Document {
                    text: rope,
                    cst: Some(cst),
                    analyzed,
                },
            );
        } else {
            diagnostics.push(Diagnostic::new_simple(
                {
                    let p = Position::new(0, 0);
                    Range::new(p, p)
                },
                "failed to parse document".to_owned(),
            ));
        }

        // TODO: special highlighting for known special names?

        self.client
            .publish_diagnostics(uri.clone(), diagnostics, Some(version))
            .await;
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let first_arg = std::env::args().nth(1);

    let (service, socket) = LspService::new(|client| Backend {
        client,
        document_map: DashMap::new(),
    });

    if first_arg == Some("tcp".to_owned()) {
        let listener =
            TcpListener::bind(SocketAddr::new(Ipv4Addr::new(127, 0, 0, 1).into(), 9257)).await?;
        let (stream, _) = listener.accept().await.unwrap();
        let (read, write) = tokio::io::split(stream);
        Server::new(read, write, socket).serve(service).await
    } else {
        let read = tokio::io::stdin();
        let write = tokio::io::stdout();
        Server::new(read, write, socket).serve(service).await
    }
    Ok(())
}

fn position_to_offset(position: Position, rope: &Rope) -> Option<usize> {
    Some(rope.try_line_to_char(position.line as usize).ok()? + position.character as usize)
}

fn offset_to_position(offset: usize, rope: &Rope) -> Option<Position> {
    let line = rope.try_char_to_line(offset).ok()?;
    let first_char_of_line = rope.try_line_to_char(line).ok()?;
    let column = offset - first_char_of_line;
    Some(Position::new(line as u32, column as u32))
}
