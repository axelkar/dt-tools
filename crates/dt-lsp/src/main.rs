use dashmap::DashMap;
use dt_analyzer::{analyze_cst, FileDefinition};
use dt_parser::{
    ast::{self, AstNode},
    cst2::{
        parser::{parse, Parse},
        RedNode,
    },
    SourceId, TextRange,
};
use include::PPInclude;
use ropey::Rope;
use std::sync::Arc;
use std::{
    net::{Ipv4Addr, SocketAddr},
    path::PathBuf,
};
use tokio::{net::TcpListener, sync::Mutex};
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use tracing::{debug, info, level_filters::LevelFilter};
use tracing_subscriber::EnvFilter;

mod hover;
mod include;

#[derive(Debug)]
pub struct Document {
    pub text: Rope,
    pub doc: Option<ast::Document>,
    pub analyzed: Option<FileDefinition>,
}

#[derive(Debug)]
struct SharedState {
    document_map: DashMap<SourceId, Document>,
    workspace_folders: Mutex<Vec<WorkspaceFolder>>,
}

#[derive(Debug, Clone)]
struct Backend {
    client: Client,
    state: Arc<SharedState>,
}

type Result<T, E = tower_lsp::jsonrpc::Error> = std::result::Result<T, E>;

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        if let Some(client_info) = params.client_info {
            if let Some(version) = client_info.version {
                tracing::info!("{} {} connected!", client_info.name, version);
            } else {
                tracing::info!("{} connected!", client_info.name);
            }
        }

        // TODO: support legacy clients using root_uri
        if let Some(workspace_folders) = params.workspace_folders {
            tracing::info!("Workspace folders: {workspace_folders:?}");
            *self.state.workspace_folders.lock().await = workspace_folders;
        } else {
            tracing::info!("Connected in single-file mode");
        }
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
                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(OneOf::Left(true)),
                    }),
                    file_operations: None,
                }),
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

    async fn did_change_workspace_folders(&self, params: DidChangeWorkspaceFoldersParams) {
        // TODO: recompute includes etc.
        let mut folders = self.state.workspace_folders.lock().await;
        for removed in params.event.removed {
            let pos = folders.iter().position(|x| *x == removed).unwrap();
            folders.swap_remove(pos);
        }
        folders.extend_from_slice(&params.event.added);
    }

    async fn initialized(&self, _: InitializedParams) {
        info!("Server initialized");
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        debug!("file opened");
        self.client
            .log_message(MessageType::INFO, "file opened!")
            .await;
        self.on_change(
            params.text_document.uri,
            params.text_document.text,
            Some(params.text_document.version),
        )
        .await
    }
    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        assert_eq!(
            params.content_changes[0].range, None,
            "asked for TextDocumentSyncKind::FULL"
        );
        self.on_change(
            params.text_document.uri,
            std::mem::take(&mut params.content_changes[0].text),
            Some(params.text_document.version),
        )
        .await
    }
    async fn did_save(&self, _: DidSaveTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file saved!")
            .await;
    }
    async fn did_close(&self, _: DidCloseTextDocumentParams) {
        debug!("file closed");
        self.client
            .log_message(MessageType::INFO, "file closed!")
            .await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        Ok(hover::hover(self, params).await)
    }
}

impl Backend {
    #[tracing::instrument(skip_all, fields(uri = %uri))]
    async fn on_change(&self, uri: Url, text: String, version: Option<i32>) {
        debug!(version, "File with URI `{uri}` was changed");
        let rope = ropey::Rope::from_str(&text);
        let source_id = SourceId::from(uri.to_string());
        self.state.document_map.insert(
            source_id.clone(),
            Document {
                text: rope.clone(),
                doc: None,
                analyzed: None,
            },
        );

        let Parse {
            green_node,
            lex_errors,
            errors,
        } = parse(&text);

        let mut diagnostics = Vec::new();

        let earliest_lex_error_range = lex_errors.first().map(|e| e.text_range);
        for lex_error in lex_errors {
            diagnostics.push(Diagnostic::new_simple(
                range_to_lsp(lex_error.text_range, &rope).unwrap(),
                format!("{} [lex error]", lex_error.inner),
            ));
        }

        for error in errors {
            if let Some(earliest_lex_error_range) = earliest_lex_error_range {
                if error.primary_span.start >= earliest_lex_error_range.start {
                    break;
                }
            }

            diagnostics.push(Diagnostic::new_simple(
                range_to_lsp(error.primary_span, &rope).unwrap(),
                format!("{} [syntax error]", error.message),
            ));
            for span_label in error.span_labels {
                diagnostics.push(Diagnostic::new(
                    range_to_lsp(span_label.span, &rope).unwrap(),
                    Some(DiagnosticSeverity::HINT),
                    None,
                    None,
                    span_label.message.into_owned(),
                    None,
                    None,
                ));
            }
        }

        let cst = RedNode::new(Arc::new(green_node));

        let parent_path = uri
            .to_file_path()
            .expect("LSP should only allow file: URIs")
            .parent()
            .expect("a file always has a parent")
            .to_owned();

        // TODO: only re-check when includes are updated or include config is changed
        for include in PPInclude::gather_includes(&cst) {
            // Linux kernel DTC include path:
            // - include (for #define's in header files)
            // - scripts/dtc/include-prefixes

            // TODO: don't resolve symlinks! use std::path::absolute once 1.80.0 is stable

            let include_dirs = [
                PathBuf::from("/home/axel/dev/mainlining/linux/include"),
                PathBuf::from("/home/axel/dev/mainlining/linux/scripts/dtc/include-prefixes"),
            ];

            let include_path = include
                .relative
                .then(|| {
                    let relative_path = parent_path.join(&include.path).canonicalize().ok()?;
                    relative_path.exists().then_some(relative_path)
                })
                .flatten()
                .or_else(|| {
                    include_dirs.iter().find_map(|base_path| {
                        let new_path = base_path.join(&include.path).canonicalize().ok()?;
                        new_path.exists().then_some(new_path)
                    })
                });

            let Some(include_path) = include_path else {
                diagnostics.push(Diagnostic::new_simple(
                    range_to_lsp(include.text_range, &rope).unwrap(),
                    "Couldn't find file to include".to_owned(),
                ));
                continue;
            };

            let new_uri = Url::from_file_path(include_path.canonicalize().unwrap()).unwrap();

            let this = self.clone();

            tracing::info!("dot {:?} -> {:?}", uri.to_string(), new_uri.to_string());

            /// <https://github.com/tokio-rs/tokio/issues/2394>
            fn spawn_indirection(this: Backend, include_path: PathBuf, new_uri: Url) {
                tokio::spawn(async move {
                    if let Ok(text) = tokio::fs::read_to_string(include_path).await {
                        this.on_change(new_uri, text, None).await;
                    }
                });
            }

            spawn_indirection(this, include_path, new_uri);
        }

        let doc = ast::Document::cast(cst).expect("parser should always return Document");

        diagnostics.extend(dt_lint::default_lint(&doc, &text).iter().flat_map(|lint| {
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

        let analyzed = analyze_cst(&doc, &text);
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
        self.state.document_map.insert(
            source_id.clone(),
            Document {
                text: rope,
                doc: Some(doc),
                analyzed,
            },
        );

        // TODO: special highlighting for known special names?

        self.client
            .publish_diagnostics(uri.clone(), diagnostics, version)
            .await;
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let first_arg = std::env::args().nth(1);

    let (service, socket) = LspService::new(|client| Backend {
        client,
        state: Arc::new(SharedState {
            document_map: DashMap::new(),
            workspace_folders: Mutex::new(Vec::new()),
        }),
    });

    let tracing_env_filter = EnvFilter::builder()
        .with_default_directive(LevelFilter::INFO.into())
        .from_env_lossy();

    #[cfg(debug_assertions)]
    let tracing_env_filter = tracing_env_filter
        .add_directive("dt_lsp=trace".parse()?)
        .add_directive("dt_parser=trace".parse()?);

    if first_arg == Some("tcp".to_owned()) {
        tracing_subscriber::fmt()
            .with_env_filter(tracing_env_filter)
            .init();

        info!("Using tcp");
        let listener =
            TcpListener::bind(SocketAddr::new(Ipv4Addr::new(127, 0, 0, 1).into(), 9257)).await?;
        let (stream, _) = listener.accept().await.unwrap();
        let (read, write) = tokio::io::split(stream);
        Server::new(read, write, socket).serve(service).await
    } else {
        tracing_subscriber::fmt()
            .with_env_filter(tracing_env_filter)
            .with_writer(std::io::stderr)
            .with_ansi(false)
            .init();

        info!("Using stdio");
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
    let line = rope.try_byte_to_line(offset).ok()?;
    let first_char_of_line = rope.try_line_to_byte(line).ok()?;
    let column = offset - first_char_of_line;
    Some(Position::new(line as u32, column as u32))
}

fn range_to_lsp(text_range: TextRange, rope: &Rope) -> Option<Range> {
    Some(Range {
        start: offset_to_position(text_range.start, rope)?,
        end: offset_to_position(text_range.end, rope)?,
    })
}
