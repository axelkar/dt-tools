use axka_rcu::{triomphe, Rcu};
use dt_analyzer::new::stage1::AnalyzedToplevel;
use dt_diagnostic::DiagnosticCollector;
use dt_parser::{
    ast::{self, AstNode},
    cst2::{
        parser::{parse, Parse},
        RedNode,
    },
    SourceId, TextRange,
};
use ropey::Rope;
use std::{borrow::Cow, sync::Arc};
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

/// A fast map that can be sent between threads
pub type FxDashMap<K, V> =
    dashmap::DashMap<K, V, std::hash::BuildHasherDefault<rustc_hash::FxHasher>>;

#[derive(Debug)]
pub struct Document {
    pub text: Rope,
    pub file: Option<ast::SourceFile>,
    pub analyzed: Option<Vec<AnalyzedToplevel>>,
}

#[derive(Debug)]
struct SharedState {
    document_map: FxDashMap<SourceId, Document>,
    workspace_folders: Mutex<Vec<WorkspaceFolder>>,
    /// The file where evaluation will start from
    ///
    /// Files not (recursively) included in this file must not be analyzed
    main_file: Rcu<Option<SourceId>>,
}

#[derive(Clone)]
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

        let source_id = SourceId::from(params.text_document.uri.as_str());

        if self.state.main_file.read().is_none() {
            self.state
                .main_file
                .write(triomphe::Arc::new(Some(source_id)));
        }

        // TODO: warn files that aren't included by main_file

        self.on_change(
            params.text_document.uri,
            params.text_document.text,
            Some(params.text_document.version),
            tokio::runtime::Handle::current(),
        );
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
            tokio::runtime::Handle::current(),
        );
    }
    async fn did_save(&self, _: DidSaveTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file saved!")
            .await;
    }
    async fn did_close(&self, _: DidCloseTextDocumentParams) {
        debug!("file closed");

        // TODO: remove main file?

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
    fn on_change(
        &self,
        uri: Url,
        text: String,
        version: Option<i32>,
        tokio_handle: tokio::runtime::Handle,
    ) {
        debug!(version, "File with URI `{uri}` was changed");
        let rope = ropey::Rope::from_str(&text);
        let source_id = SourceId::from(uri.as_str());
        self.state.document_map.insert(
            source_id.clone(),
            Document {
                text: rope.clone(),
                file: None,
                analyzed: None,
            },
        );
        // TODO: Check if it exists already, with equal text
        // It may be an included file or a reopened file

        let is_main_file = self.state.main_file.read().as_deref() == Some(&source_id);

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
                    span_label.msg.into_owned(),
                    None,
                    None,
                ));
            }
        }

        let cst = RedNode::new(Arc::new(green_node));

        // TODO: use Parse::source_file
        let file = ast::SourceFile::cast(cst).expect("parser should always return SourceFile");

        diagnostics.extend(
            dt_lint::default_lint(&file, &text, is_main_file)
                .iter()
                .flat_map(|lint| {
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
                }),
        );

        let mut new_diagnostics = Vec::new();
        let diag = parking_lot::Mutex::new(&mut new_diagnostics);

        let analyzed = dt_analyzer::new::stage1::analyze_file(&file, &text, &diag);
        let includes = &[]; // TODO
        let analyzed2 = dt_analyzer::new::stage2::compute(
            analyzed.iter().filter_map(|a| a.as_node()),
            includes,
            &diag,
        );
        //if is_main_file {
        if false {
            use std::io::Write;
            let mut f = std::fs::File::create("analyzed2.dbg").unwrap();
            write!(f, "analyzed2={:#?}", analyzed2).unwrap();
            let mut f = std::fs::File::create("analyzed1.dbg").unwrap();
            write!(f, "analyzed1={:#?}", analyzed).unwrap();
        }

        let parent_path = uri
            .to_file_path()
            .expect("LSP should only allow file: URIs")
            .parent()
            .expect("a file always has a parent")
            .to_owned();

        // Linux kernel DTC include path:
        // - include (for #define's in header files)
        // - scripts/dtc/include-prefixes

        let include_dirs = &[
            PathBuf::from("/home/axel/dev/mainlining/linux/include"),
            PathBuf::from("/home/axel/dev/mainlining/linux/scripts/dtc/include-prefixes"),
        ];

        // TODO: only re-check when includes are updated or include config is changed
        for include in analyzed.iter().filter_map(AnalyzedToplevel::as_include) {
            let include_path = include.find_file(&parent_path, include_dirs);

            let Some(include_path) = include_path else {
                diag.emit(dt_diagnostic::Diagnostic::new(
                    include.text_range,
                    Cow::Borrowed("Couldn't find file to include"),
                    dt_diagnostic::Severity::Error,
                ));
                continue;
            };

            let new_uri = Url::from_file_path(include_path.clone()).unwrap();

            tracing::info!("dot {:?} -> {:?}", uri.to_string(), new_uri.to_string());

            if !self
                .state
                .document_map
                .contains_key(&SourceId::from(new_uri.to_string()))
            {
                let this = self.clone();
                let tokio_handle = tokio_handle.clone();
                rayon::spawn(move || {
                    // AnalyzedInclude::find_file made sure it exists
                    if let Ok(text) = std::fs::read_to_string(include_path) {
                        this.on_change(new_uri, text, None, tokio_handle);
                    }
                });
            }
        }

        for new_diagnostic in new_diagnostics {
            // TODO: level
            for primary_span in new_diagnostic.span.primary_spans {
                diagnostics.push(Diagnostic::new_simple(
                    range_to_lsp(primary_span, &rope).unwrap(),
                    new_diagnostic.msg.clone().into_owned(),
                ));
            }

            // TODO: relatedInformation instead of HINT
            // https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnosticRelatedInformation
            for span_label in new_diagnostic.span.span_labels {
                diagnostics.push(Diagnostic::new(
                    range_to_lsp(span_label.span, &rope).unwrap(),
                    Some(DiagnosticSeverity::HINT),
                    None,
                    None,
                    span_label.msg.into_owned(),
                    None,
                    None,
                ));
            }
        }

        self.state.document_map.insert(
            source_id.clone(),
            Document {
                text: rope,
                file: Some(file),
                analyzed: Some(analyzed),
            },
        );

        // TODO: special highlighting for known special names?
        // phandle, #*-cells, #*-size, pinctrl-*, etc.

        diagnostics.dedup();
        let client = self.client.clone();
        tokio_handle.spawn(async move {
            client
                .publish_diagnostics(uri.clone(), diagnostics, version)
                .await;
        });
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let first_arg = std::env::args().nth(1);

    let (service, socket) = LspService::new(|client| Backend {
        client,
        state: Arc::new(SharedState {
            document_map: FxDashMap::default(),
            workspace_folders: Mutex::new(Vec::new()),
            main_file: Rcu::new(triomphe::Arc::new(None)),
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
