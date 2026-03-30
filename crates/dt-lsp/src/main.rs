use ::salsa::Setter;
use camino::{Utf8Path, Utf8PathBuf};
use dt_workspace::WorkspacePathFindResult;
use dt_workspace::config::CombinedConfig;
use dt_workspace::config::env_config::EnvConfig;
use dt_workspace::config::toml_config::TomlConfig;
use std::net::{Ipv4Addr, SocketAddr};
use tokio::{net::TcpListener, sync::Mutex};
use tower_lsp_server::ls_types::{
    DidChangeTextDocumentParams, DidChangeWorkspaceFoldersParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, DidSaveTextDocumentParams, GotoDefinitionParams,
    GotoDefinitionResponse, Hover, HoverParams, HoverProviderCapability, InitializeParams,
    InitializeResult, InitializedParams, MessageType, OneOf, ServerCapabilities, ServerInfo,
    TextDocumentSyncCapability, TextDocumentSyncKind, Uri, WorkspaceFolder,
    WorkspaceFoldersServerCapabilities, WorkspaceServerCapabilities,
};
use tower_lsp_server::{Client, LanguageServer, LspService, Server};
use tracing::{debug, info, level_filters::LevelFilter};
use tracing_subscriber::EnvFilter;
use triomphe::Arc;

use crate::capabilities::ResolvedClientCapabilities;
use crate::err_report::Report;
use crate::lsp_utils::{path_to_uri, uri_to_path};
use crate::salsa::db::{BaseDatabase, BaseDb};

mod capabilities;
mod err_report;
mod handlers;
mod lsp_utils;
pub mod salsa;

/// A fast map that can be sent between threads
pub type FxDashMap<K, V> =
    dashmap::DashMap<K, V, std::hash::BuildHasherDefault<rustc_hash::FxHasher>>;

#[derive(Debug)]
pub struct OpenDocument {
    /// The latest version of the document, set by the LSP client. The server will panic in
    /// debug mode if we attempt to update the document with an 'older' version.
    pub version: i32,
}

/// The global state for the LSP
pub struct Session {
    /// Files open in the LSP client.
    ///
    /// Key is the path on a filesystem, not the [`Uri`].
    open_docs: FxDashMap<Utf8PathBuf, OpenDocument>,

    resolved_client_capabilities: parking_lot::Mutex<Arc<ResolvedClientCapabilities>>,

    workspace_folders: Mutex<Vec<WorkspaceFolder>>,
    dt_workspace: parking_lot::Mutex<Option<dt_workspace::Workspace>>,
    /// The file where evaluation will start from
    ///
    /// Files not (recursively) included in this file must not be analyzed
    main_file: parking_lot::Mutex<Option<Uri>>,

    /// Salsa database
    // TODO: no mutex...
    db: parking_lot::Mutex<BaseDatabase>,
}

impl Session {
    pub fn set_file_contents_from_lsp(&self, path: &Utf8Path, contents: String, version: i32) {
        debug!(version, ?path, "set file contents from LSP");

        let db = &mut *self.db.lock();
        let file = db.get_files().get_file(db, path);

        // Because we got a change notification from LSP...
        // FIXME: rename `is_readable_file` to something more fitting
        file.set_contents(db).to(contents);
        file.set_is_readable_file(db).to(true);

        if let Some(mut open_doc) = self.open_docs.get_mut(path) {
            open_doc.version = version;
        }
    }
    async fn update_workspace(&self) {
        let folders = self.workspace_folders.lock().await;
        // TODO: actually support multiple workspace folders
        let folder = folders.first();
        let dt_workspace = folder.and_then(|folder| {
            let path = uri_to_path(&folder.uri).expect("Invalid document URI");
            let res = dt_workspace::Workspace::find_workspace_dir(&path);
            let workspace_dir = res.workspace_dir();

            let toml_config = match &res {
                WorkspacePathFindResult::TomlConfig { toml_file_path, .. } => {
                    Some(TomlConfig::load(toml_file_path))
                }
                WorkspacePathFindResult::LinuxMarker { .. } => {
                    Some(Ok(dt_workspace::linux_default_config(workspace_dir)))
                }
                WorkspacePathFindResult::Fallback { .. } => None,
            };

            let toml_config = match toml_config {
                Some(Err(err)) => {
                    tracing::error!(
                        "Failed to parse TOML config from LSP workspace path {path:?}: {}",
                        Report::new(err),
                    );
                    return None;
                }
                Some(Ok(toml_config)) => Some(toml_config),
                None => None,
            };

            Some(dt_workspace::Workspace {
                config: CombinedConfig::merge_no_cli(
                    EnvConfig::from_env()
                        .inspect_err(|err| {
                            tracing::warn!(
                                "Failed to parse workspace config from environment: {}",
                                Report::new(err),
                            );
                        })
                        .ok(),
                    toml_config,
                ),
                path: workspace_dir.to_path_buf(),
            })
        });
        info!(?dt_workspace, "Updated workspace");

        let db = &mut *self.db.lock();

        // TODO: recommend these if there is an include error
        // Linux kernel DTC include dirs:
        // - include (for #define's in header files)
        // - scripts/dtc/include-prefixes

        crate::salsa::includes::IncludeDirs::new(
            db,
            dt_workspace
                .as_ref()
                .map(|dt_workspace| dt_workspace.config.include_dirs.clone())
                .unwrap_or_default(),
        );

        *self.dt_workspace.lock() = dt_workspace;
    }
}

#[derive(Clone)]
struct Backend {
    /// Handle for communication with the LSP client.
    client: Client,
    // An `Arc` so it can be used on a background thread.
    session: Arc<Session>,
}

type Result<T, E = tower_lsp_server::jsonrpc::Error> = std::result::Result<T, E>;

impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        if let Some(client_info) = params.client_info {
            if let Some(version) = client_info.version {
                tracing::info!("{} {} connected!", client_info.name, version);
            } else {
                tracing::info!("{} connected!", client_info.name);
            }
        }

        *self.session.resolved_client_capabilities.lock() =
            Arc::new(ResolvedClientCapabilities::new(&params.capabilities));

        // TODO: support legacy clients using root_uri
        if let Some(workspace_folders) = params.workspace_folders {
            tracing::info!("Workspace folders: {workspace_folders:?}");
            *self.session.workspace_folders.lock().await = workspace_folders;
            self.session.update_workspace().await;
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
                definition_provider: Some(OneOf::Left(true)),
                // implementation_provider: (), // TODO: for labels
                // references_provider: (), // TODO: for labels
                // TODO: special highlighting for known special names?
                // phandle, #*-cells, #*-size, pinctrl-*, etc.
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
            offset_encoding: None,
        })
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

        let path = uri_to_path(&params.text_document.uri).expect("Invalid document URI");

        self.session.open_docs.insert(
            path.as_ref().to_owned(),
            OpenDocument {
                version: params.text_document.version,
            },
        );

        self.session.set_file_contents_from_lsp(
            &path,
            params.text_document.text,
            params.text_document.version,
        );

        {
            // Set the first opened file to be the main file
            let mut main_file = self.session.main_file.lock();
            main_file.get_or_insert_with(|| params.text_document.uri.clone());

            // TODO: main file in Salsa?
        }

        // TODO: warn files that aren't included by main_file

        self.push_diagnostics_for_open_docs().await;
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        assert_eq!(
            params.content_changes[0].range, None,
            "asked for TextDocumentSyncKind::FULL"
        );

        let path = uri_to_path(&params.text_document.uri).expect("Invalid document URI");
        self.session.set_file_contents_from_lsp(
            &path,
            params.content_changes.swap_remove(0).text,
            params.text_document.version,
        );

        self.push_diagnostics_for_open_docs().await;
    }
    async fn did_save(&self, _: DidSaveTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file saved!")
            .await;
    }
    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let path = uri_to_path(&params.text_document.uri).expect("Invalid document URI");
        self.session.open_docs.remove(path.as_ref());

        {
            // Set the first opened file to be the main file
            let mut main_file = self.session.main_file.lock();
            if *main_file == Some(params.text_document.uri) {
                *main_file = None;
            }
        }

        debug!("file closed");

        self.client
            .log_message(MessageType::INFO, "file closed!")
            .await;
    }
    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        Ok(handlers::goto_definition::goto_definition(self, params).await)
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        Ok(handlers::hover::hover(self, params).await)
    }

    async fn did_change_workspace_folders(&self, params: DidChangeWorkspaceFoldersParams) {
        {
            let mut folders = self.session.workspace_folders.lock().await;
            for removed in params.event.removed {
                let pos = folders.iter().position(|x| *x == removed).unwrap();
                folders.swap_remove(pos);
            }
            folders.extend_from_slice(&params.event.added);
        }
        self.session.update_workspace().await;

        // TODO: recompute includes etc.
    }
}

impl Backend {
    async fn push_diagnostics_for_open_docs(&self) {
        // TODO: worker thread
        for entry in &self.session.open_docs {
            let path = entry.key();
            let uri = path_to_uri(path).expect("An open document should(?) exist");

            let lsp_diagnostics = {
                let db = &mut *self.session.db.lock();
                let file = db.get_files().get_file(db, path);
                let diagnostics = crate::salsa::compute_file_diagnostics(db, file);
                let Some(rope) = crate::salsa::rope(db, file) else {
                    continue;
                };

                // TODO: add a source field to dt-diagnostic::Diagnostic or something
                // could be like "dt-tools(lint {})"
                let source = Some("dt-tools".to_owned());

                diagnostics
                    .iter()
                    .flat_map(|diagnostic| {
                        lsp_utils::dt_diagnostic_to_lsp(diagnostic, rope, &source, &uri)
                    })
                    .collect::<Vec<_>>()
            };

            self.client
                .publish_diagnostics(uri.clone(), lsp_diagnostics, Some(entry.version))
                .await;
        }
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let first_arg = std::env::args().nth(1);

    let (service, socket) = LspService::new(|client| Backend {
        client,
        session: Arc::new(Session {
            open_docs: FxDashMap::default(),
            resolved_client_capabilities: parking_lot::Mutex::default(),
            workspace_folders: Mutex::new(Vec::new()),
            dt_workspace: parking_lot::Mutex::default(),
            main_file: parking_lot::Mutex::new(None),
            db: parking_lot::Mutex::new(BaseDatabase::default()),
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
        let listener = TcpListener::bind(SocketAddr::new(Ipv4Addr::LOCALHOST.into(), 9257)).await?;
        let (stream, _) = listener.accept().await.unwrap();
        let (read, write) = tokio::io::split(stream);
        Server::new(read, write, socket).serve(service).await;
    } else {
        tracing_subscriber::fmt()
            .with_env_filter(tracing_env_filter)
            .with_writer(std::io::stderr) // stdout is used for LSP
            .with_ansi(false) // most likely ends up in a log file so ANSI colors are annoying
            .init();

        info!("Using stdio");
        let read = tokio::io::stdin();
        let write = tokio::io::stdout();
        Server::new(read, write, socket).serve(service).await;
    }
    Ok(())
}
