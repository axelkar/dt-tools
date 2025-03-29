use std::{env::current_dir, path::PathBuf};

use config::{Config, ConfigError};
use thiserror::Error;

pub mod config;
mod fields;
mod search;

#[derive(Debug)]
pub struct Workspace {
    path: PathBuf,

    config: Config,
}

impl Workspace {
    pub fn try_new(path: Option<PathBuf>) -> Result<Self, WorkspaceError> {
        let path = match path {
            // path was provided by LSP or CLI
            Some(path) => {
                if path.exists() {
                    path
                } else {
                    // provided path was invalid, fallback
                    current_dir().unwrap()
                }
            }
            // https://docs.rs/tower-lsp/latest/tower_lsp/struct.Client.html#method.workspace_folders
            None => match Config::find_config() {
                // config was found in cwd or ancestor directory
                Some(path) => path,
                // fallback to cwd and use default config
                None => current_dir().unwrap(),
            },
        };

        let config = Config::load(&path).map_err(|e| WorkspaceError::ConfigError(e))?;
        Ok(Self { path, config })
    }
}

#[derive(Debug, Error)]
pub enum WorkspaceError {
    #[error("Failed to load config: {0}")]
    ConfigError(ConfigError),
}

#[cfg(test)]
mod tests {
    use fields::{Validate, include_root::IncludeRoot};

    use super::*;

    #[test]
    fn load() {
        let workspace = Workspace::try_new(Some(PathBuf::from("tests"))).unwrap();
        assert!(workspace.config.include_root.is_valid());
    }

    #[test]
    fn load_fallback() {
        // this is equal to `Workspace::try_new(None)`
        let workspace = Workspace::try_new(Some(PathBuf::from("not_exists"))).unwrap();
        assert_eq!(
            IncludeRoot(Some(PathBuf::from("tests"))),
            workspace.config.include_root
        );
    }
}
