use std::path::PathBuf;

use config::{Config, ConfigError};
use thiserror::Error;

pub mod config;
mod search;

#[derive(Debug)]
pub struct Workspace {
    pub path: PathBuf,

    pub config: Config,
}

impl Workspace {
    pub fn try_new(path: PathBuf) -> Result<Self, WorkspaceError> {
        let config = match Config::load_from_parent(&path) {
            Ok(config) => config,
            Err(ConfigError::Io(err)) if err.kind() == std::io::ErrorKind::NotFound => {
                Config::default()
            }
            other => other?,
        };
        Ok(Self { path, config })
    }
}

#[derive(Debug, Error)]
pub enum WorkspaceError {
    #[error("Failed to load config")]
    Config(#[from] ConfigError),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn load() {
        let workspace = Workspace::try_new(PathBuf::from("test_data")).unwrap();
        assert_eq!(vec![PathBuf::from(".")], workspace.config.include_paths);
    }

    #[test]
    fn load_ancestor() {
        // this is equal to `Workspace::try_new(PathBuf::from("not_exists"))`
        // because we'll fallback to ancestor config
        let workspace = Workspace::try_new(PathBuf::from(".")).unwrap();
        assert_eq!(
            vec![PathBuf::from("test_data")],
            workspace.config.include_paths
        );
    }
}
