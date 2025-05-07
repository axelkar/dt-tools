use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};
use thiserror::Error;

use crate::search::search;

pub const CONFIG_FILENAME: &str = ".dt-tools.toml";

#[derive(Debug, PartialEq, Default, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
/// Workspace configuration in TOML format
///
/// **Note**: It's up to the caller to perform validation after deserialization.
pub struct TomlConfig {
    pub include_paths: Vec<PathBuf>,
}

impl TomlConfig {
    /// Looks for [`CONFIG_FILENAME`] in `start` or cwd
    ///
    /// Returns directory and TOML config path
    pub fn find_path(start: Option<&Path>) -> Option<(&Path, PathBuf)> {
        search(start.unwrap_or(Path::new(".")), CONFIG_FILENAME, |p| {
            p.is_file()
        })
    }

    /// Loads config file from directory
    pub fn load_from_parent(path: &Path) -> Result<Self, ConfigError> {
        Self::load(&path.join(CONFIG_FILENAME))
    }

    /// Loads config file from file
    pub fn load(path: &Path) -> Result<Self, ConfigError> {
        toml::from_str(&fs_err::read_to_string(path)?).map_err(ConfigError::Toml)
    }
}

#[derive(Debug, Error)]
pub enum ConfigError {
    #[error("Failed to read config")]
    Io(#[from] std::io::Error),

    #[error("Failed to deserialize config")]
    Toml(#[from] toml::de::Error),
}

#[cfg(test)]
mod tests {

    use std::env::current_dir;

    use super::*;

    fn cwd_with<const N: usize>(join: [&str; N]) -> PathBuf {
        join.iter()
            .fold(current_dir().unwrap(), |p, j| p.join(j))
            .canonicalize()
            .unwrap()
    }

    #[test]
    fn find() {
        // config at cwd
        assert_eq!(
            cwd_with([CONFIG_FILENAME]),
            TomlConfig::find_path(None)
                .unwrap()
                .1
                .canonicalize()
                .unwrap()
        );

        // config specified by user
        assert_eq!(
            cwd_with(["test_data", CONFIG_FILENAME]),
            TomlConfig::find_path(Some(Path::new("test_data")))
                .unwrap()
                .1
                .canonicalize()
                .unwrap()
        );

        // config at ancestor
        assert_eq!(
            cwd_with(["test_data", CONFIG_FILENAME]),
            TomlConfig::find_path(Some(&Path::new("test_data").join("empty")))
                .unwrap()
                .1
                .canonicalize()
                .unwrap()
        );
    }

    #[test]
    fn load() {
        // load using filename
        let config = TomlConfig::load(Path::new(CONFIG_FILENAME)).unwrap();
        assert_eq!(vec![PathBuf::from("test_data")], config.include_paths);

        // load using filename at directory
        let config = TomlConfig::load(&Path::new("test_data").join(CONFIG_FILENAME)).unwrap();
        assert_eq!(vec![PathBuf::from(".")], config.include_paths);

        // load using cwd as directory
        let config = TomlConfig::load_from_parent(Path::new(".")).unwrap();
        assert_eq!(vec![PathBuf::from("test_data")], config.include_paths);

        // load using directory
        let config = TomlConfig::load_from_parent(Path::new("test_data")).unwrap();
        assert_eq!(vec![PathBuf::from(".")], config.include_paths);

        // try load directory as file
        assert!(TomlConfig::load(Path::new("test_data")).is_err());

        // try load from invalid path
        assert!(
            TomlConfig::load_from_parent(Path::new("not_exists"))
                .unwrap_err()
                .to_string()
                .contains("Failed to read config")
        );
    }
}
