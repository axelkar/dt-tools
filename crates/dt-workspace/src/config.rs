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
pub struct Config {
    pub include_paths: Vec<PathBuf>,
}

impl Config {
    /// Looks for [`CONFIG_FILENAME`] in `start` or cwd
    pub fn find_config(start: Option<&Path>) -> Option<PathBuf> {
        search(start.unwrap_or(Path::new(".")), CONFIG_FILENAME, |p| {
            p.is_file()
        })
        .map(|p| p.1)
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

    use std::{env::current_dir, io};

    use super::*;

    #[test]
    fn find_cwd() {
        assert_eq!(
            current_dir()
                .unwrap()
                .join(CONFIG_FILENAME)
                .canonicalize()
                .unwrap(),
            Config::find_config(None).unwrap().canonicalize().unwrap()
        );
    }

    #[test]
    fn find_tests() {
        assert_eq!(
            current_dir()
                .unwrap()
                .join("test_data")
                .join(CONFIG_FILENAME)
                .canonicalize()
                .unwrap(),
            Config::find_config(Some(Path::new("test_data")))
                .unwrap()
                .canonicalize()
                .unwrap()
        );
    }

    #[test]
    fn find_ancestor() {
        assert_eq!(
            current_dir()
                .unwrap()
                .join("test_data")
                .join(CONFIG_FILENAME)
                .canonicalize()
                .unwrap(),
            Config::find_config(Some(&Path::new("test_data").join("empty")))
                .unwrap()
                .canonicalize()
                .unwrap()
        );
    }

    #[test]
    fn load_cwd_parent() {
        let config = Config::load_from_parent(Path::new(".")).unwrap();
        assert_eq!(vec![PathBuf::from("test_data")], config.include_paths);
    }

    #[test]
    fn load_cwd() {
        let config = Config::load(Path::new(CONFIG_FILENAME)).unwrap();
        assert_eq!(vec![PathBuf::from("test_data")], config.include_paths);
    }

    #[test]
    fn load_tests_parent() {
        let config = Config::load_from_parent(Path::new("test_data")).unwrap();
        assert_eq!(vec![PathBuf::from(".")], config.include_paths);
    }

    #[test]
    fn load_tests() {
        let config = Config::load(&Path::new("test_data").join(CONFIG_FILENAME)).unwrap();
        assert_eq!(vec![PathBuf::from(".")], config.include_paths);
    }

    #[test]
    #[should_panic]
    fn load_panic() {
        // invalid because we expect parent + CONFIG_FILENAME here
        Config::load(Path::new("test_data")).unwrap();
    }

    #[test]
    fn load_not_exists_parent() {
        if let Err(ConfigError::Io(err)) = Config::load_from_parent(Path::new("not_exists")) {
            if err.kind() == io::ErrorKind::NotFound {
                return;
            }
        };

        panic!("expected io::ErrorKind::NotFound")
    }

    #[test]
    fn load_not_exists() {
        // common mistake: the user forgot `.` prefix
        if let Err(ConfigError::Io(err)) = Config::load(Path::new("dt-tools.toml")) {
            if err.kind() == io::ErrorKind::NotFound {
                return;
            }
        };

        panic!("expected io::ErrorKind::NotFound")
    }
}
