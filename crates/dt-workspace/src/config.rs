use serde::{Deserialize, Serialize};
use std::{
    fs,
    path::{Path, PathBuf},
};
use thiserror::Error;

use crate::{
    fields::{Validate, include_root::IncludeRoot},
    search::{is_exists_and_file, search_cwd},
};

const CONFIG_FILENAME: &'static str = ".dt-tools.toml";

#[derive(Debug, PartialEq, Default, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Config {
    pub include_root: IncludeRoot,
}

impl Config {
    /// Finds directory with config
    pub fn find_config() -> Option<PathBuf> {
        search_cwd(CONFIG_FILENAME, is_exists_and_file).map(|p| p.0)
    }

    pub fn load(at: &Path) -> Result<Self, ConfigError> {
        // can fail if we falled back to cwd or config doesn't exist here
        let mut this = if at.join(CONFIG_FILENAME).exists() {
            toml::from_str(
                &fs::read_to_string(at.join(CONFIG_FILENAME)).map_err(|_| ConfigError::Utf8)?,
            )
            .map_err(|e| ConfigError::Config(e))?
        } else {
            Self::default()
        };

        this.include_root.validate(at);

        Ok(this)
    }
}

#[derive(Debug, Error)]
pub enum ConfigError {
    #[error("Failed to load config because it contains invalid UTF-8")]
    Utf8,

    #[error("Failed to deserialize config: {0}")]
    Config(toml::de::Error),

    #[error("Failed to determine root include path")]
    IncludePath,
}

#[cfg(test)]
mod tests {

    use std::env::current_dir;

    use super::*;

    #[test]
    fn search() {
        Config::find_config().unwrap();
    }

    #[test]
    fn load_tests() {
        let config = Config::load(Path::new("tests")).unwrap();
        assert!(config.include_root.is_valid());
        assert_eq!(IncludeRoot(Some(PathBuf::from("."))), config.include_root);
    }

    #[test]
    fn load() {
        let config = Config::load(&current_dir().unwrap()).unwrap();
        // include root is valid because we have another config in `dt-workspace`
        // with `include-root = "tests"`
        assert_eq!(
            IncludeRoot(Some(PathBuf::from("tests"))),
            config.include_root
        );
    }

    #[test]
    fn load_not_exists() {
        let config = Config::load(&Path::new("tests").join("empty")).unwrap();
        assert_eq!(
            IncludeRoot(Some(PathBuf::from("tests"))),
            config.include_root
        );
    }

    #[test]
    fn load_default() {
        let config = Config::load(Path::new("not_exists")).unwrap();
        assert!(!config.include_root.is_valid());
    }
}
