use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};
use thiserror::Error;

/// The TOML configuration filename
pub const CONFIG_FILENAME: &str = ".dt-tools.toml";

/// Workspace configuration in TOML format
///
/// **Note**: It's up to the caller to perform validation after deserialization.
#[derive(Debug, PartialEq, Default, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct TomlConfig {
    pub include_paths: Option<Vec<PathBuf>>,
}

impl TomlConfig {
    /// Loads the config file from the file
    pub fn load(path: &Path) -> Result<Self, ConfigError> {
        let mut config: Self = toml::from_str(&fs_err::read_to_string(path)?)?;

        // Make paths relative
        if let Some(include_paths) = config.include_paths {
            let config_parent = path.parent().ok_or(ConfigError::Io(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                "parent not found",
            )))?;
            config.include_paths = Some(
                include_paths
                    .into_iter()
                    .map(|include_path| config_parent.join(include_path))
                    .collect(),
            );
        }

        Ok(config)
    }
}

/// Configuration errors encountered when loading the TOML config
#[derive(Debug, Error)]
pub enum ConfigError {
    #[error("Failed to read config")]
    Io(#[from] std::io::Error),

    #[error("Failed to deserialize config")]
    Toml(#[from] toml::de::Error),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn load() {
        // load using filename
        let config = TomlConfig::load(Path::new(CONFIG_FILENAME)).unwrap();
        assert_eq!(config.include_paths, Some(vec![PathBuf::from("test_data")]));

        // try load
        assert!(TomlConfig::load(&Path::new("test_data").join(CONFIG_FILENAME)).is_err());

        // try load directory as file
        assert!(TomlConfig::load(Path::new("test_data")).is_err());
    }
}
