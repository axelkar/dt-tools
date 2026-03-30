use camino::{Utf8Path, Utf8PathBuf};
use serde::{Deserialize, Serialize};
use thiserror::Error;

/// The TOML configuration filename
pub const CONFIG_FILENAME: &str = ".dt-tools.toml";

/// Workspace configuration in TOML format
///
/// **Note**: It's up to the caller to perform validation after deserialization.
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case", deny_unknown_fields)]
pub struct TomlConfig {
    pub include_dirs: Option<Vec<Utf8PathBuf>>,
}

impl TomlConfig {
    /// Loads the config file from the file
    pub fn load(path: &Utf8Path) -> Result<Self, ConfigError> {
        let mut config: Self = toml::from_str(&fs_err::read_to_string(path)?)?;

        // Make paths relative to the config file's parent's path
        if let Some(include_dirs) = config.include_dirs {
            let config_parent = path.parent().ok_or(ConfigError::Io(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                "parent not found",
            )))?;
            config.include_dirs = Some(
                include_dirs
                    .into_iter()
                    .map(|include_dirs| config_parent.join(include_dirs))
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
        let config = TomlConfig::load(
            &Utf8Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("test_data")
                .join(CONFIG_FILENAME),
        )
        .unwrap();

        assert_eq!(
            config.include_dirs,
            Some(vec![
                Utf8Path::new(env!("CARGO_MANIFEST_DIR"))
                    .join("test_data")
                    .join("this_is_a_test")
            ])
        );

        // try load nonexistent
        assert!(
            TomlConfig::load(
                &Utf8Path::new(env!("CARGO_MANIFEST_DIR"))
                    .join("test_data")
                    .join("nonexistent")
                    .join(CONFIG_FILENAME)
            )
            .is_err()
        );

        // try load directory as file
        assert!(
            TomlConfig::load(&Utf8Path::new(env!("CARGO_MANIFEST_DIR")).join("test_data")).is_err()
        );
    }
}
