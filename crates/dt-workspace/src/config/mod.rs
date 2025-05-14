use std::path::PathBuf;

#[cfg(feature = "cli")]
use cli_config::CliConfig;
use env_config::EnvConfig;
use toml_config::TomlConfig;

#[cfg(feature = "cli")]
pub mod cli_config;
pub mod env_config;
pub mod toml_config;

/// Helper macro for getting a field from structs.
///
/// First argument must be `cli,` or `@without_cli`. The former automatically removes itself when the `cli` feature is not enabled.
macro_rules! config_field {
  ($cli:ident, $($rest:ident),+; $field:ident) => {
    {
        #[cfg(feature = "cli")]
        { config_field!(@without_cli $cli, $($rest),+; $field) }
        #[cfg(not(feature = "cli"))]
        { config_field!(@without_cli $($rest),+; $field) }
    }
  };
  (@without_cli $start:ident$(, $fallback:ident)*; $field:ident) => {
      $start.and_then(|value| value.$field)
        $(
        .or_else(|| $fallback.and_then(|value| value.$field))
        )*
  };
}

/// A composite configuration from multiple sources with following ordering:
///
/// - [`CliConfig`] (if `cli` feature is enabled)
/// - [`EnvConfig`]
/// - [`TomlConfig`]
#[derive(Debug, Default, PartialEq)]
pub struct CombinedConfig {
    include_paths: Vec<PathBuf>,
}

impl CombinedConfig {
    /// Merges [`CliConfig`] (if `cli` feature is enabled), [`EnvConfig`] and [`TomlConfig`]
    #[must_use]
    pub fn merge(
        #[cfg(feature = "cli")] cli: Option<CliConfig>,
        env: Option<EnvConfig>,
        toml: Option<TomlConfig>,
    ) -> Self {
        Self {
            include_paths: config_field!(cli, env, toml; include_paths).unwrap_or_default(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn merge() {
        #[cfg(feature = "cli")]
        let cli = CliConfig {
            include_paths: Some(vec!["cli".into()]),
        };
        let env = EnvConfig {
            include_paths: Some(vec!["env".into()]),
        };
        let toml = TomlConfig {
            include_paths: Some(vec!["toml".into()]),
        };

        assert_eq!(
            CombinedConfig {
                include_paths: vec![if cfg!(feature = "cli") { "cli" } else { "env" }.into()],
            },
            CombinedConfig::merge(
                #[cfg(feature = "cli")]
                Some(cli),
                Some(env),
                Some(toml),
            )
        );
    }
}
