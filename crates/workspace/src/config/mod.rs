use camino::Utf8PathBuf;
#[cfg(feature = "cli")]
use cli_config::CliConfig;
use env_config::EnvConfig;
use toml_config::TomlConfig;

#[cfg(feature = "cli")]
pub mod cli_config;
pub mod env_config;
pub mod toml_config;

/// Helper macro for getting a field from structs.
macro_rules! config_field {
  ($start:ident$(, $fallback:ident)*; $field:ident) => {
      $start.as_ref().and_then(|value| value.$field.clone())
        $(
        .or_else(|| $fallback.as_ref().and_then(|value| value.$field.clone()))
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
    pub include_dirs: Vec<Utf8PathBuf>,
    /// Predefined macros as `(name, value)` pairs.
    pub defines: Vec<(String, String)>,
}

/// Splits a `NAME` or `NAME=VALUE` definition.
fn parse_define(define: &str) -> (String, String) {
    match define.split_once('=') {
        Some((name, value)) => (name.to_owned(), value.to_owned()),
        None => (define.to_owned(), "1".to_owned()),
    }
}

impl CombinedConfig {
    /// Merges [`EnvConfig`] and [`TomlConfig`]
    #[must_use]
    pub fn merge_no_cli(env: Option<EnvConfig>, toml: Option<TomlConfig>) -> Self {
        Self {
            include_dirs: config_field!(env, toml; include_dirs).unwrap_or_default(),
            defines: config_field!(env, toml; defines)
                .unwrap_or_default()
                .into_iter()
                .map(|define| parse_define(&define))
                .collect(),
        }
    }

    /// Merges [`CliConfig`], [`EnvConfig`] and [`TomlConfig`]
    #[must_use]
    #[cfg(feature = "cli")]
    pub fn merge_cli(
        cli: Option<CliConfig>,
        env: Option<EnvConfig>,
        toml: Option<TomlConfig>,
    ) -> Self {
        Self {
            include_dirs: config_field!(cli, env, toml; include_dirs).unwrap_or_default(),
            defines: config_field!(cli, env, toml; defines)
                .unwrap_or_default()
                .into_iter()
                .map(|define| parse_define(&define))
                .collect(),
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
            include_dirs: Some(vec!["cli".into()]),
            defines: Some(vec!["CLI=1".into()]),
        };
        let env = EnvConfig {
            include_dirs: Some(vec!["env".into()]),
            defines: Some(vec!["ENV".into()]),
        };
        let toml = TomlConfig {
            include_dirs: Some(vec!["toml".into()]),
            defines: Some(vec!["TOML=".into()]),
        };

        #[cfg(feature = "cli")]
        assert_eq!(
            CombinedConfig {
                include_dirs: vec!["cli".into()],
                defines: vec![("CLI".into(), "1".into())],
            },
            CombinedConfig::merge_cli(Some(cli), Some(env.clone()), Some(toml.clone()),)
        );

        assert_eq!(
            CombinedConfig {
                include_dirs: vec!["env".into()],
                defines: vec![("ENV".into(), "1".into())],
            },
            CombinedConfig::merge_no_cli(Some(env), Some(toml),)
        );
    }
}
