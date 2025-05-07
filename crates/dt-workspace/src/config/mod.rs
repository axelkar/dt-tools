use std::path::PathBuf;

#[cfg(feature = "cli")]
use cli_config::CliConfig;
use env_config::EnvConfig;
use toml_config::TomlConfig;

#[cfg(feature = "cli")]
pub mod cli_config;
pub mod env_config;
pub mod toml_config;

macro_rules! config_field {
  ($start:ident, $($fallback:ident),+; $field:ident) => {
      $start.and_then(|value| Some(value.$field))
        $(
        .or_else(|| config_field!($fallback; $field))
        )+
  };
  ($struct:ident; $field:ident) => {
    $struct.and_then(|value| Some(value.$field))
  }
}

#[derive(Debug, Default, PartialEq)]
pub struct CombinedConfig {
    include_paths: Vec<PathBuf>,
}

impl CombinedConfig {
    pub fn merge(
        toml: Option<TomlConfig>,
        #[cfg(feature = "cli")] cli: Option<CliConfig>,
        env: Option<EnvConfig>,
    ) -> Self {
        Self {
            #[cfg(feature = "cli")]
            include_paths: config_field!(cli, env, toml; include_paths).unwrap_or_default(),
            #[cfg(not(feature = "cli"))]
            include_paths: config_field!(env, toml; include_paths).unwrap_or_default(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn merge() {
        let toml = TomlConfig {
            include_paths: vec!["toml".into()],
        };
        #[cfg(feature = "cli")]
        let cli = CliConfig {
            include_paths: vec!["cli".into()],
        };
        let env = EnvConfig {
            include_paths: vec!["env".into()],
        };

        assert_eq!(
            CombinedConfig {
                include_paths: vec![
                    #[cfg(feature = "cli")]
                    "cli".into(),
                    #[cfg(not(feature = "cli"))]
                    "env".into(),
                ],
            },
            CombinedConfig::merge(
                Some(toml),
                #[cfg(feature = "cli")]
                Some(cli),
                Some(env),
            )
        );
    }
}
