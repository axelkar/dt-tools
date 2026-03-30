use camino::Utf8PathBuf;
use serde::Deserialize;

/// Workspace configuration using environment variables
#[derive(Clone, Debug, PartialEq, Deserialize)]
pub struct EnvConfig {
    pub include_dirs: Option<Vec<Utf8PathBuf>>,
}

impl EnvConfig {
    /// Retrieves configuration from environment variables prefixed with `DT_TOOLS_`
    pub fn from_env() -> Result<Self, envy::Error> {
        envy::prefixed("DT_TOOLS_").from_env()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse() {
        assert_eq!(
            envy::from_iter::<_, EnvConfig>([(
                "INCLUDE_DIRS".to_owned(),
                "linux,linux-old".to_owned()
            )])
            .unwrap(),
            EnvConfig {
                include_dirs: Some(vec!["linux".into(), "linux-old".into()])
            }
        );
    }
}
