use std::path::PathBuf;

use serde::Deserialize;

#[derive(Debug, PartialEq, Deserialize)]
pub struct EnvConfig {
    pub include_paths: Vec<PathBuf>,
}

impl EnvConfig {
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
            EnvConfig {
                include_paths: vec!["linux".into(), "linux-old".into()]
            },
            envy::from_iter([("INCLUDE_PATHS".into(), "linux,linux-old".into())]).unwrap()
        );
    }
}
