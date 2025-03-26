use std::{
    env::current_dir,
    path::{Path, PathBuf},
};

use config::{ConfigError, File};
use serde::{Deserialize, Serialize};
use thiserror::Error;

const CONFIG_FILENAME: &'static str = ".dt-tools.toml";
const INCLUDE_MARKER: &'static str = "dt-bindings/interrupt-controller/arm-gic.h";

#[derive(Debug, PartialEq, Default, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Config {
    include_root: Option<PathBuf>,
}

impl Config {
    pub fn load() -> Result<Self, Error> {
        let builder = config::Config::builder().add_source(File::with_name(CONFIG_FILENAME));

        Ok(builder
            .build()
            .map_err(|e| Error::BadBuilder(e))?
            .try_deserialize::<Self>()
            .map_err(|e| Error::BadConfig(e))
            .unwrap_or(Config::default()))
    }
}

pub(crate) trait Backtrack: Sized {
    fn find(marker: &str, at: Option<&Path>) -> Option<Self>;
}

impl Backtrack for PathBuf {
    fn find(marker: &str, at: Option<&Path>) -> Option<Self> {
        let mut path = at.map_or(current_dir().ok()?, |p| p.to_path_buf());
        loop {
            let marker_path = path.join(marker);

            if marker_path.exists() && marker_path.is_file() {
                break Some(path);
            }

            path = path.parent()?.to_path_buf();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn load() {
        let config = Config::load().unwrap();
        assert_ne!(None, config.include_root);
    }
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("Failed to determine home")]
    BadHome,

    #[error("Failed to create builder: {0}")]
    BadBuilder(ConfigError),

    #[error("Failed to deserialize config: {0}")]
    BadConfig(ConfigError),

    #[error("Failed to determine root include path")]
    BadIncludePath,
}
