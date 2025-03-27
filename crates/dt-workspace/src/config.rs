use std::{
    env::current_dir,
    path::{Path, PathBuf},
};

use config::{ConfigError as CfgError, File};
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
    pub fn load(from: Option<&Path>) -> Result<Self, ConfigError> {
        let builder = config::Config::builder().add_source(File::with_name(
            from.unwrap_or(current_dir().map_err(|_| ConfigError::Cwd)?.as_path())
                .join(CONFIG_FILENAME)
                .as_os_str()
                .to_str()
                .ok_or(ConfigError::OsStr)?,
        ));

        Ok(builder
            .build()
            .map_err(|e| ConfigError::Builder(e))?
            .try_deserialize::<Self>()
            .map_err(|e| ConfigError::Config(e))
            .unwrap_or(Config::default()))
    }

    pub fn include_root(&mut self, at: Option<&Path>) -> &Option<PathBuf> {
        self.include_root = PathBuf::find(INCLUDE_MARKER, at);
        &self.include_root
    }
}

pub(crate) trait Backtrack: Sized {
    fn find(marker: &str, at: Option<&Path>) -> Option<Self>;
}

impl Backtrack for PathBuf {
    fn find(marker: &str, at: Option<&Path>) -> Option<Self> {
        for path in at
            .unwrap_or(current_dir().ok()?.as_path())
            .ancestors()
            .into_iter()
        {
            let marker_path = path.join(marker);

            if marker_path.exists() && marker_path.is_file() {
                return Some(path.to_path_buf());
            }
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn load() {
        let config = Config::load(Some(Path::new("tests"))).unwrap();
        assert_eq!(Some(PathBuf::from("somewhere")), config.include_root);
    }

    #[test]
    #[should_panic]
    fn load_panic() {
        Config::load(None).unwrap();
    }

    #[test]
    fn lazy_include() {
        let path = Some(Path::new("tests"));

        let mut config = Config::load(path).unwrap();
        let lazy = config.include_root(path).clone().unwrap();

        assert!(lazy.exists());
        assert_eq!(config.include_root, Some(lazy)); // check is value updated in internal field
    }
}

#[derive(Debug, Error)]
pub enum ConfigError {
    #[error("Failed to determine home")]
    Home,

    #[error("Failed to determine current working directory")]
    Cwd,

    #[error("Failed to convert OsStr to &str because it contains invalid UTF-8")]
    OsStr,

    #[error("Failed to create builder: {0}")]
    Builder(CfgError),

    #[error("Failed to deserialize config: {0}")]
    Config(CfgError),

    #[error("Failed to determine root include path")]
    IncludePath,
}
