use std::path::PathBuf;

use config::{Config, ConfigError, env_config::EnvConfig, toml_config::TomlConfig};
use thiserror::Error;

mod config;
mod search;

#[derive(Debug)]
pub struct Workspace {
    pub path: PathBuf,

    pub config: Config,
}
