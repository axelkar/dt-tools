use std::path::PathBuf;

use config::{CombinedConfig, env_config::EnvConfig, toml_config::TomlConfig};

mod config;
mod search;

#[derive(Debug)]
pub struct Workspace {
    pub path: PathBuf,

    pub config: CombinedConfig,
}
