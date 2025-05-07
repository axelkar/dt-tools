use std::path::PathBuf;

use config::CombinedConfig;

pub mod config;
mod search;

#[derive(Debug)]
pub struct Workspace {
    pub path: PathBuf,

    pub config: CombinedConfig,
}
