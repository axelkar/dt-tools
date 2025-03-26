use config::Config;

pub mod config;

pub struct Workspace {
    config: Config,
}

impl Workspace {
    pub fn config(&self) -> &Config {
        &self.config
    }
}
