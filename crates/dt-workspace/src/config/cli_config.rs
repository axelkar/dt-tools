use std::path::PathBuf;

use clap::Parser;

#[derive(Debug, PartialEq, Parser)]
pub struct CliConfig {
    #[arg(short = 'I', long, value_delimiter = ',')]
    pub include_paths: Vec<PathBuf>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse() {
        assert_eq!(
            CliConfig {
                include_paths: vec!["linux".into(), "linux-old".into()],
            },
            CliConfig::parse_from(["dt-tools", "-I", "linux,linux-old"])
        );
    }
}
