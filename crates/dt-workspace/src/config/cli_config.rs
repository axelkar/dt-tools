use std::path::PathBuf;

use clap::Parser;

/// Command-line interface (CLI) workspace configuration
#[derive(Debug, PartialEq, Parser)]
pub struct CliConfig {
    /// List of comma-separated paths to search for C macros
    #[arg(short = 'I', long, value_delimiter = ',')]
    pub include_paths: Option<Vec<PathBuf>>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse() {
        assert_eq!(
            CliConfig::parse_from(["dt-tools", "-I", "linux,linux-old"]),
            CliConfig {
                include_paths: Some(vec!["linux".into(), "linux-old".into()]),
            }
        );
    }
}
