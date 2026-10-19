use camino::Utf8PathBuf;
use clap::Parser;

/// Command-line interface (CLI) workspace configuration
#[derive(Clone, Debug, PartialEq, Parser)]
pub struct CliConfig {
    /// List of search paths for `#include` directives.
    // In the manner of GCC: https://gcc.gnu.org/onlinedocs/gcc/Directory-Options.html
    #[arg(short = 'I', long = "include-directory")]
    pub include_dirs: Option<Vec<Utf8PathBuf>>,

    /// Define a macro, optionally with a value (`NAME` or `NAME=VALUE`).
    #[arg(short = 'D', long = "define")]
    pub defines: Option<Vec<String>>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse() {
        assert_eq!(
            CliConfig::parse_from([
                "dt-tools", "-I", "a,b", "-I", "c", "-D", "FOO", "-D", "BAR=2",
            ]),
            CliConfig {
                include_dirs: Some(vec!["a,b".into(), "c".into()]),
                defines: Some(vec!["FOO".into(), "BAR=2".into()]),
            }
        );
    }
}
