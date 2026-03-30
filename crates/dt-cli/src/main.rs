use std::{
    error::Error,
    path::{Path, PathBuf},
};

use camino::Utf8PathBuf;
use clap::{
    Parser, Subcommand,
    builder::{
        Styles,
        styling::{AnsiColor, Style},
    },
};
use tracing_subscriber::filter::LevelFilter;

use dt_workspace::{
    Workspace, WorkspacePathFindResult,
    config::{
        CombinedConfig, cli_config::CliConfig, env_config::EnvConfig, toml_config::TomlConfig,
    },
};

fn styles() -> Styles {
    Styles::styled()
        .header(Style::new().bold())
        .usage(Style::new().bold())
        .literal(AnsiColor::Blue.on_default().bold())
        .placeholder(AnsiColor::White.on_default().dimmed())
}

// TODO: https://github.com/oven-sh/bun/blob/df49a5a8e4644ce833bd91db0ae14a279bb97bce/src/cli.zig#L963-L989
const HELP_TEMPLATE: &str = "\
{before-help}{about-with-newline}
{usage-heading} {usage}

{all-args}{after-help}\
    ";

/// dt is a fast Devicetree parser, linter and compiler all in one.
#[derive(Parser, Debug)]
#[command(version, author, about, long_about = None, styles = styles(), help_template = HELP_TEMPLATE)]
struct Cli {
    /// If provided, displays info log messages
    ///
    /// This can be overridden by the `RUST_LOG` environment variable
    #[arg(short, long)]
    verbose: bool,
    #[command(subcommand)]
    command: Command,
    #[clap(flatten)]
    config: CliConfig,
}

#[derive(Subcommand, Debug)]
enum Command {
    /// Parse DTS files for debugging dt-tools
    Parse {
        /// The path for the DTS file to format
        #[arg(short, long)]
        file: PathBuf,

        /// An optional path for the new DTS
        ///
        /// By default, `file` gets overwritten
        #[arg(short, long)]
        output: Option<PathBuf>,
    },
    /// Lint DTS and DTB files
    Lint {},
}

fn main() -> Result<(), Box<dyn Error>> {
    let cli = dbg!(Cli::parse());
    tracing_subscriber::fmt()
        .with_max_level(if cli.verbose {
            LevelFilter::INFO
        } else {
            LevelFilter::WARN
        })
        .init();

    let cwd = Utf8PathBuf::from_path_buf(Path::new(".").canonicalize()?)
        .map_err(|_| "Current directory should be UTF-8")?;
    let res = dt_workspace::Workspace::find_workspace_dir(&cwd);
    let workspace_dir = res.workspace_dir();

    let toml_config = match &res {
        WorkspacePathFindResult::TomlConfig { toml_file_path, .. } => {
            Some(TomlConfig::load(toml_file_path))
        }
        WorkspacePathFindResult::LinuxMarker { .. } => {
            Some(Ok(dt_workspace::linux_default_config(workspace_dir)))
        }
        WorkspacePathFindResult::Fallback { .. } => None,
    };

    let _workspace = dbg!(Workspace {
        config: CombinedConfig::merge_cli(
            Some(cli.config),
            Some(EnvConfig::from_env()?),
            toml_config.transpose()?,
        ),
        path: workspace_dir.to_path_buf()
    });

    match cli.command {
        Command::Parse { file: _, output: _ } => todo!(),
        Command::Lint {} => todo!(),
    }
}
