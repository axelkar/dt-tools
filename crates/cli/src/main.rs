use std::{
    error::Error,
    path::{Path, PathBuf},
};

use camino::Utf8PathBuf;
use dt_tools_lowering::{
    codespan_reporting::print_diagnostics, compute_diagnostics, db::{BaseDatabase, BaseDb}, emit_parse_errors, parse_file
};
use clap::{
    Parser, Subcommand,
    builder::{
        Styles,
        styling::{AnsiColor, Style},
    },
};
use dt_tools_workspace::{
    Workspace, WorkspacePathFindResult,
    config::{
        CombinedConfig, cli_config::CliConfig, env_config::EnvConfig, toml_config::TomlConfig,
    },
};
use owo_colors::OwoColorize;
use tracing_subscriber::filter::LevelFilter;

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
    /// Parse a DTS file and show lex/parse errors
    Parse {
        file: PathBuf,
    },
    /// Lint DTS and DTB files
    Lint {},
    /// Lower a DTS file to MIR and show diagnostics
    Mir {
        file: PathBuf,
    },
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
    let res = dt_tools_workspace::Workspace::find_workspace_dir(&cwd);
    let workspace_dir = res.workspace_dir();

    let toml_config = match &res {
        WorkspacePathFindResult::TomlConfig { toml_file_path, .. } => {
            Some(TomlConfig::load(toml_file_path))
        }
        WorkspacePathFindResult::LinuxMarker { .. } => {
            Some(Ok(dt_tools_workspace::linux_default_config(workspace_dir)))
        }
        WorkspacePathFindResult::Fallback { .. } => None,
    };

    let workspace = dbg!(Workspace {
        config: CombinedConfig::merge_cli(
            Some(cli.config),
            Some(EnvConfig::from_env()?),
            toml_config.transpose()?,
        ),
        path: workspace_dir.to_path_buf()
    });

    match cli.command {
        Command::Parse { file } => cmd_parse(&file)?,
        Command::Lint {} => todo!(),
        Command::Mir { file } => cmd_mir(&file, &workspace)?,
    }

    Ok(())
}

fn canonicalize_utf8(path: &Path) -> Result<Utf8PathBuf, Box<dyn Error>> {
    Ok(
        Utf8PathBuf::from_path_buf(path.canonicalize()?)
            .map_err(|_| "File path must be valid UTF-8")?,
    )
}

fn cmd_parse(file: &Path) -> Result<(), Box<dyn Error>> {
    let abs_path = canonicalize_utf8(file)?;
    let db = BaseDatabase::default();
    let file = db.get_files().get_file(&db, &abs_path);

    let parse = parse_file(&db, file).ok_or("File does not exist or is not readable")?;
    let parse = parse.parse(&db);

    eprintln!("{}", "  Parsed".green().bold());
    print!("{}", parse.green_node.print_tree());

    let mut diagnostics = Vec::new();
    let diag = parking_lot::Mutex::new(&mut diagnostics);
    emit_parse_errors(parse, &diag, &mut |tr| tr.within_file(file));

    if print_diagnostics(diagnostics, &db)? {
        std::process::exit(1);
    }
    Ok(())
}

fn cmd_mir(file: &Path, workspace: &Workspace) -> Result<(), Box<dyn Error>> {
    use dt_tools_lowering::{includes::IncludeDirs, lowering::lower_root_file};

    let abs_path = canonicalize_utf8(file)?;
    let db = BaseDatabase::default();

    let include_dirs: Vec<Utf8PathBuf> = workspace.config.include_dirs.clone();
    IncludeDirs::new(&db, include_dirs);

    let root_file = db.get_files().get_file(&db, &abs_path);
    let result = lower_root_file(&db, root_file).ok_or("File does not exist or is not readable")?;

    let (diagnostics, _processed_files) = compute_diagnostics(&db, root_file);

    eprintln!("{} diagnostics and MIR", "  Computed".green().bold());
    print!("{}{}", "MIR:\n".cyan(), result.mir(&db).display(&db));

    println!("{}", "Diagnostics".red());
    if print_diagnostics(diagnostics.clone(), &db)? {
        std::process::exit(1);
    }
    Ok(())
}
