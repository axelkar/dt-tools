use std::{mem::ManuallyDrop, path::Path, time::Instant};

use camino::{Utf8Path, Utf8PathBuf};
use clap::{
    Parser, Subcommand,
    builder::{
        Styles,
        styling::{AnsiColor, Style},
    },
};
use color_eyre::eyre::{OptionExt, Result, WrapErr, eyre};
use dt_tools_lowering::{
    codespan_reporting::print_diagnostics,
    compute_diagnostics,
    db::{BaseDatabase, BaseDb},
    emit_parse_errors,
    includes::IncludeDirs,
    lowering::lower_root_file,
    parse_file,
};
use dt_tools_workspace::{
    Workspace, WorkspacePathFindResult,
    config::{
        CombinedConfig, cli_config::CliConfig, env_config::EnvConfig, toml_config::TomlConfig,
    },
};
use owo_colors::OwoColorize;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
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
    Parse { file: Utf8PathBuf },
    /// Lint DTS and DTB files
    Lint {},
    /// Lower a DTS file to MIR and show diagnostics
    Mir { file: Utf8PathBuf },
    /// Check many DTS files for errors with a single Salsa database for reuse
    CheckMany {
        /// One or more DTS files to check
        #[arg(required = true)]
        files: Vec<Utf8PathBuf>,

        /// Print diagnostics
        #[arg(long = "print-diagnostics")]
        do_print_diagnostics: bool,
    },
}

fn main() -> Result<()> {
    color_eyre::install()?;

    let cli = Cli::parse();
    tracing_subscriber::fmt()
        .with_max_level(if cli.verbose {
            LevelFilter::INFO
        } else {
            LevelFilter::WARN
        })
        .init();

    let cwd = Utf8PathBuf::from_path_buf(Path::new(".").canonicalize()?)
        .map_err(|_| eyre!("Current directory isn't UTF-8"))?;
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

    let workspace = Workspace {
        config: CombinedConfig::merge_cli(
            Some(cli.config),
            Some(EnvConfig::from_env()?),
            toml_config.transpose()?,
        ),
        path: workspace_dir.to_path_buf(),
    };

    match cli.command {
        Command::Parse { file } => cmd_parse(&file)?,
        Command::Lint {} => todo!(),
        Command::Mir { file } => cmd_mir(&file, &workspace)?,
        Command::CheckMany {
            files,
            do_print_diagnostics,
        } => cmd_check_many(&files, do_print_diagnostics, &workspace)?,
    }

    Ok(())
}

fn cmd_parse(file: &Utf8Path) -> Result<()> {
    let abs_path = file
        .canonicalize_utf8()
        .wrap_err_with(|| format!("File {file} doesn't exist"))?;
    let db = BaseDatabase::default();
    let file = db.get_files().get_file(&db, &abs_path);

    let parse = parse_file(&db, file).ok_or_eyre("File does not exist or is not readable")?;
    let parse = parse.parse(&db);

    println!("{}", "  Parsed".green().bold());
    print!("{}", parse.green_node.print_tree());

    let mut diagnostics = Vec::new();
    let diag = parking_lot::Mutex::new(&mut diagnostics);
    emit_parse_errors(parse, &diag, &mut |tr| tr.within_file(file));

    if print_diagnostics(diagnostics, &db)? {
        std::process::exit(1);
    }
    Ok(())
}

fn cmd_mir(file: &Utf8Path, workspace: &Workspace) -> Result<()> {
    let abs_path = file
        .canonicalize_utf8()
        .wrap_err_with(|| format!("File {file} doesn't exist"))?;
    let db = BaseDatabase::default();

    let include_dirs: Vec<Utf8PathBuf> = workspace.config.include_dirs.clone();
    IncludeDirs::new(&db, include_dirs);

    let root_file = db.get_files().get_file(&db, &abs_path);
    let result =
        lower_root_file(&db, root_file).ok_or_eyre("File does not exist or is not readable")?;

    let (diagnostics, _processed_files) = compute_diagnostics(&db, root_file);

    eprintln!("{} diagnostics and MIR", "  Computed".green().bold());
    print!("{}{}", "MIR:\n".cyan(), result.mir(&db).display(&db));

    println!("{}", "Diagnostics".red());
    if print_diagnostics(diagnostics.clone(), &db)? {
        std::process::exit(1);
    }
    Ok(())
}

fn cmd_check_many(
    files: &[Utf8PathBuf],
    do_print_diagnostics: bool,
    workspace: &Workspace,
) -> Result<()> {
    struct FileResult {
        passed: bool,
    }

    // One Salsa database for all files
    let db = BaseDatabase::default();

    let include_dirs: Vec<Utf8PathBuf> = workspace.config.include_dirs.clone();
    IncludeDirs::new(&db, include_dirs);

    let start = Instant::now();

    // Leak the database because the OS frees the memory faster when the process exits than we can
    let db = ManuallyDrop::new(db);

    let results = files
        .into_par_iter()
        .map_with(db, |db, file| {
            let db = &**db;

            let abs_path = file
                .canonicalize_utf8()
                .wrap_err_with(|| format!("File {file} doesn't exist"))?;
            let root_file = db.get_files().get_file(db, &abs_path);

            let (diagnostics, _processed_files) = compute_diagnostics(db, root_file);

            let has_errors = diagnostics
                .iter()
                .any(|d| d.severity == dt_tools_diagnostic::Severity::Error);
            let passed = !has_errors;

            if passed {
                println!("{} ... {}", file, "PASS".green().bold());
            } else {
                println!("{} ... {}", file, "FAIL".red().bold());
                if do_print_diagnostics {
                    print_diagnostics(diagnostics.clone(), db)?;
                }
            }

            Ok(FileResult { passed })
        })
        .collect::<Result<Vec<_>>>()?;

    let elapsed = start.elapsed();

    let passed = results.iter().filter(|r| r.passed).count();
    let failed = results.len() - passed;
    println!("\n{}", "--- Summary ---".bold());
    println!(
        "{} {}: {} passed, {} failed",
        results.len(),
        if files.len() == 1 { "file" } else { "files" },
        passed.green().bold(),
        failed.red().bold(),
    );
    eprintln!("Took {:?}", elapsed.bright_green());

    println!("\n{}", "--- Current Memory Usage ---".bold());
    #[expect(
        clippy::cast_precision_loss,
        reason = "We don't need high precision here"
    )]
    if let Some(memory_stats) = memory_stats::memory_stats() {
        let physical_mb = memory_stats.physical_mem as f64 / 1024.0 / 1024.0;
        let virtual_mb = memory_stats.virtual_mem as f64 / 1024.0 / 1024.0;
        println!("Physical Memory (RSS): {physical_mb:.2} MB");
        println!("Virtual Memory:        {virtual_mb:.2} MB");
    } else {
        println!("{}", "Failed to query current memory usage".red());
    }

    if failed > 0 {
        std::process::exit(1);
    }
    Ok(())
}
