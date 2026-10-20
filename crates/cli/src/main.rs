use std::{io::Write as _, mem::ManuallyDrop, sync::Mutex, time::Instant};

use camino::{Utf8Path, Utf8PathBuf};
use clap::{
    Parser, Subcommand,
    builder::{
        Styles,
        styling::{AnsiColor, Style},
    },
};
use color_eyre::eyre::{OptionExt, Result, WrapErr};
use dt_tools_lowering::{
    codespan_reporting::print_diagnostics,
    compute_diagnostics,
    db::{BaseDatabase, BaseDb},
    emit_parse_errors,
    lowering::lower_root_file,
    parse_file,
    preprocess::{preprocess_root, preprocessor_diagnostics},
    settings::PreprocessSettings,
};
use dt_tools_workspace::{
    Workspace, WorkspacePathFindResult,
    config::{
        CombinedConfig, cli_config::CliConfig, env_config::EnvConfig, toml_config::TomlConfig,
    },
};
use owo_colors::OwoColorize;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use tracing_subscriber::{EnvFilter, filter::LevelFilter};

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

/// dt-tools-cli is a fast Devicetree parser, linter and compiler all in one.
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
    /// Lower a DTS file to MIR and show diagnostics
    Mir { file: Utf8PathBuf },
    /// Preprocess a DTS file and show the preprocessed text and diagnostics
    Preprocess { file: Utf8PathBuf },
    /// Check many DTS files for errors with a single Salsa database for reuse
    CheckMany {
        /// One or more DTS files to check
        #[arg(required = true)]
        files: Vec<Utf8PathBuf>,

        /// Print diagnostics
        #[arg(long = "print-diagnostics")]
        do_print_diagnostics: bool,

        /// Print only the summary
        #[arg(long)]
        only_summary: bool,
    },
}

fn main() -> Result<()> {
    color_eyre::install()?;

    // Set up profiling
    #[cfg(feature = "tracy")]
    profiling::tracy_client::Client::start();
    profiling::register_thread!("Main Thread");

    let cli = Cli::parse();

    let mut tracing_env_filter = EnvFilter::builder()
        .with_default_directive(LevelFilter::INFO.into())
        .from_env_lossy()
        // Too noisy logging at info level
        .add_directive("salsa::function::execute=warn".parse()?);

    if cfg!(debug_assertions) || cli.verbose {
        tracing_env_filter = tracing_env_filter
            .add_directive("dt_tools_lsp=debug".parse()?)
            .add_directive("dt_tools_lowering=debug".parse()?)
            .add_directive("dt_tools_parser=debug".parse()?);
    }

    tracing_subscriber::fmt()
        .with_env_filter(tracing_env_filter)
        .init();

    let cwd = Utf8Path::new(".").canonicalize_utf8()?;
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
        Command::Mir { file } => cmd_mir(&file, &workspace)?,
        Command::Preprocess { file } => cmd_preprocess(&file, &workspace)?,
        Command::CheckMany {
            files,
            do_print_diagnostics,
            only_summary,
        } => cmd_check_many(&files, do_print_diagnostics, only_summary, &workspace)?,
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
    emit_parse_errors(parse, &mut diagnostics, file);

    if print_diagnostics(diagnostics, &db)? {
        std::process::exit(1);
    }
    Ok(())
}

/// Installs the workspace preprocessing settings into `db`.
fn init_preprocess_settings(db: &BaseDatabase, workspace: &Workspace) {
    PreprocessSettings::new(
        db,
        workspace.config.include_dirs.clone(),
        workspace.config.defines.clone(),
    );
}

fn cmd_preprocess(file: &Utf8Path, workspace: &Workspace) -> Result<()> {
    let abs_path = file
        .canonicalize_utf8()
        .wrap_err_with(|| format!("File {file} doesn't exist"))?;
    let db = BaseDatabase::default();

    init_preprocess_settings(&db, workspace);

    let root_file = db.get_files().get_file(&db, &abs_path);
    let preprocessed =
        preprocess_root(&db, root_file).ok_or_eyre("File does not exist or is not readable")?;

    eprintln!("{} {}", "Preprocessed".green().bold(), file);
    print!("{}", preprocessed.text(&db));
    std::io::stdout().flush()?;

    let diagnostics = preprocessor_diagnostics(&db, preprocessed);

    eprintln!("{}", "\nDiagnostics:".red());
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

    init_preprocess_settings(&db, workspace);

    let root_file = db.get_files().get_file(&db, &abs_path);
    let result =
        lower_root_file(&db, root_file).ok_or_eyre("File does not exist or is not readable")?;

    let (diagnostics, _processed_files) = compute_diagnostics(&db, root_file);

    let preprocessed =
        preprocess_root(&db, root_file).ok_or_eyre("File does not exist or is not readable")?;

    eprintln!("{} diagnostics and MIR", "  Computed".green().bold());
    print!(
        "{}{}",
        "MIR:\n".cyan(),
        result.mir(&db).display(&db, preprocessed)
    );

    println!("{}", "\nDiagnostics:".red());
    if print_diagnostics(diagnostics.clone(), &db)? {
        std::process::exit(1);
    }
    Ok(())
}

fn cmd_check_many(
    files: &[Utf8PathBuf],
    do_print_diagnostics: bool,
    only_summary: bool,
    workspace: &Workspace,
) -> Result<()> {
    struct FileResult {
        passed: bool,
    }

    // One Salsa database for all files
    let db = BaseDatabase::default();

    init_preprocess_settings(&db, workspace);

    let start = Instant::now();

    // Leak the database because the OS frees the memory faster when the process exits than we can
    let db = ManuallyDrop::new(db);

    // Lock output so a single file's status gets printed independently
    let print_lock = Mutex::new(());

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

            if !only_summary {
                let _guard = print_lock.lock();
                if passed {
                    println!("{} ... {}", file, "PASS".green().bold());
                } else {
                    println!("{} ... {}", file, "FAIL".red().bold());
                    if do_print_diagnostics {
                        print_diagnostics(diagnostics.clone(), db)?;
                    }
                }
            }

            Ok(FileResult { passed })
        })
        .collect::<Result<Vec<_>>>()?;

    let elapsed = start.elapsed();

    let passed = results.iter().filter(|r| r.passed).count();
    let failed = results.len() - passed;

    if !only_summary {
        println!();
    }
    println!("{}", "--- Summary ---".bold());
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
