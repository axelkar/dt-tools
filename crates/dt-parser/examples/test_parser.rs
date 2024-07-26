//! Parses the file at argv 1 and prints the CST

use owo_colors::{colors::xterm::Gray, OwoColorize as _};
use tracing::level_filters::LevelFilter;
use tracing_subscriber::EnvFilter;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    tracing_subscriber::fmt()
        .with_env_filter(
            EnvFilter::builder()
                .with_default_directive(LevelFilter::INFO.into())
                .from_env_lossy()
                .add_directive("dt_parser=trace".parse()?),
        )
        .init();

    let path = std::env::args()
        .nth(1)
        .expect("Should have a path as an argument");
    let text = std::fs::read_to_string(path)?;
    let parse = dt_parser::cst2::parser::parse(&text);
    eprintln!("{}", "Parsed!".green());
    if !parse.lex_errors.is_empty() || !parse.errors.is_empty() {
        eprintln!("{}", "Invalid DTS!".red());
        if !parse.lex_errors.is_empty() {
            eprintln!("{}: {:#?}", "Lex errors".red(), parse.errors);
        }
        if !parse.errors.is_empty() {
            eprintln!("{}: {:#?}", "Parse errors".red(), parse.errors);
        }
        eprintln!("{}", "CST tree:".fg::<Gray>());
        eprintln!("{}", parse.green_node.print_tree());
        std::process::exit(1);
    };
    println!("{}", parse.green_node.print_tree());
    Ok(())
}
