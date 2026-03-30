//! Parses the file at argv 1 and prints the analyzed data

use dt_tools_analyzer::new::outline::AnalyzedToplevel;
use owo_colors::{OwoColorize as _, colors::xterm::Gray};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let path = std::env::args()
        .nth(1)
        .expect("Should have a path as an argument");
    let text = std::fs::read_to_string(&path)?;

    let parse = dt_tools_parser::parser::parse(&text);
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
    }

    let file = parse.source_file();

    let mut new_diagnostics = Vec::new();
    let diag = std::sync::Mutex::new(&mut new_diagnostics);

    let outline = dt_tools_analyzer::new::outline::analyze_file(&file, &text, &diag);
    println!(
        "{}={:#?}",
        "macro defs".cyan(),
        outline
            .iter()
            .filter_map(AnalyzedToplevel::as_macro_definition)
            .collect::<Vec<_>>()
    );

    let includes = &[]; // TODO
    let analyzed2 = dt_tools_analyzer::new::stage2::compute(&outline, includes, &diag);
    println!("{}={:#?}", "analyzed2".cyan(), analyzed2);

    if !new_diagnostics.is_empty() {
        dt_tools_diagnostic::codespan_reporting::print_diagnostics_single_file(
            &path,
            &text,
            new_diagnostics,
        )?;

        std::process::exit(1);
    }

    Ok(())
}
