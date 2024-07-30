//! Parses the file at argv 1 and prints the analyzed data

use owo_colors::{colors::xterm::Gray, OwoColorize as _};

fn main() -> Result<(), Box<dyn std::error::Error>> {
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
    };

    let file = parse.source_file();

    let Some(def) = dt_analyzer::analyze_cst(&file, &text) else {
        eprintln!("analyze_cst returned None");
        std::process::exit(1);
    };

    let mut vec: Vec<_> = def.tree.dfs_iter().collect();
    vec.sort_by(|a, b| a.0.cmp(&b.0));
    for (name, value) in vec {
        let name = name.join("/");
        eprintln!("{} -> {:#?}", name, value);
    }

    if !parse.lex_errors.is_empty() || !parse.errors.is_empty() {
        std::process::exit(1);
    }
    Ok(())
}
