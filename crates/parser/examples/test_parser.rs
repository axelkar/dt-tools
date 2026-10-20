//! Parses the file at argv 1 and prints the CST

use owo_colors::{OwoColorize as _, colors::xterm::Gray};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let path = std::env::args()
        .nth(1)
        .expect("Should have a path as an argument");
    let text = std::fs::read_to_string(path)?;

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
        std::process::exit(1);
    }
    print!("{}", parse.green_node.print_tree());

    Ok(())
}
