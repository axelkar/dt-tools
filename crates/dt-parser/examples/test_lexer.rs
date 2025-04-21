// See https://github.com/maciejhirsz/logos/issues/399

use dt_parser::lexer;
use owo_colors::{colors::xterm::Gray, OwoColorize as _};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let path = std::env::args()
        .nth(1)
        .expect("Should have a path as an argument");
    let input = std::fs::read_to_string(path)?;

    for token in lexer::lex(&input) {
        match token.kind {
            Ok(kind) => println!("{kind} {:?} {}", token.text, token.text_range.fg::<Gray>()),
            Err(err) => println!(
                "{} {:?} {}",
                format_args!("Error: {err}").red(),
                token.text,
                token.text_range.fg::<Gray>()
            ),
        }
    }
    Ok(())
}
