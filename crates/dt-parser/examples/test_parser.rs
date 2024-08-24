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
    print!("{}", parse.green_node.print_tree());

    visualize();
    Ok(())
}

#[cfg(feature = "visualize")]
fn visualize() {
    use dt_parser::cst2::parser::visualizer;
    use owo_colors::OwoColorize;
    use serde_json::json;

    let Ok(path) = std::env::var("EXPORT_VISUALIZATION") else {
        return;
    };

    let events = visualizer::take_events()
        .into_iter()
        .map(|event| match event {
            visualizer::Event::Init { tokens } => json!({
                "kind": "init",
                "tokens": tokens.into_iter().map(|tok| json!({
                    "error": tok.kind.err().map(|err| err.to_string()),
                    "kind": tok.kind.ok().map(|kind| format!("{:?}", kind)),
                    "text": tok.text,
                    "text_range": {
                        "start": tok.text_range.start,
                        "end": tok.text_range.end,
                    }
                })).collect::<Vec<_>>()
            }),
            visualizer::Event::NextToken {
                cursor,
                prev_next_cursor,
            } => json!({
                "kind": "next_token",
                "cursor": cursor,
                "prev_next_cursor": prev_next_cursor
            }),
            visualizer::Event::PeekKindImmediate => json!({ "kind": "peek_kind_immediate" }),
            visualizer::Event::SkippedTrivia { cursor } => json!({
                "kind": "skipped_trivia",
                "cursor": cursor,
            }),
            visualizer::Event::GramBegin(name) => json!({
                "kind": "grammar_begin",
                "name": name,
            }),
            visualizer::Event::GramEnd(name) => json!({
                "kind": "grammar_end",
                "name": name,
            }),
        })
        .collect::<Vec<_>>();

    let file = std::fs::File::create(path).unwrap();

    serde_json::to_writer(&file, &events).unwrap();
    println!(
        "{}{}",
        "Visualized! ".green().bold(),
        "Open it with ./examples/visualizer.html".green()
    );
}

#[cfg(not(feature = "visualize"))]
fn visualize() {
    if std::env::var("EXPORT_VISUALIZATION").is_ok() {
        eprintln!("{}", "Visualization is not compiled in!".yellow());
    }
}
