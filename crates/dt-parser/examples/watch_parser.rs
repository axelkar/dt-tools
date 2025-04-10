//! Parses the file at argv 1 and prints the CST

use std::{
    mem,
    path::Path,
    sync::{mpsc, Arc},
    time::Duration,
};

use dt_parser::cst2::{GreenItem, GreenNode, NodeKind};
use notify::{Config, PollWatcher, RecursiveMode, Watcher as _};
use owo_colors::OwoColorize;

fn run(path: impl AsRef<Path>) -> Result<(), Box<dyn std::error::Error>> {
    let text = std::fs::read_to_string(path)?;
    let parse = dt_parser::cst2::parser::parse(&text);
    //if !parse.lex_errors.is_empty() || !parse.errors.is_empty() {
    //    eprintln!("Invalid DTS!");
    //    std::process::exit(1);
    //};
    print!("\x1b[H\x1b[2J\x1b[3J");
    //println!("{}", parse.green_node.print_tree());
    //print_tree_rec(&parse.green_node, 0, 0, &mut std::io::stdout())?;
    print_tree(Arc::new(parse.green_node));

    Ok(())
}

fn print_tree(node: Arc<GreenNode>) {
    const INDENT: &str = "  ";

    let mut stack = Vec::new();
    eprintln!("{:?}", node.kind.yellow());
    let mut current_node = (node, 0);
    let mut byte_offset = 0;
    let mut indent_level = 1;
    let mut error_level = None;

    loop {
        if let Some(child) = current_node.0.children.get(current_node.1) {
            current_node.1 += 1;
            let indent = INDENT.repeat(indent_level);
            match child {
                GreenItem::Node(node) => {
                    indent_level += 1;
                    if node.kind == NodeKind::ParseError && error_level.is_none() {
                        error_level = Some(indent_level);
                    }

                    if error_level.is_some() {
                        eprintln!("{indent}{:?}", node.kind.red());
                    } else {
                        eprintln!("{indent}{:?}", node.kind.yellow());
                    }

                    let new_current_node = node.clone();
                    stack.push(mem::replace(&mut current_node, (new_current_node, 0)));
                }
                GreenItem::Token(token) => {
                    let old_byte_offset = byte_offset;
                    byte_offset += token.length();

                    let byte_offset_str = format!("{old_byte_offset}..{byte_offset}");
                    if error_level.is_some() {
                        eprintln!(
                            "{indent}{:?}@{} {:?}",
                            token.kind.bright_red(),
                            byte_offset_str.red(),
                            token.text.red()
                        );
                    } else {
                        eprintln!(
                            "{indent}{:?}@{} {:?}",
                            token.kind.bright_magenta(),
                            byte_offset_str.bright_blue(),
                            token.text
                        );
                    }
                }
            }
        } else if let Some(new_current_node) = stack.pop() {
            // No more children -> go to parent
            if current_node.0.kind == NodeKind::ParseError && error_level == Some(indent_level) {
                error_level = None;
            }
            current_node = new_current_node;
            indent_level -= 1;
        } else {
            break;
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let path = std::env::args()
        .nth(1)
        .expect("Should have a path as an argument");

    run(&path)?;

    let (tx, rx) = mpsc::channel();

    let mut debouncer = PollWatcher::new(
        tx,
        Config::default().with_poll_interval(Duration::from_millis(500)),
    )
    .unwrap();
    debouncer
        .watch(
            Path::new(&path).parent().unwrap(),
            RecursiveMode::NonRecursive,
        )
        .unwrap();

    for event in rx {
        let event = event?;
        if event.kind.is_modify() {
            run(&path)?;
        }
    }
    Ok(())
}
