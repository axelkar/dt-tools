//! Parses the file at argv 1 and prints the analyzed data as JSON
//!
//! Use `--assert` in argv[2] to pretty_assertions::assert_eq

use dt_parser::ast::SourceFile;
use owo_colors::{colors::xterm::Gray, OwoColorize as _};
use std::process::Command;
use std::time::Instant;

fn remove_first<T>(vec: &mut Vec<T>) -> Option<T> {
    if vec.is_empty() {
        return None;
    }
    Some(vec.swap_remove(0))
}

/// This should be more efficient than "re-serializing" serde_yaml::Value into serde_json::Value
/// but more importantly, this never errors and skips tags (not the values though), because thay
/// can't be represented as serde_json::Value
///
/// NOTE: this cuts out node children with the name phandle and returns `-1` for anything with the
/// YAML tag `phandle` e.g. `!phandle 0x1`
///
/// # Panics
///
/// shouldn't panic trust me
fn yaml_to_json(value: serde_yaml::Value) -> serde_json::Value {
    use serde_json::Value as JValue;
    use serde_yaml::Value as YValue;
    match value {
        YValue::Null => JValue::Null,
        YValue::Bool(v) => JValue::Bool(v),
        YValue::Number(v) => JValue::Number(if let Some(n) = v.as_u64() {
            n.into()
        } else if let Some(n) = v.as_i64() {
            n.into()
        } else if let Some(n) = v.as_f64() {
            serde_json::Number::from_f64(n)
                .expect("serde_yaml should not give NaN or Infinite values")
        } else {
            unreachable!()
        }),
        YValue::String(v) => JValue::String(v),
        YValue::Sequence(v) => JValue::Array(v.into_iter().map(yaml_to_json).collect()),
        YValue::Mapping(v) => JValue::Object(
            v.into_iter()
                .filter(|(k, _)| k != "phandle")
                .filter_map(|(k, v)| {
                    Some((
                        match k {
                            YValue::String(s) => s,
                            _ => return None,
                        },
                        yaml_to_json(v),
                    ))
                })
                .collect(),
        ),
        YValue::Tagged(v) => {
            if v.tag == "phandle" {
                JValue::Number((-1).into())
            } else {
                yaml_to_json(v.value)
            }
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let path = std::env::args()
        .nth(1)
        .expect("Should have a path as an argument");

    let assert = std::env::args().nth(2) == Some("--assert".to_owned());

    let total_own_start = Instant::now();
    let text = std::fs::read_to_string(&path)?;
    let start = Instant::now();
    let parse = SourceFile::parse(&text);
    eprintln!(
        "{}{:?}",
        "Parsed in ".fg::<Gray>(),
        start.elapsed().bright_green()
    );

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

    let file = parse.source_file();

    let start = Instant::now();
    let Some(def) = dt_analyzer::analyze_cst(&file, &text) else {
        eprintln!("analyze_cst returned None");
        std::process::exit(1);
    };
    eprintln!(
        "{}{:?}",
        "Analyzed CST in ".fg::<Gray>(),
        start.elapsed().bright_green()
    );

    let start = Instant::now();
    let own_json = def.tree.into_json();
    eprintln!(
        "{}{:?}",
        "Turned own into JSON in ".fg::<Gray>(),
        start.elapsed().bright_green()
    );
    if !assert {
        eprintln!(
            "Analyzer JSON output: {}",
            serde_json::to_string(&own_json).unwrap()
        );
    }
    eprintln!(
        "{}{:?}",
        "Total own time: ".fg::<Gray>(),
        total_own_start.elapsed().bright_green()
    );

    let total_dtc_start = Instant::now();
    let start = Instant::now();
    let dtc_out = Command::new("dtc")
        .args([&path, "-O", "yaml"])
        .output()
        .unwrap();
    if !dtc_out.status.success() {
        eprintln!("DTC errored: {}", dtc_out.status);
        std::process::exit(1);
    }
    eprintln!(
        "{}{:?}",
        "Ran DTC in ".fg::<Gray>(),
        start.elapsed().bright_green()
    );

    let start = Instant::now();
    let mut dtc_json = yaml_to_json(serde_yaml::from_slice(&dtc_out.stdout).unwrap());
    eprintln!(
        "{}{:?}",
        "Turned DTC output into JSON in ".fg::<Gray>(),
        start.elapsed().bright_green()
    );

    let Some(dtc_json) = dtc_json.as_array_mut().and_then(remove_first) else {
        eprintln!("Invalid DTC output");
        std::process::exit(1);
    };

    if !assert {
        eprintln!("DTC output: {}", serde_json::to_string(&dtc_json).unwrap());
    } else {
        pretty_assertions::assert_eq!(own_json, dtc_json);
    }
    eprintln!(
        "{}{:?}",
        "Total DTC time: ".fg::<Gray>(),
        total_dtc_start.elapsed().bright_green()
    );
    Ok(())
}
