//! Parses the file at argv 1 and prints the analyzed data as JSON

use std::process::Command;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let path = std::env::args()
        .nth(1)
        .expect("Should have a path as an argument");


    let text = std::fs::read_to_string(&path)?;
    let Some(red) = dt_parser::parse(&text) else {
        eprintln!("Invalid DTS!");
        std::process::exit(1);
    };
    let Some(def) = dt_analyzer::analyze_cst(red, &text) else {
        eprintln!("analyze_cst returned None");
        std::process::exit(1);
    };
    let json = def.tree.into_json();
    eprintln!("Analyzer JSON output: {}", serde_json::to_string(&json).unwrap());


    let dtc_out = Command::new("dtc").args([&path, "-O", "yaml"]).output().unwrap();
    if !dtc_out.status.success() {
        eprintln!("DTC errored: {}", dtc_out.status);
        std::process::exit(1);
    }
    let yaml: serde_json::Value = serde_yaml::from_slice(&dtc_out.stdout).unwrap();
    eprintln!("DTC output: {}", serde_json::to_string(&yaml).unwrap());
    Ok(())
}
