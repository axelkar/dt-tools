//! Parses the file at argv 1 and prints the analyzed data

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let path = std::env::args()
        .nth(1)
        .expect("Should have a path as an argument");
    let text = std::fs::read_to_string(&path)?;
    let Some(red) = dt_parser::parse(&text, path.into()) else {
        eprintln!("Invalid DTS!");
        std::process::exit(1);
    };
    let Some(def) = dt_analyzer::analyze_cst(red, &text) else {
        eprintln!("analyze_cst returned None");
        std::process::exit(1);
    };
    let mut vec: Vec<_> = def.props.into_iter().collect();
    vec.sort_by(|a, b| a.0.cmp(&b.0));
    for (name, value) in vec {
        eprintln!("{} -> {:#?}", name, value.last());
    }
    Ok(())
}
