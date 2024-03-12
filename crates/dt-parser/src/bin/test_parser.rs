//! Parses the file at argv 1 and prints the CST

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let path = std::env::args()
        .nth(1)
        .expect("Should have a path as an argument");
    let text = std::fs::read_to_string(path)?;
    let Some(green_node) = dt_parser::cst::parser::parse(&text).0 else {
        eprintln!("Invalid DTS!");
        std::process::exit(1);
    };
    println!("{}", green_node.print_tree(&text));
    Ok(())
}
