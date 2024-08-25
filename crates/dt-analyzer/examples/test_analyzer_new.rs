//! Parses the file at argv 1 and prints the analyzed data

use codespan_reporting::{diagnostic::Label, files::SimpleFiles};
use dt_analyzer::new::stage1::AnalyzedToplevel;
use dt_diagnostic::Severity;
use owo_colors::{colors::xterm::Gray, OwoColorize as _};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let path = std::env::args()
        .nth(1)
        .expect("Should have a path as an argument");
    let text = std::fs::read_to_string(&path)?;

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

    let mut new_diagnostics = Vec::new();
    let diag = std::sync::Mutex::new(&mut new_diagnostics);

    let analyzed = dt_analyzer::new::stage1::analyze_file(&file, &text, &diag);
    let includes = &[]; // TODO
    let analyzed2 = dt_analyzer::new::stage2::compute(
        analyzed.iter().filter_map(|a| a.as_node()),
        includes,
        &diag,
    );
    println!("{}={:#?}", "analyzed2".cyan(), analyzed2);
    println!(
        "{}={:#?}",
        "macro defs".cyan(),
        analyzed
            .iter()
            .filter_map(AnalyzedToplevel::as_macro_definition)
            .collect::<Vec<_>>()
    );

    if !new_diagnostics.is_empty() {
        let mut files = SimpleFiles::new();
        let file_id = files.add(&path, &text);

        let writer = codespan_reporting::term::termcolor::StandardStream::stderr(
            codespan_reporting::term::termcolor::ColorChoice::Always,
        );
        let config = codespan_reporting::term::Config::default();

        for new_diagnostic in new_diagnostics {
            /*for primary_span in new_diagnostic.span.primary_spans {
                eprint!("{}", format_args!("{primary_span}: ").fg::<Gray>());
            }
            let color = match new_diagnostic.severity {
                Severity::Warn => AnsiColors::Yellow,
                Severity::Error => AnsiColors::Red,
            };
            eprintln!(
                "{}",
                format_args!("{:?}: {}", new_diagnostic.severity, new_diagnostic.msg).color(color)
            );

            for span_label in new_diagnostic.span.span_labels {
                eprintln!(
                    "  {}",
                    format_args!("hint = {}", span_label.msg).fg::<Gray>()
                );
            }*/
            let diagnostic =
                codespan_reporting::diagnostic::Diagnostic::new(match new_diagnostic.severity {
                    Severity::Error => codespan_reporting::diagnostic::Severity::Error,
                    Severity::Warn => codespan_reporting::diagnostic::Severity::Warning,
                })
                .with_message(new_diagnostic.msg)
                .with_labels(
                    new_diagnostic
                        .span
                        .primary_spans
                        .iter()
                        .map(|span| Label::primary(file_id, *span))
                        .chain(new_diagnostic.span.span_labels.iter().map(|span| {
                            Label::secondary(file_id, span.span).with_message(span.msg.as_ref())
                        }))
                        .collect(),
                );

            codespan_reporting::term::emit(&mut writer.lock(), &config, &files, &diagnostic)?;
        }

        std::process::exit(1);
    }

    Ok(())
}
