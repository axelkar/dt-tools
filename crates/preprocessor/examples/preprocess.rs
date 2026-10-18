//! Preprocesses a Devicetree source file and prints the expanded text.

use std::{env, fs, process::ExitCode};

use camino::{Utf8Path, Utf8PathBuf};
use dt_tools_diagnostic::Severity;
use dt_tools_preprocessor::{PreprocessorConfig, preprocess};

/// The command-line flag that adds an include directory.
const INCLUDE_FLAG: &str = "-I";

fn main() -> ExitCode {
    let Some((root, include_dirs)) = parse_args() else {
        eprintln!("usage: preprocess [-I DIR]... FILE");
        return ExitCode::FAILURE;
    };

    let config = PreprocessorConfig {
        include_dirs,
        ..PreprocessorConfig::default()
    };
    let result = preprocess(Utf8Path::new(&root), &config, |path| {
        fs::read_to_string(path).ok()
    });

    for diagnostic in &result.diagnostics {
        let Some(span) = diagnostic.span.primary_spans.first() else {
            continue;
        };
        let range = span.text_range;
        eprintln!(
            "{}:{}..{}: {:?}: {}",
            result.source(span.file).path,
            range.start,
            range.end,
            diagnostic.severity,
            diagnostic.msg
        );
    }

    print!("{}", result.text);

    let has_errors = result
        .diagnostics
        .iter()
        .any(|diagnostic| diagnostic.severity == Severity::Error);
    if has_errors {
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}

/// Parses `[-I DIR]... FILE`, returning the root path and include directories.
fn parse_args() -> Option<(Utf8PathBuf, Vec<Utf8PathBuf>)> {
    let mut include_dirs = Vec::new();
    let mut root = None;
    let mut args = env::args().skip(1);

    while let Some(arg) = args.next() {
        if arg == INCLUDE_FLAG {
            include_dirs.push(Utf8PathBuf::from(args.next()?));
        } else if let Some(dir) = arg.strip_prefix(INCLUDE_FLAG) {
            include_dirs.push(Utf8PathBuf::from(dir));
        } else if root.is_none() {
            root = Some(Utf8PathBuf::from(arg));
        } else {
            return None;
        }
    }

    Some((root?, include_dirs))
}
