//! Preprocesses a Devicetree source file and prints the expanded text.

use std::{env, fs, process::ExitCode};

use camino::{Utf8Path, Utf8PathBuf};
use dt_tools_diagnostic::Severity;
use dt_tools_preprocessor::{Define, PreprocessorConfig, preprocess};

/// The command-line flag that adds an include directory.
const INCLUDE_FLAG: &str = "-I";
/// The command-line flag that defines a macro.
const DEFINE_FLAG: &str = "-D";

fn main() -> ExitCode {
    let Some((root, include_dirs, defines)) = parse_args() else {
        eprintln!("usage: preprocess [-I DIR]... [-D NAME[=VALUE]]... FILE");
        return ExitCode::FAILURE;
    };

    let config = PreprocessorConfig {
        include_dirs,
        defines,
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

/// Parses `[-I DIR]... [-D NAME[=VALUE]]... FILE`.
fn parse_args() -> Option<(Utf8PathBuf, Vec<Utf8PathBuf>, Vec<Define>)> {
    let mut include_dirs = Vec::new();
    let mut defines = Vec::new();
    let mut root = None;
    let mut args = env::args().skip(1);

    while let Some(arg) = args.next() {
        if arg == INCLUDE_FLAG {
            include_dirs.push(Utf8PathBuf::from(args.next()?));
        } else if let Some(dir) = arg.strip_prefix(INCLUDE_FLAG) {
            include_dirs.push(Utf8PathBuf::from(dir));
        } else if arg == DEFINE_FLAG {
            defines.push(parse_define(&args.next()?));
        } else if let Some(define) = arg.strip_prefix(DEFINE_FLAG) {
            defines.push(parse_define(define));
        } else if root.is_none() {
            root = Some(Utf8PathBuf::from(arg));
        } else {
            return None;
        }
    }

    Some((root?, include_dirs, defines))
}

/// Parses a `NAME` or `NAME=VALUE` macro definition.
fn parse_define(define: &str) -> Define {
    match define.split_once('=') {
        Some((name, value)) => Define {
            name: name.to_owned(),
            value: value.to_owned(),
        },
        None => Define {
            name: define.to_owned(),
            value: "1".to_owned(),
        },
    }
}
