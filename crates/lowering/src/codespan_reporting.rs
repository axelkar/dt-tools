//! Multi-file diagnostic printing via [`codespan_reporting`].

use std::collections::HashMap;

use codespan_reporting::{
    files::SimpleFiles,
    term::{self, termcolor},
};
use dt_tools_diagnostic::{Diagnostic, Severity};

use crate::{db::BaseDb, file::File};

/// Print diagnostics across multiple files using codespan-reporting.
///
/// Returns `true` if any diagnostic has error severity.
///
/// # Errors
///
/// Propagates I/O errors from codespan-reporting.
pub fn print_diagnostics(
    diagnostics: Vec<Diagnostic<File>>,
    db: &dyn BaseDb,
) -> Result<bool, codespan_reporting::files::Error> {
    let has_errors = diagnostics.iter().any(|d| d.severity == Severity::Error);

    // Collect unique files from all diagnostics
    let mut files: Vec<File> = Vec::new();
    for diag in &diagnostics {
        for span in &diag.span.primary_spans {
            files.push(span.file);
        }
        for label in &diag.span.span_labels {
            files.push(label.span.file);
        }
    }
    files.sort_by_key(|f| f.path(db).as_str().len());
    files.dedup_by_key(|f| f.path(db).as_str());

    // Build SimpleFiles + file -> FileId map
    let mut simple_files = SimpleFiles::new();
    let mut file_to_id: HashMap<File, usize> = HashMap::new();
    for &file in &files {
        let id = simple_files.add(file.path(db).clone(), file.contents(db).as_str().to_owned());
        file_to_id.insert(file, id);
    }

    let writer = termcolor::StandardStream::stderr(termcolor::ColorChoice::Always);
    let config = term::Config::default();

    for diag in diagnostics {
        term::emit_to_write_style(
            &mut writer.lock(),
            &config,
            &simple_files,
            &diag.into_codespan_reporting(|f| file_to_id[&f]),
        )?;
    }

    Ok(has_errors)
}
