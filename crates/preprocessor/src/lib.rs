//! Token-level C preprocessing for Devicetree sources.

use std::borrow::Cow;

use camino::Utf8Path;
use dt_tools_diagnostic::{Diagnostic, Severity, text_range::TextRange};

mod cleaner;
mod config;
mod emit;
pub mod files;
mod lexer;
pub mod origin;
mod output;
mod token;

pub use config::{Define, PreprocessorConfig};
pub use files::{SourceFile, SourceId, SourceSet};
pub use lexer::{LexError, LexErrorAt, LexedFile, PpTokenKind, lex};
pub use origin::{Origin, ProvenanceMap, Segment};
pub use output::{IncludeEdge, MacroDefSite, PreprocessedRoot};
pub use token::PpToken;

/// Preprocesses `root`, reading files through `read_file`.
///
/// `read_file` should return `None` for files that cannot be read.
#[must_use]
pub fn preprocess(
    root: &Utf8Path,
    config: &PreprocessorConfig,
    mut read_file: impl FnMut(&Utf8Path) -> Option<String>,
) -> PreprocessedRoot {
    let _ = config; // TODO: use the config for macros and includes.

    let mut sources = SourceSet::default();
    let root_id = sources.add(root);

    let Some(contents) = read_file(root) else {
        return PreprocessedRoot {
            text: String::new(),
            map: ProvenanceMap::new(),
            sources: sources.into_files(),
            includes: Vec::new(),
            skipped: Vec::new(),
            macro_defs: Vec::new(),
            diagnostics: vec![Diagnostic::new(
                TextRange::new(0, 0).within_file(root_id),
                Cow::Borrowed("File does not exist"),
                Severity::Error,
            )],
        };
    };

    let cleaned = lex(&contents);
    let diagnostics = cleaned
        .errors
        .iter()
        .map(|error| {
            Diagnostic::new(
                error.orig_range.within_file(root_id),
                Cow::Owned(error.error.to_string()),
                Severity::Error,
            )
        })
        .collect();

    let (text, map) = emit::emit(&cleaned.tokens, &cleaned.cleaned_text, root_id);

    PreprocessedRoot {
        text,
        map,
        sources: sources.into_files(),
        includes: Vec::new(),
        skipped: Vec::new(),
        macro_defs: Vec::new(),
        diagnostics,
    }
}

#[cfg(test)]
mod tests {
    use camino::Utf8Path;

    use super::{PreprocessorConfig, preprocess};

    #[test]
    fn preprocesses_the_root_without_expansion() {
        let root = Utf8Path::new("test.dts");
        let result = preprocess(root, &PreprocessorConfig::default(), |path| {
            (path == root).then(|| "/dts-v1/;\n".to_owned())
        });

        assert_eq!(result.text, "/dts-v1/;");
        assert!(result.diagnostics.is_empty());
        assert_eq!(result.sources.len(), 1);
        assert_eq!(result.sources[0].path.as_path(), root);
        assert!(!result.map.segments().is_empty());
    }

    #[test]
    fn reports_a_missing_root() {
        let root = Utf8Path::new("missing.dts");
        let result = preprocess(root, &PreprocessorConfig::default(), |_| None);

        assert!(result.text.is_empty());
        assert_eq!(result.diagnostics.len(), 1);
        assert_eq!(result.sources.len(), 1);
    }
}
