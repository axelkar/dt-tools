//! Token-level C preprocessing for Devicetree sources.

use std::borrow::Cow;

use camino::Utf8Path;
use dt_tools_diagnostic::{Diagnostic, Severity, text_range::TextRange};

mod cleaner;
mod config;
mod directive;
mod emit;
mod env;
mod expand;
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
    let mut diagnostics: Vec<Diagnostic<SourceId>> = cleaned
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

    let expansion = expand::expand(&cleaned.tokens, &cleaned.cleaned_text, root_id, config);
    diagnostics.extend(expansion.diagnostics);

    let (text, map) = emit::emit(&expansion.tokens, &cleaned.cleaned_text, root_id);

    PreprocessedRoot {
        text,
        map,
        sources: sources.into_files(),
        includes: Vec::new(),
        skipped: Vec::new(),
        macro_defs: expansion.macro_defs,
        diagnostics,
    }
}

#[cfg(test)]
mod tests {
    use camino::Utf8Path;
    use dt_tools_diagnostic::Severity;

    use super::{PreprocessedRoot, PreprocessorConfig, preprocess};

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

    fn preprocess_str(input: &str) -> PreprocessedRoot {
        let root = Utf8Path::new("test.dts");
        preprocess(root, &PreprocessorConfig::default(), |_| {
            Some(input.to_owned())
        })
    }

    #[test]
    fn expands_object_like_macros() {
        // Nested and spaced replacement lists are expanded in place.
        assert_eq!(preprocess_str("#define A 1\nA\n").text, "1");
        assert_eq!(preprocess_str("#define A B\n#define B 2\nA\n").text, "2");
        assert_eq!(preprocess_str("#define A foo bar\nA\n").text, "foo bar");
    }

    #[test]
    fn stops_recursive_expansion() {
        // The active-name set blocks self and mutual recursion.
        assert_eq!(preprocess_str("#define A A\nA\n").text, "A");
        assert_eq!(preprocess_str("#define A B\n#define B A\nA\n").text, "A");
    }

    #[test]
    fn undefines_macros() {
        // A name is ordinary again after #undef.
        assert_eq!(preprocess_str("#define A 1\n#undef A\nA\n").text, "A");
    }

    #[test]
    fn expands_an_empty_replacement() {
        // The invocation disappears and the following token is emitted alone.
        assert_eq!(preprocess_str("#define A\nA x\n").text, "x");
    }

    #[test]
    fn keeps_unknown_hash_lines() {
        // An unknown directive name is ordinary assembler-mode text.
        let result = preprocess_str("#address-cells = <1>;\n");
        assert_eq!(result.text, "#address-cells = <1>;");
        assert!(result.diagnostics.is_empty());
    }

    #[test]
    fn reports_unsupported_directives() {
        // Known but unimplemented directives are diagnosed and skipped.
        let result = preprocess_str("#include <a.h>\n");
        assert_eq!(result.diagnostics.len(), 1);
        assert_eq!(result.diagnostics[0].severity, Severity::Warn);
    }

    #[test]
    fn reports_error_and_warning_directives() {
        // Both directive severities reach the diagnostics list.
        let result = preprocess_str("#error boom\n#warning careful\n");
        assert_eq!(result.diagnostics.len(), 2);
        assert_eq!(result.diagnostics[0].severity, Severity::Error);
        assert_eq!(result.diagnostics[1].severity, Severity::Warn);
    }

    #[test]
    fn reports_function_like_macros() {
        // Function-like definitions are not implemented yet.
        let result = preprocess_str("#define F(x) x\n");
        assert_eq!(result.diagnostics.len(), 1);
        assert_eq!(result.diagnostics[0].severity, Severity::Warn);
    }

    #[test]
    fn warns_on_different_redefinitions() {
        // The new definition wins, with one warning.
        let result = preprocess_str("#define A 1\n#define A 2\nA\n");
        assert_eq!(result.text, "2");
        assert_eq!(result.diagnostics.len(), 1);
        assert_eq!(result.diagnostics[0].severity, Severity::Warn);
    }

    #[test]
    fn accepts_identical_redefinitions() {
        // Repeating the same replacement list is not diagnosed.
        let result = preprocess_str("#define A 1\n#define A 1\nA\n");
        assert_eq!(result.text, "1");
        assert!(result.diagnostics.is_empty());
    }

    #[test]
    fn warns_on_extra_undef_tokens() {
        // The undef still applies, and the extra token is diagnosed.
        let result = preprocess_str("#define A 1\n#undef A extra\nA\n");
        assert_eq!(result.text, "A");
        assert_eq!(result.diagnostics.len(), 1);
        assert_eq!(result.diagnostics[0].severity, Severity::Warn);
    }

    #[test]
    fn reports_malformed_directives() {
        // Missing and non-identifier macro names are errors.
        for input in ["#define\n", "#define 1 A\n", "#undef\n"] {
            let result = preprocess_str(input);
            assert_eq!(result.diagnostics.len(), 1, "input: {input:?}");
            assert_eq!(
                result.diagnostics[0].severity,
                Severity::Error,
                "input: {input:?}"
            );
        }
    }
}
