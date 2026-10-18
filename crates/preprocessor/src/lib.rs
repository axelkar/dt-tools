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

    let expansion = expand::expand(
        &cleaned.tokens,
        &cleaned.cleaned_text,
        cleaned.orig_len,
        root_id,
        config,
    );
    diagnostics.extend(expansion.diagnostics);

    let (text, map) = emit::emit(&expansion.tokens, &cleaned.cleaned_text, root_id);

    PreprocessedRoot {
        text,
        map,
        sources: sources.into_files(),
        includes: Vec::new(),
        skipped: expansion.skipped,
        macro_defs: expansion.macro_defs,
        diagnostics,
    }
}

#[cfg(test)]
mod tests {
    use camino::Utf8Path;
    use dt_tools_diagnostic::{Severity, text_range::TextRange};

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

    /// Returns the value of a condition and asserts that it has no diagnostics.
    fn condition(source: &str) -> bool {
        let input = format!("#if {source}\nfoo\n#endif\n");
        let result = preprocess_str(&input);
        assert!(
            result.diagnostics.is_empty(),
            "{source}: {:?}",
            result.diagnostics
        );
        !result.text.is_empty()
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
    fn accepts_function_like_definitions() {
        // Definitions are accepted and recorded.
        let result = preprocess_str("#define F(x) x\n");
        assert!(result.diagnostics.is_empty());
        assert_eq!(result.macro_defs.len(), 1);
        assert_eq!(result.macro_defs[0].name, "F");
    }

    #[test]
    fn accepts_variadic_definitions() {
        let result = preprocess_str("#define F(a, ...) a __VA_ARGS__\n");
        assert!(result.diagnostics.is_empty());
        assert_eq!(result.macro_defs.len(), 1);
    }

    #[test]
    fn expands_variadic_macros() {
        assert_eq!(
            preprocess_str("#define F(...) __VA_ARGS__\nF(1, 2)\n").text,
            "1, 2"
        );
        assert_eq!(
            preprocess_str("#define F(a, ...) a __VA_ARGS__\nF(1, 2, 3)\n").text,
            "1 2, 3"
        );
        assert_eq!(
            preprocess_str("#define F(a, ...) a __VA_ARGS__\nF(1)\n").text,
            "1"
        );
        assert_eq!(preprocess_str("#define F(...) __VA_ARGS__\nF()\n").text, "");
    }

    #[test]
    fn prescans_variadic_arguments() {
        assert_eq!(
            preprocess_str("#define A 1\n#define F(...) __VA_ARGS__\nF(A, 2)\n").text,
            "1, 2"
        );
    }

    #[test]
    fn reports_too_few_variadic_arguments() {
        let result = preprocess_str("#define F(a, b, ...) a b __VA_ARGS__\nF(1)\n");
        assert_eq!(result.diagnostics.len(), 1);
        assert_eq!(result.diagnostics[0].severity, Severity::Error);
    }

    #[test]
    fn stringifies_arguments() {
        assert_eq!(preprocess_str("#define S(x) #x\nS(abc)\n").text, "\"abc\"");
        assert_eq!(preprocess_str("#define S(x) #x\nS(a b)\n").text, "\"a b\"");
        assert_eq!(
            preprocess_str("#define S(x) #x\nS( a   b )\n").text,
            "\"a b\""
        );
        assert_eq!(
            preprocess_str("#define S(x) #x\nS(\"a b\")\n").text,
            r#""\"a b\"""#
        );
        assert_eq!(preprocess_str("#define S(x) #x\nS()\n").text, "\"\"");
    }

    #[test]
    fn pastes_tokens() {
        assert_eq!(
            preprocess_str("#define CAT(a, b) a ## b\nCAT(foo, bar)\n").text,
            "foobar"
        );
        assert_eq!(
            preprocess_str("#define CAT(a, b) a ## b\nCAT(+, +)\n").text,
            "++"
        );
    }

    #[test]
    fn pastes_tokens_in_object_like_macros() {
        // `##` applies to both macro kinds, unlike `#`.
        assert_eq!(preprocess_str("#define C 0##0\nC\n").text, "00");
        assert_eq!(preprocess_str("#define H # x\nH\n").text, "# x");
    }

    #[test]
    fn does_not_prescan_arguments_used_only_by_hash_or_paste() {
        // The wrong arity of `B(2)` must not be diagnosed in either case.
        let result = preprocess_str("#define A(a, b) #b\n#define B(a, b) a b\nA(1, B(2))\n");
        assert_eq!(result.text, "\"B(2)\"");
        assert!(result.diagnostics.is_empty());

        let result = preprocess_str("#define A(a, b) a ## b\n#define B(a, b) a b\nA(1, B(2))\n");
        assert_eq!(result.text, "1B(2)");
        assert!(result.diagnostics.is_empty());
    }

    #[test]
    fn does_not_prescan_operands_of_hash_and_paste() {
        assert_eq!(
            preprocess_str("#define A 1\n#define S(x) #x\nS(A)\n").text,
            "\"A\""
        );
        assert_eq!(
            preprocess_str("#define A 1\n#define B 2\n#define CAT(a, b) a ## b\nCAT(A, B)\n").text,
            "AB"
        );
    }

    #[test]
    fn handles_empty_paste_operands() {
        assert_eq!(
            preprocess_str("#define CAT(a, b) a ## b\nCAT(, x)\n").text,
            "x"
        );
        assert_eq!(
            preprocess_str("#define CAT(a, b) a ## b\nCAT(x,)\n").text,
            "x"
        );
        assert_eq!(
            preprocess_str("#define CAT(a, b) a ## b\nCAT(,)\n").text,
            ""
        );
    }

    #[test]
    fn reports_invalid_pastes() {
        for input in [
            "#define BAD(a, b) a ## b\nBAD(+, -)\n",
            "#define BAD(a, b) a ## b\nBAD(/, *)\n",
            "#define BAD(x) ## x\nBAD(1)\n",
            "#define BAD(x) x ##\nBAD(1)\n",
        ] {
            let result = preprocess_str(input);
            assert!(!result.diagnostics.is_empty(), "input: {input:?}");
        }
    }

    #[test]
    fn elides_the_gnu_variadic_comma() {
        assert_eq!(
            preprocess_str("#define F(a, ...) a, ## __VA_ARGS__\nF(1)\n").text,
            "1"
        );
        assert_eq!(
            preprocess_str("#define F(a, ...) a, ## __VA_ARGS__\nF(1, 2)\n").text,
            "1, 2"
        );
        assert_eq!(
            preprocess_str("#define F(a, ...) a, ## __VA_ARGS__\nF(1,)\n").text,
            "1,"
        );
    }

    #[test]
    fn reports_bad_macro_parameter_lists() {
        // Malformed lists are errors, and no macro is defined.
        for input in [
            "#define F(1) x\n",
            "#define F(a b) x\n",
            "#define F(a,) x\n",
            "#define F(a, a) x\n",
            "#define F(a x\n",
        ] {
            let result = preprocess_str(input);
            assert_eq!(result.diagnostics.len(), 1, "input: {input:?}");
            assert_eq!(
                result.diagnostics[0].severity,
                Severity::Error,
                "input: {input:?}"
            );
        }
    }

    #[test]
    fn distinguishes_object_and_function_like_definitions() {
        // A space before `(` makes the definition object-like.
        assert_eq!(preprocess_str("#define F (x) x\nF\n").text, "(x) x");
        // A function-like name without `(` is not expanded.
        assert_eq!(preprocess_str("#define F(x) x\nF\n").text, "F");
    }

    #[test]
    fn warns_on_different_function_like_redefinitions() {
        let result = preprocess_str("#define F(x) x\n#define F(y) y\n");
        assert_eq!(result.diagnostics.len(), 1);
        assert_eq!(result.diagnostics[0].severity, Severity::Warn);
    }

    #[test]
    fn accepts_identical_function_like_redefinitions() {
        let result = preprocess_str("#define F(x) x\n#define F(x) x\n");
        assert!(result.diagnostics.is_empty());
    }

    #[test]
    fn expands_function_like_macros() {
        assert_eq!(preprocess_str("#define F(x) x\nF(1)\n").text, "1");
        assert_eq!(preprocess_str("#define F(a, b) a b\nF(1, 2)\n").text, "1 2");
        assert_eq!(preprocess_str("#define F() 1\nF()\n").text, "1");
        assert_eq!(preprocess_str("#define F(x) x\nF (1)\n").text, "1");
    }

    #[test]
    fn prescans_arguments() {
        // The inner F must expand before the outer F's hide set applies. Without
        // prescan, the result would be `F(1)` instead of `1`.
        assert_eq!(preprocess_str("#define F(x) x\nF(F(1))\n").text, "1");
    }

    #[test]
    fn rescans_with_following_tokens() {
        // F expands to G, and the following `(` makes it a function-like invocation.
        assert_eq!(
            preprocess_str("#define F G\n#define G(x) x\nF(1)\n").text,
            "1"
        );
    }

    #[test]
    fn handles_empty_and_nested_arguments() {
        assert_eq!(preprocess_str("#define F(x) [x]\nF()\n").text, "[]");
        assert_eq!(
            preprocess_str("#define F(x) [x]\nF((1, 2))\n").text,
            "[(1, 2)]"
        );
    }

    #[test]
    fn reports_wrong_argument_counts() {
        let result = preprocess_str("#define F(a, b) a b\nF(1)\n");
        assert_eq!(result.diagnostics.len(), 1);
        assert_eq!(result.diagnostics[0].severity, Severity::Error);
    }

    #[test]
    fn stops_function_like_recursion() {
        assert_eq!(preprocess_str("#define F(x) F(x)\nF(1)\n").text, "F(1)");
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

    #[test]
    fn skips_false_branches() {
        assert_eq!(preprocess_str("#if 0\nfoo\n#endif\n").text, "");
        assert_eq!(preprocess_str("#if 1\nfoo\n#endif\n").text, "foo");
        assert_eq!(preprocess_str("#if 2\nfoo\n#endif\n").text, "foo");
        assert_eq!(
            preprocess_str("#if 0\nfoo\n#else\nbar\n#endif\n").text,
            "bar"
        );
        assert_eq!(
            preprocess_str("#if 1\nfoo\n#else\nbar\n#endif\n").text,
            "foo"
        );
    }

    #[test]
    fn handles_elif_chains() {
        assert_eq!(
            preprocess_str("#if 0\na\n#elif 1\nb\n#else\nc\n#endif\n").text,
            "b"
        );
        assert_eq!(
            preprocess_str("#if 1\na\n#elif 1\nb\n#else\nc\n#endif\n").text,
            "a"
        );
        assert_eq!(
            preprocess_str("#if 0\na\n#elif 0\nb\n#else\nc\n#endif\n").text,
            "c"
        );
        // A later #elif is not evaluated once a branch was taken.
        let result = preprocess_str("#if 1\na\n#elif defined\nb\n#endif\n");
        assert_eq!(result.text, "a");
        assert!(result.diagnostics.is_empty());
    }

    #[test]
    fn checks_definedness() {
        assert_eq!(
            preprocess_str("#define A 1\n#ifdef A\nfoo\n#endif\n").text,
            "foo"
        );
        assert_eq!(preprocess_str("#ifdef A\nfoo\n#endif\n").text, "");
        assert_eq!(preprocess_str("#ifndef A\nfoo\n#endif\n").text, "foo");
        assert_eq!(
            preprocess_str("#define A\n#ifdef A\nfoo\n#endif\n").text,
            "foo"
        );
        assert_eq!(
            preprocess_str("#define A 1\n#undef A\n#ifdef A\nfoo\n#endif\n").text,
            ""
        );
    }

    #[test]
    fn evaluates_defined_and_undefined_identifiers() {
        assert_eq!(
            preprocess_str("#define A 1\n#if defined(A)\nfoo\n#endif\n").text,
            "foo"
        );
        assert_eq!(preprocess_str("#if defined A\nfoo\n#endif\n").text, "");
        assert_eq!(preprocess_str("#if !defined(A)\nfoo\n#endif\n").text, "foo");
        assert_eq!(preprocess_str("#if UNDEF\nfoo\n#endif\n").text, "");
        assert_eq!(preprocess_str("#if !UNDEF\nfoo\n#endif\n").text, "foo");
        // The operand of `defined` is not expanded.
        assert_eq!(
            preprocess_str("#define A B\n#if defined(A)\nfoo\n#endif\n").text,
            "foo"
        );
    }

    #[test]
    fn evaluates_minimal_conditions() {
        assert_eq!(preprocess_str("#if !0\nfoo\n#endif\n").text, "foo");
        assert_eq!(preprocess_str("#if !1\nfoo\n#endif\n").text, "");
        assert_eq!(preprocess_str("#if (1)\nfoo\n#endif\n").text, "foo");
        assert_eq!(preprocess_str("#if (0)\nfoo\n#endif\n").text, "");
        assert_eq!(
            preprocess_str("#define A 1\n#if A\nfoo\n#endif\n").text,
            "foo"
        );
    }

    #[test]
    fn evaluates_arithmetic_and_precedence() {
        assert!(condition("1 + 1"));
        assert!(condition("7 / 2 == 3"));
        assert!(condition("7 % 2 == 1"));
        assert!(condition("1 + 2 * 3 == 7"));
        assert!(condition("(1 + 2) * 3 == 9"));
        assert!(condition("10 - 3 - 2 == 5"));
        assert!(condition("1 << 2 + 1 == 8"));
    }

    #[test]
    fn evaluates_bitwise_operators() {
        assert!(condition("(6 & 3) == 2"));
        assert!(condition("(6 | 3) == 7"));
        assert!(condition("(6 ^ 3) == 5"));
        assert!(condition("~0 == -1"));
        assert!(condition("(1 << 4) == 16"));
        assert!(condition("(16 >> 4) == 1"));
    }

    #[test]
    fn evaluates_comparisons_and_logic() {
        assert!(condition("1 < 2 && 2 <= 2"));
        assert!(condition("3 > 2 && 3 >= 3"));
        assert!(condition("1 == 1 && 1 != 2"));
        assert!(condition("0 || 1"));
        assert!(condition("!0"));
    }

    #[test]
    fn evaluates_ternary() {
        assert!(condition("(1 ? 2 : 3) == 2"));
        assert!(condition("(0 ? 2 : 3) == 3"));
        // `?:` is right-associative.
        assert!(condition("(1 ? 0 : 1 ? 2 : 3) == 0"));
    }

    #[test]
    fn short_circuits_dead_operands() {
        assert!(!condition("0 && (1 / 0)"));
        assert!(condition("1 || (1 / 0)"));
        assert!(condition("0 ? (1 / 0) : 1"));
        assert!(condition("1 ? 1 : (1 / 0)"));
    }

    #[test]
    fn evaluates_character_constants() {
        assert!(condition("'a' == 97"));
        assert!(condition("'\\n' == 10"));
        assert!(condition("'\\0' == 0"));
        assert!(condition("'\\\\' == 92"));
        assert!(condition("'\\x41' == 65"));
        assert!(condition("'\\101' == 65"));
        assert!(condition("'\\xFF' == -1"));
    }

    #[test]
    fn evaluates_integer_bases_and_suffixes() {
        assert!(condition("0x10 == 16"));
        assert!(condition("0b101 == 5"));
        assert!(condition("010 == 8"));
        assert!(condition("1U + 1L + 1UL + 1LL == 4"));
    }

    #[test]
    fn reports_division_by_zero() {
        for input in ["#if 1 / 0\nfoo\n#endif\n", "#if 1 % 0\nfoo\n#endif\n"] {
            let result = preprocess_str(input);
            assert!(!result.diagnostics.is_empty(), "input: {input:?}");
        }
    }

    #[test]
    fn reports_invalid_integer_suffixes() {
        for condition in ["1lll", "1lul", "12abc"] {
            let input = format!("#if {condition}\nfoo\n#endif\n");
            let result = preprocess_str(&input);
            assert!(!result.diagnostics.is_empty(), "{condition}");
        }
    }

    #[test]
    fn handles_elifdef_and_elifndef() {
        assert_eq!(
            preprocess_str("#define A 1\n#if 0\na\n#elifdef A\nb\n#endif\n").text,
            "b"
        );
        assert_eq!(
            preprocess_str("#if 0\na\n#elifndef A\nb\n#endif\n").text,
            "b"
        );
        assert_eq!(
            preprocess_str("#define A 1\n#if 0\na\n#elifndef A\nb\n#endif\n").text,
            ""
        );
    }

    #[test]
    fn skips_directives_in_inactive_branches() {
        let result = preprocess_str("#if 0\n#define A 1\n#error boom\nA\n#endif\n");
        assert_eq!(result.text, "");
        assert!(result.diagnostics.is_empty());

        // A malformed condition inside a skipped branch is not evaluated.
        let result = preprocess_str("#if 0\n#if defined\nx\n#endif\n#endif\n");
        assert_eq!(result.text, "");
        assert!(result.diagnostics.is_empty());

        assert_eq!(preprocess_str("#if 0\n#define A 1\n#endif\nA\n").text, "A");
    }

    #[test]
    fn nests_conditionals() {
        assert_eq!(
            preprocess_str("#if 1\n#if 0\na\n#else\nb\n#endif\n#endif\n").text,
            "b"
        );
        assert_eq!(
            preprocess_str("#if 1\n#if 1\nx\n#endif\n#endif\n").text,
            "x"
        );
        assert_eq!(
            preprocess_str("#if 0\n#if 1\na\n#endif\nb\n#endif\n").text,
            ""
        );
        assert_eq!(
            preprocess_str("#if 0\na\n#else\n#if 1\nb\n#endif\n#endif\n").text,
            "b"
        );
    }

    #[test]
    fn records_skipped_branch_ranges() {
        let result = preprocess_str("#if 0\nfoo\n#endif\n");
        assert_eq!(result.skipped.len(), 1);
        assert_eq!(result.skipped[0].text_range, TextRange::new(5, 10));

        let result = preprocess_str("#if 0\nfoo\n#else\nbar\n#endif\n");
        assert_eq!(result.skipped.len(), 1);
        assert_eq!(result.skipped[0].text_range, TextRange::new(5, 10));

        let result = preprocess_str("#if 1\nfoo\n#else\nbar\n#endif\n");
        assert_eq!(result.skipped.len(), 1);
        assert_eq!(result.skipped[0].text_range, TextRange::new(15, 20));

        // A nested group inside an inactive branch adds no range of its own.
        let result = preprocess_str("#if 0\n#if 1\na\n#endif\nb\n#endif\n");
        assert_eq!(result.skipped.len(), 1);
    }

    #[test]
    fn reports_unbalanced_conditionals() {
        for input in [
            "#endif\n",
            "#if 1\n",
            "#else\n",
            "#elif 1\n",
            "#if\n#endif\n",
            "#if 1\n#else\n#else\n#endif\n",
            "#if 1\n#elif 1\n#else\n#elif 1\n#endif\n",
            "#ifdef\n#endif\n",
            "#ifndef\n#endif\n",
        ] {
            let result = preprocess_str(input);
            assert!(!result.diagnostics.is_empty(), "input: {input:?}");
        }
    }
}
