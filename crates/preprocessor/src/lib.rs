//! Token-level C preprocessing for Devicetree sources.

use std::borrow::Cow;

use camino::Utf8Path;
use dt_tools_diagnostic::{Diagnostic, Severity, text_range::TextRange};

mod cleaner;
mod config;
mod directive;
mod dts_include;
mod emit;
mod env;
mod expand;
pub mod files;
mod lexer;
pub mod origin;
mod output;
mod token;

pub use config::{Define, PreprocessorConfig};
use files::LoadedFiles;
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
    profiling::scope!("preprocessor::preprocess", root.as_str());

    let mut files = LoadedFiles::default();
    let root_id = files.sources.add(root);

    let Some(contents) = read_file(root) else {
        return PreprocessedRoot {
            text: String::new(),
            map: ProvenanceMap::new(),
            sources: files.sources.into_files(),
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

    let loaded = files.add(root, &contents);
    let mut diagnostics: Vec<Diagnostic<SourceId>> = loaded
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
        &loaded.tokens,
        &mut files,
        loaded.orig_len,
        root_id,
        config,
        &mut read_file,
    );
    diagnostics.extend(expansion.diagnostics);

    let (text, map) = emit::emit(&expansion.tokens, &files.cleaned);
    let spliced = dts_include::splice(
        &text,
        &map,
        &mut files.sources,
        config,
        &mut read_file,
        root_id,
    );
    diagnostics.extend(spliced.diagnostics);
    let mut includes = expansion.includes;
    includes.extend(spliced.includes);

    PreprocessedRoot {
        text: spliced.text,
        map: spliced.map,
        sources: files.sources.into_files(),
        includes,
        skipped: expansion.skipped,
        macro_defs: expansion.macro_defs,
        diagnostics,
    }
}

#[cfg(test)]
mod tests {
    use camino::Utf8Path;
    use dt_tools_diagnostic::{Severity, text_range::TextRange};

    use super::{Define, PreprocessedRoot, PreprocessorConfig, preprocess};

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

    /// Predefined macros are visible before the root file and can be undefined.
    #[test]
    fn applies_predefined_macros() {
        let config = PreprocessorConfig {
            defines: vec![
                Define {
                    name: "FLAG".to_owned(),
                    value: String::new(),
                },
                Define {
                    name: "VALUE".to_owned(),
                    value: "42".to_owned(),
                },
            ],
            ..PreprocessorConfig::default()
        };
        let run = |input: &str| {
            preprocess(Utf8Path::new("test.dts"), &config, |_| {
                Some(input.to_owned())
            })
        };

        let result = run("#ifdef FLAG\n#ifdef VALUE\nVALUE\n#endif\n#endif\n");
        assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics);
        assert_eq!(result.text, "42");

        assert_eq!(run("#undef VALUE\nVALUE\n").text, "VALUE");
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
        let result = preprocess_str("#pragma once\n");
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
    fn expands_named_variadic_macros() {
        // GNU allows naming the variadic parameter instead of writing `__VA_ARGS__`.
        assert_eq!(
            preprocess_str("#define F(x, y...) x y\nF(1, 2, 3)\n").text,
            "1 2, 3"
        );
        assert_eq!(preprocess_str("#define V(y...) y\nV(1, 2)\n").text, "1, 2");
        assert_eq!(
            preprocess_str("#define S(x, y...) #y\nS(1, 2, 3)\n").text,
            "\"2, 3\""
        );
        assert_eq!(
            preprocess_str("#define C(x, y...) x, ## y\nC(1)\n").text,
            "1"
        );
        assert_eq!(
            preprocess_str("#define C(x, y...) x, ## y\nC(1, 2)\n").text,
            "1, 2"
        );
        // A space between the name and `...` is allowed.
        assert_eq!(
            preprocess_str("#define F(x, y ...) y\nF(1, 2, 3)\n").text,
            "2, 3"
        );
        // `__VA_ARGS__` is only the variadic operand when the parameter is unnamed.
        assert_eq!(
            preprocess_str("#define H(x, y...) x __VA_ARGS__\nH(1, 2)\n").text,
            "1 __VA_ARGS__"
        );
    }

    #[test]
    fn reports_bad_named_variadic_parameters() {
        for input in ["#define F(y, y...) y\n", "#define F(a..., b) a\n"] {
            let result = preprocess_str(input);
            assert!(!result.diagnostics.is_empty(), "input: {input:?}");
        }
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
    fn accepts_invalid_pastes_in_assembler_mode() {
        // Assembler mode keeps the operands adjacent when their concatenation is not one token.
        // The emitter still separates tokens that would otherwise form a comment.
        for (input, expected) in [
            ("#define BAD(a, b) a ## b\nBAD(+, -)\n", "+-"),
            ("#define BAD(a, b) a ## b\nBAD(/, *)\n", "/ *"),
            (
                r"#define PASTER(x, y) x ## y
#define EVALUATOR(x, y) PASTER(x, y)
#define NAME cp0
#define NODE(name) EVALUATOR(NAME, EVALUATOR(-, name))
NODE(bus)
",
                "cp0-bus",
            ),
        ] {
            let result = preprocess_str(input);
            assert!(result.diagnostics.is_empty(), "input: {input:?}");
            assert_eq!(result.text, expected, "input: {input:?}");
        }
    }

    #[test]
    fn reports_misplaced_pastes() {
        // `##` at either end of a replacement list is an error in assembler mode too.
        for input in [
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
    fn evaluates_macros_in_conditions() {
        let result = preprocess_str("#define FOO BAR\n#define BAR 42\n#if FOO\nfoo\n#endif\n");
        assert_eq!(result.text, "foo");
        assert!(result.diagnostics.is_empty());
    }

    #[test]
    fn stops_recursive_expansion_in_conditions() {
        let result = preprocess_str("#define A A\n#if A\nfoo\n#endif\n");
        assert_eq!(result.text, "");
        assert!(result.diagnostics.is_empty());
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

    /// Preprocesses `root` from an in-memory file set with the default config.
    fn preprocess_files(files: &[(&str, &str)], root: &str) -> PreprocessedRoot {
        preprocess_files_with(files, root, &PreprocessorConfig::default())
    }

    /// Preprocesses `root` from an in-memory file set.
    fn preprocess_files_with(
        files: &[(&str, &str)],
        root: &str,
        config: &PreprocessorConfig,
    ) -> PreprocessedRoot {
        let files: std::collections::HashMap<camino::Utf8PathBuf, String> = files
            .iter()
            .map(|(path, contents)| (camino::Utf8PathBuf::from(path), (*contents).to_owned()))
            .collect();
        preprocess(Utf8Path::new(root), config, |path| files.get(path).cloned())
    }

    #[test]
    fn includes_a_quoted_file() {
        // The included file's macros and text join the parent's output.
        let result = preprocess_files(
            &[
                ("main.dts", "#include \"a.h\"\nroot\n"),
                ("a.h", "#define A 1\nA\n"),
            ],
            "main.dts",
        );

        assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics);
        assert_eq!(result.text, "1\nroot");
        assert_eq!(result.sources.len(), 2);
        assert_eq!(result.includes.len(), 1);
        assert_eq!(result.includes[0].file, result.sources[1].id);
        assert_eq!(result.includes[0].span.file, result.sources[0].id);
    }

    #[test]
    fn includes_an_angle_file_from_the_include_dirs() {
        let mut config = PreprocessorConfig::default();
        config.include_dirs.push(camino::Utf8PathBuf::from("inc"));
        let result = preprocess_files_with(
            &[
                ("main.dts", "#include <a.h>\nroot\n"),
                ("inc/a.h", "header\n"),
            ],
            "main.dts",
            &config,
        );

        assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics);
        assert_eq!(result.text, "header\nroot");
        assert_eq!(result.sources[1].path.as_str(), "inc/a.h");
    }

    #[test]
    fn quoted_includes_search_the_includer_first() {
        let mut config = PreprocessorConfig::default();
        config.include_dirs.push(camino::Utf8PathBuf::from("inc"));
        let result = preprocess_files_with(
            &[
                ("main.dts", "#include \"a.h\"\n"),
                ("a.h", "near\n"),
                ("inc/a.h", "far\n"),
            ],
            "main.dts",
            &config,
        );

        assert_eq!(result.text, "near");
    }

    #[test]
    fn angle_includes_skip_the_includer_directory() {
        let result = preprocess_files(
            &[("main.dts", "#include <a.h>\nroot\n"), ("a.h", "near\n")],
            "main.dts",
        );

        assert_eq!(result.text, "root");
        assert_eq!(result.diagnostics.len(), 1);
        assert_eq!(result.diagnostics[0].severity, Severity::Error);
    }

    #[test]
    fn resolves_nested_includes_from_the_included_file() {
        let result = preprocess_files(
            &[
                ("main.dts", "#include \"sub/a.h\"\n"),
                ("sub/a.h", "#include \"b.h\"\n"),
                ("sub/b.h", "deep\n"),
            ],
            "main.dts",
        );

        assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics);
        assert_eq!(result.text, "deep");
    }

    #[test]
    fn shares_macros_across_include_boundaries() {
        let result = preprocess_files(
            &[
                ("main.dts", "#define A 1\n#include \"a.h\"\nB\n"),
                ("a.h", "A\n#define B 2\n"),
            ],
            "main.dts",
        );

        assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics);
        assert_eq!(result.text, "1\n2");
    }

    #[test]
    fn reports_a_missing_include() {
        let result = preprocess_files(&[("main.dts", "#include \"nope.h\"\nroot\n")], "main.dts");

        assert_eq!(result.text, "root");
        assert_eq!(result.diagnostics.len(), 1);
        assert_eq!(result.diagnostics[0].severity, Severity::Error);
        assert_eq!(
            result.diagnostics[0].span.primary_spans[0].file,
            result.sources[0].id
        );
    }

    #[test]
    fn caps_include_depth() {
        let config = PreprocessorConfig {
            max_include_depth: 2,
            ..PreprocessorConfig::default()
        };
        let result = preprocess_files_with(
            &[
                ("main.dts", "#include \"self.h\"\n"),
                ("self.h", "#include \"self.h\"\n"),
            ],
            "main.dts",
            &config,
        );

        assert_eq!(result.diagnostics.len(), 1);
        assert_eq!(result.diagnostics[0].severity, Severity::Error);
    }

    #[test]
    fn reports_lex_errors_in_included_files() {
        let result = preprocess_files(
            &[
                ("main.dts", "#include \"a.h\"\n"),
                ("a.h", "/* unterminated\n"),
            ],
            "main.dts",
        );

        assert_eq!(result.diagnostics.len(), 1);
        assert_eq!(result.diagnostics[0].span.primary_spans[0].file.0, 1);
    }

    #[test]
    fn skips_includes_in_inactive_branches() {
        let result = preprocess_files(
            &[("main.dts", "#if 0\n#include \"missing.h\"\n#endif\nroot\n")],
            "main.dts",
        );

        assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics);
        assert_eq!(result.text, "root");
    }

    #[test]
    fn maps_included_text_to_its_file() {
        let result = preprocess_files(
            &[("main.dts", "#include \"a.h\"\n"), ("a.h", "header\n")],
            "main.dts",
        );

        assert_eq!(result.text, "header");
        let span = result.resolve(TextRange::new(0, 6));
        assert_eq!(span.primary_spans.len(), 1);
        assert_eq!(span.primary_spans[0].file, result.sources[1].id);
        assert_eq!(span.primary_spans[0].text_range, TextRange::new(0, 6));
    }

    #[test]
    fn keeps_conditionals_within_their_file() {
        let result = preprocess_files(
            &[
                ("main.dts", "#include \"open.h\"\n#endif\n"),
                ("open.h", "#if 1\nx\n"),
            ],
            "main.dts",
        );

        assert_eq!(result.text, "x");
        assert_eq!(result.diagnostics.len(), 2);
        let files: Vec<_> = result
            .diagnostics
            .iter()
            .map(|diagnostic| diagnostic.span.primary_spans[0].file)
            .collect();
        assert!(files.contains(&result.sources[0].id));
        assert!(files.contains(&result.sources[1].id));
    }

    #[test]
    fn include_guards_allow_repeated_includes() {
        let result = preprocess_files(
            &[
                ("main.dts", "#include \"a.h\"\n#include \"a.h\"\n"),
                ("a.h", "#ifndef A_H\n#define A_H\nonce\n#endif\n"),
            ],
            "main.dts",
        );

        assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics);
        assert_eq!(result.text, "once");
    }

    #[test]
    fn includes_a_computed_quoted_name() {
        let result = preprocess_files(
            &[
                ("main.dts", "#define H \"a.h\"\n#include H\n"),
                ("a.h", "header\n"),
            ],
            "main.dts",
        );

        assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics);
        assert_eq!(result.text, "header");
    }

    #[test]
    fn includes_a_computed_angle_name() {
        let config = PreprocessorConfig {
            include_dirs: vec![camino::Utf8PathBuf::from("inc")],
            ..PreprocessorConfig::default()
        };
        let result = preprocess_files_with(
            &[
                ("main.dts", "#define H <a.h>\n#include H\n"),
                ("inc/a.h", "header\n"),
            ],
            "main.dts",
            &config,
        );

        assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics);
        assert_eq!(result.text, "header");
    }

    #[test]
    fn includes_a_stringified_header_name() {
        let result = preprocess_files(
            &[
                ("main.dts", "#define S(x) #x\n#include S(a.h)\n"),
                ("a.h", "header\n"),
            ],
            "main.dts",
        );

        assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics);
        assert_eq!(result.text, "header");
    }

    #[test]
    fn resolves_nested_computed_names() {
        let result = preprocess_files(
            &[
                ("main.dts", "#define H \"a.h\"\n#define I H\n#include I\n"),
                ("a.h", "header\n"),
            ],
            "main.dts",
        );

        assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics);
        assert_eq!(result.text, "header");
    }

    #[test]
    fn reports_a_computed_name_that_is_not_a_header() {
        let result = preprocess_files(
            &[("main.dts", "#define H a.h\n#include H\nroot\n")],
            "main.dts",
        );

        assert_eq!(result.text, "root");
        assert_eq!(result.diagnostics.len(), 1);
        assert_eq!(result.diagnostics[0].severity, Severity::Error);
    }

    #[test]
    fn preserves_whitespace_only_in_raw_header_names() {
        // gcc and clang keep the exact text of a raw `<...>` name, but re-render macro-expanded
        // names with one space per token boundary.
        let config = PreprocessorConfig {
            include_dirs: vec![camino::Utf8PathBuf::from("inc")],
            ..PreprocessorConfig::default()
        };
        let result = preprocess_files_with(
            &[
                (
                    "main.dts",
                    "#define H <a  b.h>\n#include <a  b.h>\n#include H\n",
                ),
                ("inc/a  b.h", "two\n"),
                ("inc/a b.h", "one\n"),
            ],
            "main.dts",
            &config,
        );

        assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics);
        assert_eq!(result.text, "two\none");
    }

    #[test]
    fn warns_on_extra_tokens_after_a_header_name() {
        let result = preprocess_files(
            &[
                ("main.dts", "#include \"a.h\" extra\n"),
                ("a.h", "header\n"),
            ],
            "main.dts",
        );

        assert_eq!(result.text, "header");
        assert_eq!(result.diagnostics.len(), 1);
        assert_eq!(result.diagnostics[0].severity, Severity::Warn);
    }

    #[test]
    fn splices_dts_includes() {
        let result = preprocess_files(
            &[
                ("main.dts", "/dts-v1/;\n/include/ \"a.dtsi\"\n"),
                ("a.dtsi", "included;\n"),
            ],
            "main.dts",
        );

        assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics);
        assert_eq!(result.text, "/dts-v1/;\nincluded;\n");
        assert_eq!(result.sources.len(), 2);
        assert_eq!(result.includes.len(), 1);
        assert_eq!(result.includes[0].file, result.sources[1].id);
        assert_eq!(result.includes[0].span.file, result.sources[0].id);
    }

    #[test]
    fn resolves_nested_dts_includes_from_the_included_file() {
        let result = preprocess_files(
            &[
                ("main.dts", "/include/ \"sub/a.dtsi\"\n"),
                ("sub/a.dtsi", "/include/ \"b.dtsi\"\n"),
                ("sub/b.dtsi", "deep;\n"),
            ],
            "main.dts",
        );

        assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics);
        assert!(result.text.contains("deep;"), "{:?}", result.text);
        assert_eq!(result.sources.len(), 3);
        assert_eq!(result.includes.len(), 2);
    }

    #[test]
    fn does_not_preprocess_dts_included_files() {
        let result = preprocess_files(
            &[
                ("main.dts", "/include/ \"a.dtsi\"\n"),
                ("a.dtsi", "#define A 1\nA\n"),
            ],
            "main.dts",
        );

        assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics);
        assert_eq!(result.text, "#define A 1\nA\n");
    }

    #[test]
    fn splices_macro_produced_dts_includes() {
        let result = preprocess_files(
            &[
                ("main.dts", "#define I /include/\nI \"a.dtsi\"\n"),
                ("a.dtsi", "included;\n"),
            ],
            "main.dts",
        );

        assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics);
        assert_eq!(result.text, "included;\n");
        assert_eq!(result.includes.len(), 1);
    }

    #[test]
    fn reports_a_missing_dts_include() {
        let result = preprocess_files(
            &[("main.dts", "/include/ \"nope.dtsi\"\nroot\n")],
            "main.dts",
        );

        assert_eq!(result.diagnostics.len(), 1);
        assert_eq!(result.diagnostics[0].severity, Severity::Error);
        assert_eq!(
            result.diagnostics[0].span.primary_spans[0].file,
            result.sources[0].id
        );
        assert!(result.text.contains("root"), "{:?}", result.text);
        assert!(!result.text.contains("/include/"), "{:?}", result.text);
    }

    #[test]
    fn caps_dts_include_depth() {
        let config = PreprocessorConfig {
            max_include_depth: 2,
            ..PreprocessorConfig::default()
        };
        let result = preprocess_files_with(
            &[
                ("main.dts", "/include/ \"self.dtsi\"\n"),
                ("self.dtsi", "/include/ \"self.dtsi\"\n"),
            ],
            "main.dts",
            &config,
        );

        assert_eq!(result.diagnostics.len(), 1);
        assert_eq!(result.diagnostics[0].severity, Severity::Error);
    }

    #[test]
    fn maps_dts_included_text_to_its_file() {
        let result = preprocess_files(
            &[
                ("main.dts", "/include/ \"a.dtsi\"\n"),
                ("a.dtsi", "included;\n"),
            ],
            "main.dts",
        );

        let start = result.text.find("included;").expect("included text");
        let span = result.resolve(TextRange::new(start, start + "included;".len()));
        assert_eq!(span.primary_spans[0].file, result.sources[1].id);
        assert_eq!(span.primary_spans[0].text_range, TextRange::new(0, 9));
    }

    #[test]
    fn ignores_dts_include_like_text_in_strings() {
        let result = preprocess_files(
            &[("main.dts", "/dts-v1/;\n/ { x = \"/include/\"; };\n")],
            "main.dts",
        );

        assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics);
        assert!(result.text.contains("/include/"), "{:?}", result.text);
        assert!(result.includes.is_empty());
    }

    #[test]
    fn reports_a_dts_include_without_a_string() {
        let result = preprocess_files(&[("main.dts", "/include/ foo\n")], "main.dts");

        assert_eq!(result.diagnostics.len(), 1);
        assert_eq!(result.diagnostics[0].severity, Severity::Error);
    }

    #[test]
    fn labels_macro_expansions() {
        let source = "#define A 1\nA\n";
        let result = preprocess_str(source);
        assert_eq!(result.text, "1");

        let invocation = source.rfind('A').expect("invocation");
        let span = result.resolve(TextRange::new(0, 1));

        assert!(span.span_labels.iter().any(|label| {
            label.msg.contains("in this expansion of `A`")
                && label.span.text_range == TextRange::new(invocation, invocation + 1)
        }));
        let name = source.find('A').expect("macro name");
        assert!(span.span_labels.iter().any(|label| {
            label.msg.contains("`A` defined here")
                && label.span.text_range == TextRange::new(name, name + 1)
        }));
    }

    #[test]
    fn labels_macro_arguments() {
        let result = preprocess_str("#define F(x) x\nF(abc)\n");
        assert_eq!(result.text, "abc");

        let span = result.resolve(TextRange::new(0, 3));
        assert!(
            span.span_labels
                .iter()
                .any(|label| label.msg == "argument `x` of `F`")
        );
    }

    #[test]
    fn labels_pasted_tokens() {
        let source = "#define CAT(a, b) a ## b\nCAT(foo, bar)\n";
        let result = preprocess_str(source);
        assert_eq!(result.text, "foobar");

        let foo = source.find("foo").expect("left operand");
        let bar = source.find("bar").expect("right operand");
        let operator = source.find("##").expect("paste operator");
        let span = result.resolve(TextRange::new(0, 6));

        assert!(
            span.primary_spans
                .iter()
                .any(|span| span.text_range == TextRange::new(foo, foo + 3))
        );
        assert!(
            span.primary_spans
                .iter()
                .any(|span| span.text_range == TextRange::new(bar, bar + 3))
        );
        assert!(span.span_labels.iter().any(|label| {
            label.msg.contains("pasted here with `##`")
                && label.span.text_range == TextRange::new(operator, operator + 2)
        }));
        let argument_labels: Vec<_> = span
            .span_labels
            .iter()
            .filter(|label| label.msg.starts_with("argument "))
            .collect();
        assert_eq!(argument_labels.len(), 2);
        assert!(argument_labels.iter().any(|label| {
            label.msg == "argument `a` of `CAT`"
                && label.span.text_range == TextRange::new(foo, foo + 3)
        }));
        assert!(argument_labels.iter().any(|label| {
            label.msg == "argument `b` of `CAT`"
                && label.span.text_range == TextRange::new(bar, bar + 3)
        }));
    }

    #[test]
    fn labels_stringified_tokens() {
        let source = "#define S(x) #x\nS(abc)\n";
        let result = preprocess_str(source);
        assert_eq!(result.text, "\"abc\"");

        let argument = source.find("abc").expect("argument");
        let operator = source.find("#x").expect("stringify operator");
        let span = result.resolve(TextRange::new(0, 5));

        assert!(
            span.primary_spans
                .iter()
                .any(|span| span.text_range == TextRange::new(argument, argument + 3))
        );
        assert!(span.span_labels.iter().any(|label| {
            label.msg.contains("stringified here")
                && label.span.text_range == TextRange::new(operator, operator + 1)
        }));
        assert!(
            span.span_labels
                .iter()
                .any(|label| label.msg == "argument `x` of `S`")
        );
    }

    #[test]
    fn labels_nested_expansions() {
        let result = preprocess_str("#define A B\n#define B 1\nA\n");
        assert_eq!(result.text, "1");

        let span = result.resolve(TextRange::new(0, 1));
        assert!(
            span.span_labels
                .iter()
                .any(|label| label.msg.contains("in this expansion of `A`"))
        );
        assert!(
            span.span_labels
                .iter()
                .any(|label| label.msg.contains("in this expansion of `B`"))
        );
    }

    #[test]
    fn labels_prescanned_arguments() {
        let result = preprocess_str("#define A 1\n#define F(x) x\nF(A)\n");
        assert_eq!(result.text, "1");

        let span = result.resolve(TextRange::new(0, 1));
        assert!(
            span.span_labels
                .iter()
                .any(|label| label.msg.contains("in this expansion of `A`"))
        );
        assert!(
            span.span_labels
                .iter()
                .any(|label| label.msg.contains("in this expansion of `F`"))
        );
        assert!(
            span.span_labels
                .iter()
                .any(|label| label.msg == "argument `x` of `F`")
        );
    }
}
