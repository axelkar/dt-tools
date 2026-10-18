//! Expanded text emission.

use dt_tools_diagnostic::text_range::TextRange;
use logos::Logos;

use crate::{
    files::SourceId,
    lexer::PpTokenKind,
    origin::{Origin, ProvenanceMap},
    token::PpToken,
};

/// The longest C punctuator is four characters, so a merge can span at most four one-character
/// tokens.
const MAX_RUN: usize = 4;

/// Emits `tokens` as text, preserving source spacing where it matters.
///
/// One space is written where the source had one. Otherwise tokens stay adjacent unless they would
/// merge into different tokens when re-lexed. Separators are recorded as [`Origin::Generated`].
// TODO: take the file from each token once expansion produces tokens from other files.
#[must_use]
pub(crate) fn emit(tokens: &[PpToken], cleaned: &str, file: SourceId) -> (String, ProvenanceMap) {
    let mut out = String::with_capacity(cleaned.len());
    let mut map = ProvenanceMap::new();
    let mut run: Vec<PpToken> = Vec::new();

    let mut push = |str, origin| {
        let start = out.len();
        out.push_str(str);
        map.push(TextRange::new(start, out.len()), origin);
    };

    for (index, token) in tokens.iter().enumerate() {
        // Emit separator before token
        let is_first = index == 0;
        if !is_first {
            let separator = if token.line_start {
                Some("\n")
            } else if token.has_space_before || !can_follow(&run, token, cleaned) {
                Some(" ")
            } else {
                None
            };

            if let Some(separator) = separator {
                push(separator, Origin::Generated);
                run.clear();
            }
        }

        // Emit token
        push(
            token.text(cleaned),
            Origin::File {
                file,
                range: token.orig,
            },
        );

        if run.len() == MAX_RUN {
            run.remove(0);
        }
        run.push(*token);
    }

    (out, map)
}

/// Returns true if `next` can directly follow `run` without a separator.
fn can_follow(run: &[PpToken], next: &PpToken, cleaned: &str) -> bool {
    if run.is_empty() {
        return true;
    }

    let expected: Vec<_> = run.iter().chain([next]).collect();
    let mut text = String::new();
    for token in &expected {
        text.push_str(token.text(cleaned));
    }

    let mut lexer = PpTokenKind::lexer(&text);
    for token in &expected {
        let Some(result) = lexer.next() else {
            return false;
        };
        let Ok(kind) = result else {
            return false;
        };
        if kind != token.kind || lexer.slice() != token.text(cleaned) {
            return false;
        }
    }

    lexer.next().is_none()
}

#[cfg(test)]
mod tests {
    use dt_tools_diagnostic::text_range::TextRange;
    use logos::Logos as _;

    use super::emit;
    use crate::{
        files::SourceId,
        lexer::{PpTokenKind, lex},
        token::PpToken,
    };

    /// Builds a cleaned text and its tokens from `(kind, spelling, has_space_before, line_start)`.
    fn tokens(specs: &[(PpTokenKind, &str, bool, bool)]) -> (String, Vec<PpToken>) {
        let mut cleaned = String::new();
        let mut tokens = Vec::new();

        for &(kind, spelling, has_space_before, line_start) in specs {
            let start = cleaned.len();
            cleaned.push_str(spelling);
            tokens.push(PpToken {
                kind,
                cleaned: TextRange::new(start, cleaned.len()),
                orig: TextRange::new(start, cleaned.len()),
                has_space_before,
                line_start,
            });
        }

        (cleaned, tokens)
    }

    fn emitted(specs: &[(PpTokenKind, &str, bool, bool)]) -> String {
        let (cleaned, tokens) = tokens(specs);
        emit(&tokens, &cleaned, SourceId(0)).0
    }

    #[test]
    fn preserves_source_spacing() {
        assert_eq!(
            emitted(&[
                (PpTokenKind::Ident, "foo", false, true),
                (PpTokenKind::Punct, "=", true, false),
                (PpTokenKind::Punct, "<", true, false),
                (PpTokenKind::Number, "1", false, false),
                (PpTokenKind::Punct, ">", false, false),
                (PpTokenKind::Punct, ";", false, false),
            ]),
            "foo = <1>;"
        );
    }

    #[test]
    fn emits_line_starts() {
        assert_eq!(
            emitted(&[
                (PpTokenKind::Ident, "a", false, true),
                (PpTokenKind::Punct, ";", false, false),
                (PpTokenKind::Ident, "b", false, true),
                (PpTokenKind::Punct, ";", false, false),
            ]),
            "a;\nb;"
        );
    }

    #[test]
    fn separates_tokens_that_would_merge() {
        for (left_kind, left, right_kind, right, expected) in [
            (PpTokenKind::Punct, "+", PpTokenKind::Punct, "+", "+ +"),
            (PpTokenKind::Punct, "-", PpTokenKind::Punct, ">", "- >"),
            (PpTokenKind::Punct, "/", PpTokenKind::Punct, "*", "/ *"),
            (PpTokenKind::Punct, "/", PpTokenKind::Punct, "/", "/ /"),
            (
                PpTokenKind::Ident,
                "foo",
                PpTokenKind::Ident,
                "bar",
                "foo bar",
            ),
            (PpTokenKind::Number, "1", PpTokenKind::Ident, "abc", "1 abc"),
        ] {
            assert_eq!(
                emitted(&[
                    (left_kind, left, false, true),
                    (right_kind, right, false, false),
                ]),
                expected,
                "{left} {right}"
            );
        }
    }

    #[test]
    fn separates_a_run_that_would_merge() {
        // `...` is one token, but two dots are still two tokens.
        assert_eq!(
            emitted(&[
                (PpTokenKind::Punct, ".", false, true),
                (PpTokenKind::Punct, ".", false, false),
            ]),
            ".."
        );
        // A separator is only needed before the dot that would complete `...`. What matters is
        // that the re-lexed token sequence is preserved.
        let (cleaned, dots) = tokens(&[
            (PpTokenKind::Punct, ".", false, true),
            (PpTokenKind::Punct, ".", false, false),
            (PpTokenKind::Punct, ".", false, false),
        ]);
        assert_eq!(
            raw_tokens(&emit(&dots, &cleaned, SourceId(0)).0),
            vec![
                (PpTokenKind::Punct, "."),
                (PpTokenKind::Punct, "."),
                (PpTokenKind::Punct, "."),
            ]
        );
    }

    #[test]
    fn keeps_required_adjacency() {
        assert_eq!(
            emitted(&[
                (PpTokenKind::Punct, "&", false, true),
                (PpTokenKind::Ident, "gpio1", false, false),
            ]),
            "&gpio1"
        );
        assert_eq!(
            emitted(&[
                (PpTokenKind::Punct, "#", false, true),
                (PpTokenKind::Ident, "size", false, false),
                (PpTokenKind::Punct, "-", false, false),
                (PpTokenKind::Ident, "cells", false, false),
            ]),
            "#size-cells"
        );
    }

    #[test]
    fn relexes_to_the_same_tokens() {
        for input in [
            "foo = <1>;",
            "a::b",
            "#size-cells = <1>;",
            "a/*c*/b",
            "FOO\\\nBAR",
            "\"a b\" 'c'",
            "1.5e+3 + 12abc",
        ] {
            let cleaned = lex(input);
            let (emitted, _) = emit(&cleaned.tokens, &cleaned.cleaned_text, SourceId(0));

            let expected: Vec<_> = cleaned
                .tokens
                .iter()
                .map(|token| (token.kind, token.text(&cleaned.cleaned_text)))
                .collect();
            assert_eq!(
                raw_tokens(&emitted),
                expected,
                "input: {input:?}, emitted: {emitted:?}"
            );
        }
    }

    #[test]
    fn emitted_dts_parses() {
        let input = "/dts-v1/;\n\n/ {\n    #address-cells = <1>;\n    #size-cells = <0>;\n    foo = <&gpio1 1 2>;\n};\n";
        let cleaned = lex(input);
        let (emitted, _) = emit(&cleaned.tokens, &cleaned.cleaned_text, SourceId(0));

        let parse = dt_tools_parser::ast::SourceFile::parse(&emitted);
        assert!(
            parse.lex_errors.is_empty() && parse.errors.is_empty(),
            "emitted: {emitted:?}\nlex errors: {:?}\nparse errors: {:?}",
            parse.lex_errors,
            parse.errors
        );
    }

    #[test]
    fn maps_emitted_tokens_to_their_origins() {
        let (cleaned, tokens) = tokens(&[
            (PpTokenKind::Ident, "foo", false, true),
            (PpTokenKind::Punct, "=", true, false),
            (PpTokenKind::Number, "1", true, false),
        ]);
        let (text, map) = emit(&tokens, &cleaned, SourceId(0));

        assert_eq!(text, "foo = 1");
        let span = map.resolve(TextRange::new(0, 3));
        assert_eq!(span.primary_spans.len(), 1);
        assert_eq!(span.primary_spans[0].text_range, TextRange::new(0, 3));
    }

    fn raw_tokens(text: &str) -> Vec<(PpTokenKind, &str)> {
        let mut lexer = PpTokenKind::lexer(text);
        let mut tokens = Vec::new();
        while let Some(result) = lexer.next() {
            tokens.push((result.expect("emitted text must lex"), lexer.slice()));
        }
        tokens
    }
}
