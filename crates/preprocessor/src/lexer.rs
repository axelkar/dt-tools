//! C preprocessing lexer.

use dt_tools_diagnostic::text_range::TextRange;
use logos::Logos;

use crate::{
    cleaner::{Checkpoint, splice_lines},
    files::SourceId,
    token::PpToken,
};

/// An error detected while cleaning or tokenizing a file.
#[derive(
    thiserror::Error, Debug, Clone, Copy, PartialEq, Eq, Default, displaydoc::Display, Hash,
)]
pub enum LexError {
    /// unterminated block comment
    UnterminatedBlockComment,
    /// unexpected input
    #[default]
    Unknown,
}

/// A [`LexError`] with the range where the error occurred.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LexErrorAt {
    /// Range in the original file.
    pub orig_range: TextRange,
    pub error: LexError,
}

/// The kind of a [`PpToken`].
#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[logos(error = LexError)]
#[logos(skip r"[ \t\r\n\x0b\x0c]+")]
pub enum PpTokenKind {
    /// An identifier.
    #[regex(r"[A-Za-z_][A-Za-z0-9_]*")]
    Ident,

    /// A preprocessing number (`12abc`, `1.5e+3`, ...).
    #[regex(r"\.?[0-9]([0-9A-Za-z_.]|[eEpP][+-])*")]
    Number,

    /// A string literal.
    #[token("\"", callback = lex_string)]
    String,

    /// A character literal.
    #[token("'", callback = lex_char)]
    Char,

    /// A `//` comment. Not returned as a token by [`lex`].
    #[regex(r"//[^\n\r]*?")]
    LineComment,

    /// A `/* */` comment. Not returned as a token by [`lex`].
    #[token("/*", callback = lex_block_comment)]
    BlockComment,

    /// A punctuator.
    #[token("<<=")]
    #[token(">>=")]
    #[token("...")]
    #[token("%:%:")]
    #[token("->")]
    #[token("++")]
    #[token("--")]
    #[token("<<")]
    #[token(">>")]
    #[token("<=")]
    #[token(">=")]
    #[token("==")]
    #[token("!=")]
    #[token("&&")]
    #[token("||")]
    #[token("##")]
    #[token("+=")]
    #[token("-=")]
    #[token("*=")]
    #[token("/=")]
    #[token("%=")]
    #[token("&=")]
    #[token("|=")]
    #[token("^=")]
    #[token("<:")]
    #[token(":>")]
    #[token("<%")]
    #[token("%>")]
    #[token("%:")]
    #[token("!")]
    #[token("#")]
    #[token("%")]
    #[token("&")]
    #[token("(")]
    #[token(")")]
    #[token("*")]
    #[token("+")]
    #[token(",")]
    #[token("-")]
    #[token(".")]
    #[token("/")]
    #[token(":")]
    #[token(";")]
    #[token("<")]
    #[token("=")]
    #[token(">")]
    #[token("?")]
    #[token("[")]
    #[token("]")]
    #[token("^")]
    #[token("{")]
    #[token("|")]
    #[token("}")]
    #[token("~")]
    Punct,

    /// Any other characters, grouped into one token per run.
    ///
    /// The class holds exactly the characters no other pattern starts with, so runs never swallow
    /// a recognized token. A missing starting character here fails logos' ambiguity check.
    #[regex(r#"[^ \t\r\n\x0b\x0cA-Za-z0-9_."'!#%&()*+,\-/:;<=>?\[\]^{|}~]+"#)]
    Other,
}

/// Scans a quoted literal up to its closing `terminator`, or to the end of the line.
///
/// An unterminated literal is not an error, matching cpp in assembler mode.
fn lex_quoted(lex: &mut logos::Lexer<PpTokenKind>, terminator: char) {
    let remainder = lex.remainder();
    let mut escaped = false;
    let mut total_len = 0;

    for c in remainder.chars() {
        if c == '\n' || c == '\r' {
            break;
        }

        total_len += c.len_utf8();

        if c == '\\' {
            escaped = !escaped;
            continue;
        }

        if c == terminator && !escaped {
            break;
        }

        escaped = false;
    }

    lex.bump(total_len);
}

fn lex_string(lex: &mut logos::Lexer<PpTokenKind>) {
    lex_quoted(lex, '"');
}

fn lex_char(lex: &mut logos::Lexer<PpTokenKind>) {
    lex_quoted(lex, '\'');
}

/// Scans a block comment up to and including its closing `*/`.
fn lex_block_comment(lex: &mut logos::Lexer<PpTokenKind>) -> Result<(), LexError> {
    let remainder = lex.remainder();
    let end = remainder.find("*/");
    let len = remainder.len();

    if let Some(end) = end {
        lex.bump(end + 2);
        Ok(())
    } else {
        lex.bump(len);
        Err(LexError::UnterminatedBlockComment)
    }
}

/// The result of cleaning and lexing one file.
#[derive(Debug)]
pub struct LexedFile {
    /// The cleaned text.
    pub cleaned_text: String,
    /// The tokens of the cleaned text, in order.
    pub tokens: Vec<PpToken>,
    /// Lexical errors, sorted by position.
    pub errors: Vec<LexErrorAt>,
    /// Length of the original file in bytes.
    pub orig_len: usize,
    /// Maps cleaned offsets to spliced offsets.
    comment_checkpoints: Vec<Checkpoint>,
    /// Maps spliced offsets to original offsets.
    splice_checkpoints: Vec<Checkpoint>,
}

impl LexedFile {
    /// Maps an offset in the cleaned text back to the original file.
    #[must_use]
    pub fn to_orig_offset(&self, cleaned: usize) -> usize {
        let spliced = Checkpoint::map_offset(&self.comment_checkpoints, cleaned);
        Checkpoint::map_offset(&self.splice_checkpoints, spliced)
    }

    /// Maps a range in the cleaned text back to the original file.
    #[must_use]
    pub fn to_orig_range(&self, cleaned: TextRange) -> TextRange {
        let spliced = Checkpoint::map_range(&self.comment_checkpoints, cleaned);
        Checkpoint::map_range(&self.splice_checkpoints, spliced)
    }
}

/// Lexes `contents`, first removing line continuations, then replacing comments with one space
/// while tokenizing.
///
/// Corresponds to the [C translation phases](https://en.cppreference.com/c/language/translation_phases) 1-3.
///
/// # Panics
///
/// Panic in debug mode when bugs are detected.
#[must_use]
pub fn lex(contents: &str, file: SourceId) -> LexedFile {
    let (spliced, splice_checkpoints) = splice_lines(contents);

    let mut cleaned_text = String::with_capacity(spliced.len());
    let mut tokens: Vec<PpToken> = Vec::new();
    let mut comment_checkpoints = Vec::new();
    let mut errors = Vec::new();
    let mut prev_spliced_end = 0;
    let mut prev_token_cleaned_end: Option<usize> = None;
    let mut lexer = PpTokenKind::lexer(&spliced);

    while let Some(result) = lexer.next() {
        let span = lexer.span();
        let orig = Checkpoint::map_range(&splice_checkpoints, TextRange::new(span.start, span.end));

        // Whitespace between tokens is kept as-is.
        cleaned_text.push_str(&spliced[prev_spliced_end..span.start]);

        match result {
            Ok(PpTokenKind::LineComment | PpTokenKind::BlockComment) => {
                // A comment becomes one space, including any newlines inside it. A following
                // token is therefore only at line start if a real newline precedes it.
                cleaned_text.push(' ');
                comment_checkpoints.push(Checkpoint {
                    from: cleaned_text.len(),
                    to: span.end,
                });
            }
            Ok(kind) => {
                let cleaned_start = cleaned_text.len();
                cleaned_text.push_str(lexer.slice());

                let (has_space_before, line_start) = match prev_token_cleaned_end {
                    None => (false, true),
                    Some(end) => (
                        end < cleaned_start,
                        cleaned_text[end..cleaned_start].contains('\n'),
                    ),
                };

                tokens.push(PpToken {
                    file,
                    kind,
                    cleaned: TextRange::new(cleaned_start, cleaned_text.len()),
                    orig,
                    has_space_before,
                    line_start,
                });
                prev_token_cleaned_end = Some(cleaned_text.len());
            }
            Err(error) => {
                if matches!(error, LexError::UnterminatedBlockComment) {
                    // The unterminated comment still becomes one space.
                    cleaned_text.push(' ');
                    comment_checkpoints.push(Checkpoint {
                        from: cleaned_text.len(),
                        to: span.end,
                    });
                } else {
                    // Preserve text that was not lexed as a token.
                    cleaned_text.push_str(lexer.slice());
                }

                errors.push(LexErrorAt {
                    orig_range: orig,
                    error,
                });
            }
        }

        prev_spliced_end = span.end;
    }

    // Whitespace after tokens
    cleaned_text.push_str(&spliced[prev_spliced_end..]);

    // TODO: into_boxed_str?
    cleaned_text.shrink_to_fit();

    errors.sort_by_key(|error| error.orig_range.start);

    #[cfg(debug_assertions)]
    for token in &tokens {
        let orig_text = contents
            .get(token.orig.byte_range())
            .expect("Should be valid TextRange");

        // A token with a backslash may have had a continuation removed, so its original and
        // cleaned text can differ legitimately.
        if !orig_text.contains('\\') {
            assert_eq!(
                contents.get(token.orig.byte_range()),
                Some(token.text(&cleaned_text)),
                "Original and cleaned text should not differ without line continuations. Token: {token:?}",
            );
        }
    }

    LexedFile {
        cleaned_text,
        tokens,
        errors,
        orig_len: contents.len(),
        comment_checkpoints,
        splice_checkpoints,
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Write as _;

    use dt_tools_diagnostic::text_range::TextRange;
    use expect_test::{Expect, expect};

    use super::*;

    /// Returns a readable snapshot of the cleaned text, original text, tokens and errors in `cleaned`.
    fn render(cleaned: &LexedFile, original: &str) -> String {
        let mut output = format!("cleaned: {:?}\n", cleaned.cleaned_text);

        for token in &cleaned.tokens {
            let flags = match (token.has_space_before, token.line_start) {
                (false, false) => "",
                (true, false) => " [space-before]",
                (false, true) => " [line-start]",
                (true, true) => " [space-before, line-start]",
            };

            let cleaned_part = token.text(&cleaned.cleaned_text);
            let orig_part = &original[token.orig.byte_range()];

            if cleaned_part == orig_part {
                writeln!(&mut output, "{:?} {:?}{flags}", token.kind, cleaned_part).unwrap();
            } else {
                writeln!(
                    &mut output,
                    "{:?} {:?} (from orig {:?}){flags}",
                    token.kind, cleaned_part, orig_part,
                )
                .unwrap();
            }
        }

        for error in &cleaned.errors {
            writeln!(
                &mut output,
                "error: {:?} at {} ({:?})",
                error.error,
                error.orig_range,
                &original[error.orig_range.byte_range()],
            )
            .unwrap();
        }

        output
    }

    /// Asserts that lexing `input` matches `expected` and returns the result.
    #[track_caller]
    #[expect(clippy::needless_pass_by_value, reason = "ergonomics")]
    fn check(input: &str, expected: Expect) -> LexedFile {
        let cleaned = lex(input, SourceId(0));
        expected.assert_eq(&render(&cleaned, input));
        cleaned
    }

    #[test]
    fn dts_property() {
        check(
            "foo = <1>;",
            expect![[r#"
                cleaned: "foo = <1>;"
                Ident "foo" [line-start]
                Punct "=" [space-before]
                Punct "<" [space-before]
                Number "1"
                Punct ">"
                Punct ";"
            "#]],
        );
    }

    #[test]
    fn line_comment() {
        check(
            "foo//x\nbar",
            expect![[r#"
                cleaned: "foo \nbar"
                Ident "foo" [line-start]
                Ident "bar" [space-before, line-start]
            "#]],
        );
    }

    #[test]
    fn multiline_block_comment_is_one_space() {
        check(
            "a/* x\ny */b",
            expect![[r#"
                cleaned: "a b"
                Ident "a" [line-start]
                Ident "b" [space-before]
            "#]],
        );
    }

    #[test]
    fn line_comment_can_be_continued() {
        check(
            "foo//x\\\nbar\nbaz",
            expect![[r#"
                cleaned: "foo \nbaz"
                Ident "foo" [line-start]
                Ident "baz" [space-before, line-start]
            "#]],
        );
    }

    #[test]
    fn splice() {
        let cleaned = check(
            "FOO\\\nBAR",
            expect![[r#"
                cleaned: "FOOBAR"
                Ident "FOOBAR" (from orig "FOO\\\nBAR") [line-start]
            "#]],
        );
        assert_eq!(
            cleaned.to_orig_range(TextRange::new(0, 6)),
            TextRange::new(0, 8)
        );
    }

    #[test]
    fn double_backslash_splice() {
        check(
            "a\\\\\n\nb",
            expect![[r#"
                cleaned: "a\\\nb"
                Ident "a" [line-start]
                Other "\\"
                Ident "b" [space-before, line-start]
            "#]],
        );
    }

    #[test]
    fn number_constants() {
        for text in ["12abc", "1.5e+3", "0xE+foo", "1e+", ".5", "0", "1..E+3.foo"] {
            let expected = format!("cleaned: {text:?}\nNumber {text:?} [line-start]\n");
            assert_eq!(
                render(&lex(text, SourceId(0)), text),
                expected,
                "input: {text}"
            );
        }
    }

    #[test]
    fn identifiers_stop_at_dot_dollar_and_dash() {
        check(
            "foo.bar",
            expect![[r#"
                cleaned: "foo.bar"
                Ident "foo" [line-start]
                Punct "."
                Ident "bar"
            "#]],
        );
        check(
            "foo$bar",
            expect![[r#"
                cleaned: "foo$bar"
                Ident "foo" [line-start]
                Other "$"
                Ident "bar"
            "#]],
        );
        check(
            "foo-bar",
            expect![[r#"
                cleaned: "foo-bar"
                Ident "foo" [line-start]
                Punct "-"
                Ident "bar"
            "#]],
        );
    }

    #[test]
    fn other_characters_group_into_runs() {
        check(
            "a$@\\$b",
            expect![[r#"
                cleaned: "a$@\\$b"
                Ident "a" [line-start]
                Other "$@\\$"
                Ident "b"
            "#]],
        );
    }

    #[test]
    fn char_constants() {
        for text in ["'a'", "'ab'", "'a b'", "'\\''", "'a", "''"] {
            let expected = format!("cleaned: {text:?}\nChar {text:?} [line-start]\n");
            assert_eq!(
                render(&lex(text, SourceId(0)), text),
                expected,
                "input: {text}"
            );
        }
    }

    #[test]
    fn char_constant_ends_at_newline() {
        check(
            "'a\nb",
            expect![[r#"
                cleaned: "'a\nb"
                Char "'a" [line-start]
                Ident "b" [space-before, line-start]
            "#]],
        );
    }

    #[test]
    fn string_literals() {
        check(
            r#""a\"b""#,
            expect![[r#"
                cleaned: "\"a\\\"b\""
                String "\"a\\\"b\"" [line-start]
            "#]],
        );
    }

    #[test]
    fn comment_starters_in_literals_are_kept() {
        check(
            r#""/*" "//" '/*' '//'"#,
            expect![[r#"
                cleaned: "\"/*\" \"//\" '/*' '//'"
                String "\"/*\"" [line-start]
                String "\"//\"" [space-before]
                Char "'/*'" [space-before]
                Char "'//'" [space-before]
            "#]],
        );
    }

    #[test]
    fn splice_inside_string() {
        check(
            "\"a\\\nb\"",
            expect![[r#"
                cleaned: "\"ab\""
                String "\"ab\"" (from orig "\"a\\\nb\"") [line-start]
            "#]],
        );
    }

    #[test]
    fn splice_after_escape_in_string() {
        check(
            "\"a\\\\\nb\"",
            expect![[r#"
                cleaned: "\"a\\b\""
                String "\"a\\b\"" (from orig "\"a\\\\\nb\"") [line-start]
            "#]],
        );
    }

    #[test]
    fn splice_inside_char_constant() {
        check(
            "'a\\\n'",
            expect![[r#"
                cleaned: "'a'"
                Char "'a'" (from orig "'a\\\n'") [line-start]
            "#]],
        );
    }

    #[test]
    fn unterminated_string() {
        check(
            "\"abc\nx",
            expect![[r#"
                cleaned: "\"abc\nx"
                String "\"abc" [line-start]
                Ident "x" [space-before, line-start]
            "#]],
        );
    }

    #[test]
    fn unterminated_block_comment() {
        check(
            "a /* x",
            expect![[r#"
                cleaned: "a  "
                Ident "a" [line-start]
                error: UnterminatedBlockComment at 2..6 ("/* x")
            "#]],
        );
    }

    #[test]
    fn punctuators_are_maximal_munch() {
        check(
            "<<= ... %:%: ## ->",
            expect![[r###"
                cleaned: "<<= ... %:%: ## ->"
                Punct "<<=" [line-start]
                Punct "..." [space-before]
                Punct "%:%:" [space-before]
                Punct "##" [space-before]
                Punct "->" [space-before]
            "###]],
        );
    }

    #[test]
    fn double_colon_is_two_punctuators() {
        // `::` is C++ punctuation, not a C punctuator, so it must not lex as one token.
        check(
            "a::b",
            expect![[r#"
                cleaned: "a::b"
                Ident "a" [line-start]
                Punct ":"
                Punct ":"
                Ident "b"
            "#]],
        );
    }

    #[test]
    fn line_start_after_comments() {
        // The `#` is the first token, so it starts a line even after a multi-line comment.
        check(
            "/* x\n*/#define FOO 1\n",
            expect![[r##"
                cleaned: " #define FOO 1\n"
                Punct "#" [line-start]
                Ident "define"
                Ident "FOO" [space-before]
                Number "1" [space-before]
            "##]],
        );
        check(
            "/* x */#define FOO 1\n",
            expect![[r##"
                cleaned: " #define FOO 1\n"
                Punct "#" [line-start]
                Ident "define"
                Ident "FOO" [space-before]
                Number "1" [space-before]
            "##]],
        );
        check(
            " /* x */#define FOO 1\n",
            expect![[r##"
                cleaned: "  #define FOO 1\n"
                Punct "#" [line-start]
                Ident "define"
                Ident "FOO" [space-before]
                Number "1" [space-before]
            "##]],
        );

        // A token precedes the comment and the comment is one space, so the `#` does not.
        check(
            "a/* x\n*/#define FOO 1\n",
            expect![[r##"
                cleaned: "a #define FOO 1\n"
                Ident "a" [line-start]
                Punct "#" [space-before]
                Ident "define"
                Ident "FOO" [space-before]
                Number "1" [space-before]
            "##]],
        );
    }

    #[test]
    fn splice_allows_whitespace_before_newline() {
        check(
            "foo\\  \nbar",
            expect![[r#"
                cleaned: "foobar"
                Ident "foobar" (from orig "foo\\  \nbar") [line-start]
            "#]],
        );
        check(
            "foo\\\t\r\nbar",
            expect![[r#"
                cleaned: "foobar"
                Ident "foobar" (from orig "foo\\\t\r\nbar") [line-start]
            "#]],
        );
    }

    #[test]
    fn block_comment_formed_by_splice() {
        check(
            "a/\\\n* c */b",
            expect![[r#"
                cleaned: "a b"
                Ident "a" [line-start]
                Ident "b" [space-before]
            "#]],
        );
    }

    #[test]
    fn line_comment_formed_by_splice() {
        check(
            "a/\\\n/ c\nb",
            expect![[r#"
                cleaned: "a \nb"
                Ident "a" [line-start]
                Ident "b" [space-before, line-start]
            "#]],
        );
    }

    #[test]
    fn line_endings_are_normalized() {
        check(
            "a\r\nb\rc\nd",
            expect![[r#"
                cleaned: "a\nb\nc\nd"
                Ident "a" [line-start]
                Ident "b" [space-before, line-start]
                Ident "c" [space-before, line-start]
                Ident "d" [space-before, line-start]
            "#]],
        );
    }

    #[test]
    fn bom_is_stripped() {
        let cleaned = check(
            "\u{feff}a",
            expect![[r#"
                cleaned: "a"
                Ident "a" [line-start]
            "#]],
        );
        assert_eq!(cleaned.tokens[0].orig, TextRange::new(3, 4));
        assert_eq!(cleaned.to_orig_offset(0), 3);
    }

    #[test]
    fn provenance_through_splice_and_comment() {
        let cleaned = check(
            "a\\\nb/*c*/d",
            expect![[r#"
                cleaned: "ab d"
                Ident "ab" (from orig "a\\\nb") [line-start]
                Ident "d" [space-before]
            "#]],
        );
        assert_eq!(cleaned.to_orig_offset(3), 9);
    }

    #[test]
    fn map_range_example() {
        check(
            "a/\\\n;",
            expect![[r#"
                cleaned: "a/;"
                Ident "a" [line-start]
                Punct "/"
                Punct ";"
            "#]],
        );
    }
}
