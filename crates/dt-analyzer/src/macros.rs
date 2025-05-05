//! Preprocessor macro implementation
//!
//! Macro substitution, parameters, ternary operator are evaluated here
// TODO: rewrite the parser to understand UTF-8 and to have clearer code

use std::{iter::Peekable, str::Chars};

use dt_parser::{ast, parser::Entrypoint, TextRange};

use crate::new::stage1::subslice_offset;

// TODO: use this enum
#[expect(dead_code, reason = "not yet implemented")]
pub(crate) enum MacroContext {
    /// In value context or extension name context, after ampersand: `foo = &BAR(baz);`
    Reference,
    /// Node, property or label name, e.g. `FOO(baz): BAR(baz) {};`
    NameDefinition,
    /// Property value context, e.g. `foo = BAR(baz);`
    Value,
    /// Cell value context, e.g. `foo = <BAR(baz)>;`
    Cell,
    /// Item context, e.g. `/ { FOO(bar); };`
    Item,
}
#[expect(dead_code, reason = "not yet implemented")]
impl MacroContext {
    /// Returns the entrypoint for parsing the macro's output.
    fn entrypoint(&self) -> Entrypoint {
        match self {
            Self::Reference => Entrypoint::ReferenceNoamp,
            Self::NameDefinition => Entrypoint::Name,
            Self::Value => Entrypoint::PropValues,
            Self::Cell => Entrypoint::Cells,
            Self::Item => Entrypoint::SourceFile,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum MacroTokenKind {
    Word(String),
    Parameter(usize),
    /// Only difference to `Word` is that this can be pasted next to any token
    WordNoWhitespace(String),
    ConcatOperator,
    StringifyOperator,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct MacroToken {
    kind: MacroTokenKind,
    text_range: TextRange,
}

struct Parser<'params, 'input> {
    /// Current text offset - the index of the character that `peek` returns
    offset: usize,
    current_token_start_offset: usize,
    iter: Peekable<Chars<'input>>,
    current_token: String,
    params: &'params [String],
    /// List of (already) processed tokens
    body_tokens: Vec<MacroToken>,
    dont_prescan_indices: Vec<usize>,
    expect_stringify_param: bool,
    /// If expecting a concat param, the concat token's range is stored here
    expect_concat_param: Option<TextRange>,
}
impl Parser<'_, '_> {
    fn text_range(&self) -> TextRange {
        TextRange {
            start: self.current_token_start_offset,
            end: self.current_token_start_offset + self.current_token.len(),
        }
    }
    /// Flush the data from [`Self::current_token`] into a new `Word` or `Parameter` token
    fn complete_token(&mut self) -> Result<(), MacroDefinitionParseError> {
        if !self.current_token.is_empty() {
            // Check if current token is a parameter
            if let Some(i) = self.params.iter().position(|x| x == &self.current_token) {
                self.push_tok(MacroToken {
                    text_range: self.text_range(),
                    kind: MacroTokenKind::Parameter(i),
                })?;
                self.current_token.clear();
            } else {
                let tok = MacroToken {
                    text_range: self.text_range(),
                    kind: MacroTokenKind::Word(std::mem::take(&mut self.current_token)),
                };
                self.push_tok(tok)?;
            }
        }
        Ok(())
    }
    /// Consumes a character from the input, and returns it. Increments `offset`.
    #[inline]
    fn next(&mut self) -> Option<char> {
        let ch = self.iter.next();
        if let Some(ch) = ch {
            self.offset += ch.len_utf8();
        }
        ch
    }
    /// Peeks ahead and returns the character which will be returned by `next()`.
    #[inline]
    fn peek(&mut self) -> Option<&char> {
        self.iter.peek()
    }
    /// Pushes `ch` to `current_token`, usually to be used by `complete_token()`.
    #[inline]
    fn bump(&mut self, ch: char) {
        if self.current_token.is_empty() {
            // FIXME: assumes thet the character is a single byte
            self.current_token_start_offset = self.offset - 1;
        }
        self.current_token.push(ch);
    }
    #[inline]
    fn push_tok(&mut self, tok: MacroToken) -> Result<(), MacroDefinitionParseError> {
        if self.expect_concat_param.is_some() {
            if let MacroTokenKind::Parameter(i) = tok.kind {
                self.dont_prescan_indices.push(i);
            } else if let MacroTokenKind::Word(_) = tok.kind {
            } else {
                return Err(MacroDefinitionParseError::ConcatInvalidToken {
                    text_range: tok.text_range,
                });
            }
        }
        self.expect_concat_param = None;
        if self.expect_stringify_param {
            if let MacroTokenKind::Parameter(i) = tok.kind {
                self.dont_prescan_indices.push(i);
            } else {
                return Err(MacroDefinitionParseError::StringifyNotAParam {
                    text_range: tok.text_range,
                });
            }
        }
        self.expect_stringify_param = false;

        self.body_tokens.push(tok);
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, thiserror::Error, displaydoc::Display)]
pub enum MacroDefinitionParseError {
    /// Missing name
    MissingName,
    /// '#' not followed by a macro parameter
    StringifyNotAParam { text_range: TextRange },
    /// invalid token for '##'
    ConcatInvalidToken { text_range: TextRange },
    /// '##' cannot be at start
    ConcatAtStart { text_range: TextRange },
    /// '##' cannot be at end
    ConcatAtEnd { text_range: TextRange },
    /// Invalid backslash
    InvalidBackslash { text_range: TextRange },
}
impl MacroDefinitionParseError {
    /// Returns the range of the error, if it exists.
    ///
    /// Note that it's relative to the token's textual contents.
    pub fn text_range(&self) -> Option<TextRange> {
        match self {
            Self::StringifyNotAParam { text_range }
            | Self::ConcatInvalidToken { text_range }
            | Self::ConcatAtStart { text_range }
            | Self::ConcatAtEnd { text_range }
            | Self::InvalidBackslash { text_range } => Some(*text_range),
            Self::MissingName => None,
        }
    }
}

fn parse_params(iter: &mut Peekable<Chars<'_>>, offset: &mut usize) -> Vec<String> {
    let mut params = Vec::new();

    if iter.peek() == Some(&'(') {
        iter.next();
        *offset += 1;
        let mut current_param = String::new();
        for ch in iter.by_ref() {
            *offset += 1;
            match ch {
                ')' => {
                    if !current_param.trim().is_empty() {
                        params.push(current_param.trim().to_string());
                    }
                    break;
                }
                ',' => {
                    if !current_param.trim().is_empty() {
                        params.push(current_param.trim().to_string());
                    }
                    current_param.clear();
                }
                _ => {
                    current_param.push(ch);
                }
            }
        }
    }
    params
}

fn parse_string(p: &mut Parser, ch: char) -> Result<(), MacroDefinitionParseError> {
    p.complete_token()?;
    p.bump(ch);
    let mut escaped = false;
    while let Some(ch) = p.next() {
        p.bump(ch);

        if ch == '\\' {
            escaped = true;
            continue;
        }

        if ch == '"' && !escaped {
            break;
        }
    }

    let text_range = p.text_range();
    let tok = std::mem::take(&mut p.current_token);
    p.push_tok(MacroToken {
        kind: MacroTokenKind::WordNoWhitespace(tok),
        text_range,
    })
}

/// These change meaning when two are pasted together
fn parse_double_special_characters(
    p: &mut Parser,
    ch: char,
) -> Result<(), MacroDefinitionParseError> {
    p.complete_token()?;
    if p.peek() == Some(&ch) {
        p.next();
        p.push_tok(MacroToken {
            text_range: TextRange {
                start: p.offset - 2,
                end: p.offset,
            },
            kind: MacroTokenKind::Word({
                let mut s = String::with_capacity(2);
                s.push(ch);
                s.push(ch);
                s
            }),
        })?;
        p.next();
    } else {
        p.push_tok(MacroToken {
            text_range: TextRange {
                start: p.offset - 1,
                end: p.offset,
            },
            kind: MacroTokenKind::Word(ch.to_string()),
        })?;
    }
    Ok(())
}

fn parse_body(p: &mut Parser) -> Result<(), MacroDefinitionParseError> {
    // TODO: comment support
    while let Some(ch) = p.next() {
        match ch {
            '\\' => {
                p.complete_token()?;
                if let Some('\n') = p.peek() {
                    p.next(); // Consume the newline for line continuation
                } else {
                    p.next();
                    return Err(MacroDefinitionParseError::InvalidBackslash {
                        text_range: TextRange {
                            start: p.offset - 2,
                            end: p.offset,
                        },
                    });
                }
            }
            '"' => parse_string(p, ch)?,
            '#' => {
                p.complete_token()?;
                if let Some('#') = p.peek() {
                    p.next();
                    let concat_text_range = TextRange {
                        start: p.offset - 2,
                        end: p.offset,
                    };

                    match p.body_tokens.last() {
                        Some(MacroToken {
                            kind: MacroTokenKind::Parameter(i),
                            ..
                        }) => {
                            p.dont_prescan_indices.push(*i);
                        }
                        Some(MacroToken {
                            kind: MacroTokenKind::Word(_),
                            ..
                        }) => {}
                        Some(tok) => {
                            return Err(MacroDefinitionParseError::ConcatInvalidToken {
                                text_range: tok.text_range,
                            })
                        }
                        None => {
                            return Err(MacroDefinitionParseError::ConcatAtStart {
                                text_range: concat_text_range,
                            })
                        }
                    }

                    p.push_tok(MacroToken {
                        kind: MacroTokenKind::ConcatOperator,
                        text_range: concat_text_range,
                    })?;
                    p.expect_concat_param = Some(concat_text_range);
                    continue;
                }
                p.push_tok(MacroToken {
                    kind: MacroTokenKind::StringifyOperator,
                    text_range: TextRange {
                        start: p.offset - 1,
                        end: p.offset,
                    },
                })?;

                p.expect_stringify_param = true;
            }
            _ if ch.is_whitespace() => {
                p.complete_token()?;
            }
            // Special characters, break the word
            '<' | '>' | '&' | '|' => parse_double_special_characters(p, ch)?,
            '(' | ')' | '{' | '}' | ';' | '+' | '*' | '/' | '~' | '?' | ':' => {
                p.complete_token()?;
                p.push_tok(MacroToken {
                    text_range: TextRange {
                        start: p.offset - 1,
                        end: p.offset,
                    },
                    kind: MacroTokenKind::WordNoWhitespace(ch.to_string()),
                })?;
            }
            '-' => {
                // Can't be pasted next to words
                p.complete_token()?;
                p.push_tok(MacroToken {
                    text_range: TextRange {
                        start: p.offset - 1,
                        end: p.offset,
                    },
                    kind: MacroTokenKind::Word(ch.to_string()),
                })?;
            }
            _ => {
                p.bump(ch);
            }
        }
    }
    Ok(())
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MacroDefinition {
    /// Macro name
    pub name: String,
    /// Parameter names
    params: Vec<String>,
    /// Parsed macro body tokens, used for substitution
    body_tokens: Vec<MacroToken>,
    /// Parameter indices not to prescan.
    dont_prescan_indices: Vec<usize>,
}
impl MacroDefinition {
    pub(crate) fn parse(input: &str) -> Result<Self, MacroDefinitionParseError> {
        debug_assert!(input.starts_with('#'));
        let s = input
            .get(1..)
            .expect("lexer safe")
            .trim_start_matches([' ', '\t']);

        debug_assert!(s.starts_with("define"));
        let s = s
            .get("define".len()..)
            .expect("lexer safe")
            .trim_start_matches([' ', '\t']);

        let mut iter = s.chars().peekable();
        let mut offset = subslice_offset(input, s).unwrap();

        let mut macro_name = String::new();

        while let Some(&ch) = iter.peek() {
            if ch.is_whitespace() || ch == '(' {
                break;
            }
            macro_name.push(ch);
            iter.next(); // Consume the character
            offset += 1;
        }

        if macro_name.is_empty() {
            return Err(MacroDefinitionParseError::MissingName);
        }

        let params = parse_params(&mut iter, &mut offset);

        let mut p = Parser {
            offset,
            current_token_start_offset: offset,
            iter,
            body_tokens: Vec::new(),
            current_token: String::new(),
            params: &params,
            dont_prescan_indices: Vec::new(),
            expect_stringify_param: false,
            expect_concat_param: None,
        };

        parse_body(&mut p)?;

        if p.current_token.is_empty() {
            if p.expect_stringify_param {
                return Err(MacroDefinitionParseError::StringifyNotAParam {
                    text_range: TextRange {
                        start: p.offset - 1,
                        end: p.offset,
                    },
                });
            }

            if let Some(concat_text_range) = p.expect_concat_param {
                return Err(MacroDefinitionParseError::ConcatAtEnd {
                    text_range: concat_text_range,
                });
            }
        }

        // Eat remaining current_token
        p.complete_token()?;

        p.dont_prescan_indices.dedup();

        Ok(MacroDefinition {
            name: macro_name,
            body_tokens: p.body_tokens,
            dont_prescan_indices: p.dont_prescan_indices,
            params,
        })
    }
    #[expect(clippy::too_many_lines, reason = "Hard to make this shorter")]
    fn substitute(&self, arguments: &[String]) -> (Vec<TextRangeMap>, String) {
        let mut s = String::new();
        let mut iter = self.body_tokens.iter().peekable();
        let mut push_ws = false;
        let mut trmaps = Vec::new();

        while let Some(token) = iter.next() {
            match &token.kind {
                MacroTokenKind::Word(text) => {
                    if push_ws {
                        s.push(' ');
                    }
                    trmaps.push(TextRangeMap {
                        from_offset: s.len(),
                        to: TextRangeMapTo::MacroTextOffset(token.text_range.start),
                    });
                    s.push_str(text);
                }
                MacroTokenKind::Parameter(idx) => {
                    let text = &arguments[*idx];
                    if push_ws {
                        s.push(' ');
                    }
                    trmaps.push(TextRangeMap {
                        from_offset: s.len(),
                        to: TextRangeMapTo::ArgumentIdx(*idx),
                    });
                    s.push_str(text);
                }
                MacroTokenKind::WordNoWhitespace(text) => {
                    trmaps.push(TextRangeMap {
                        from_offset: s.len(),
                        to: TextRangeMapTo::MacroTextOffset(token.text_range.start),
                    });
                    s.push_str(text);
                    push_ws = false;
                    continue;
                }
                MacroTokenKind::ConcatOperator => {
                    let map_to = match iter.next() {
                        Some(MacroToken {
                            kind: MacroTokenKind::Word(text),
                            ..
                        }) => {
                            s.push_str(text);
                            TextRangeMapTo::MacroTextOffset(token.text_range.start)
                        }
                        Some(MacroToken {
                            kind: MacroTokenKind::Parameter(idx),
                            ..
                        }) => {
                            let text = &arguments[*idx];
                            s.push_str(text);
                            TextRangeMapTo::ArgumentIdx(*idx)
                        }
                        Some(MacroToken {
                            kind:
                                MacroTokenKind::WordNoWhitespace(_)
                                | MacroTokenKind::ConcatOperator
                                | MacroTokenKind::StringifyOperator,
                            ..
                        })
                        | None => {
                            // Forbidden by parser
                            unreachable!()
                        }
                    };

                    // The parser assures that there is a token before a concat
                    let prev = trmaps.last_mut().unwrap();

                    // Extend previous concat or make a new text range
                    if let TextRangeMapTo::Concat(concat) = &mut prev.to {
                        concat.push(map_to);
                    } else {
                        prev.to = TextRangeMapTo::Concat(vec![prev.to.clone(), map_to]);
                    }
                }
                MacroTokenKind::StringifyOperator => {
                    let argument_idx = match iter.next() {
                        Some(MacroToken {
                            kind: MacroTokenKind::Parameter(idx),
                            ..
                        }) => {
                            let text = &arguments[*idx];
                            s.push_str(&stringify(text));
                            idx
                        }
                        Some(MacroToken {
                            kind:
                                MacroTokenKind::Word(_)
                                | MacroTokenKind::WordNoWhitespace(_)
                                | MacroTokenKind::ConcatOperator
                                | MacroTokenKind::StringifyOperator,
                            ..
                        })
                        | None => {
                            // Forbidden by parser
                            unreachable!()
                        }
                    };
                    trmaps.push(TextRangeMap {
                        from_offset: s.len(),
                        to: TextRangeMapTo::Stringify {
                            macro_text_offset: token.text_range.start,
                            argument_idx: *argument_idx,
                        },
                    });
                }
            }
            push_ws = true;
        }
        (trmaps, s)
    }
}

/// A mapping from text in the output to some text ranges related to macros.
///
/// Use this to map errors' text ranges in macros.
///
/// Also known as `trmap`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct TextRangeMap {
    /// Map ranges in the output from `from_offset` to the next `TextRangeMap`'s `from_offset`
    pub from_offset: usize,
    /// Where to map it to
    pub to: TextRangeMapTo,
}
impl TextRangeMap {
    #[cfg(test)]
    fn test_fmt(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        write!(f, "{}.. ", self.from_offset)?;
        self.to.test_fmt(f)
    }
}

// TODO: concat into stringification?

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum TextRangeMapTo {
    /// Forward to argument at index
    ArgumentIdx(usize),
    /// Forward to offset in macro definition string
    MacroTextOffset(usize),
    /// Forward to two or more places
    Concat(Vec<TextRangeMapTo>),
    /// Forward to offset inrmacro definition and argument at index
    Stringify {
        macro_text_offset: usize,
        argument_idx: usize,
    },
}
impl TextRangeMapTo {
    #[cfg(test)]
    fn test_fmt(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        match self {
            Self::ArgumentIdx(idx) => write!(f, "arg_{idx}"),
            Self::MacroTextOffset(offset) => write!(f, "macro+{offset}"),
            Self::Concat(vec) => {
                for (i, to) in vec.iter().enumerate() {
                    if i != 0 {
                        f.write_str(" ## ")?;
                    }
                    to.test_fmt(f)?;
                }
                Ok(())
            }
            Self::Stringify {
                macro_text_offset: macro_text,
                argument_idx: argument,
            } => write!(f, "stringify(macro+{macro_text}, arg_{argument})"),
        }
    }
}

// TODO:
// https://en.cppreference.com/w/cpp/preprocessor/replace
// * Scanning keeps track of macros they replaced. If scan finds text matching such macro, it marks it "to be ignored" (all scans will ignore it). This prevents recursion.
// * If scanning found function-like macro, arguments are scanned before put inside replacement-list. Except # and ## operators take argument without scan.
// * After macro was replaced, result text is scanned.
//
// https://gcc.gnu.org/onlinedocs/gcc-3.4.3/cpp/Argument-Prescan.html

pub(crate) fn evaluate_macro(
    ast: Option<&ast::MacroInvocation>,
    def: &MacroDefinition,
) -> Result<(Vec<TextRangeMap>, String), String> {
    let arguments = ast.map(|ast| {
        ast.arguments()
            .map(|arg| {
                arg.green
                    .child_tokens()
                    .map(|tok| tok.text.as_str())
                    .collect()
            })
            .collect::<Vec<String>>()
    });
    if def.params.len() != arguments.as_ref().map_or(0, std::vec::Vec::len) {
        return Err("ERROR: invalid argument length".to_owned());
    }
    let out = def.substitute(arguments.as_ref().map_or(&[], |args| args));
    Ok(out)
}

/// Stringifies `input` like Clang.
fn stringify(input: &str) -> String {
    let mut result = String::new();
    let mut in_string = false;
    let mut in_char = false;
    let mut was_whitespace = false;

    result.push('"');

    let mut iter = input.chars().peekable();

    while let Some(ch) = iter.next() {
        match ch {
            // Handle string literals
            '"' if !in_char => {
                in_string = !in_string;
                result.push('\\');
                result.push('"');
            }
            // Handle character literals
            '\'' if !in_string => {
                in_char = !in_char;
                result.push('\'');
            }
            // Handle escape sequences within string or char literals
            '\\' if in_string || in_char => {
                result.push('\\');
                result.push('\\');
                if let Some(&next_ch) = iter.peek() {
                    if next_ch == '"' {
                        result.push('\\');
                        result.push(iter.next().unwrap());
                    } else if next_ch == '\\' || next_ch == '\'' {
                        result.push(iter.next().unwrap());
                    }
                }
            }
            // Handle whitespace outside of literals
            ch if ch.is_whitespace() && !in_string && !in_char => {
                if !was_whitespace {
                    result.push(' ');
                    was_whitespace = true;
                }
            }
            // Handle normal characters
            _ => {
                result.push(ch);
                was_whitespace = false;
            }
        }
    }

    result.push('"');
    result
}

// TODO: validate number of arguments!
// TODO: vararg macros??

#[cfg(test)]
#[expect(
    clippy::needless_raw_string_hashes,
    reason = "expect-test auto update adds r#"
)]
mod tests {
    use std::fmt::Write;

    use super::*;
    use expect_test::{expect, Expect};
    use pretty_assertions::assert_eq;

    #[test]
    fn test_stringify() {
        #[expect(clippy::needless_pass_by_value, reason = "ergonomics")]
        fn check(actual: &str, expect: Expect) {
            expect.assert_eq(actual);
        }
        check(&stringify(""), expect![[r#""""#]]);
        check(
            &stringify(r#"Hello "world"!\nNewline and \\ backslash."#),
            expect![[r#""Hello \"world\"!\nNewline and \\ backslash.""#]],
        );
        check(
            &stringify(r#""Hello \"world\"!\nNewline and \ backslash.""#),
            expect![[r#""\"Hello \\\"world\\\"!\\nNewline and \\ backslash.\"""#]],
        );
    }

    /// `trmaps_expect`: offset in `output_expect` and then map sources
    /// ```text
    /// 0.. $1 ## $2
    /// ```
    #[expect(clippy::needless_pass_by_value, reason = "ergonomics")]
    fn check_substitute(
        def_input: &str,
        arguments: &[String],
        output_expect: Expect,
        trmaps_expect: Expect,
    ) {
        let def = MacroDefinition::parse(def_input).unwrap();
        let (text_range_maps, output) = def.substitute(arguments);

        output_expect.assert_eq(&output);

        let mut trmaps_str = String::new();
        for trmap in text_range_maps {
            trmap.test_fmt(&mut trmaps_str).ok();
            trmaps_str.push('\n');
        }
        trmaps_expect.assert_eq(&trmaps_str);
    }

    #[test]
    fn substitute_concat() {
        check_substitute(
            "#define CONCAT(a, b) a ## b",
            &["foo".to_owned(), "bar".to_owned()],
            expect![[r#"foobar"#]],
            expect![[r#"
                0.. arg_0 ## arg_1
            "#]],
        );
    }

    #[test]
    fn substitute_stringify() {
        check_substitute(
            "#define STRINGIFY(a) #a",
            &["1 + 2".to_owned()],
            expect![[r#""1 + 2""#]],
            expect![[r#"
                7.. stringify(macro+21, arg_0)
            "#]],
        );
    }

    /// Special characters break the word and add spaces to make sure the reparsing doesn't combine
    /// them with other special characters or words. The output isn't identical with clangd's
    /// pretty printing but it works perfectly.
    #[test]
    fn substitute_break_the_word() {
        check_substitute(
            r#"#define FOO "bar" <123>"#,
            &[],
            expect![[r#""bar"< 123 >"#]],
            expect![[r#"
                0.. macro+12
                5.. macro+18
                7.. macro+19
                11.. macro+22
            "#]],
        );
    }

    /// Like above. Example from Linux `include/dt-bindings/input/input.h`
    // TODO: combine applicable macro trmaps; the trmaps expect below could be a lot smaller
    #[test]
    fn substitute_break_the_word_example() {
        check_substitute(
            r#"#define MATRIX_KEY(row, col, code) \
                ((((row) & 0xFF) << 24) | (((col) & 0xFF) << 16) | ((code) & 0xFFFF))"#,
            &["0x00".to_owned(), "0x02".to_owned(), "KEY_BACK".to_owned()],
            expect!["((((0x00)& 0xFF)<< 24)|(((0x02)& 0xFF)<< 16)|((KEY_BACK)& 0xFFFF))"],
            expect![[r#"
                0.. macro+53
                1.. macro+54
                2.. macro+55
                3.. macro+56
                4.. arg_0
                8.. macro+60
                9.. macro+62
                11.. macro+64
                15.. macro+68
                16.. macro+70
                19.. macro+73
                21.. macro+75
                22.. macro+77
                23.. macro+79
                24.. macro+80
                25.. macro+81
                26.. arg_1
                30.. macro+85
                31.. macro+87
                33.. macro+89
                37.. macro+93
                38.. macro+95
                41.. macro+98
                43.. macro+100
                44.. macro+102
                45.. macro+104
                46.. macro+105
                47.. arg_2
                55.. macro+110
                56.. macro+112
                58.. macro+114
                64.. macro+120
                65.. macro+121
            "#]],
        );
    }

    #[test]
    fn substitute_double_special_characters() {
        check_substitute(
            r#"#define FOO < < > > & & | |"#,
            &[],
            expect!["< < > > & & | |"],
            expect![[r#"
                0.. macro+12
                2.. macro+14
                4.. macro+16
                6.. macro+18
                8.. macro+20
                10.. macro+22
                12.. macro+24
                14.. macro+26
            "#]],
        );
        check_substitute(
            r#"#define FOO << >> && ||"#,
            &[],
            expect!["<< >> && ||"],
            expect![[r#"
                0.. macro+12
                3.. macro+15
                6.. macro+18
                9.. macro+21
            "#]],
        );
    }

    #[expect(clippy::needless_pass_by_value, reason = "ergonomics")]
    fn check_parse(input: &str, expect: Expect) {
        let def = MacroDefinition::parse(input).unwrap();

        let mut actual = String::new();
        actual.push_str(&def.name);
        actual.push('(');

        for (i, param) in def.params.iter().enumerate() {
            if i != 0 {
                actual.push_str(", ");
            }
            actual.push_str(param);
        }

        actual.push_str(")\n");

        for body_token in def.body_tokens {
            writeln!(
                &mut actual,
                "{:?} {}",
                body_token.kind, body_token.text_range
            )
            .ok();
        }

        writeln!(
            &mut actual,
            "dont_prescan_indices: {:?}",
            def.dont_prescan_indices
        )
        .ok();

        expect.assert_eq(&actual);
    }

    #[test]
    fn parse_basic() {
        check_parse(
            "#define CONCAT(a, b) a \\\n## b",
            expect![[r#"
                CONCAT(a, b)
                Parameter(0) 21..22
                ConcatOperator 25..27
                Parameter(1) 28..29
                dont_prescan_indices: [0, 1]
            "#]],
        );
        assert_eq!(
            MacroDefinition::parse("#define FOO \\\\\n"),
            Err(MacroDefinitionParseError::InvalidBackslash {
                text_range: (12..14).into()
            })
        );

        check_parse(
            "#define FOO 1 + 2",
            expect![[r#"
                FOO()
                Word("1") 12..13
                WordNoWhitespace("+") 14..15
                Word("2") 16..17
                dont_prescan_indices: []
            "#]],
        );
    }

    #[test]
    fn parse_stringify() {
        check_parse(
            "#define FOO(a) #a",
            expect![[r#"
                FOO(a)
                StringifyOperator 15..16
                Parameter(0) 16..17
                dont_prescan_indices: [0]
            "#]],
        );
        check_parse(
            "#define FOO(a) # a",
            expect![[r#"
                FOO(a)
                StringifyOperator 15..16
                Parameter(0) 17..18
                dont_prescan_indices: [0]
            "#]],
        );
        assert_eq!(
            MacroDefinition::parse("#define FOO(a) #"),
            Err(MacroDefinitionParseError::StringifyNotAParam {
                text_range: (15..16).into(),
            })
        );
        assert_eq!(
            MacroDefinition::parse("#define FOO(a) #   "),
            Err(MacroDefinitionParseError::StringifyNotAParam {
                text_range: (18..19).into(),
            })
        );
        assert_eq!(
            MacroDefinition::parse("#define FOO(a) #b"),
            Err(MacroDefinitionParseError::StringifyNotAParam {
                text_range: (16..17).into(),
            })
        );
        assert_eq!(
            MacroDefinition::parse("#define FOO(a) #\"\""),
            Err(MacroDefinitionParseError::StringifyNotAParam {
                text_range: (16..18).into(),
            })
        );
    }

    #[test]
    fn parse_concat_errors() {
        assert_eq!(
            MacroDefinition::parse(r#"#define FOO(a) "abc" ## def"#),
            Err(MacroDefinitionParseError::ConcatInvalidToken {
                text_range: (15..20).into(),
            })
        );
        assert_eq!(
            MacroDefinition::parse(r#"#define FOO(a) abc ## "def""#),
            Err(MacroDefinitionParseError::ConcatInvalidToken {
                text_range: (22..27).into(),
            })
        );
        assert_eq!(
            MacroDefinition::parse("#define FOO(a) abc ##"),
            Err(MacroDefinitionParseError::ConcatAtEnd {
                text_range: (19..21).into(),
            })
        );
        assert_eq!(
            MacroDefinition::parse("#define FOO(a) ##"),
            Err(MacroDefinitionParseError::ConcatAtStart {
                text_range: (15..17).into(),
            })
        );
    }

    #[test]
    fn parse_words_symbols() {
        check_parse(
            r#"#define FOO "bar" <123>"#,
            expect![[r#"
                FOO()
                WordNoWhitespace("\"bar\"") 12..17
                Word("<") 18..19
                Word("123") 19..22
                Word(">") 22..23
                dont_prescan_indices: []
            "#]],
        );
    }

    #[test]
    fn parse_symbols_not_together() {
        check_parse(
            r#"#define FOO << >> && ||"#,
            expect![[r#"
                FOO()
                Word("<<") 12..14
                Word(">>") 15..17
                Word("&&") 18..20
                Word("||") 21..23
                dont_prescan_indices: []
            "#]],
        );
    }

    #[test]
    fn parse_symbols_together() {
        // FIXME: parsed improperly
        check_parse(
            r#"#define FOO <<>>&&||"#,
            expect![[r#"
                FOO()
                Word("<<") 12..14
                Word(">") 15..16
                Word("&&") 16..18
                Word("|") 19..20
                dont_prescan_indices: []
            "#]],
        );
    }

    #[test]
    fn parse_utf8() {
        // FIXME: parsed improperly
        check_parse(
            r#"#define FOO ä a a"#,
            expect![[r#"
                FOO()
                Word("ä") 13..15
                Word("a") 15..16
                Word("a") 17..18
                dont_prescan_indices: []
            "#]],
        );
    }
}
