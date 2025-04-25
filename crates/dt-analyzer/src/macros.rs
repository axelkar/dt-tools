//! Preprocessor macro implementation
//!
//! Macro substitution, parameters, ternary operator are evaluated here

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
    Symbol(String),
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
    #[inline]
    fn next(&mut self) -> Option<char> {
        let ch = self.iter.next();
        if ch.is_some() {
            self.offset += 1;
        }
        ch
    }
    #[inline]
    fn peek(&mut self) -> Option<&char> {
        self.iter.peek()
    }
    #[inline]
    fn bump(&mut self, ch: char) {
        if self.current_token.is_empty() {
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
        kind: MacroTokenKind::Symbol(tok),
        text_range,
    })
}

fn parse_double_special_characters(
    p: &mut Parser,
    ch: char,
) -> Result<(), MacroDefinitionParseError> {
    // These change meaning when two are pasted together
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
                    kind: MacroTokenKind::Symbol(ch.to_string()),
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
    fn substitute(&self, arguments: &[String]) -> String {
        let mut s = String::new();
        let mut iter = self.body_tokens.iter().peekable();
        let mut push_ws = false;
        while let Some(token) = iter.next() {
            match &token.kind {
                MacroTokenKind::Word(text) => {
                    if push_ws {
                        s.push(' ');
                    }
                    s.push_str(text);
                }
                MacroTokenKind::Parameter(idx) => {
                    let text = &arguments[*idx];
                    if push_ws {
                        s.push(' ');
                    }
                    s.push_str(text);
                }
                MacroTokenKind::Symbol(text) => {
                    s.push_str(text);
                    push_ws = false;
                    continue;
                }
                MacroTokenKind::ConcatOperator => match iter.next() {
                    Some(MacroToken {
                        kind: MacroTokenKind::Word(text),
                        ..
                    }) => {
                        s.push_str(text);
                    }
                    Some(MacroToken {
                        kind: MacroTokenKind::Parameter(idx),
                        ..
                    }) => {
                        let text = &arguments[*idx];
                        s.push_str(text);
                    }
                    Some(MacroToken {
                        kind:
                            MacroTokenKind::Symbol(_)
                            | MacroTokenKind::ConcatOperator
                            | MacroTokenKind::StringifyOperator,
                        ..
                    })
                    | None => {
                        unreachable!()
                    }
                },
                MacroTokenKind::StringifyOperator => match iter.next() {
                    Some(MacroToken {
                        kind: MacroTokenKind::Word(text),
                        ..
                    }) => {
                        s.push_str(&stringify(text));
                    }
                    Some(MacroToken {
                        kind: MacroTokenKind::Parameter(idx),
                        ..
                    }) => {
                        let text = &arguments[*idx];
                        s.push_str(&stringify(text));
                    }
                    Some(MacroToken {
                        kind:
                            MacroTokenKind::Symbol(_)
                            | MacroTokenKind::ConcatOperator
                            | MacroTokenKind::StringifyOperator,
                        ..
                    })
                    | None => {
                        unreachable!()
                    }
                },
            }
            push_ws = true;
        }
        s
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
) -> Result<String, String> {
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

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    fn mt(kind: MacroTokenKind, range: std::ops::Range<usize>) -> MacroToken {
        MacroToken {
            kind,
            text_range: range.into(),
        }
    }

    #[test]
    fn test_stringify() {
        #[track_caller]
        fn assert_str_eq(left: &str, right: &str) {
            assert!(left == right, "strings are not equal:\n<{left}\n>{right}\n");
        }
        assert_str_eq(&stringify(""), r#""""#);
        assert_str_eq(
            &stringify(r#"Hello "world"!\nNewline and \\ backslash."#),
            r#""Hello \"world\"!\nNewline and \\ backslash.""#,
        );
        assert_str_eq(
            &stringify(r#""Hello \"world\"!\nNewline and \ backslash.""#),
            r#""\"Hello \\\"world\\\"!\\nNewline and \\ backslash.\"""#,
        );
    }

    #[test]
    fn substitute() {
        let def = MacroDefinition::parse("#define CONCAT(a, b) a ## b").unwrap();
        assert_eq!(
            def.substitute(&["foo".to_owned(), "bar".to_owned()]),
            "foobar".to_owned()
        );

        let def = MacroDefinition::parse("#define STRINGIFY(a) #a").unwrap();
        assert_eq!(
            def.substitute(&["1 + 2".to_owned()]),
            "\"1 + 2\"".to_owned()
        );

        let def = MacroDefinition::parse(r#"#define FOO "bar" <123>"#).unwrap();
        assert_eq!(def.substitute(&[]), "\"bar\"< 123 >".to_owned());

        let def = MacroDefinition::parse(
            "#define MATRIX_KEY(row, col, code) \
                ((((row) & 0xFF) << 24) | (((col) & 0xFF) << 16) | ((code) & 0xFFFF))",
        )
        .unwrap();

        assert_eq!(
            def.substitute(&["0x00".to_owned(), "0x02".to_owned(), "KEY_BACK".to_owned()]),
            "((((0x00)& 0xFF)<< 24)|(((0x02)& 0xFF)<< 16)|((KEY_BACK)& 0xFFFF))".to_owned()
        );
    }

    #[test]
    fn substitute_double_symbols() {
        let def = MacroDefinition::parse("#define FOO < < > > & & | |").unwrap();
        assert_eq!(def.substitute(&[]), "< < > > & & | |".to_owned());
        let def = MacroDefinition::parse("#define FOO << >> && ||").unwrap();
        assert_eq!(def.substitute(&[]), "<< >> && ||".to_owned());
    }

    #[test]
    fn parse_basic() {
        assert_eq!(
            MacroDefinition::parse("#define CONCAT(a, b) a \\\n ## b"),
            Ok(MacroDefinition {
                name: "CONCAT".to_owned(),
                params: vec!["a".to_owned(), "b".to_owned()],
                body_tokens: vec![
                    mt(MacroTokenKind::Parameter(0), 21..22),
                    mt(MacroTokenKind::ConcatOperator, 26..28),
                    mt(MacroTokenKind::Parameter(1), 29..30),
                ],
                dont_prescan_indices: vec![0, 1]
            })
        );
        assert_eq!(
            MacroDefinition::parse("#define FOO \\\\\n"),
            Err(MacroDefinitionParseError::InvalidBackslash {
                text_range: (12..14).into()
            })
        );

        assert_eq!(
            MacroDefinition::parse("#define FOO 1 + 2"),
            Ok(MacroDefinition {
                name: "FOO".to_owned(),
                params: Vec::new(),
                body_tokens: vec![
                    mt(MacroTokenKind::Word("1".to_owned()), 12..13),
                    mt(MacroTokenKind::Symbol("+".to_owned()), 14..15),
                    mt(MacroTokenKind::Word("2".to_owned()), 16..17),
                ],
                dont_prescan_indices: Vec::new()
            })
        );
    }

    #[test]
    fn parse_stringify() {
        assert_eq!(
            MacroDefinition::parse("#define FOO(a) #a"),
            Ok(MacroDefinition {
                name: "FOO".to_owned(),
                params: vec!["a".to_owned()],
                body_tokens: vec![
                    mt(MacroTokenKind::StringifyOperator, 15..16),
                    mt(MacroTokenKind::Parameter(0), 16..17),
                ],
                dont_prescan_indices: vec![0]
            })
        );
        assert_eq!(
            MacroDefinition::parse("#define FOO(a) # a"),
            Ok(MacroDefinition {
                name: "FOO".to_owned(),
                params: vec!["a".to_owned()],
                body_tokens: vec![
                    mt(MacroTokenKind::StringifyOperator, 15..16),
                    mt(MacroTokenKind::Parameter(0), 17..18),
                ],
                dont_prescan_indices: vec![0]
            })
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
        assert_eq!(
            MacroDefinition::parse(r#"#define FOO "bar" <123>"#),
            Ok(MacroDefinition {
                name: "FOO".to_owned(),
                params: Vec::new(),
                body_tokens: vec![
                    mt(MacroTokenKind::Symbol("\"bar\"".to_owned()), 12..17),
                    mt(MacroTokenKind::Word("<".to_owned()), 18..19),
                    mt(MacroTokenKind::Word("123".to_owned()), 19..22),
                    mt(MacroTokenKind::Word(">".to_owned()), 22..23),
                ],
                dont_prescan_indices: Vec::new()
            })
        );
    }
}
