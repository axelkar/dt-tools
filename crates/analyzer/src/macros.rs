//! Preprocessor macro implementation
//!
//! Macro substitution, parameters, ternary operator are evaluated here
// TODO: rewrite the parser to understand UTF-8 and to have clearer code

use std::{borrow::Cow, iter::Peekable, str::Chars};

use dt_tools_parser::{TextRange, ast, parser::Entrypoint};

use crate::new::outline::subslice_offset;

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

#[derive(Debug, Clone, PartialEq, Eq)]
struct Param {
    // TODO: handle vararg substitution with commas etc.
    is_vararg: bool,
    name: String,
}

struct Parser<'params, 'input> {
    /// Current text offset - the index of the character that `peek` returns
    offset: usize,
    current_token_start_offset: usize,
    iter: Peekable<Chars<'input>>,
    current_token: String,
    params: &'params [Param],
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
            if let Some(i) = self
                .params
                .iter()
                .position(|p| p.name == self.current_token)
            {
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
            self.current_token_start_offset = self.offset - ch.len_utf8();
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

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error, displaydoc::Display)]
pub enum MacroDefinitionParseError {
    /// Lex error; the lexer should have caught this.
    LexError,
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
    /// Only the last parameter may be variadic.
    NonLastVararg { name: String },
}
impl MacroDefinitionParseError {
    /// Returns the range of the error, if it exists.
    ///
    /// Note that it's relative to the token's textual contents.
    #[must_use]
    pub fn text_range(&self) -> Option<TextRange> {
        match self {
            Self::StringifyNotAParam { text_range }
            | Self::ConcatInvalidToken { text_range }
            | Self::ConcatAtStart { text_range }
            | Self::ConcatAtEnd { text_range }
            | Self::InvalidBackslash { text_range } => Some(*text_range),
            Self::LexError | Self::MissingName | Self::NonLastVararg { name: _ } => None,
        }
    }
}

fn parse_params(
    iter: &mut Peekable<Chars<'_>>,
    offset: &mut usize,
) -> Result<Vec<Param>, MacroDefinitionParseError> {
    let mut params = Vec::new();

    if iter.peek() == Some(&'(') {
        iter.next();
        *offset += 1;

        let mut current_param = String::new();
        for ch in iter.by_ref() {
            *offset += ch.len_utf8();

            let mut new_param = || {
                if !current_param.trim().is_empty() {
                    let name = current_param.trim();
                    let vararg_name = name.strip_suffix("...");

                    params.push(Param {
                        is_vararg: vararg_name.is_some(),
                        name: vararg_name.unwrap_or(name).to_owned(),
                    });
                }
            };

            match ch {
                ')' => {
                    new_param();
                    break;
                }
                ',' => {
                    new_param();
                    current_param.clear();
                }
                _ => {
                    current_param.push(ch);
                }
            }
        }
    }

    if let Some((last, rest)) = params.split_last_mut() {
        if let Some(param) = rest.iter().find(|param| param.is_vararg) {
            return Err(MacroDefinitionParseError::NonLastVararg {
                name: param.name.clone(),
            });
        }

        if last.is_vararg && last.name.is_empty() {
            // Specifying the name is a GNU extension. This is the default.
            "__VA_ARGS__".clone_into(&mut last.name);
        }
    }

    Ok(params)
}

fn parse_string(p: &mut Parser, ch: char) -> Result<(), MacroDefinitionParseError> {
    p.complete_token()?;
    p.bump(ch);
    let mut escaped = false;
    while let Some(ch) = p.next() {
        p.bump(ch);

        if escaped {
            escaped = false;
            continue;
        }

        if ch == '\\' {
            escaped = true;
            continue;
        }

        if ch == '"' {
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
                start: p.offset - ch.len_utf8() * 2,
                end: p.offset,
            },
            kind: MacroTokenKind::Word({
                let mut s = String::with_capacity(2);
                s.push(ch);
                s.push(ch);
                s
            }),
        })?;
    } else {
        p.push_tok(MacroToken {
            text_range: TextRange {
                start: p.offset - ch.len_utf8(),
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
                    // concat operator
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
                            });
                        }
                        None => {
                            return Err(MacroDefinitionParseError::ConcatAtStart {
                                text_range: concat_text_range,
                            });
                        }
                    }

                    p.push_tok(MacroToken {
                        kind: MacroTokenKind::ConcatOperator,
                        text_range: concat_text_range,
                    })?;
                    p.expect_concat_param = Some(concat_text_range);
                } else {
                    // stringify operator
                    p.push_tok(MacroToken {
                        kind: MacroTokenKind::StringifyOperator,
                        text_range: TextRange {
                            start: p.offset - 1,
                            end: p.offset,
                        },
                    })?;

                    p.expect_stringify_param = true;
                }
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
            '-' | ',' => {
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

/// Parsed macro definition.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MacroDefinition {
    /// Macro name
    pub name: String,
    /// Parameter names
    params: Vec<Param>,
    /// Parsed macro body tokens, used for substitution
    body_tokens: Vec<MacroToken>,
    /// Parameter indices not to prescan.
    dont_prescan_indices: Vec<usize>,
}
impl MacroDefinition {
    /// Parses a macro definition from its source code.
    ///
    /// # Example
    ///
    /// ```
    /// use dt_tools_analyzer::macros::{MacroDefinition, MacroDefinitionParseError};
    ///
    /// assert_eq!(MacroDefinition::parse("#define"), Err(MacroDefinitionParseError::MissingName));
    /// ```
    ///
    /// # Errors
    ///
    /// [`MacroDefinitionParseError`] is returned if the input cannot be parsed into a macro definition.
    pub fn parse(input: &str) -> Result<Self, MacroDefinitionParseError> {
        let s = input
            .strip_prefix('#')
            .ok_or(MacroDefinitionParseError::LexError)?
            .trim_start_matches([' ', '\t']);

        let s = s
            .get("define".len()..)
            .ok_or(MacroDefinitionParseError::LexError)?
            .trim_start_matches([' ', '\t']);

        let mut iter = s.chars().peekable();
        let mut offset = subslice_offset(input, s).ok_or(MacroDefinitionParseError::LexError)?;

        let mut macro_name = String::new();

        while let Some(&ch) = iter.peek() {
            if ch.is_whitespace() || ch == '(' {
                break;
            }
            macro_name.push(ch);
            iter.next(); // Consume the character
            offset += ch.len_utf8();
        }

        if macro_name.is_empty() {
            return Err(MacroDefinitionParseError::MissingName);
        }

        let params = parse_params(&mut iter, &mut offset)?;

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

    /// Returns parameter indices that must not be prescanned (`#` or `##`).
    #[must_use]
    pub fn dont_prescan_indices(&self) -> &[usize] {
        &self.dont_prescan_indices
    }

    /// Returns the number of parameters this macro expects.
    #[must_use]
    pub fn param_count(&self) -> usize {
        self.params.len()
    }

    /// Returns the index of the variadic parameter.
    #[must_use]
    pub fn variadic_param_idx(&self) -> Option<usize> {
        self.params.iter().position(|p| p.is_vararg)
    }

    /// Value of parameter `idx`. For the trailing variadic parameter, joins the
    /// remaining arguments with `, ` like the C preprocessor's `__VA_ARGS__`.
    ///
    /// Returns `Err` if a parameter (variadic or not) doesn't have a corresponding argument.
    fn param_value<'a>(&self, arguments: &'a [String], idx: usize) -> Result<Cow<'a, str>, &Param> {
        let param = self.params.get(idx).unwrap();
        let arg = arguments.get(idx).ok_or(param)?;

        if param.is_vararg {
            Ok(Cow::Owned(arguments.get(idx..).unwrap().join(", ")))
        } else {
            Ok(Cow::Borrowed(arg))
        }
    }

    /// Substitutes a macro's arguments.
    ///
    /// Note: the arguments aren't prescanned.
    ///
    /// # Errors
    ///
    /// See [`MacroSubstitutionError`] for details.
    ///
    /// # Panics
    ///
    /// May panic if there is an internal compiler error.
    #[expect(clippy::too_many_lines, reason = "Hard to make this shorter")]
    pub fn substitute(
        &self,
        arguments: &[String],
    ) -> Result<SubstitutedBody, MacroSubstitutionError> {
        let mut s = String::new();
        let mut iter = self.body_tokens.iter().peekable();
        let mut push_ws = false;
        let mut trmaps = Vec::new();

        let is_variadic = self.params.last().is_some_and(|p| p.is_vararg);
        if is_variadic {
            // The variadic parameter may match zero arguments.
            let required = self.params.len() - 1;
            if arguments.len() < required {
                return Err(MacroSubstitutionError::TooFewVariadic {
                    expected: required,
                    got: arguments.len(),
                });
            }
        } else if arguments.len() < self.params.len() {
            return Err(MacroSubstitutionError::TooFew {
                expected: self.params.len(),
                got: arguments.len(),
            });
        } else if arguments.len() > self.params.len() {
            return Err(MacroSubstitutionError::TooMany {
                expected: self.params.len(),
                got: arguments.len(),
            });
        }

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
                    if push_ws {
                        s.push(' ');
                    }
                    if self.params.get(*idx).is_some_and(|p| p.is_vararg) {
                        // Expand each argument separately so a diagnostic can blame the
                        // exact argument, and the injected `, ` on the vararg parameter.
                        for (i, arg) in arguments.iter().enumerate().skip(*idx) {
                            if i != *idx {
                                trmaps.push(TextRangeMap {
                                    from_offset: s.len(),
                                    to: TextRangeMapTo::VarargSeparator {
                                        macro_text: token.text_range,
                                    },
                                });
                                s.push_str(", ");
                            }
                            trmaps.push(TextRangeMap {
                                from_offset: s.len(),
                                to: TextRangeMapTo::ArgumentIdx(i),
                            });
                            s.push_str(arg);
                        }
                    } else {
                        trmaps.push(TextRangeMap {
                            from_offset: s.len(),
                            to: TextRangeMapTo::ArgumentIdx(*idx),
                        });
                        s.push_str(arguments.get(*idx).map_or("", String::as_str));
                    }
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
                    let mut vararg_trailing: Option<(TextRange, usize)> = None;

                    let source = match iter.next() {
                        Some(MacroToken {
                            kind: MacroTokenKind::Word(text),
                            ..
                        }) => {
                            s.push_str(text);
                            TextRangeMapTo::MacroTextOffset(token.text_range.start)
                        }
                        Some(MacroToken {
                            kind: MacroTokenKind::Parameter(idx),
                            text_range,
                        }) => {
                            s.push_str(arguments.get(*idx).map_or("", String::as_str));
                            // TODO: GNU behavior removes comma if no arguments: ", ##__VA_ARGS__"

                            let is_vararg = self.params.get(*idx).is_some_and(|p| p.is_vararg);
                            if is_vararg && arguments.len() > *idx + 1 {
                                vararg_trailing = Some((*text_range, *idx + 1));
                            }
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
                    let prev = trmaps.last_mut().expect("should be a token before concat");

                    prev.to = TextRangeMapTo::Concat {
                        operator: token.text_range,
                        sources: Box::new([prev.to.clone(), source]),
                    };

                    // Append the remaining vararg arguments after the paste.
                    if let Some((macro_text, first_trailing)) = vararg_trailing {
                        for (i, arg) in arguments.iter().enumerate().skip(first_trailing) {
                            trmaps.push(TextRangeMap {
                                from_offset: s.len(),
                                to: TextRangeMapTo::VarargSeparator { macro_text },
                            });
                            s.push_str(", ");
                            trmaps.push(TextRangeMap {
                                from_offset: s.len(),
                                to: TextRangeMapTo::ArgumentIdx(i),
                            });
                            s.push_str(arg);
                        }
                    }
                }
                MacroTokenKind::StringifyOperator => {
                    let from_offset = s.len();
                    let (argument_idx, param_range) = match iter.next() {
                        Some(MacroToken {
                            kind: MacroTokenKind::Parameter(idx),
                            text_range,
                        }) => {
                            let text = self.param_value(arguments, *idx).unwrap_or_default();
                            s.push_str(&stringify(&text));
                            (idx, text_range)
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
                        from_offset,
                        to: TextRangeMapTo::Stringify {
                            macro_text: token.text_range.to(*param_range),
                            argument_idx: *argument_idx,
                        },
                    });
                }
            }
            push_ws = true;
        }

        Ok(SubstitutedBody {
            source_mappings: trmaps,
            substituted_text: s,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error, displaydoc::Display)]
pub enum MacroSubstitutionError {
    /// too few arguments: expected at least {expected}, got {got}
    TooFewVariadic { expected: usize, got: usize },
    /// too few arguments: expected {expected}, got {got}
    TooFew { expected: usize, got: usize },
    /// too many arguments: expected {expected}, got {got}
    TooMany { expected: usize, got: usize },
}

/// A mapping from text in the output to some text ranges related to macros.
///
/// Use this to map errors' text ranges in macros.
///
/// Also known as `trmap`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TextRangeMap {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TextRangeMapTo {
    /// Forward to argument at index
    ArgumentIdx(usize),
    /// Forward to offset in macro definition string
    MacroTextOffset(usize),
    /// Forward to sources
    Concat {
        /// The concat operator's range in the macro body.
        operator: TextRange,
        /// The left and right sources.
        sources: Box<[TextRangeMapTo; 2]>,
    },
    /// The `, ` joining two variadic arguments; not present in the source.
    VarargSeparator {
        /// The vararg parameter's range in the macro body.
        macro_text: TextRange,
    },
    /// Forward to offset in macro definition and argument at index
    Stringify {
        /// [ `TextRange` ] instead of offset because we aren't doing precise mapping in strings.
        macro_text: TextRange,
        argument_idx: usize,
    },
}

impl TextRangeMapTo {
    #[cfg(test)]
    fn test_fmt(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        match self {
            Self::ArgumentIdx(idx) => write!(f, "arg_{idx}"),
            Self::MacroTextOffset(offset) => write!(f, "macro+{offset}"),
            Self::Concat { operator, sources } => {
                for (i, to) in sources.iter().enumerate() {
                    if i != 0 {
                        write!(f, " ## ({operator}) ")?;
                    }
                    to.test_fmt(f)?;
                }
                Ok(())
            }
            Self::VarargSeparator { macro_text } => {
                write!(f, "vararg_separator(macro+{macro_text})")
            }
            Self::Stringify {
                macro_text,
                argument_idx: argument,
            } => write!(f, "stringify(macro+{macro_text}, arg_{argument})"),
        }
    }
}

/// Substituted macro body from [`MacroDefinition::substitute`].
pub struct SubstitutedBody {
    pub source_mappings: Vec<TextRangeMap>,
    pub substituted_text: String,
}

/// Substitutes a macro's arguments from a macro invocation AST.
///
/// Note: the arguments aren't prescanned.
///
/// If the `ast` parameter is set to `None`, it is assumed that it's an object-like macro and not
/// a function-like macro.
///
/// # Errors
///
/// See [`MacroSubstitutionError`].
pub fn substitute_macro_ast(
    ast: Option<&ast::MacroInvocation>,
    def: &MacroDefinition,
) -> Result<SubstitutedBody, MacroSubstitutionError> {
    let arguments = ast
        .map(|ast| {
            ast.arguments()
                .map(|arg| {
                    arg.green
                        .child_tokens()
                        .map(|tok| tok.text.as_str())
                        .collect()
                })
                .collect::<Vec<String>>()
        })
        .unwrap_or_default();

    def.substitute(&arguments)
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
#[expect(
    clippy::needless_raw_string_hashes,
    reason = "expect-test auto update adds r#"
)]
mod tests {
    use std::fmt::Write;

    use expect_test::{Expect, expect};

    use super::*;

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
        let SubstitutedBody {
            source_mappings,
            substituted_text,
        } = match def.substitute(arguments) {
            Ok(val) => val,
            Err(err) => {
                output_expect.assert_eq(&format!("--- substitution error ---\n{err}"));
                return;
            }
        };

        output_expect.assert_eq(&substituted_text);

        let mut trmaps_str = String::new();
        for trmap in source_mappings {
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
                0.. arg_0 ## (23..25) arg_1
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
                0.. stringify(macro+21..23, arg_0)
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
            "#define FOO < < > > & & | |",
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
            "#define FOO << >> && ||",
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

    #[test]
    fn substitute_vararg_comma() {
        check_substitute(
            "#define MACRO(...) function(argument, __VA_ARGS__)",
            &["foo".to_owned(), "bar".to_owned()],
            expect!["function(argument , foo, bar)"],
            expect![[r#"
                0.. macro+19
                8.. macro+27
                9.. macro+28
                18.. macro+36
                20.. arg_0
                23.. vararg_separator(macro+38..49)
                25.. arg_1
                28.. macro+49
            "#]],
        );
    }

    #[test]
    fn substitute_vararg_empty_comma() {
        check_substitute(
            "#define MACRO(...) function(argument, __VA_ARGS__)",
            &[],
            expect!["function(argument , )"],
            expect![[r#"
                0.. macro+19
                8.. macro+27
                9.. macro+28
                18.. macro+36
                20.. macro+49
            "#]],
        );
    }

    // TODO: implement
    #[test]
    #[ignore = "TODO: implement"]
    fn substitute_vararg_empty_gnu_comma() {
        // GNU extension to remove the comma
        check_substitute(
            "#define MACRO(...) function(argument, ##__VA_ARGS__)",
            &[],
            expect![r#"function(argument)"#],
            expect![[r#"
                0.. stringify(macro+23..35, arg_0)
            "#]],
        );
    }

    // TODO: implement
    #[test]
    #[ignore = "TODO: implement"]
    fn substitute_vararg_empty_va_opt_comma() {
        // TODO
        check_substitute(
            "#define MACRO(...) function(argument __VA_OPT__(,) __VA_ARGS__)",
            &[],
            expect![r#"function(argument)"#],
            expect![[r#"
                0.. stringify(macro+23..35, arg_0)
            "#]],
        );
    }

    #[test]
    fn parse_vararg_empty_va_opt_comma() {
        check_parse(
            "#define MACRO(...) function(argument __VA_OPT__(,) __VA_ARGS__)",
            expect![[r#"
                MACRO(__VA_ARGS__...)
                Word("function") 19..27
                WordNoWhitespace("(") 27..28
                Word("argument") 28..36
                Word("__VA_OPT__") 37..47
                WordNoWhitespace("(") 47..48
                Word(",") 48..49
                WordNoWhitespace(")") 49..50
                Parameter(0) 51..62
                WordNoWhitespace(")") 62..63
                dont_prescan_indices: []
            "#]],
        );
    }

    #[test]
    fn substitute_stringify_vararg() {
        check_substitute(
            "#define STRINGIFY(...) #__VA_ARGS__",
            &["foo".to_owned(), "bar".to_owned()],
            expect![r#""foo, bar""#],
            expect![[r#"
                0.. stringify(macro+23..35, arg_0)
            "#]],
        );
    }

    #[test]
    fn substitute_concat_vararg() {
        check_substitute(
            "#define __concat_1(x, y...)     x ## y",
            &["foo".to_owned(), "bar".to_owned(), "baz".to_owned()],
            expect![r#"foobar, baz"#],
            expect![[r#"
                0.. arg_0 ## (34..36) arg_1
                6.. vararg_separator(macro+37..38)
                8.. arg_2
            "#]],
        );
    }

    #[expect(clippy::needless_pass_by_value, reason = "ergonomics")]
    fn check_parse(input: &str, expect: Expect) {
        let def = match MacroDefinition::parse(input) {
            Ok(def) => def,
            Err(err) => {
                let text_range = err
                    .text_range()
                    .map(|tr| format!(" ({tr})"))
                    .unwrap_or_default();
                expect.assert_eq(&format!("--- error ---\n{err}{text_range}\n"));
                return;
            }
        };

        let mut actual = String::new();
        actual.push_str(&def.name);
        actual.push('(');

        for (i, param) in def.params.iter().enumerate() {
            if i != 0 {
                actual.push_str(", ");
            }
            actual.push_str(&param.name);
            if param.is_vararg {
                actual.push_str("...");
            }
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
            "#define FOO \\\\\n",
            expect![[r#"
                --- error ---
                Invalid backslash (12..14)
            "#]],
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
    fn parse_basic_args() {
        // Note how dont_prescan_indices is empty
        check_parse(
            "#define ADD(a, b) a + b",
            expect![[r#"
                ADD(a, b)
                Parameter(0) 18..19
                WordNoWhitespace("+") 20..21
                Parameter(1) 22..23
                dont_prescan_indices: []
            "#]],
        );
    }

    #[test]
    fn parse_concat() {
        // Note how dont_prescan_indices is not empty due to the ## operator
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
        check_parse(
            "#define FOO(a) #",
            expect![[r#"
                --- error ---
                '#' not followed by a macro parameter (15..16)
            "#]],
        );
        check_parse(
            "#define FOO(a) #   ",
            expect![[r#"
                --- error ---
                '#' not followed by a macro parameter (18..19)
            "#]],
        );
        check_parse(
            "#define FOO(a) #b",
            expect![[r#"
                --- error ---
                '#' not followed by a macro parameter (16..17)
            "#]],
        );
        check_parse(
            "#define FOO(a) #\"\"",
            expect![[r#"
                --- error ---
                '#' not followed by a macro parameter (16..18)
            "#]],
        );
    }

    #[test]
    fn parse_concat_errors() {
        check_parse(
            r#"#define FOO(a) "abc" ## def"#,
            expect![[r#"
                --- error ---
                invalid token for '##' (15..20)
            "#]],
        );
        check_parse(
            r#"#define FOO(a) abc ## "def""#,
            expect![[r#"
                --- error ---
                invalid token for '##' (22..27)
            "#]],
        );
        check_parse(
            "#define FOO(a) abc ##",
            expect![[r#"
                --- error ---
                '##' cannot be at end (19..21)
            "#]],
        );
        check_parse(
            "#define FOO(a) ##",
            expect![[r#"
                --- error ---
                '##' cannot be at start (15..17)
            "#]],
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
    fn parse_string_escaped_quote() {
        check_parse(
            r#"#define FOO "a\"b" c"#,
            expect![[r#"
                FOO()
                WordNoWhitespace("\"a\\\"b\"") 12..18
                Word("c") 19..20
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
        check_parse(
            r#"#define FOO <<>>&&||"#,
            expect![[r#"
                FOO()
                Word("<<") 12..14
                Word(">>") 14..16
                Word("&&") 16..18
                Word("||") 18..20
                dont_prescan_indices: []
            "#]],
        );
    }

    #[test]
    fn parse_utf8() {
        check_parse(
            r#"#define FOO ä a"#,
            expect![[r#"
                FOO()
                Word("ä") 12..14
                Word("a") 15..16
                dont_prescan_indices: []
            "#]],
        );
    }

    #[test]
    fn parse_vararg() {
        check_parse(
            r#"#define __stringify_1(x...) #x"#,
            expect![[r#"
                __stringify_1(x...)
                StringifyOperator 28..29
                Parameter(0) 29..30
                dont_prescan_indices: [0]
            "#]],
        );
    }
}
