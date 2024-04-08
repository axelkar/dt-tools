//! - <https://rust-analyzer.github.io/blog/2020/09/16/challeging-LR-parsing.html#error-resilience>
//! - <https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html>

use self::event::Event;
use self::source::Source;
#[allow(unused_imports)]
pub use marker::{CompletedMarker, Marker};
use smallvec::SmallVec;

use super::{grammar, lexer::TokenKind, GreenNode, NodeKind};

mod errors;
mod event;
mod marker;
mod sink;
mod source;

pub use errors::{ParseError, WrappedLexError};

// FIXME: previous is Semicolon or RCurly
//const RECOVERY_SET: [TokenKind; 2] = [TokenKind::Semicolon, TokenKind::RCurly];
//const RECOVERY_SET: [TokenKind; 1] = [TokenKind::RCurly];
const RECOVERY_SET: [TokenKind; 1] = [TokenKind::Semicolon];
//const RECOVERY_SET: [TokenKind; 0] = [];

// TODO: Event incremental reparse:
// > Events can make our parser faster if we have a previous parse tree lying around; rather than
// > constructing a new parse tree every time, we can patch an existing one with the events of the
// > new parse.

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parse<'input> {
    pub green_node: GreenNode,
    pub lex_errors: Vec<WrappedLexError<'input>>,
    pub errors: Vec<ParseError>,
}

pub fn parse(input: &str) -> Parse {
    use super::lexer::Lexer;

    let tokens: Vec<_> = Lexer::new(input).collect();
    let source = Source::new(&tokens);
    let mut parser = Parser::new(source);

    grammar::root(&mut parser);

    let sink = sink::Sink::new(&tokens, parser.events);

    let parse = sink.finish();
    eprintln!("tree = {}", parse.green_node.print_tree());
    parse
}

/// CST parser.
#[derive(Debug)]
pub struct Parser<'t, 'input> {
    /// The token source.
    source: Source<'t, 'input>,
    /// Events to feed into [`Sink`].
    events: Vec<Event>,
    /// The tokens that are expected at the current position.
    expected_kinds: SmallVec<[TokenKind; 2]>,
}

impl<'t, 'input> Parser<'t, 'input> {
    /// Creates and initializes a parser.
    fn new(source: Source<'t, 'input>) -> Self {
        Self {
            source,
            events: Vec::new(),
            expected_kinds: SmallVec::new(),
        }
    }

    /// Starts a node using a [`Marker`].
    pub fn start(&mut self) -> Marker {
        let pos = self.events.len();
        self.events.push(Event::Placeholder);

        Marker::new(pos)
    }

    /// Peeks ahead at the next token's kind.
    ///
    /// Returns None on EOF
    fn peek(&mut self) -> Option<TokenKind> {
        self.source.peek_kind()
    }

    /// Returns true if `kind` is the next token's kind.
    pub fn at(&mut self, kind: TokenKind) -> bool {
        self.expected_kinds.push(kind);
        self.peek() == Some(kind)
    }

    /// Returns true if `kind` is the next token's kind.
    /// 
    /// - This does not add `kind` to `expected_kinds`.
    pub fn silent_at(&mut self, kind: TokenKind) -> bool {
        self.peek() == Some(kind)
    }

    /// Errors and returns true if `kind` is the next token's kind.
    ///
    /// - This adds an error if it matches.
    /// - This does not add `kind` to `expected_kinds`.
    /// - This never bumps forward.
    ///
    /// You can use this to take care of `RCurly` tokens when there should be `Semicolon` for
    /// example.
    pub fn expect_no_ending(&mut self, kind: TokenKind) -> bool {
        if self.peek() == Some(kind) {
            self.error_no_bump();
            true
        } else {
            false
        }
    }

    /// Returns true if the next token's kind is in `set`.
    pub fn at_set(&mut self, set: &[TokenKind]) -> bool {
        self.expected_kinds.extend_from_slice(set);
        self.peek().map_or(false, |k| set.contains(&k))
    }

    /// Bumps when `set` contains the current token's kind and errors otherwise.
    pub fn expect_set(&mut self, set: &[TokenKind]) {
        if self.at_set(set) {
            self.bump();
        } else {
            self.error();
        }
    }

    /// Bumps when `kind` is the current token's kind and errors otherwise.
    pub fn expect(&mut self, kind: TokenKind) {
        if self.at(kind) {
            self.bump();
        } else {
            self.error();
        }
    }

    /// Reports the current token as an error.
    pub fn error(&mut self) {
        self.error_no_bump();

        if !self.at_set(&RECOVERY_SET) && !self.at_end() {
            let m = self.start();
            self.bump();
            m.complete(self, NodeKind::ParseError);
        }
    }

    /// Reports the current token as an error.
    ///
    /// In contrast to the `error` method, this will never `bump` the token.
    pub fn error_no_bump(&mut self) {
        let current_token = self.source.peek_token();

        let (found, range) = if let Some(token) = current_token {
            (
                // Sink will handle lex errors
                Some(token.kind.unwrap_or(TokenKind::LexError)),
                token.text_range,
            )
        } else {
            (None, self.source.last_token_range().unwrap())
        };

        self.events.push(Event::Error(ParseError {
            expected: std::mem::take(&mut self.expected_kinds),
            found,
            text_range: range,
        }));
    }

    /// Returns true if at the end-of-file.
    // TODO: Rust-analyzer has a token for EOF, should I?
    pub fn at_end(&mut self) -> bool {
        self.peek().is_none()
    }

    /// Adds the next token to the parse tree.
    pub fn bump(&mut self) {
        self.expected_kinds.clear();
        self.source.next_token().unwrap();
        self.events.push(Event::AddToken)
    }
}

#[cfg(test)]
mod tests {
    use super::sink::Sink;
    use crate::cst2::grammar::tests::{node, dynamic_token};
    use crate::cst2::lexer::{LexError, Lexer};
    use crate::cst2::{GreenNode, NodeKind};

    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn builder_api_lex_error() {
        let tokens: Vec<_> = Lexer::new("\"abc").collect();
        let mut parser = Parser::new(Source::new(&tokens));

        while !parser.at_end() {
            parser.bump();
        }

        assert_eq!(parser.events.len(), 1);

        let output = Sink::new(&tokens, parser.events).finish();

        assert_eq!(
            output,
            Parse {
                green_node: GreenNode {
                    kind: NodeKind::Document,
                    children: vec![dynamic_token(
                        TokenKind::LexError,
                        "\"abc"
                    )]
                },
                lex_errors: vec![WrappedLexError {
                    inner: LexError::UnexpectedEofString,
                    text_range: (0..4).into(),
                    text: "\"abc",
                }],
                errors: Vec::new()
            }
        );
    }

    #[test]
    fn builder_api() {
        let tokens: Vec<_> = Lexer::new("ident").collect();
        let mut parser = Parser::new(Source::new(&tokens));

        let m_node = parser.start();
        let m_prop = parser.start();
        let m_cell = parser.start();

        // Add the "ident" token
        parser.bump();

        // They aren't in the same order as they were defined in
        m_cell.complete(&mut parser, NodeKind::DtCell);
        m_node.complete(&mut parser, NodeKind::DtNode);
        m_prop.complete(&mut parser, NodeKind::DtProperty);

        // FIXME: with Sink code
        let output = Sink::new(&tokens, parser.events).finish();
        assert_eq!(
            output.green_node,
            GreenNode {
                kind: NodeKind::Document,
                children: vec![node(
                    NodeKind::DtNode,
                    vec![node(
                        NodeKind::DtProperty,
                        vec![node(
                            NodeKind::DtCell,
                            vec![dynamic_token(
                                TokenKind::Ident,
                                "ident"
                            )]
                        )]
                    )]
                )]
            }
        );
    }

    #[test]
    fn builder_api_precede() {
        let tokens: Vec<_> = Lexer::new("hello world").collect();
        let mut parser = Parser::new(Source::new(&tokens));

        let m_node = parser.start();
        let m_cell = parser.start();

        // Add the "ident" and whitespace tokens
        parser.bump();

        let completed_cell = m_cell.complete(&mut parser, NodeKind::DtCell);

        // actually, let's wrap the cell in a DtProperty
        let m_prop = completed_cell.precede(&mut parser);

        // Add the "world" token
        parser.bump();

        m_prop.complete(&mut parser, NodeKind::DtProperty);

        m_node.complete(&mut parser, NodeKind::DtNode);

        // FIXME: with Sink code
        let output = Sink::new(&tokens, parser.events).finish();
        assert_eq!(
            output.green_node,
            GreenNode {
                kind: NodeKind::Document,
                children: vec![node(
                    NodeKind::DtNode,
                    vec![node(
                        NodeKind::DtProperty,
                        vec![
                            node(
                                NodeKind::DtCell,
                                vec![
                                    dynamic_token(TokenKind::Ident, "hello"),
                                    dynamic_token(
                                        TokenKind::Whitespace,
                                        " "
                                    )
                                ]
                            ),
                            dynamic_token(TokenKind::Ident, "world")
                        ]
                    )]
                )]
            }
        );
    }
}
