//! - <https://rust-analyzer.github.io/blog/2020/09/16/challeging-LR-parsing.html#error-resilience>
//! - <https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html>

use self::event::Event;
use self::marker::Marker;
use self::sink::Sink;
use self::source::Source;
use crate::TextRange;

use super::lexer::{LexError, Lexer, Token, TokenKind};
use super::{GreenNode, NodeKind};

mod event;
mod marker;
mod sink;
mod source;

// XXX: For events: https://lunacookies.github.io/lang/13/:
// > Crucially, it is at this step that we locate the whitespace tokens missing from the parser’s
// > events, allowing us to add them to the syntax tree as necessary. This way we can handle
// > whitespace in one place, divorcing it from the parser.
//
// > Events can make our parser faster if we have a previous parse tree lying around; rather than
// > constructing a new parse tree every time, we can patch an existing one with the events of the
// > new parse.

/// CST parser error
// TODO: If possible, errors are not reported during parsing and are postponed for a separate validation step. For example, parser accepts visibility modifiers on trait methods, but then a separate tree traversal flags all such visibilities as erroneous.
#[derive(thiserror::Error, Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParserErrorKind {
    /// The parser supports lexer errors but should stop upon meeting one
    #[error("Lexer error: {0}")]
    Lex(#[from] LexError),
    #[error("Expected top-level item")]
    ExpectedToplevelItem,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ParseError {
    inner: ParserErrorKind,
    text_range: TextRange,
}

/// CST parser builder.
#[derive(Debug)]
struct Parser<'t, 'input> {
    /// The token source
    source: Source<'t, 'input>,
    /// Events to feed into [`Sink`]
    events: Vec<Event>,
}

impl<'t, 'input> Parser<'t, 'input> {
    /// Creates and initializes a parser builder.
    pub fn new(tokens: &'t [Token<'input>]) -> Self {
        Self {
            source: Source::new(tokens),
            events: Vec::new(),
        }
    }

    /// Starts a node using a [`Marker`].
    fn start(&mut self) -> Marker {
        let pos = self.events.len();
        self.events.push(Event::Placeholder);

        Marker::new(pos)
    }

    fn error(&mut self, error: impl Into<ParserErrorKind>, text_range: TextRange) {
        self.events.push(Event::Error(ParseError {
            inner: error.into(),
            text_range,
        }))
    }

    /// Peeks ahead at the next token's kind.
    ///
    /// Returns None on EOF
    fn peek(&mut self) -> Option<TokenKind> {
        self.source.peek_kind()
    }

    /// Returns true if `kind` is the next token's kind.
    fn at(&mut self, kind: TokenKind) -> bool {
        self.peek() == Some(kind)
    }

    /// Returns true if at the end-of-file.
    // TODO: Rust-analyzer has a token for EOF, should I?
    #[allow(dead_code)]
    fn at_end(&mut self) -> bool {
        self.peek().is_none()
    }

    /// Adds the next token to the parse tree.
    fn bump(&mut self) {
        self.source.next_token().unwrap();
        self.events.push(Event::AddToken)
    }
}

fn parse_dt_node(p: &mut Parser) {
    let m = p.start();
    if p.at(TokenKind::Ampersand) {
        p.bump();
    }
    match p.peek().unwrap() {
        TokenKind::Slash => {}
        TokenKind::Ident => {
            // parse a node
        }
        _ => {}
    }
    m.complete(p, NodeKind::DtNode);
}

/// TODO: mod_contents: while !(p.at(EOF) || (p.at(T!['}']) && stop_on_r_curly)) {
fn parse_toplevel_item(p: &mut Parser) -> Option<()> {
    match p.peek()? {
        TokenKind::Slash => {
            // can be a node or a directive
        }
        TokenKind::Ident => {
            // parse a node
            parse_dt_node(p);
        }
        TokenKind::Ampersand => {
            // parse a node
            parse_dt_node(p);
        }
        _ => {
            //let text_range = parser.lexer.peek().unwrap().2;
            let text_range = (0..0).into(); // FIXME
            p.error(ParserErrorKind::ExpectedToplevelItem, text_range);
        }
    }
    p.bump();
    Some(())
}

pub fn parse_api(input: &str) -> (GreenNode, Vec<ParseError>) {
    let tokens: Vec<_> = Lexer::new(input).collect();
    let mut parser = Parser::new(&tokens);

    while parse_toplevel_item(&mut parser).is_some() {}

    let sink = Sink::new(&tokens, parser.events);
    (sink.finish(), Vec::new())
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use crate::cst2::{GreenItem, GreenToken, TokenText};

    use super::*;
    use pretty_assertions::assert_eq;

    fn node(kind: NodeKind, children: Vec<GreenItem>) -> GreenItem {
        GreenItem::Node(Arc::new(GreenNode { kind, children }))
    }

    fn token(kind: TokenKind, text: TokenText) -> GreenItem {
        GreenItem::Token(Arc::new(GreenToken { kind, text }))
    }

    #[test]
    fn parse_full() {
        // TODO: make the parser and add the tokens
        /*assert_eq!(
            parse_api("/dts-v1/;").0.children,
            vec![node(NodeKind::Directive, vec![
                // add tokens here
            ])]
        );*/
    }

    #[test]
    fn parse_trivia() {
        assert_eq!(
            parse_api("  ").0.children,
            vec![token(
                TokenKind::Whitespace,
                TokenText::Dynamic("  ".to_owned())
            )]
        );
        assert_eq!(
            parse_api("/* test */ // test").0.children,
            vec![
                token(
                    TokenKind::BlockComment,
                    TokenText::Dynamic("/* test */".to_owned())
                ),
                token(TokenKind::Whitespace, TokenText::Dynamic(" ".to_owned())),
                token(
                    TokenKind::LineComment,
                    TokenText::Dynamic("// test".to_owned())
                )
            ]
        );
    }

    #[test]
    fn builder_api() {
        let tokens: Vec<_> = Lexer::new("ident").collect();
        let mut parser = Parser::new(&tokens);

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
            output,
            GreenNode {
                kind: NodeKind::Document,
                children: vec![node(
                    NodeKind::DtNode,
                    vec![node(
                        NodeKind::DtProperty,
                        vec![node(
                            NodeKind::DtCell,
                            vec![token(
                                TokenKind::Ident,
                                TokenText::Dynamic("ident".to_owned())
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
        let mut parser = Parser::new(&tokens);

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
            output,
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
                                    token(TokenKind::Ident, TokenText::Dynamic("hello".to_owned())),
                                    token(
                                        TokenKind::Whitespace,
                                        TokenText::Dynamic(" ".to_owned())
                                    )
                                ]
                            ),
                            token(TokenKind::Ident, TokenText::Dynamic("world".to_owned()))
                        ]
                    )]
                )]
            }
        );
    }
}
