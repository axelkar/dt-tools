//! - <https://rust-analyzer.github.io/blog/2020/09/16/challeging-LR-parsing.html#error-resilience>
//! - <https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html>

use std::{borrow::Cow, sync::Arc};

use crate::{ast, TextRange};

use self::event::Event;
use self::source::Source;
use itertools::Itertools;
pub(crate) use marker::{CompletedMarker, Marker};
use smallvec::SmallVec;
use tracing::debug;
#[cfg(feature = "grammar-tracing")]
use tracing::error;

use super::{grammar, lexer::TokenKind, GreenNode, NodeKind};

mod errors;
mod event;
mod marker;
mod sink;
mod source;

pub(crate) use errors::Expected;
pub use errors::{ParseError, WrappedLexError};

// TODO: axka style naming naming lints
const NAME_SET: [TokenKind; 4] = [
    TokenKind::Ident,
    TokenKind::Number,
    TokenKind::Comma,
    TokenKind::Minus,
];

const PREPROCESSOR_DIRECTIVE_SET: [TokenKind; 9] = [
    TokenKind::UndefDirective,
    TokenKind::PragmaDirective,
    TokenKind::ElseDirective,
    TokenKind::EndifDirective,
    TokenKind::IfndefDirective,
    TokenKind::IfdefDirective,
    TokenKind::IfDirective,
    TokenKind::DefineDirective,
    TokenKind::IncludeDirective,
];

// TODO: Event incremental reparse:
// > Events can make our parser faster if we have a previous parse tree lying around; rather than
// > constructing a new parse tree every time, we can patch an existing one with the events of the
// > new parse.
// kinda like duct-taping different parts of the parse tree together

// TODO: take inspiration from rust-analyzer syntax::Parse
// TODO: rust-analyzer syntax::validation
// TODO: Parse::ok
// TODO: remove 'input bound so I can just clone this wherever
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parse<'input> {
    pub green_node: GreenNode,
    pub lex_errors: Vec<WrappedLexError<'input>>,
    pub errors: Vec<ParseError>,
}
impl Parse<'_> {
    pub fn source_file(&self) -> ast::SourceFile {
        use ast::AstNode as _;
        ast::SourceFile::cast(super::RedNode::new(Arc::new(self.green_node.clone())))
            .expect("parser should only output SourceFile")
    }
}

pub fn parse(input: &str) -> Parse {
    use super::lexer::Lexer;

    let tokens: Vec<_> = Lexer::new(input).collect();

    #[cfg(feature = "visualize")]
    visualizer::Event::Init {
        tokens: tokens
            .iter()
            .map(|tok| visualizer::OwnedToken {
                kind: tok.kind,
                text: tok.text.to_owned(),
                text_range: tok.text_range,
            })
            .collect(),
    }
    .visualize();

    let source = Source::new(&tokens);
    let mut parser = Parser::new(source);

    grammar::root(&mut parser);

    let sink = sink::Sink::new(&tokens, parser.events);

    sink.finish()
}

/// CST parser.
#[derive(Debug)]
pub(crate) struct Parser<'t, 'input> {
    /// The token source.
    source: Source<'t, 'input>,
    /// Events to feed into [`Sink`].
    events: Vec<Event>,
    /// The values that are expected at the current position.
    expected: SmallVec<[Expected; 2]>,
}

impl<'t, 'input> Parser<'t, 'input> {
    /// Creates and initializes a parser.
    fn new(source: Source<'t, 'input>) -> Self {
        Self {
            source,
            events: Vec::new(),
            expected: SmallVec::new(),
        }
    }

    /// Starts a node using a [`Marker`].
    #[cfg_attr(debug_assertions, track_caller)]
    pub fn start(&mut self) -> Marker {
        let pos = self.events.len();
        self.events.push(Event::Placeholder);

        #[cfg(feature = "grammar-tracing")]
        debug!(pos, "start node");

        Marker::new(pos)
    }

    /// Peeks ahead at the current token's kind.
    ///
    /// Returns None on EOF
    #[inline(always)]
    fn peek_immediate(&mut self) -> Option<TokenKind> {
        self.source.peek_kind_immediate()
    }

    /// Peeks ahead at the current token's kind.
    ///
    /// Returns None on EOF
    #[inline(always)]
    pub fn peek(&mut self) -> Option<TokenKind> {
        self.source.peek_kind()
    }

    /// Returns true if `kind` is the current token's kind.
    pub fn at(&mut self, kind: TokenKind) -> bool {
        self.expected.push(Expected::Kind(kind));
        self.peek() == Some(kind)
    }

    /// Returns true if `kind` is the current token's kind.
    pub fn at_immediate(&mut self, kind: TokenKind) -> bool {
        self.expected.push(Expected::Kind(kind));
        self.peek_immediate() == Some(kind)
    }

    /// Returns true if `first_kind` is the current token's kind and `second_kind` is the immediate
    /// next token's kind.
    ///
    /// - This does not add kinds to `expected_kinds`.
    pub fn silent_at2(&mut self, first_kind: TokenKind, second_kind: TokenKind) -> bool {
        self.silent_at(first_kind) && self.source.peek_immediate_next_kind() == Some(second_kind)
    }

    /// Bumps and returns true if `kind` is the current token's kind.
    ///
    /// Basically a combination of [`Parser::at`] and [`Parser::bump`]
    pub fn eat(&mut self, kind: TokenKind) -> bool {
        if self.at(kind) {
            self.bump();
            true
        } else {
            false
        }
    }

    /// Returns true if `kind` is the current token's kind.
    ///
    /// - This does not add `kind` to `expected_kinds`.
    pub fn silent_at(&mut self, kind: TokenKind) -> bool {
        self.peek() == Some(kind)
    }

    /// Returns true if the current token's kind is in `set`.
    pub fn at_set(&mut self, set: &[TokenKind]) -> bool {
        self.expected.reserve(set.len());
        for kind in set {
            self.expected.push(Expected::Kind(*kind));
        }
        self.peek().map_or(false, |k| set.contains(&k))
    }

    /// Returns true if the current token's kind is in `set`.
    ///
    /// - This doesn't add `set` to `expected_kinds`.
    pub fn silent_at_set(&mut self, set: &[TokenKind]) -> bool {
        self.peek().map_or(false, |k| set.contains(&k))
    }

    /// Bumps when `kind` is the current token's kind and errors otherwise.
    pub fn expect(&mut self, kind: TokenKind) {
        #[cfg(feature = "grammar-tracing")]
        debug!(?kind, "expect");

        if self.at(kind) {
            self.bump();
        } else {
            self.emit_expect_error();
        }
    }

    pub fn emit_expect_error(&mut self) {
        #[cfg(feature = "grammar-tracing")]
        error!("emit_expect_error");

        use std::fmt::Write;
        let mut message = "Expected ".to_owned();

        self.expected = std::mem::take(&mut self.expected)
            .into_iter()
            .unique()
            .collect::<Vec<_>>()
            .into();

        let num_expected = self.expected.len();

        for (idx, expected_kind) in self.expected.iter().enumerate() {
            let is_first = idx == 0;
            let is_last = idx == num_expected - 1;

            if is_first {
                write!(message, "{}", expected_kind).ok();
            } else if is_last {
                write!(message, " or {}", expected_kind).ok();
            } else {
                write!(message, ", {}", expected_kind).ok();
            }
        }

        if let Some(kind) = self.peek() {
            write!(message, ", but found {}", kind).ok();
        } else {
            write!(message, ", but found end-of-file").ok();
        }

        self.simple_error(message.into());
    }

    /// Errors when the current token's kind isn't `kind`.
    ///
    /// - Doesn't bump when at `recovery_set`.
    pub fn expect_recoverable(&mut self, kind: TokenKind, recovery_set: &[TokenKind]) {
        #[cfg(feature = "grammar-tracing")]
        debug!(?kind, ?recovery_set, "expect_recoverable");

        if self.at(kind) {
            self.bump();
        } else if self.silent_at_set(recovery_set) {
            self.emit_expect_error();
        } else {
            self.error2();
        }
    }

    /// Reports the current token as an expect error.
    // TODO: rename this function to expect_error
    pub fn error2(&mut self) {
        #[cfg(feature = "grammar-tracing")]
        error!("error2");

        self.emit_expect_error();

        if !self.at_end() {
            let e = self.start();
            self.bump();
            e.complete(self, NodeKind::ParseError);
        }
    }

    /// Reports the current token as an error.
    pub fn simple_error(&mut self, message: Cow<'static, str>) {
        self.fancy_error(message, Vec::new())
    }

    /// Returns the range of the current token if it exists.
    pub fn range(&self) -> Option<crate::TextRange> {
        self.source.range()
    }

    /// Reports the current token as an error.
    pub fn fancy_error(
        &mut self,
        message: Cow<'static, str>,
        span_labels: Vec<dt_diagnostic::SpanLabel>,
    ) {
        let primary_span = if self.at_end() {
            let pos = self.source.last_token_range().unwrap().start;
            crate::TextRange {
                start: pos,
                end: pos,
            }
        } else if let [Expected::Kind(TokenKind::RCurly)] = self.expected.as_slice() {
            let offset = self
                .source
                .prev_next_text()
                .unwrap()
                .find('\n')
                .map(|offset| offset + 1)
                .unwrap_or(0);
            let pos = self.source.prev_next_range().unwrap().start + offset;
            TextRange {
                start: pos,
                end: pos + 1,
            }
        } else if let [Expected::Kind(TokenKind::Semicolon | TokenKind::Comma)]
        | [Expected::Kind(TokenKind::Comma), Expected::Kind(TokenKind::Semicolon)] =
            self.expected.as_slice()
        {
            let pos = self.source.prev_next_range().unwrap().start;
            TextRange {
                start: pos,
                end: pos + 1,
            }
        } else {
            self.source.range().unwrap()
        };

        debug!(?primary_span, %message, "fancy_error");

        self.events.push(Event::Error(ParseError {
            message,
            primary_span,
            span_labels,
        }));
    }

    /// Sets the hint for the error.
    ///
    /// Does nothing if the last event was not an error.
    pub fn add_hint(&mut self, hint: Cow<'static, str>) {
        if let Some(Event::Error(err)) = self.events.last_mut() {
            err.span_labels.push(dt_diagnostic::SpanLabel {
                msg: hint,
                span: err.primary_span,
            });
        }
    }

    /// Returns true if at a name token.
    pub fn at_name(&mut self) -> bool {
        if self.silent_at_set(&NAME_SET) {
            true
        } else {
            self.expected.push(Expected::Kind(TokenKind::Name));
            false
        }
    }

    /// Returns true if at a preprocessor directive token.
    pub fn at_preprocessor_directive(&mut self) -> bool {
        if self.silent_at_set(&PREPROCESSOR_DIRECTIVE_SET) {
            true
        } else {
            self.expected.push(Expected::PreprocessorDirective);
            false
        }
    }

    #[inline]
    pub fn add_expected(&mut self, expected: Expected) {
        self.expected.push(expected)
    }

    /// Bumps name tokens into [`TokenKind::Name`].
    pub fn bump_name(&mut self) {
        #[cfg(feature = "grammar-tracing")]
        tracing::info!("bump_name");

        assert!(self.silent_at_set(&NAME_SET));

        // Make sure no trivia tokens get into the combined token
        self.source.skip_trivia();

        //let name_m = self.start();
        let mut n_raw_tokens = 0;
        let mut text = String::new();

        while self
            .peek_immediate()
            .map_or(false, |k| NAME_SET.contains(&k))
        {
            let token = self.source.next_token().expect("Tried to bump at EOF");
            n_raw_tokens += 1;
            text.push_str(token.text);
        }

        self.events.push(Event::AddCombinedToken {
            kind: TokenKind::Name,
            n_raw_tokens,
            text,
        });

        // Not cleared at any other point
        self.expected.clear();
    }

    /// Bumps and returns true if at a name token.
    ///
    /// Basically a combination of [`Parser::at_name`] and [`Parser::bump_name`]
    pub fn eat_name(&mut self) -> bool {
        #[cfg(feature = "grammar-tracing")]
        debug!("eat_name");

        if self.at_name() {
            self.bump_name();
            true
        } else {
            false
        }
    }

    /// Returns true if at the end-of-file.
    // TODO: Rust-analyzer has a token for EOF, should I?
    pub fn at_end(&mut self) -> bool {
        self.peek().is_none()
    }

    /// Adds the current token to the parse tree.
    ///
    /// This equals the result of any previous `peek_*` calls
    pub fn bump(&mut self) {
        self.expected.clear();

        #[cfg(feature = "grammar-tracing")]
        debug!(pos = self.events.len(), kind = ?self.source.peek_kind(), "push token");

        assert!(self.source.next_token().is_some(), "Tried to bump at EOF");
        self.events.push(Event::AddToken)
    }
}

#[cfg(feature = "visualize")]
pub mod visualizer {
    use super::*;
    use std::cell::RefCell;

    use crate::cst2::lexer::{LexError, TokenKind};

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct OwnedToken {
        pub kind: Result<TokenKind, LexError>,
        pub text: String,
        pub text_range: TextRange,
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum Event {
        Init {
            tokens: Vec<OwnedToken>,
        },
        /// Always clears steps
        NextToken {
            cursor: usize,
            prev_next_cursor: usize,
        },
        /// Always increases steps
        PeekKindImmediate,
        /// Clears steps if cursor has increased
        SkippedTrivia {
            cursor: usize,
        },
        GramBegin(&'static str),
        GramEnd(&'static str),
    }
    impl Event {
        pub fn visualize(self) {
            VEC.with_borrow_mut(|vec| vec.push(self));
        }
    }

    thread_local! {
        static VEC: RefCell<Vec<Event>> = const { RefCell::new(Vec::new()) };
    }

    pub fn take_events() -> Vec<Event> {
        VEC.with_borrow_mut(std::mem::take)
    }
}

#[cfg(test)]
mod tests {
    use super::sink::Sink;
    use crate::cst2::lexer::{LexError, Lexer};
    use crate::cst2::{GreenNode, NodeKind};

    use super::*;
    use grammar::tests::{dynamic_token, node, static_token};
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
                    kind: NodeKind::SourceFile,
                    width: "\"abc".len(),
                    children: vec![dynamic_token(TokenKind::Unrecognized, "\"abc")]
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
        m_cell.complete(&mut parser, NodeKind::DtCellList);
        m_node.complete(&mut parser, NodeKind::DtNode);
        m_prop.complete(&mut parser, NodeKind::DtProperty);

        // FIXME: with Sink code
        let output = Sink::new(&tokens, parser.events).finish();
        assert_eq!(
            output.green_node,
            GreenNode {
                kind: NodeKind::SourceFile,
                width: "ident".len(),
                children: vec![node(
                    NodeKind::DtNode,
                    vec![node(
                        NodeKind::DtProperty,
                        vec![node(
                            NodeKind::DtCellList,
                            vec![dynamic_token(TokenKind::Ident, "ident")]
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

        let completed_cell = m_cell.complete(&mut parser, NodeKind::DtCellList);

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
                kind: NodeKind::SourceFile,
                width: "hello world".len(),
                children: vec![node(
                    NodeKind::DtNode,
                    vec![node(
                        NodeKind::DtProperty,
                        vec![
                            node(
                                NodeKind::DtCellList,
                                vec![dynamic_token(TokenKind::Ident, "hello"),]
                            ),
                            dynamic_token(TokenKind::Whitespace, " "),
                            dynamic_token(TokenKind::Ident, "world")
                        ]
                    )]
                )]
            }
        );
    }

    #[test]
    fn silent_at2() {
        let tokens: Vec<_> = Lexer::new(" foo (").collect();
        let mut parser = Parser::new(Source::new(&tokens));

        assert!(parser.silent_at2(TokenKind::Ident, TokenKind::Whitespace));

        let tokens: Vec<_> = Lexer::new(" foo(").collect();
        let mut parser = Parser::new(Source::new(&tokens));

        assert!(parser.silent_at2(TokenKind::Ident, TokenKind::LParen));
    }

    #[test]
    fn builder_api_prev_next() {
        let tokens: Vec<_> = Lexer::new("{\n  // foo").collect();
        let mut parser = Parser::new(Source::new(&tokens));

        parser.eat(TokenKind::LCurly);
        parser.expect(TokenKind::RCurly);

        let output = Sink::new(&tokens, parser.events).finish();

        assert_eq!(
            output,
            Parse {
                green_node: GreenNode {
                    kind: NodeKind::SourceFile,
                    width: "{\n  // foo".len(),
                    children: vec![
                        static_token(TokenKind::LCurly),
                        dynamic_token(TokenKind::Whitespace, "\n  "),
                        dynamic_token(TokenKind::LineComment, "// foo"),
                    ]
                },
                lex_errors: Vec::new(),
                errors: vec![ParseError {
                    message: Cow::Borrowed("Expected ‘}’, but found end-of-file"),
                    primary_span: TextRange::new(4, 4),
                    span_labels: Vec::new(),
                }]
            }
        );
    }
}
