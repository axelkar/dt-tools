//! Module to help with implementing the grammar. Ties together the token source, the syntax tree
//! sink and errors.
//!
//! - <https://rust-analyzer.github.io/blog/2020/09/16/challeging-LR-parsing.html#error-resilience>

use std::sync::Arc;

pub(crate) use marker::{CompletedMarker, Marker};
use smallvec::SmallVec;
#[cfg(feature = "grammar-tracing")]
use tracing::debug;

use self::{event::Event, source::Source};
use crate::{
    TextRange, ast,
    cst::{GreenNode, NodeKind, RedNode},
    grammar,
    lexer::{Lexer, TokenKind},
};

mod errors;
mod event;
mod marker;
mod sink;
mod source;

pub(crate) use errors::{ErrorBuilder, Expected};
pub use errors::{ParseError, WrappedLexError};

const NAME_SET: [TokenKind; 4] = [
    TokenKind::Ident,
    TokenKind::Number,
    TokenKind::Comma,
    TokenKind::Minus,
];
const LABEL_NAME_SET: [TokenKind; 2] = [TokenKind::Ident, TokenKind::Number];

// TODO: Event incremental reparse:
// > Events can make our parser faster if we have a previous parse tree lying around; rather than
// > constructing a new parse tree every time, we can patch an existing one with the events of the
// > new parse.
// kinda like duct-taping different parts of the parse tree together

// TODO: take inspiration from rust-analyzer syntax::Parse
// TODO: Parse::ok
// TODO: remove 'input bound so I can just clone this wherever

/// The result from the parser.
///
/// To traverse the syntax tree up and down, wrap `green_node` with [`RedNode`](crate::cst::RedNode).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Parse<'input> {
    pub green_node: GreenNode,
    pub lex_errors: Vec<WrappedLexError<'input>>,
    pub errors: Vec<ParseError>,
}
impl Parse<'_> {
    /// # Panics
    ///
    /// This function panics when you haven't parsed using [`Entrypoint::SourceFile`].
    #[must_use]
    pub fn source_file(&self) -> ast::SourceFile {
        use ast::AstNode as _;
        ast::SourceFile::cast(self.red_node()).unwrap()
    }

    /// Returns a [`RedNode`] of the root node.
    #[must_use]
    pub fn red_node(&self) -> Arc<RedNode> {
        RedNode::new(Arc::new(self.green_node.clone()))
    }

    /// Removes any references to the input.
    pub fn into_static(self) -> Parse<'static> {
        Parse {
            green_node: self.green_node,
            lex_errors: self
                .lex_errors
                .into_iter()
                .map(WrappedLexError::into_static)
                .collect(),
            errors: self.errors,
        }
    }
}

/// Parser entrypoint system to enable parsing macro output
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Entrypoint {
    /// Parses into an [`ast::SourceFile`]
    SourceFile,
    /// Parses into a [`NodeKind::EntryName`]
    // TODO: labels + name?
    Name,
    /// Parses into a [`NodeKind::DtPhandle`]
    ///
    /// Please note that `DtPhandle`s returned don't have ampersands.
    ReferenceNoamp,
    /// Parses into a [`NodeKind::EntryPropValues`] with [`ast::PropValue`]s
    PropValues,
    /// Parses into a [`NodeKind::EntryCells`] with [`ast::Cell`]s
    Cells,
    /// Parses into a [`NodeKind::EntryPreprocessorConditional`] with an [`ast::Expr`].
    PreprocessorConditional,
}
impl Entrypoint {
    /// Parses the input according to the entrypoint.
    #[must_use]
    pub fn parse(self, input: &str) -> Parse<'_> {
        let span = tracy_client::span!("parser::parse");
        span.emit_text(&format!("Entrypoint: {self:?}"));

        // TODO: typesafe Entrypoint -> correct AST struct
        let tokens: Vec<_> = {
            let _span = tracy_client::span!("parser::parse::lex");
            Lexer::new(input).collect()
        };

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
        let p = &mut parser;

        let root_kind = {
            let _span = tracy_client::span!("parser::grammar");
            match self {
                Self::SourceFile => {
                    grammar::entry_sourcefile(p);
                    NodeKind::SourceFile
                }
                Self::Name => {
                    grammar::entry_name(p);
                    NodeKind::EntryName
                }
                Self::ReferenceNoamp => {
                    grammar::reference_noamp(p);
                    NodeKind::DtPhandle
                }
                Self::PropValues => {
                    grammar::propvalues(p, &[]).ok();
                    NodeKind::EntryPropValues
                }
                Self::Cells => {
                    grammar::cells::<true>(p).ok();
                    NodeKind::EntryCells
                }
                Self::PreprocessorConditional => {
                    grammar::expr::entry_preprocessor_conditional(p);
                    NodeKind::EntryPreprocessorConditional
                }
            }
        };

        // Add all tokens to CST and error
        if !p.at_end() {
            p.add_expected(Expected::Eof);
            let m = p.start();

            p.error().msg_expected().emit();

            // Bump all
            while !p.at_end() {
                p.bump();
            }

            m.complete(p, NodeKind::ParseError);
        }

        let sink = sink::Sink::new(&tokens, parser.events, root_kind);

        sink.finish()
    }
}

/// Parses the `input` as an [`Entrypoint::SourceFile`].
#[must_use]
pub fn parse(input: &str) -> Parse<'_> {
    Entrypoint::SourceFile.parse(input)
}

/// CST parser.
#[derive(Debug)]
pub(crate) struct Parser<'t, 'input> {
    /// The token source.
    source: Source<'t, 'input>,
    /// Events to feed into [`Sink`](sink::Sink).
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
        Marker::new(self)
    }

    /// Peeks ahead at the current token's kind.
    ///
    /// Returns None on EOF
    #[inline]
    fn peek_immediate(&mut self) -> Option<TokenKind> {
        self.source.peek_kind_immediate()
    }

    /// Peeks ahead at the current token's kind.
    ///
    /// Returns None on EOF
    #[inline]
    pub fn peek(&mut self) -> Option<TokenKind> {
        self.source.peek_kind()
    }

    /// See [`Check::at`].
    pub fn at(&mut self, matcher: impl TokenMatcher) -> bool {
        self.check(matcher).at()
    }

    /// Returns true if `kind` is the current token's kind.
    pub fn at_immediate(&mut self, kind: TokenKind) -> bool {
        self.expected.push(Expected::Kind(kind));
        self.peek_immediate() == Some(kind)
    }

    /// See [`Check::eat`].
    pub fn eat(&mut self, matcher: impl TokenMatcher) -> bool {
        self.check(matcher).eat()
    }

    /// See [`Check::eat_starting`].
    #[must_use]
    pub fn eat_starting(&mut self, matcher: impl TokenMatcher) -> Option<Marker> {
        self.check(matcher).eat_starting()
    }

    /// Bumps when `kind` is the current token's kind and errors otherwise.
    ///
    /// Returns true if a token was bumped.
    pub fn expect(&mut self, matcher: impl TokenMatcher) -> bool {
        #[cfg(feature = "grammar-tracing")]
        debug!(?kind, "expect");
        self.check(matcher).expect()
    }

    /// Returns an error builder, which will report the current token (the one that is tested by
    /// [`Parser::at`]) as an error.
    ///
    /// Note that everything is only done at [`ErrorBuilder::emit`].
    #[inline]
    #[must_use]
    pub fn error(&mut self) -> ErrorBuilder<'_, 't, 'input, errors::MessageMissing> {
        ErrorBuilder::new(self)
    }

    // For the error builder
    fn emit_parse_error(&mut self, error: ParseError) {
        self.events.push(Event::Error(error));
    }
    fn current_token_error_range(&mut self) -> TextRange {
        if self.at_end() {
            // Emit the error at the second-last character before EOF, if possible. This is so the
            // errors show consistently in editors that don't display the trailing newline.
            self.source
                .last_token_range()
                .map_or(TextRange { start: 0, end: 0 }, |tr| TextRange {
                    start: tr.end - 1,
                    end: tr.end,
                })
        } else if let [Expected::Kind(TokenKind::Semicolon | TokenKind::Comma)]
        | [
            Expected::Kind(TokenKind::Comma),
            Expected::Kind(TokenKind::Semicolon),
        ] = self.expected.as_slice()
        {
            // TODO: don't hardcode it here and make it explicit in the grammar
            // Display the error at the start of the token after the previous non-trivia token
            let pos = self.source.prev_next_range().unwrap().start;
            TextRange {
                start: pos,
                end: pos + 1,
            }
        } else {
            self.source.range().unwrap()
        }
    }

    /// Returns the range of the current token if it exists.
    pub fn range(&self) -> Option<crate::TextRange> {
        self.source.range()
    }

    /// Returns true if at a macro invocation with arguments.
    pub fn silent_at_macro_invocation_with_args(&mut self) -> bool {
        self.check(TokenKind::Ident).silent().at()
            && self.source.peek_immediate_next_kind() == Some(TokenKind::LParen)
    }

    #[inline]
    pub fn add_expected(&mut self, expected: Expected) {
        self.expected.push(expected);
    }

    pub fn text(&mut self) -> Option<&str> {
        self.source.peek_text()
    }

    #[inline]
    fn bump_name_generic(&mut self, set: &[TokenKind]) {
        let _span = tracy_client::span!("parser::Parser::bump_name_generic");

        #[cfg(feature = "grammar-tracing")]
        tracing::info!("bump_name_generic");

        assert!(self.check(set).silent().at());

        // Make sure no trivia tokens get into the combined token
        self.source.skip_trivia();

        let mut n_raw_tokens = 0;
        let mut text = String::new();

        while self.peek_immediate().is_some_and(|k| set.contains(&k)) {
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

    /// Returns true if at the end-of-file.
    pub fn at_end(&mut self) -> bool {
        self.peek().is_none()
    }

    /// Adds the current token to the parse tree.
    ///
    /// This equals the result of any previous `peek_*` calls
    pub fn bump(&mut self) {
        let _span = tracy_client::span!("parser::Parser::bump");

        self.expected.clear();

        #[cfg(feature = "grammar-tracing")]
        debug!(pos = self.events.len(), kind = ?self.source.peek_kind(), "push token");

        assert!(self.source.next_token().is_some(), "Tried to bump at EOF");
        self.events.push(Event::AddToken);
    }

    /// Returns a token check builder with the given matcher.
    ///
    /// Defaults to expected-tracking mode; use [`Check::silent()`] to opt out.
    #[must_use]
    pub fn check<M: TokenMatcher>(&mut self, matcher: M) -> Check<'_, 't, 'input, M, false> {
        Check { p: self, matcher }
    }
}

/// Types that can match and consume tokens from the parser.
pub(crate) trait TokenMatcher: Copy {
    /// Returns true if the current token matches this matcher.
    ///
    /// Does NOT push to the expected list.
    fn matches(self, p: &mut Parser) -> bool;

    /// Push expected entries for error messages.
    fn push_expected(self, p: &mut Parser);

    /// Consume the matched token(s). Only called after `matches` returned true.
    fn consume(self, p: &mut Parser);
}

/// Builder returned by [`Parser::check()`].
pub(crate) struct Check<'p, 't, 'input, M: TokenMatcher, const SILENT: bool> {
    p: &'p mut Parser<'t, 'input>,
    matcher: M,
}

impl<'p, 't, 'input, M: TokenMatcher> Check<'p, 't, 'input, M, false> {
    /// Tests and consumes, or emits an error.
    pub fn expect(&mut self) -> bool {
        if self.eat() {
            true
        } else {
            self.p.error().msg_expected().emit();
            false
        }
    }

    /// Same as [`Self::expect()`] except bumps when not at `recovery`.
    ///
    /// Use this when you know that `recovery` will be bumped.
    pub fn expect_recoverable(&mut self, recovery: impl TokenMatcher) -> bool {
        if self.eat() {
            true
        } else {
            if recovery.matches(self.p) {
                self.p.error().msg_expected().emit();
            } else {
                self.p.error().msg_expected().bump_wrap_err().emit();
            }
            false
        }
    }

    /// Switch to silent mode (no expected-list pollution).
    #[must_use]
    pub fn silent(self) -> Check<'p, 't, 'input, M, true> {
        Check {
            p: self.p,
            matcher: self.matcher,
        }
    }
}

impl<M: TokenMatcher, const SILENT: bool> Check<'_, '_, '_, M, SILENT> {
    /// Tests whether the matcher matches.
    pub fn at(&mut self) -> bool {
        let _span = tracy_client::span!("parser::Check::at");

        if !SILENT {
            self.matcher.push_expected(self.p);
        }
        self.matcher.matches(self.p)
    }

    /// Tests and consumes.
    pub fn eat(&mut self) -> bool {
        if self.at() {
            self.matcher.consume(self.p);
            true
        } else {
            false
        }
    }

    /// Same as [`Self::eat`] except starts a node between testing and consuming.
    #[must_use]
    pub fn eat_starting(&mut self) -> Option<Marker> {
        if self.at() {
            let m = self.p.start();
            self.matcher.consume(self.p);
            Some(m)
        } else {
            None
        }
    }
}

impl TokenMatcher for TokenKind {
    fn matches(self, p: &mut Parser) -> bool {
        p.peek() == Some(self)
    }
    fn push_expected(self, p: &mut Parser) {
        p.add_expected(Expected::Kind(self));
    }
    fn consume(self, p: &mut Parser) {
        p.bump();
    }
}

impl TokenMatcher for &[TokenKind] {
    fn matches(self, p: &mut Parser) -> bool {
        p.peek().is_some_and(|k| self.contains(&k))
    }
    fn push_expected(self, p: &mut Parser) {
        p.expected.extend(self.iter().copied().map(Expected::Kind));
    }
    fn consume(self, p: &mut Parser) {
        p.bump();
    }
}
impl<const N: usize> TokenMatcher for &[TokenKind; N] {
    fn matches(self, p: &mut Parser) -> bool {
        p.peek().is_some_and(|k| self.contains(&k))
    }
    fn push_expected(self, p: &mut Parser) {
        p.expected.extend(self.iter().copied().map(Expected::Kind));
    }
    fn consume(self, p: &mut Parser) {
        p.bump();
    }
}

/// Matches a devicetree name token.
#[derive(Clone, Copy)]
pub(crate) struct Name;

impl TokenMatcher for Name {
    fn matches(self, p: &mut Parser) -> bool {
        NAME_SET.matches(p)
    }
    fn push_expected(self, p: &mut Parser) {
        p.add_expected(Expected::Kind(TokenKind::Name));
    }
    fn consume(self, p: &mut Parser) {
        p.bump_name_generic(&NAME_SET);
    }
}

/// Matches a devicetree label name token.
#[derive(Clone, Copy)]
pub(crate) struct LabelName;

impl TokenMatcher for LabelName {
    fn matches(self, p: &mut Parser) -> bool {
        LABEL_NAME_SET.matches(p)
    }
    fn push_expected(self, p: &mut Parser) {
        p.add_expected(Expected::LabelName);
    }
    fn consume(self, p: &mut Parser) {
        p.bump_name_generic(&LABEL_NAME_SET);
    }
}

#[cfg(feature = "visualize")]
pub mod visualizer {
    use std::cell::RefCell;

    use crate::lexer::{LexError, TokenKind};

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct OwnedToken {
        pub kind: Result<TokenKind, LexError>,
        pub text: String,
        pub text_range: crate::TextRange,
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
    use expect_test::{Expect, expect};

    use super::{sink::Sink, *};
    use crate::{cst::NodeKind, lexer::Lexer};

    #[track_caller]
    #[expect(clippy::needless_pass_by_value, reason = "ergonomics")]
    fn check_parse_output(parse_output: Parse, expect: Expect) {
        expect.assert_eq(&format!(
            "Errors: {:#?}

Tree:
{}",
            parse_output.errors,
            parse_output.green_node.print_tree()
        ));
    }

    #[test]
    fn builder_api_lex_error() {
        let tokens: Vec<_> = Lexer::new("\"abc").collect();
        let mut parser = Parser::new(Source::new(&tokens));

        while !parser.at_end() {
            parser.bump();
        }

        assert_eq!(parser.events.len(), 1);

        let output = Sink::new(&tokens, parser.events, NodeKind::SourceFile).finish();

        check_parse_output(
            output,
            expect![[r#"
            Errors: []

            Tree:
            SourceFile@0..4
              Unrecognized@0..4 "\"abc"
        "#]],
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
        let output = Sink::new(&tokens, parser.events, NodeKind::SourceFile).finish();
        check_parse_output(
            output,
            expect![[r#"
            Errors: []

            Tree:
            SourceFile@0..5
              DtNode@0..5
                DtProperty@0..5
                  DtCellList@0..5
                    Ident@0..5 "ident"
        "#]],
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
        let output = Sink::new(&tokens, parser.events, NodeKind::SourceFile).finish();
        check_parse_output(
            output,
            expect![[r#"
            Errors: []

            Tree:
            SourceFile@0..11
              DtNode@0..11
                DtProperty@0..11
                  DtCellList@0..5
                    Ident@0..5 "hello"
                  Whitespace@5..6 " "
                  Ident@6..11 "world"
        "#]],
        );
    }

    #[test]
    fn silent_at_macro_invocation_with_args() {
        let tokens: Vec<_> = Lexer::new(" foo (").collect();
        let mut parser = Parser::new(Source::new(&tokens));

        assert!(!parser.silent_at_macro_invocation_with_args());

        let tokens: Vec<_> = Lexer::new(" foo(").collect();
        let mut parser = Parser::new(Source::new(&tokens));

        assert!(parser.silent_at_macro_invocation_with_args());
    }

    #[test]
    fn builder_api_prev_next() {
        let tokens: Vec<_> = Lexer::new("{\n  // foo").collect();
        let mut parser = Parser::new(Source::new(&tokens));

        parser.eat(TokenKind::LCurly);
        parser.expect(TokenKind::RCurly);

        let output = Sink::new(&tokens, parser.events, NodeKind::SourceFile).finish();

        // FIXME: this is NOT an example for prev_next currently. expect comma, semicolon or both!!
        // oh and the code that does it will be defined in the grammar instead of the parser later
        check_parse_output(
            output,
            expect![[r#"
                Errors: [
                    ParseError {
                        message: "Expected ‘}’, but found end-of-file",
                        primary_text_range: TextRange {
                            start: 9,
                            end: 10,
                        },
                        span_labels: [],
                    },
                ]

                Tree:
                SourceFile@0..10
                  LCurly@0..1 "{"
                  Whitespace@1..4 "\n  "
                  LineComment@4..10 "// foo"
            "#]],
        );
    }
}
