//! The [`Sink`] turns the parser's output to a concrete syntax tree.

use std::{mem, sync::Arc};

// TODO: && and || as joined https://nnethercote.github.io/2022/10/05/quirks-of-rusts-token-representation.html

use crate::{
    cst::{GreenItem, GreenNode, GreenToken, NodeKind, TokenText},
    lexer::{Token, TokenKind},
};

use super::{event::Event, Parse, ParseError, WrappedLexError};

pub(crate) struct Sink<'t, 'input> {
    /// The incoming tokens for trivia handling.
    tokens: &'t [Token<'input>],
    /// The incoming events.
    events: Vec<Event>,
    /// Source token index.
    ///
    /// This is used to automatically get the token kind and text so I don't have to add them to
    /// [`Event::AddToken`]
    cursor: usize,
    /// Stack of parent nodes.
    stack: Vec<GreenNode>,
    /// The current node.
    current_node: GreenNode,
    /// Collected parse errors.
    errors: Vec<ParseError>,
    /// Collected lex errors.
    lex_errors: Vec<WrappedLexError<'input>>,
}

impl<'t, 'input> Sink<'t, 'input> {
    pub(super) fn new(
        tokens: &'t [Token<'input>],
        events: Vec<Event>,
        root_kind: NodeKind,
    ) -> Self {
        Self {
            tokens,
            events,
            cursor: 0,
            stack: Vec::new(),
            current_node: GreenNode {
                kind: root_kind,
                width: 0,
                children: Vec::new(),
            },
            errors: Vec::new(),
            lex_errors: Vec::new(),
        }
    }

    /// Turns the parser's output to a concrete syntax tree.
    pub(super) fn finish(mut self) -> Parse<'input> {
        for idx in 0..self.events.len() {
            #[cfg(feature = "grammar-tracing")]
            let _span = tracing::span!(tracing::Level::TRACE, "loop", pos = idx).entered();

            match std::mem::replace(&mut self.events[idx], Event::Placeholder) {
                Event::StartNode {
                    kind,
                    forward_parent,
                } => {
                    // Eat trivia before starting the node
                    // If trivia is attached, the node's range won't be what you expect
                    self.eat_trivia();

                    #[cfg(feature = "grammar-tracing")]
                    tracing::debug!(?kind, "start node");
                    let mut kinds = vec![kind];

                    let mut idx = idx;
                    let mut forward_parent = forward_parent;

                    // Walk through the forward parent of the forward parent, and the forward parent
                    // of that, and of that, etc. until we reach a StartNode event without a forward
                    // parent.
                    while let Some(fp) = forward_parent {
                        idx += fp;

                        forward_parent = if let Event::StartNode {
                            kind,
                            forward_parent,
                        } =
                            mem::replace(&mut self.events[idx], Event::Placeholder)
                        {
                            kinds.push(kind);
                            forward_parent
                        } else {
                            unreachable!()
                        };
                    }

                    for kind in kinds.into_iter().rev() {
                        self.start_node(kind);
                    }
                }
                Event::AddToken => {
                    self.eat_trivia();
                    self.token();
                }
                Event::AddCombinedToken {
                    kind,
                    n_raw_tokens,
                    text,
                } => {
                    self.eat_trivia();
                    #[cfg(feature = "grammar-tracing")]
                    tracing::debug!(cursor = self.cursor, ?kind, "add combined token");
                    self.current_node.width += text.len();

                    self.current_node
                        .children
                        .push(GreenItem::Token(Arc::new(GreenToken {
                            kind,
                            text: TokenText::Dynamic(text),
                        })));

                    self.cursor += n_raw_tokens;
                }
                Event::FinishNode => {
                    #[cfg(feature = "grammar-tracing")]
                    tracing::debug!(kind = ?self.current_node.kind, "finish node");
                    let next_current_node = self.stack.pop().unwrap();
                    let old_current_node =
                        std::mem::replace(&mut self.current_node, next_current_node);

                    self.current_node.width += old_current_node.width;
                    self.current_node
                        .children
                        .push(GreenItem::Node(Arc::new(old_current_node)));
                }
                Event::Placeholder => {}
                Event::Error(error) => self.errors.push(error),
            }
        }

        // Eat trailing trivia
        self.eat_trivia();

        Parse {
            green_node: self.current_node,
            lex_errors: self.lex_errors,
            errors: self.errors,
        }
    }

    fn start_node(&mut self, kind: NodeKind) {
        let new_current_node = GreenNode {
            kind,
            width: 0,
            children: Vec::new(),
        };
        self.stack
            .push(std::mem::replace(&mut self.current_node, new_current_node));
    }

    fn token(&mut self) {
        let Token {
            kind,
            text,
            text_range,
        } = self.tokens.get(self.cursor).unwrap();

        let kind = match kind {
            Ok(kind) => *kind,
            Err(error) => {
                self.lex_errors.push(WrappedLexError {
                    inner: *error,
                    text_range: *text_range,
                    text,
                });

                TokenKind::Unrecognized
            }
        };
        #[cfg(feature = "grammar-tracing")]
        tracing::debug!(cursor = self.cursor, ?kind, "add token");

        self.current_node.width += text.len();

        self.current_node
            .children
            .push(GreenItem::Token(Arc::new(GreenToken {
                kind,
                text: match kind.static_text() {
                    Some(static_text) => {
                        debug_assert_eq!(
                            static_text, *text,
                            "Static text did not equal to the parsed contents for token {kind:?}"
                        );
                        TokenText::Static(static_text)
                    }
                    None => TokenText::Dynamic((*text).to_owned()),
                },
            })));
        self.cursor += 1;
    }

    fn eat_trivia(&mut self) {
        while let Some(token) = self.tokens.get(self.cursor) {
            let Ok(kind) = token.kind else {
                break;
            };
            if !kind.is_trivia() {
                break;
            }

            self.token();
        }
    }
}
