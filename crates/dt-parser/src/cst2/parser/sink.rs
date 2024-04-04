use std::{mem, sync::Arc};

use crate::cst2::{
    lexer::{Token, TokenKind},
    GreenItem, GreenNode, GreenToken, NodeKind, TokenText,
};

use super::event::Event;

pub(super) struct Sink<'t, 'input> {
    /// The incoming tokens for trivia handling
    tokens: &'t [Token<'input>],
    /// The incoming events
    events: Vec<Event>,
    /// Token index for trivia handling
    cursor: usize,
    /// Stack of parent nodes
    stack: Vec<GreenNode>,
    /// The current node
    current_node: GreenNode,
}

impl<'t, 'input> Sink<'t, 'input> {
    pub(super) fn new(tokens: &'t [Token<'input>], events: Vec<Event>) -> Self {
        Self {
            tokens,
            events,
            cursor: 0,
            current_node: GreenNode {
                kind: NodeKind::Document,
                children: Vec::new(),
            },
            stack: Vec::new(),
        }
    }

    pub(super) fn finish(mut self) -> GreenNode {
        if self.events.is_empty() {
            self.eat_trivia();
        }
        for idx in 0..self.events.len() {
            match std::mem::replace(&mut self.events[idx], Event::Placeholder) {
                Event::StartNode {
                    kind,
                    forward_parent,
                } => {
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
                Event::AddToken => self.token(),
                Event::FinishNode => {
                    let next_current_node = self.stack.pop().unwrap();
                    let old_current_node =
                        std::mem::replace(&mut self.current_node, next_current_node);
                    self.current_node
                        .children
                        .push(GreenItem::Node(Arc::new(old_current_node)));
                }
                Event::Placeholder => {}
                Event::Error(_err) => {
                    // TODO: implement error sink
                }
            }

            self.eat_trivia();
        }

        //(self.current_node, self.errors)
        self.current_node
    }

    fn start_node(&mut self, kind: NodeKind) {
        let new_current_node = GreenNode {
            kind,
            children: Vec::new(),
        };
        self.stack
            .push(std::mem::replace(&mut self.current_node, new_current_node));
    }

    fn token(&mut self) {
        let Token { kind, text, .. } = self.tokens.get(self.cursor).unwrap();

        let kind = match kind {
            Ok(kind) => *kind,
            Err(_err) => {
                // TODO: add lexer error
                TokenKind::LexerError
            }
        };

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
