use crate::{cst::NodeKind, lexer::TokenKind};

use super::ParseError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Event {
    StartNode {
        kind: NodeKind,
        /// Offset to another `StartNode` event, which should also be started at the current
        /// position.
        ///
        /// This is for supporting [`CompletedMarker::precede`](super::CompletedMarker::precede).
        forward_parent: Option<usize>,
    },
    AddToken,
    /// Turns multiple tokens from the lexer to a single token. Used for e.g. name tokens
    AddCombinedToken {
        kind: TokenKind,
        /// The number of raw tokens to be replaced.
        n_raw_tokens: usize,
        /// The combined text of the tokens.
        ///
        /// This is stored here and not computed for performance.
        text: String,
    },
    FinishNode,
    Placeholder,
    Error(ParseError),
}
