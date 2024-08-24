use crate::cst2::{lexer::TokenKind, NodeKind};

use super::ParseError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Event {
    StartNode {
        kind: NodeKind,
        forward_parent: Option<usize>,
    },
    AddToken,
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
