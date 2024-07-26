use crate::cst2::NodeKind;

use super::ParseError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Event {
    StartNode {
        kind: NodeKind,
        forward_parent: Option<usize>,
    },
    AddToken,
    AddTokenNoTrivia,
    FinishNode,
    Placeholder,
    Error(ParseError),
}
