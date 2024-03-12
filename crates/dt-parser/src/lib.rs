use std::sync::Arc;

use cst::RedNode;
pub use span::*;

pub mod ast;
pub mod cst;
mod printer;
mod span;
pub use printer::Printer;

pub type SourceId = Arc<str>;

/// Parse a document into a [CST node](RedNode)
pub fn parse(text: &str) -> Option<Arc<RedNode>> {
    let (o, e) = cst::parser::parse(text);
    Some(RedNode::new(o.filter(|_| e.is_empty())?))
}
