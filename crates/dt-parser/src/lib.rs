//! # Devicetree parser
//!
//! A crate for parsing [Devicetree][1] source documents into <abbr title="Concrete Syntax Tree">CST</abbr> [nodes](RedNode).
//!
//! [1]: https://www.devicetree.org/

use std::sync::Arc;

pub use span::*;

pub mod ast;
pub mod cst2;
mod span;

pub type SourceId = Arc<str>;
