//! # Devicetree parser
//!
//! A crate for parsing [Devicetree][1] source documents into <abbr title="Concrete Syntax Tree">CST</abbr> [nodes](RedNode).
//!
//! [1]: https://www.devicetree.org/

use std::sync::Arc;

pub use dt_diagnostic::text_range::TextRange;

pub mod ast;
pub mod cst2;

pub type SourceId = Arc<str>;
