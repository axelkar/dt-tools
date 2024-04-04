//! # Devicetree parser
//!
//! A crate for parsing [Devicetree][1] documents into <abbr title="Concrete Syntax Tree">CST</abbr> [nodes](RedNode).
//!
//! [1]: https://www.devicetree.org/
//!
//! # Examples
//!
//! ```
//! // Error-resilient parser
//! dt_parser::parse(b";").unwrap();
//! dt_parser::parse(b" ").unwrap();
//! dt_parser::parse(b"").unwrap();
//! dt_parser::parse(b"asdf").unwrap();
//! dt_parser::parse(b"/ {").unwrap();
//! dt_parser::parse(b"/ { a = > }").unwrap();
//! dt_parser::parse(b"/ { a = };").unwrap();
//!
//! let src = "
//! /dts-v1/;
//!
//! / {
//!   a = <1>;
//! };
//! ";
//! let first = dt_parser::parse(src.as_bytes()).unwrap();
//!
//! let not_equal = dt_parser::parse(b"
//! /dts-v1/;
//!
//! / { a = <1>; };
//! ").unwrap();
//!
//! // The concrete syntax tree is not equal since the whitespace is different
//! assert_ne!(first.green, not_equal.green);
//!
//! assert_eq!(first.green.print_tree(src),
//! r#"Document@0..30
//!   Whitespace@0..1 "\n"
//!   Directive@1..10
//!     Slash@1..2 "/"
//!     Ident@2..8
//!       Ident@2..8 "dts-v1"
//!     Slash@8..9 "/"
//!     Semicolon@9..10 ";"
//!   Whitespace@10..12 "\n\n"
//!   DtNode@12..29
//!     Ident@12..13
//!       Ident@12..13 "/"
//!     Whitespace@13..14 " "
//!     LCurly@14..15 "{"
//!     Whitespace@15..18 "\n  "
//!     DtProperty@18..26
//!       Ident@18..19
//!         Ident@18..19 "a"
//!       Whitespace@19..20 " "
//!       Equals@20..21 "="
//!       Whitespace@21..22 " "
//!       DtCell@22..25
//!         LAngle@22..23 "<"
//!         DtNumber@23..24 "1"
//!         RAngle@24..25 ">"
//!       Semicolon@25..26 ";"
//!     Whitespace@26..27 "\n"
//!     RCurly@27..28 "}"
//!     Semicolon@28..29 ";"
//!   Whitespace@29..30 "\n"
//! "#);
//! ```

// The strings are 28-30 bytes yet the CST's are
// (approximated by the get-size crate) more than 1.5KiB each
// !!!, FIXME: Large CST size

use std::sync::Arc;

use cst::RedNode;
pub use span::*;

pub mod ast;
pub mod cst;
pub mod cst2;
mod printer;
mod span;
pub use printer::Printer;

pub type SourceId = Arc<str>;

pub use cst::parser::raw_parse;

/// Parse a document into a [red CST node](RedNode)
///
/// None will be returned if any fatal errors occur (they shouldn't)
pub fn parse(input: &[u8]) -> Option<Arc<RedNode>> {
    Some(RedNode::new(Arc::new(raw_parse(input)?)))
}
