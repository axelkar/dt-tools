//! Token-level C preprocessing for Devicetree sources.

mod cleaner;
mod lexer;
mod token;

pub use lexer::{LexError, LexErrorAt, LexedFile, PpTokenKind, lex};
pub use token::PpToken;
