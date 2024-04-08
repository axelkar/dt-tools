use std::sync::Arc;

use self::lexer::TokenKind;
use crate::cst::TreeItem;

pub mod grammar;
pub mod lexer;
pub mod parser;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NodeKind {
    Document,
    /// A parse error
    ParseError,
    Directive,
    /// A directive's parameters, if any.
    DirectiveParams,
    PreprocessorDirective,
    DtNode,
    DtProperty,
    DtCell,
    DtLabel,
    DtPhandle,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// A "green node".
pub struct GreenNode {
    /// The kind of node
    pub kind: NodeKind,
    /// The child nodes and tokens
    pub children: Vec<GreenItem>,
}
impl GreenNode {
    /// Print a tree like rust-analyzer's debug AST
    pub fn print_tree(&self) -> String {
        let mut out = String::new();
        self.print_tree_file(&mut out).unwrap();
        out
    }

    /// Print a tree like rust-analyzer's debug AST
    #[inline(always)]
    pub fn print_tree_file(&self, out: &mut impl std::fmt::Write) -> std::fmt::Result {
        self.print_tree_rec(0, 0, out)
    }

    fn print_tree_rec(
        &self,
        level: usize,
        mut offset: usize,
        out: &mut impl std::fmt::Write,
    ) -> std::fmt::Result {
        const INDENT: &str = "  ";
        writeln!(
            out,
            "{}{:?}@{}..{}",
            INDENT.repeat(level),
            self.kind,
            offset,
            offset + self.length()
        )?;

        for child in self.children.iter() {
            match child {
                GreenItem::Node(ref node) => node.print_tree_rec(level + 1, offset, out)?,
                GreenItem::Token(ref token) => writeln!(
                    out,
                    "{}{:?}@{}..{} {:?}",
                    INDENT.repeat(level + 1),
                    token.kind,
                    offset,
                    offset + token.length(),
                    token.text,
                )?,
            }
            offset += child.length();
        }
        Ok(())
    }

    /// Returns an iterator over immediate child nodes.
    pub fn child_nodes(&self) -> impl Iterator<Item = &Arc<GreenNode>> + '_ {
        self.children.iter().flat_map(GreenItem::as_node)
    }

    /// Returns an iterator over immediate child tokens.
    pub fn child_tokens(&self) -> impl Iterator<Item = &Arc<GreenToken>> + '_ {
        self.children.iter().flat_map(GreenItem::as_token)
    }

    /// Returns the length of the textual contents.
    ///
    /// Use with caution: This is a recursive function!
    pub fn length(&self) -> usize {
        self.children.iter().map(|item| item.length()).sum()
    }
}

/// Static or dynamic string
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum TokenText {
    Static(&'static str),
    Dynamic(String),
}
impl std::fmt::Display for TokenText {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Static(str) => std::fmt::Display::fmt(str, f),
            Self::Dynamic(str) => std::fmt::Display::fmt(str, f),
        }
    }
}
impl std::fmt::Debug for TokenText {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Static(str) => std::fmt::Debug::fmt(str, f),
            Self::Dynamic(str) => std::fmt::Debug::fmt(str, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GreenToken {
    pub kind: TokenKind,
    /// The text the token represents
    ///
    /// - [`Either::Left`] represents a static token (e.g. symbol)
    /// - [`Either::Right`] represents a string from the input
    pub text: TokenText,
}
impl GreenToken {
    /// Returns the length of the textual contents.
    #[inline]
    pub fn length(&self) -> usize {
        match &self.text {
            TokenText::Static(s) => s.len(),
            TokenText::Dynamic(s) => s.len(),
        }
    }
}

pub type GreenItem = TreeItem<Arc<GreenNode>, Arc<GreenToken>>;

impl GreenItem {
    /// Returns the length of the textual contents.
    ///
    /// Use with caution: This may recurse on nodes!
    #[inline]
    pub fn length(&self) -> usize {
        match self {
            GreenItem::Node(node) => node.length(),
            GreenItem::Token(token) => token.length(),
        }
    }
}

// TODO: RedNode, RedToken, RedItem AND RedNode should know its text offset
