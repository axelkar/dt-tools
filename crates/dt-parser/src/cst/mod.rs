//! I take a lot of inspiration from [cstree] & [rust-analyzer's syntax architecture][1],
//! although I haven't written a single line of unsafe code in this crate.
//!
//!
//! Note: This does NOT [leak memory] and does not use cycled Arcs since [RedNode]s only point to
//! parents and the respective [GreenNode]s.
//!
//! [cstree]: https://lib.rs/crates/cstree
//! [1]: https://github.com/rust-lang/rust-analyzer/blob/5346002d07d09badaf37949bec68012d963d61fc/docs/dev/syntax.md
//! [leak memory]: https://doc.rust-lang.org/std/sync/struct.Arc.html#breaking-cycles-with-weak
// XXX, FIXME: Don't leak memory with cycled Arcs:

use kinds::{NodeKind, TokenKind};
use std::{fmt::Write, sync::Arc};

use crate::TextRange;
pub(super) mod parser;

// TODO: partial reparse?

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// A "green node".
///
/// This is what the parser ([`raw_parse`](crate::raw_parse)) outputs.
pub struct GreenNode {
    /// The kind of node
    pub kind: NodeKind,
    /// The location in the source document
    pub text_range: TextRange,
    /// The child nodes and tokens
    pub children: Vec<GreenItem>,
}
impl GreenNode {
    /// Print a tree like rust-analyzer's debug AST
    pub fn print_tree(&self, source: &str) -> String {
        let mut out = String::new();
        self.print_tree_file(source, &mut out).unwrap();
        out
    }

    /// Print a tree like rust-analyzer's debug AST
    #[inline(always)]
    pub fn print_tree_file(&self, source: &str, out: &mut impl Write) -> std::fmt::Result {
        self.print_tree_rec(0, source, out)
    }

    fn print_tree_rec(&self, level: usize, source: &str, out: &mut impl Write) -> std::fmt::Result {
        const INDENT: &str = "  ";
        writeln!(
            out,
            "{}{:?}@{}",
            INDENT.repeat(level),
            self.kind,
            self.text_range
        )?;

        for child in self.children.iter() {
            match child {
                GreenItem::Node(ref node) => node.print_tree_rec(level + 1, source, out)?,
                GreenItem::Token(ref token) => writeln!(
                    out,
                    "{}{:?}@{} {:?}",
                    INDENT.repeat(level + 1),
                    token.kind,
                    token.text_range,
                    token.text_range.text(source).unwrap_or_default()
                )?,
            }
        }
        Ok(())
    }

    /// Returns an iterator over immediate child nodes
    pub fn child_nodes(&self) -> impl Iterator<Item = &Arc<GreenNode>> + '_ {
        self.children.iter().flat_map(GreenItem::as_node)
    }

    /// Returns an iterator over immediate child tokens
    pub fn child_tokens(&self) -> impl Iterator<Item = &Arc<GreenToken>> + '_ {
        self.children.iter().flat_map(GreenItem::as_token)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GreenToken {
    pub kind: TokenKind,
    pub text_range: TextRange,
}

pub type GreenItem = TreeItem<Arc<GreenNode>, Arc<GreenToken>>;
pub type RedItem = TreeItem<Arc<RedNode>, Arc<RedToken>>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TreeItem<Node, Token> {
    Node(Node),
    Token(Token),
}
impl<Node, Token> TreeItem<Node, Token> {
    pub fn as_node(&self) -> Option<&Node> {
        match self {
            Self::Node(node) => Some(node),
            _ => None,
        }
    }
    pub fn as_token(&self) -> Option<&Token> {
        match self {
            Self::Token(token) => Some(token),
            _ => None,
        }
    }
    pub fn into_node(self) -> Option<Node> {
        match self {
            Self::Node(node) => Some(node),
            _ => None,
        }
    }
    pub fn into_token(self) -> Option<Token> {
        match self {
            Self::Token(token) => Some(token),
            _ => None,
        }
    }
    pub fn map_node<NewNode>(self, f: impl FnOnce(Node) -> NewNode) -> TreeItem<NewNode, Token> {
        match self {
            Self::Node(node) => TreeItem::Node(f(node)),
            Self::Token(token) => TreeItem::Token(token),
        }
    }
    pub fn map_token<NewToken>(
        self,
        f: impl FnOnce(Token) -> NewToken,
    ) -> TreeItem<Node, NewToken> {
        match self {
            Self::Node(node) => TreeItem::Node(node),
            Self::Token(token) => TreeItem::Token(f(token)),
        }
    }
    pub fn filter_node(self, f: impl FnOnce(&Node) -> bool) -> Option<Self> {
        match self {
            Self::Node(node) if !f(&node) => None,
            other => Some(other),
        }
    }
    pub fn filter_token(self, f: impl FnOnce(&Token) -> bool) -> Option<Self> {
        match self {
            Self::Token(token) if !f(&token) => None,
            other => Some(other),
        }
    }
}
impl RedItem {
    pub fn text_range(&self) -> TextRange {
        match self {
            Self::Node(node) => node.green.text_range,
            Self::Token(token) => token.green.text_range,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct RedToken {
    pub parent: Arc<RedNode>,
    pub green: Arc<GreenToken>,
}
impl std::fmt::Debug for RedToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RedToken")
            .field("green", &self.green)
            .finish()
    }
}
impl RedToken {
    pub fn text_range(&self) -> &TextRange {
        &self.green.text_range
    }

    pub fn text<'i>(&self, src: &'i str) -> Option<&'i str> {
        self.green.text_range.text(src)
    }

    /// Iterator over all the ancestors of this token excluding itself.
    pub fn parent_ancestors(&self) -> impl Iterator<Item = Arc<RedNode>> {
        std::iter::successors(Some(self.parent.clone()), |node| node.parent.clone())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// A "red node".
///
/// This references the green nodes and its own parent nodes.
///
/// You basically get a tree you can go up and down in without cyclic references!
pub struct RedNode {
    /// The parent node if it exists.
    ///
    /// All of the `child_` functions set this to `self` for their children.
    pub parent: Option<Arc<RedNode>>,
    /// The respective [`GreenNode`].
    pub green: Arc<GreenNode>,
}

impl RedNode {
    /// Creates a new red node with no parent.
    pub fn new(root: Arc<GreenNode>) -> Arc<Self> {
        Arc::new(RedNode {
            parent: None,
            green: root,
        })
    }

    /// Returns the text range from the green node.
    #[inline(always)]
    pub fn text_range(&self) -> &TextRange {
        &self.green.text_range
    }

    /// Returns the node at the specified byte offset if found.
    pub fn node_at_offset(self: &Arc<RedNode>, offset: usize) -> Option<Arc<RedNode>> {
        // TODO: non recursing algorithm? binary search?
        self.child_nodes()
            .find(|node| node.text_range().byte_range().contains(&offset))
            .map(|node| node.node_at_offset(offset).unwrap_or(node))
    }

    /// Returns the token at the specified byte offset if found.
    pub fn token_at_offset(self: &Arc<RedNode>, offset: usize) -> Option<Arc<RedToken>> {
        // TODO: non recursing algorithm? binary search?
        self.child_nodes()
            .find(|node| node.text_range().byte_range().contains(&offset))
            .and_then(|node| node.token_at_offset(offset))
            .or_else(|| {
                self.child_tokens()
                    .find(|token| token.text_range().byte_range().contains(&offset))
            })
    }

    /// Returns an iterator over all children.
    pub fn children<'a>(self: &'a Arc<RedNode>) -> impl Iterator<Item = RedItem> + 'a {
        let arc = Arc::clone(self);
        self.green
            .children
            .iter()
            .map(move |green_child| match green_child {
                GreenItem::Node(node) => RedItem::Node(Arc::new(RedNode {
                    parent: Some(Arc::clone(&arc)),
                    green: Arc::clone(node),
                })),
                GreenItem::Token(token) => RedItem::Token(Arc::new(RedToken {
                    parent: Arc::clone(&arc),
                    green: Arc::clone(token),
                })),
            })
    }

    /// Returns an iterator over the child nodes.
    pub fn child_nodes<'a>(self: &'a Arc<RedNode>) -> impl Iterator<Item = Arc<RedNode>> + 'a {
        let arc = Arc::clone(self);
        self.green.child_nodes().map(move |green_child| {
            Arc::new(RedNode {
                parent: Some(Arc::clone(&arc)),
                green: Arc::clone(green_child),
            })
        })
    }

    /// Returns an iterator over the child tokens.
    pub fn child_tokens<'a>(self: &'a Arc<RedNode>) -> impl Iterator<Item = Arc<RedToken>> + 'a {
        let arc = Arc::clone(self);
        self.green.child_tokens().map(move |green_child| {
            Arc::new(RedToken {
                parent: Arc::clone(&arc),
                green: Arc::clone(green_child),
            })
        })
    }

    /// Find nodes and tokens corresponding to syntax errors
    #[deprecated = "Use `dt_lint::SyntaxErrors` instead"]
    pub fn find_syntax_errors<'a, 'i: 'a>(
        self: &'a Arc<RedNode>,
        src: &'i str,
    ) -> impl Iterator<Item = (TextRange, MySyntaxError<'i>)> + 'a {
        self.child_tokens()
            .filter_map(|token| {
                Some((
                    token.green.text_range,
                    match token.green.kind {
                        TokenKind::Error if token.parent.green.kind == NodeKind::Ident => {
                            MySyntaxError::MissingIdent
                        }
                        TokenKind::Error => MySyntaxError::TokenError,
                        TokenKind::SeparatedMissingFirst => MySyntaxError::MissingSeparatedFirst,
                        TokenKind::MissingPunct(c) => MySyntaxError::MissingPunct(c),
                        TokenKind::UnexpectedWhitespace => MySyntaxError::UnexpectedWhitespace,
                        TokenKind::UnexpectedItem => {
                            MySyntaxError::UnexpectedItem(token.green.text_range.text(src)?)
                        }
                        _ => return None,
                    },
                ))
            })
            .chain(
                std::iter::once_with(|| {
                    Some((
                        self.green.text_range,
                        match self.green.kind {
                            NodeKind::Error => MySyntaxError::NodeError,
                            NodeKind::InvalidPunct => {
                                MySyntaxError::InvalidPunct(self.green.text_range.text(src)?)
                            }
                            _ => return None,
                        },
                    ))
                })
                .flatten(),
            )
            .chain(
                // FIXME: collect is inefficient but satisfies borrow checker
                self.child_nodes()
                    .flat_map(|node| node.find_syntax_errors(src).collect::<Vec<_>>()),
            )
    }

    /// Iterator over all the ancestors of this node excluding itself.
    pub fn parent_ancestors(&self) -> impl Iterator<Item = Arc<RedNode>> {
        std::iter::successors(self.parent.clone(), |node| node.parent.clone())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
/// Kinds of errors returned from [`RedNode::find_syntax_errors`]
// TODO: remove since dt_lint exists
pub enum MySyntaxError<'i> {
    TokenError,
    NodeError,
    MissingIdent,
    MissingSeparatedFirst,
    MissingPunct(char),
    UnexpectedWhitespace,
    UnexpectedItem(&'i str),
    InvalidPunct(&'i str),
}
impl std::fmt::Display for MySyntaxError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TokenError => write!(f, "Syntax error"),
            Self::NodeError => write!(f, "Syntax error"),
            Self::MissingIdent => write!(f, "Missing identifier"),
            Self::MissingSeparatedFirst => write!(f, "Missing first item"),
            Self::MissingPunct(c) => write!(f, "Missing punctuation: `{c}`"),
            Self::UnexpectedWhitespace => write!(f, "Unexpected whitespace"),
            Self::UnexpectedItem(s) => write!(f, "Unexpected item: `{s}`"),
            Self::InvalidPunct(s) => write!(f, "Invalid punctuation: `{s}`"),
        }
    }
}

pub mod kinds {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub enum NodeKind {
        Directive,
        PreprocessorInclude,
        PreprocessorDefine,
        Document,
        DtNode,
        DtProperty,
        DtCell,
        DtBytestring,
        DtString,
        /// TokenKind::{Ident, Colon}
        DtLabel,
        DtPhandle,
        DtNodeUnitAddress,
        Error,
        /// TokenKind::{Ident, Error}
        /// TODO: split this to Name and NameRef, for definition and label
        Ident,
        /// Always T!['&']
        DtNodeExtension,

        Separator,
        /// Wrong punctuation
        InvalidPunct,
    }
    impl NodeKind {
        pub fn is_error(&self) -> bool {
            matches!(self, NodeKind::Error | NodeKind::InvalidPunct)
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub enum TokenKind {
        // variable contents
        Whitespace,
        Comment,
        Ident,
        Error,
        DtNumber,
        DtNumberArithmetic,
        DtStringContents,
        DirectiveArg,
        PreprocessorArg,
        UnexpectedItem,
        UnexpectedWhitespace,

        // empty markers
        SeparatedMissingFirst,
        DtPathPhandle,
        MissingPunct(char),

        // punctuation
        LCurly,
        RCurly,
        LParen,
        RParen,
        LBRack,
        RBRack,
        LAngle,
        RAngle,
        DoubleQuote,
        Equals,
        Colon,
        Semicolon,
        Slash,
        Comma,
        Newline,
        Ampersand,
        Pound,
        AtSign,
        KwInclude,
        KwDefine,
    }
    impl TokenKind {
        pub fn is_error(&self) -> bool {
            matches!(
                self,
                TokenKind::Error
                    | TokenKind::UnexpectedItem
                    | TokenKind::SeparatedMissingFirst
                    | TokenKind::MissingPunct(_)
            )
        }
        pub fn is_trivia(&self) -> bool {
            matches!(self, TokenKind::Whitespace | TokenKind::Comment)
        }
    }
}
