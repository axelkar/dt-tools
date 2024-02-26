//! I take a lot of inspiration from cstree & [rust-analyzer's syntax architecture](https://github.com/rust-lang/rust-analyzer/blob/5346002d07d09badaf37949bec68012d963d61fc/docs/dev/syntax.md).

use kinds::{NodeKind, TokenKind};
use std::{fmt::Write, sync::Arc};

use crate::Span;
pub mod parser;

// On the cst + ast level I could use a less-descriptive Span, to avoid cloning the SourceId
// TODO: Span to Arc/Rc/Box<str> or usize to aid in subtree interning/deduplication?
// TODO: partial reparse?

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GreenNode {
    pub kind: NodeKind,
    pub span: Span,
    pub children: Vec<GreenItem>,
}
impl GreenNode {
    /// Print a tree like rust-analyzer's debug AST
    /// This panics?
    pub fn print_tree(&self, source: &str) -> String {
        let mut out = String::new();
        self.print_tree_rec(0, source, &mut out).unwrap();
        out
    }

    fn print_tree_rec(&self, level: usize, source: &str, out: &mut String) -> std::fmt::Result {
        const INDENT: &str = "  ";
        writeln!(out, "{}{:?}@{}", INDENT.repeat(level), self.kind, self.span)?;

        for child in self.children.iter() {
            match child {
                GreenItem::Node(ref node) => node.print_tree_rec(level + 1, source, out)?,
                GreenItem::Token(ref token) => writeln!(
                    out,
                    "{}{:?}@{} {:?}",
                    INDENT.repeat(level + 1),
                    token.kind,
                    token.span,
                    token.span.text(source).unwrap_or_default()
                )?,
            }
        }
        Ok(())
    }

    /// For convenience, use RedNode::child_nodes
    pub fn child_nodes(&self) -> impl Iterator<Item = &Arc<GreenNode>> + '_ {
        self.children.iter().flat_map(GreenItem::as_node)
    }

    /// For convenience, use RedNode::child_tokens
    pub fn child_tokens(&self) -> impl Iterator<Item = &Arc<GreenToken>> + '_ {
        self.children.iter().flat_map(GreenItem::as_token)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GreenToken {
    pub kind: TokenKind,
    pub span: Span,
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
    pub fn map_token<NewToken>(self, f: impl FnOnce(Token) -> NewToken) -> TreeItem<Node, NewToken> {
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
    pub fn span(&self) -> Span {
        match self {
            Self::Node(node) => node.green.span,
            Self::Token(token) => token.green.span,
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
    pub fn span(&self) -> &Span {
        &self.green.span
    }

    pub fn text<'i>(&self, src: &'i str) -> Option<&'i str> {
        self.green.span.text(src)
    }

    /// Iterator over all the ancestors of this token excluding itself.
    pub fn parent_ancestors(&self) -> impl Iterator<Item = Arc<RedNode>> {
        std::iter::successors(Some(self.parent.clone()), |node| node.parent.clone())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RedNode {
    pub parent: Option<Arc<RedNode>>,
    pub green: Arc<GreenNode>,
}

impl RedNode {
    pub fn new(root: Arc<GreenNode>) -> Arc<Self> {
        Arc::new(RedNode {
            parent: None,
            green: root,
        })
    }

    pub fn span(&self) -> &Span {
        &self.green.span
    }

    pub fn node_at_offset(self: &Arc<RedNode>, offset: usize) -> Option<Arc<RedNode>> {
        // TODO: non recursing algorithm?
        self.child_nodes()
            .find(|node| node.span().range().contains(&offset))
            .map(|node| node.node_at_offset(offset).unwrap_or(node))
    }

    pub fn token_at_offset(self: &Arc<RedNode>, offset: usize) -> Option<Arc<RedToken>> {
        // TODO: non recursing algorithm?
        self.child_nodes()
            .find(|node| node.span().range().contains(&offset))
            .and_then(|node| node.token_at_offset(offset))
            .or_else(|| {
                self.child_tokens()
                    .find(|token| token.span().range().contains(&offset))
            })
    }

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

    pub fn child_nodes<'a>(self: &'a Arc<RedNode>) -> impl Iterator<Item = Arc<RedNode>> + 'a {
        let arc = Arc::clone(self);
        self.green.child_nodes().map(move |green_child| {
            Arc::new(RedNode {
                parent: Some(Arc::clone(&arc)),
                green: Arc::clone(green_child),
            })
        })
    }
    pub fn child_tokens<'a>(self: &'a Arc<RedNode>) -> impl Iterator<Item = Arc<RedToken>> + 'a {
        let arc = Arc::clone(self);
        self.green.child_tokens().map(move |green_child| {
            Arc::new(RedToken {
                parent: Arc::clone(&arc),
                green: Arc::clone(green_child),
            })
        })
    }

    /// Find Error tokens
    pub fn find_syntax_errors<'a, 'i: 'a>(
        self: &'a Arc<RedNode>,
        src: &'i str,
    ) -> impl Iterator<Item = (Span, MySyntaxError<'i>)> + 'a {
        self.child_tokens()
            .filter_map(|token| {
                Some((
                    token.green.span,
                    match token.green.kind {
                        TokenKind::Error if token.parent.green.kind == NodeKind::Ident => {
                            MySyntaxError::MissingIdent
                        }
                        TokenKind::Error => MySyntaxError::TokenError,
                        TokenKind::SeparatedMissingFirst => MySyntaxError::MissingSeparatedFirst,
                        TokenKind::MissingPunct(c) => MySyntaxError::MissingPunct(c),
                        TokenKind::UnexpectedWhitespace => MySyntaxError::UnexpectedWhitespace,
                        TokenKind::UnexpectedItem => {
                            MySyntaxError::UnexpectedItem(token.green.span.text(src)?)
                        }
                        _ => return None,
                    },
                ))
            })
            .chain(
                std::iter::once_with(|| {
                    Some((
                        self.green.span,
                        match self.green.kind {
                            NodeKind::Error => MySyntaxError::NodeError,
                            NodeKind::InvalidPunct => {
                                MySyntaxError::InvalidPunct(self.green.span.text(src)?)
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

        /// [`crate::cst::parser::green_separated`] separator
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
