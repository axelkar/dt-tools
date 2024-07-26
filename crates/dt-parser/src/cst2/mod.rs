use std::sync::Arc;

use self::lexer::TokenKind;
use crate::{cst::TreeItem, TextRange};

pub mod grammar;
pub mod lexer;
pub mod parser;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NodeKind {
    Document,
    /// A parse error
    ParseError,
    Directive,
    /// A property or node name.
    ///
    /// Name = 'Ident' | 'Number' | ',' | '-'
    ///
    /// NOTE: parsed using `Parser::bump_name` without whitespace
    Name,
    /// A directive's parameters, if any.
    DirectiveParams,
    PreprocessorDirective,
    DtNode,
    DtProperty,
    DtCellList,
    DtExpr,
    DtLabel,
    /// TODO: How to model enum ast::DtPropValue in ungrammar?
    /// PropValueList = (('String' | 'DtBytestring' | DtCellList | DtPhandle) ','?)*
    PropValueList,
    DtPhandle,
    UnitAddress,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// A "green node".
pub struct GreenNode {
    /// The kind of node
    pub kind: NodeKind,
    /// Cached length of all of the textual contents
    pub width: usize,
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
            offset + self.width
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
}

// TODO: benchmark without Static tokens
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

impl std::ops::Deref for TokenText {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::Static(str) => str,
            Self::Dynamic(str) => str.as_str(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GreenToken {
    pub kind: TokenKind,
    /// The text the token represents
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
pub type RedItem = TreeItem<Arc<RedNode>, Arc<RedToken>>;

impl GreenItem {
    /// Returns the length of the textual contents.
    #[inline]
    pub fn length(&self) -> usize {
        match self {
            GreenItem::Node(node) => node.width,
            GreenItem::Token(token) => token.length(),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct RedToken {
    pub parent: Arc<RedNode>,
    pub green: Arc<GreenToken>,
    /// Offset in the document
    pub text_offset: usize,
}
impl std::fmt::Debug for RedToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RedToken")
            .field("green", &self.green)
            .finish()
    }
}
impl RedToken {
    /// Returns the text range.
    #[inline(always)]
    pub fn text_range(&self) -> TextRange {
        TextRange {
            start: self.text_offset,
            end: self.text_offset + self.green.length(),
        }
    }

    pub fn text(&self) -> &TokenText {
        &self.green.text
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
    /// Offset in the document
    pub text_offset: usize,
}

impl RedNode {
    /// Creates a new red node with no parent.
    pub fn new(root: Arc<GreenNode>) -> Arc<Self> {
        Arc::new(RedNode {
            parent: None,
            green: root,
            text_offset: 0,
        })
    }

    /// Returns the text range.
    #[inline(always)]
    pub fn text_range(&self) -> TextRange {
        TextRange {
            start: self.text_offset,
            end: self.text_offset + self.green.width,
        }
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
        // TODO: Make the signature self: Arc<RedNode>
        let arc = Arc::clone(self);
        let mut current_text_offset = self.text_offset;
        self.green
            .children
            .iter()
            .map(move |green_child| match green_child {
                GreenItem::Node(node) => {
                    let text_offset = current_text_offset;
                    current_text_offset += node.width;

                    RedItem::Node(Arc::new(RedNode {
                        parent: Some(Arc::clone(&arc)),
                        green: Arc::clone(node),
                        text_offset,
                    }))
                }
                GreenItem::Token(token) => {
                    let text_offset = current_text_offset;
                    current_text_offset += token.text.len();

                    RedItem::Token(Arc::new(RedToken {
                        parent: Arc::clone(&arc),
                        green: Arc::clone(token),
                        text_offset,
                    }))
                }
            })
    }

    /// Returns an iterator over the child nodes.
    pub fn child_nodes<'a>(self: &'a Arc<RedNode>) -> impl Iterator<Item = Arc<RedNode>> + 'a {
        self.children().flat_map(RedItem::into_node)
    }

    /// Returns an iterator over the child tokens.
    pub fn child_tokens<'a>(self: &'a Arc<RedNode>) -> impl Iterator<Item = Arc<RedToken>> + 'a {
        self.children().flat_map(RedItem::into_token)
    }

    /// Iterator over all the ancestors of this node excluding itself.
    pub fn parent_ancestors(&self) -> impl Iterator<Item = Arc<RedNode>> {
        std::iter::successors(self.parent.clone(), |node| node.parent.clone())
    }
}
