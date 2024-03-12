use std::{collections::HashMap, num::ParseIntError};

use dt_parser::{
    ast::{self, DtNode, HasIdent},
    cst::TreeItem,
};
use either::Either;
use vec1::Vec1;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// A property definition from a single file
pub struct PropDefinition {
    pub value: Value,
    pub node: ast::DtProperty,
}

// TODO: make this lazy, i.e. only find a certain node from the tree when referencing it

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DefinitionTree {
    Prop { value: Value, ast: ast::DtProperty },
    Node(DefinitionTreeNode),
}
impl DefinitionTree {
    /// Wraps self in a [`DefinitionTree::Node`] until `prefix` returns none
    pub fn prefix(self, mut prefix: impl Iterator<Item = String>) -> Self {
        if let Some(name) = prefix.next() {
            Self::Node(DefinitionTreeNode {
                children: {
                    let mut hm = HashMap::new();
                    hm.insert(name, vec1::vec1![self.prefix(prefix)]);
                    hm
                },
            })
        } else {
            self
        }
    }
    pub fn into_json(self) -> serde_json::Value {
        match self {
            Self::Prop { value, .. } => {
                serde_json::Value::Array(
                    value
                        .into_custom_value()
                        .into_iter()
                        .flat_map(|cv| match cv.into_json() {
                            // flatten cells
                            serde_json::Value::Array(arr) => Either::Left(arr.into_iter()),
                            other => Either::Right(std::iter::once(other)),
                        })
                        .collect(),
                )
            }
            Self::Node(node) => serde_json::Value::Object({
                let mut map = serde_json::Map::new();
                for (name, definitions) in node.children {
                    map.insert(name, definitions.last().clone().into_json());
                }
                map
            }),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefinitionTreeNode {
    pub children: HashMap<String, Vec1<DefinitionTree>>,
}
impl DefinitionTreeNode {
    /// Wraps self in another node until `prefix` returns none
    pub fn prefix(self, mut prefix: impl Iterator<Item = String>) -> Self {
        if let Some(name) = prefix.next() {
            Self {
                children: {
                    let mut hm = HashMap::new();
                    hm.insert(name, vec1::vec1![DefinitionTree::Node(self.prefix(prefix))]);
                    hm
                },
            }
        } else {
            self
        }
    }
    pub fn into_json(self) -> serde_json::Value {
        DefinitionTree::Node(self).into_json()
    }
    /// TODO: return Vec<Value> instead of Value
    /// TODO: create "linked list" type with Arc<str> and efficient pushing to left
    pub fn dfs_iter(self) -> Box<dyn Iterator<Item = (Vec<String>, Value)>> {
        Box::new(self.children.into_iter().flat_map(|(parent_n, v)| {
            v.into_iter().flat_map(move |tree| {
                let parent_n = parent_n.clone();
                match tree {
                    DefinitionTree::Prop { value, .. } => {
                        Either::Left(std::iter::once((vec![parent_n], value)))
                    }
                    DefinitionTree::Node(node) => {
                        Either::Right(node.dfs_iter().map(move |(n, v)| {
                            (
                                {
                                    let mut n = n.clone();
                                    n.insert(0, parent_n.clone());
                                    n
                                },
                                v,
                            )
                        }))
                    }
                }
            })
        }))
    }
    pub fn dfs_iter_nodes(self) -> Box<dyn Iterator<Item = (Vec<String>, DefinitionTreeNode)>> {
        Box::new(self.children.into_iter().flat_map(|(parent_n, v)| {
            v.into_iter()
                .filter_map(|tree| match tree {
                    DefinitionTree::Prop { .. } => None,
                    DefinitionTree::Node(node) => Some(node),
                })
                .flat_map(move |node| {
                    let parent_n = parent_n.clone();
                    std::iter::once((vec![], node.clone()))
                        .chain(node.dfs_iter_nodes())
                        .map(move |(n, v)| {
                            (
                                {
                                    let mut n = n.clone();
                                    n.insert(0, parent_n.clone());
                                    n
                                },
                                v,
                            )
                        })
                })
        }))
    }
    pub fn merge(&mut self, other: Self) {
        for (other_name, other_vec) in other.children {
            if let Some(vec) = self.children.get_mut(&other_name) {
                if let DefinitionTree::Node(first) = vec.first_mut() {
                    if let DefinitionTree::Node(other_first) = other_vec.first() {
                        first.merge(other_first.clone());
                    } else {
                        vec.extend(other_vec);
                    }
                } else {
                    vec.extend(other_vec);
                }
            } else {
                self.children.insert(other_name, other_vec);
            }
        }
    }
}

/// Analyze and AST node and its subnodes for properties, skipping over errors
///
/// Returns [None] when the node's name cannot be found
pub fn analyze_node(node: ast::DtNode, src: &str) -> Option<DefinitionTreeNode> {
    let children = node
        .properties()
        .flat_map(|prop| {
            Some((
                prop.ident()?.text(src)?.to_owned(),
                DefinitionTree::Prop {
                    value: Value::from_ast(&prop.values().collect::<Vec<_>>(), src).ok()?,
                    ast: prop,
                },
            ))
        })
        .chain(
            node.subnodes()
                .filter(DtNode::is_concrete)
                .filter_map(|node| {
                    Some((
                        // TODO: what to do with unit address?, $nodename?
                        node.name(src)?.into_owned(),
                        DefinitionTree::Node(analyze_node(node, src)?),
                    ))
                }),
        )
        .collect::<Vec<_>>();
    let children = {
        let mut hm: HashMap<String, Vec1<DefinitionTree>> = HashMap::new();
        for (name, child) in children {
            let name = name.to_owned();
            if let Some(vec) = hm.get_mut(&name) {
                vec.push(child);
            } else {
                hm.insert(name, vec1::vec1![child]);
            }
        }
        hm
    };

    Some(DefinitionTreeNode { children })
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// Devicetree property values as taken from [the spec](https://devicetree-specification.readthedocs.io/en/latest/chapter2-devicetree-basics.html#property-values)
pub enum Value {
    Empty,
    /// e.g. `<0x11223344>`
    U32(u32),
    /// e.g. `<1 0>` (which is [`u32::MAX`] + 1)
    U64(u64),
    /// A printable and null-terminated string.
    ///
    /// e.g. `"hello"` (represented in DTB/FDT as `hello\0`)
    String(String),
    /// Format-specific property.
    ///
    /// e.g. `<1 0>, [0x1122], "hi"`
    PropEncodedArray(Vec<CustomValue>),
    /// A reference to another node
    ///
    /// <div class="warning">
    /// Note: Syntactically it could also be to a prop but it doesn't have a representation in the
    /// DTB so I choose not to support it in the analyzer
    /// </div>
    ///
    /// e.g. `&UART_1`
    Phandle(PhandleTarget),
    /// A list of [`Value::String`]s concatenated together
    ///
    /// e.g. `"hello", "world"` (represented in DTB/FDT as `hello\0world\0`)
    Stringlist(Vec<String>),
}

#[derive(thiserror::Error, Debug)]
pub enum ValueFromAstError {
    #[error("failed to parse string: {0}")]
    StringParseError(#[from] StringParseError),
    #[error("failed to parse number: {0}")]
    ParseIntError(#[from] ParseIntError),
    #[error("failed to parse number: missing hex prefix")]
    MissingHexPrefix,
    #[error("cannot find text - make sure AST matches source text")]
    CannotFindText,
    #[error("AST is missing items")]
    MissingAst,
}
use crate::string::StringParseError;
use ValueFromAstError::{CannotFindText, MissingAst};

impl Value {
    pub fn into_custom_value(self) -> Vec<CustomValue> {
        vec![match self {
            Self::Empty => return Vec::new(),
            Self::U32(value) => CustomValue::Cell(vec![CustomValueCellItem::U32(value)]),
            Self::U64(value) => CustomValue::Cell(vec![
                CustomValueCellItem::U32((value >> 32) as u32),
                CustomValueCellItem::U32(value as u32),
            ]),
            Self::String(string) => CustomValue::String(string),
            Self::PropEncodedArray(vec) => return vec,
            Self::Phandle(phandle) => {
                CustomValue::Cell(vec![CustomValueCellItem::Phandle(phandle)])
            }
            Self::Stringlist(vec) => return vec.into_iter().map(CustomValue::String).collect(),
        }]
    }
    fn from_ast(ast: &[ast::PropValue], src: &str) -> Result<Self, ValueFromAstError> {
        Ok(match ast {
            [] => Value::Empty,
            [ast::PropValue::Cell(cell)] => match &cell.values().collect::<Vec<_>>()[..] {
                [TreeItem::Node(phandle)] => Value::Phandle({
                    let ident = phandle
                        .ident()
                        .ok_or(MissingAst)?
                        .text(src)
                        .ok_or(CannotFindText)?
                        .to_owned();
                    if phandle.is_path() {
                        PhandleTarget::Path(ident)
                    } else {
                        PhandleTarget::Label(ident)
                    }
                }),
                [TreeItem::Token(number)] => {
                    Value::U32(parse_u32(number.text(src).ok_or(CannotFindText)?)?)
                }
                [TreeItem::Token(low), TreeItem::Token(high)] => Value::U64(
                    ((parse_u32(low.text(src).ok_or(CannotFindText)?)? as u64) << 32)
                        + parse_u32(high.text(src).ok_or(CannotFindText)?)? as u64,
                ),
                [..] => Value::PropEncodedArray(
                    ast.iter()
                        .map(|ast| CustomValue::from_ast(ast, src))
                        .collect::<Result<_, _>>()?,
                ),
            },
            [ast::PropValue::String(ast)] => Value::String(common_string(ast, src)?),
            stringlist
                if !stringlist.is_empty()
                    && stringlist
                        .iter()
                        .all(|v| matches!(v, ast::PropValue::String(_))) =>
            {
                Value::Stringlist(
                    stringlist
                        .iter()
                        .filter_map(|v| {
                            if let ast::PropValue::String(string) = v {
                                Some(string)
                            } else {
                                None
                            }
                        })
                        .map(|ast| common_string(ast, src))
                        .collect::<Result<_, ValueFromAstError>>()?,
                )
            }
            other => Value::PropEncodedArray(
                other
                    .iter()
                    .map(|ast| CustomValue::from_ast(ast, src))
                    .collect::<Result<_, _>>()?,
            ),
        })
    }
}

fn parse_u32(src: &str) -> Result<u32, ValueFromAstError> {
    src.parse().or_else(|_| {
        src.strip_prefix("0x")
            .or_else(|| src.strip_prefix("0X"))
            .ok_or(ValueFromAstError::MissingHexPrefix)
            .and_then(|src| Ok(u32::from_str_radix(src, 16)?))
    })
}

fn common_string(ast: &ast::DtString, src: &str) -> Result<String, ValueFromAstError> {
    Ok(crate::string::interpret_escaped_string(
        ast.contents()
            .ok_or(MissingAst)?
            .text(src)
            .ok_or(CannotFindText)?,
    )?)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// See [`Value`] documentation
pub enum CustomValue {
    String(String),
    Cell(Vec<CustomValueCellItem>),
    Bytestring(Vec<u8>), // TODO: support for bytestrings in parser and AST
                         // TODO: more?
}
impl CustomValue {
    pub fn into_json(self) -> serde_json::Value {
        match self {
            Self::String(s) => serde_json::Value::String(s),
            Self::Cell(cell_items) => {
                serde_json::Value::Array(cell_items.into_iter().map(|ci| ci.into_json()).collect())
            }
            Self::Bytestring(_bytes) => todo!(),
        }
    }
    fn from_ast(ast: &ast::PropValue, src: &str) -> Result<Self, ValueFromAstError> {
        Ok(match ast {
            ast::PropValue::String(ast) => CustomValue::String(common_string(ast, src)?),
            ast::PropValue::Cell(cell) => CustomValue::Cell(
                cell.values()
                    .map(|item| {
                        Ok(Some(match item {
                            TreeItem::Node(phandle) => CustomValueCellItem::Phandle({
                                let ident = phandle
                                    .ident()
                                    .ok_or(MissingAst)?
                                    .text(src)
                                    .ok_or(CannotFindText)?
                                    .to_owned();
                                if phandle.is_path() {
                                    PhandleTarget::Path(ident)
                                } else {
                                    PhandleTarget::Label(ident)
                                }
                            }),
                            TreeItem::Token(token) => CustomValueCellItem::U32(parse_u32(
                                token.text(src).ok_or(MissingAst)?,
                            )?),
                        }))
                    })
                    .filter_map(|v| match v {
                        Ok(None) => None,
                        Ok(Some(v)) => Some(Ok(v)),
                        Err(e) => Some(Err(e)),
                    })
                    .collect::<Result<_, ValueFromAstError>>()?,
            ),
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// See [CustomValue] and [`Value`] documentation
pub enum CustomValueCellItem {
    /// e.g. `0x11223344`
    U32(u32),
    /// A reference to another node
    ///
    /// <div class="warning">
    /// Note: Syntactically it could also be to a prop but it doesn't have a representation in the
    /// DTB so I choose not to support it in the analyzer
    /// </div>
    ///
    /// e.g. `&UART_1`
    Phandle(PhandleTarget),
}
impl CustomValueCellItem {
    pub fn into_json(self) -> serde_json::Value {
        match self {
            Self::U32(n) => serde_json::Value::Number(n.into()),
            Self::Phandle(_phandle_target) => todo!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// A phandle's target
///
/// TODO: figure out how to error in dt-lsp when labels are used before declaration
pub enum PhandleTarget {
    /// e.g. `&{/soc/uart}` -> `Path("/soc/uart")`
    Path(String),
    /// e.g. `&UART_1` -> `Label("UART_1")`
    Label(String),
}
