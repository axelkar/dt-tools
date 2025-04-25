use std::{num::ParseIntError, sync::Arc};

use dt_parser::{
    ast::{self, AstToken, DtNode, HasName},
    cst::RedToken,
};
use either::Either;
use rustc_hash::FxHashMap;
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
    #[must_use]
    pub fn prefix(self, mut prefix: impl Iterator<Item = String>) -> Self {
        if let Some(name) = prefix.next() {
            Self::Node(DefinitionTreeNode {
                children: {
                    let mut hm = FxHashMap::default();
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
                let vec = value.into_custom_value();
                if vec.is_empty() {
                    serde_json::Value::Bool(true)
                } else {
                    serde_json::Value::Array(vec.into_iter().map(CustomValue::into_json).collect())
                }
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
    // Vec1 can have multiple elements due to multiple definitions.
    // For example, the following would result in 3 items.
    // TODO: error and return the last one defined
    // ```
    // foo = <1>;
    // foo {};
    // foo = <2>;
    // ```
    pub children: FxHashMap<String, Vec1<DefinitionTree>>,
}
impl DefinitionTreeNode {
    /// Wraps self in another node until `prefix` returns none
    #[must_use]
    pub fn prefix(self, mut prefix: impl Iterator<Item = String>) -> Self {
        if let Some(name) = prefix.next() {
            Self {
                children: {
                    let mut hm = FxHashMap::default();
                    hm.insert(name, vec1::vec1![DefinitionTree::Node(self.prefix(prefix))]);
                    hm
                },
            }
        } else {
            self
        }
    }
    #[must_use]
    pub fn into_json(self) -> serde_json::Value {
        DefinitionTree::Node(self).into_json()
    }

    // TODO: return Vec<Value> instead of Value
    // TODO: create "linked list" type with Arc<str> and efficient pushing to left
    #[must_use]
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
    #[must_use]
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
pub fn analyze_node(node: &ast::DtNode, src: &str) -> Option<DefinitionTreeNode> {
    let children = node
        .properties()
        .filter_map(|prop| {
            Some((
                prop.name()?.syntax().text().to_owned(),
                DefinitionTree::Prop {
                    value: Value::from_ast(&prop.values().collect::<Vec<_>>()).ok()?,
                    ast: prop,
                },
            ))
        })
        .chain(
            node.subnodes()
                .filter(DtNode::is_concrete)
                .filter_map(|node| {
                    Some((
                        // TODO: what to do with unit address?, $nodename (jsonschema property)?
                        // Kernel 6.10:
                        // Documentation/devicetree/bindings/thermal/thermal-zones.yaml#L41
                        // Documentation/devicetree/bindings/riscv/sifive.yaml#L17
                        // Documentation/devicetree/bindings/i2c/i2c-virtio.yaml#L20
                        // Documentation/devicetree/bindings/serial/serial.yaml#L23
                        node.text_name(src)?.into_owned(),
                        DefinitionTree::Node(analyze_node(&node, src)?),
                    ))
                }),
        )
        .collect::<Vec<_>>();
    let children = {
        let mut hm: FxHashMap<String, Vec1<DefinitionTree>> = FxHashMap::default();
        for (name, child) in children {
            let name = name.clone();
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

#[derive(thiserror::Error, Debug, displaydoc::Display)]
pub enum ValueFromAstError {
    /// failed to parse string: {0}
    StringParseError(#[from] StringParseError),
    /// failed to parse number: {0}
    ParseIntError(#[from] ParseIntError),
    /// failed to parse number: missing hex prefix
    MissingHexPrefix,
    /// AST is missing items
    MissingAst,
}
use crate::string::StringParseError;
use ValueFromAstError::MissingAst;

impl Value {
    pub fn into_custom_value(self) -> Vec<CustomValue> {
        vec![match self {
            Self::Empty => return Vec::new(),
            Self::U32(value) => CustomValue::Cell(vec![CustomValueCellItem::U32(value)]),
            #[expect(
                clippy::cast_possible_truncation,
                reason = "splitting u64 into two u32s"
            )]
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
    fn from_ast(ast: &[ast::PropValue]) -> Result<Self, ValueFromAstError> {
        Ok(match ast {
            [] => Value::Empty,
            [ast::PropValue::CellList(cell_list)] => {
                match &cell_list.cells().collect::<Vec<_>>()[..] {
                    [ast::Cell::Phandle(phandle)] => Value::Phandle({
                        let ident = phandle.name().ok_or(MissingAst)?.syntax().text().to_owned();
                        if phandle.is_path() {
                            PhandleTarget::Path(ident)
                        } else {
                            PhandleTarget::Label(ident)
                        }
                    }),
                    [ast::Cell::Number(number)] => Value::U32(parse_u32(number.text())?),
                    [ast::Cell::Number(low), ast::Cell::Number(high)] => Value::U64(
                        (u64::from(parse_u32(low.text())?) << 32)
                            + u64::from(parse_u32(high.text())?),
                    ),
                    [..] => Value::PropEncodedArray(
                        ast.iter()
                            .map(CustomValue::from_ast)
                            .collect::<Result<_, _>>()?,
                    ),
                }
            }
            [ast::PropValue::String(tok)] => Value::String(common_string(tok)?),
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
                        .map(common_string)
                        .collect::<Result<_, ValueFromAstError>>()?,
                )
            }
            other => Value::PropEncodedArray(
                other
                    .iter()
                    .map(CustomValue::from_ast)
                    .collect::<Result<_, _>>()?,
            ),
        })
    }
}

pub(crate) fn parse_u32(src: &str) -> Result<u32, ValueFromAstError> {
    src.parse().or_else(|_| {
        src.strip_prefix("0x")
            .or_else(|| src.strip_prefix("0X"))
            .ok_or(ValueFromAstError::MissingHexPrefix)
            .and_then(|src| Ok(u32::from_str_radix(src, 16)?))
    })
}

fn common_string(tok: &Arc<RedToken>) -> Result<String, ValueFromAstError> {
    // TODO: fix it!
    Ok(crate::string::interpret_escaped_string(tok.text())?)
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
    #[must_use]
    pub fn into_json(self) -> serde_json::Value {
        match self {
            Self::String(s) => serde_json::Value::String(s),
            Self::Cell(cell_items) => serde_json::Value::Array(
                cell_items
                    .into_iter()
                    .map(CustomValueCellItem::into_json)
                    .collect(),
            ),
            Self::Bytestring(_bytes) => todo!(),
        }
    }
    fn from_ast(ast: &ast::PropValue) -> Result<Self, ValueFromAstError> {
        Ok(match ast {
            ast::PropValue::String(tok) => CustomValue::String(common_string(tok)?),
            ast::PropValue::CellList(cell_list) => CustomValue::Cell(
                cell_list
                    .cells()
                    .map(|cell| {
                        Ok(Some(match cell {
                            ast::Cell::Phandle(phandle) => CustomValueCellItem::Phandle({
                                let ident =
                                    phandle.name().ok_or(MissingAst)?.syntax().text().to_owned();
                                if phandle.is_path() {
                                    PhandleTarget::Path(ident)
                                } else {
                                    PhandleTarget::Label(ident)
                                }
                            }),
                            ast::Cell::Number(token) => {
                                CustomValueCellItem::U32(parse_u32(token.text())?)
                            }
                            ast::Cell::Macro(_) => todo!(),
                        }))
                    })
                    .filter_map(|v| match v {
                        Ok(None) => None,
                        Ok(Some(v)) => Some(Ok(v)),
                        Err(e) => Some(Err(e)),
                    })
                    .collect::<Result<_, ValueFromAstError>>()?,
            ),
            ast::PropValue::Phandle(_) => todo!(),
            ast::PropValue::Bytestring(_) => todo!(),
            ast::PropValue::Macro(_) => todo!(),
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// See [`CustomValue`] and [`Value`] documentation
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
    #[must_use]
    pub fn into_json(self) -> serde_json::Value {
        match self {
            Self::U32(n) => serde_json::Value::Number(n.into()),
            Self::Phandle(_phandle_target) => serde_json::Value::Number((-1).into()),
            // FIXME: make phandles somehow 😭😭
            // tbh we don't need phandle numbers at the binding validation phase
            // DTS `<&foo>` -> YAML `[[!phandle foo]]`
            // Do I have to fork jsonschema-rs to work with serde_yaml::Value ?
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
