use std::num::ParseIntError;

use dt_parser::{
    ast::{self, AstNode as _, DtNode, HasIdent},
    cst::{kinds::TokenKind, RedItem, TreeItem},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// A property definition from a single file
pub struct PropDefinition {
    pub value: Value,
    pub node: ast::DtProperty,
}

/// Analyze and AST node and its subnodes for properties, skipping over errors
///
/// Returns [None] when the node's name cannot be found
///
/// * `parent_path`: Parent path including a trailing slash
pub fn analyze_node(
    parent_path: Option<&str>,
    node: ast::DtNode,
    src: &str,
) -> Option<Vec<(String, PropDefinition)>> {
    let parent_path = parent_path.unwrap_or_default();
    Some(
        node.properties()
            .flat_map(|prop| {
                Some((
                    format!("{}{}", parent_path, prop.ident()?.text(src)?),
                    PropDefinition {
                        value: Value::from_ast(&prop.values().collect::<Vec<_>>(), src).ok()?,
                        node: prop,
                    },
                ))
            })
            .chain(
                node.subnodes()
                    .filter(DtNode::is_concrete)
                    .flat_map(|node| {
                        analyze_node(
                            Some(&format!("{}{}/", parent_path, node.ident()?.text(src)?)),
                            node,
                            src,
                        )
                    })
                    .flatten(),
            )
            .collect(),
    )
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
