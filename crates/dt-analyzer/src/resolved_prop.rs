use dt_parser::ast::{self, AstToken, HasName};

#[derive(thiserror::Error, Debug)]
pub enum ValueFromAstError {
    #[error("failed to parse string: {0}")]
    StringParseError(#[from] crate::StringParseError),
    #[error("failed to parse number: {0}")]
    ParseIntError(#[from] std::num::ParseIntError),
    #[error("failed to parse number: missing hex prefix")]
    MissingHexPrefix,
    #[error("AST is missing items")]
    MissingAst,
    #[error("bytestring is missing a hex digit")]
    IncompleteBytestring,
}

pub(crate) fn parse_u32(src: &str) -> Result<u32, ValueFromAstError> {
    src.parse().or_else(|_| {
        src.strip_prefix("0x")
            .or_else(|| src.strip_prefix("0X"))
            .ok_or(ValueFromAstError::MissingHexPrefix)
            .and_then(|src| Ok(u32::from_str_radix(src, 16)?))
    })
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// Devicetree property values
pub enum Value {
    String(String),
    CellList(Vec<Cell>),
    Bytestring(Vec<u8>),
    // TODO: more?
    /// String-based reference
    Phandle(PhandleTarget),
}
impl Value {
    pub fn into_json(self) -> serde_json::Value {
        use serde_json::Value as JValue;
        match self {
            Self::String(s) => JValue::String(s),
            Self::CellList(cell_items) => {
                JValue::Array(cell_items.into_iter().map(|ci| ci.into_json()).collect())
            }
            // TODO: DTC outputs `!u8 [0x12, 0x34, 0x56]` for `[123456]`
            // This is very easily confused with a cell like
            // `[0x12, 0x34, 0x56]` from `<0x12 0x34 0x56>`, which is 4 times larger
            Self::Bytestring(bytes) => JValue::Array(
                bytes
                    .into_iter()
                    .map(|byte| JValue::Number(byte.into()))
                    .collect(),
            ), // TODO
            Self::Phandle(_phandle_target) => JValue::String("".to_owned()), // TODO
        }
    }
    // TODO: resolve_label
    // TODO: arithmetics + macros (evaluation)
    pub(crate) fn from_ast(
        ast: &ast::PropValue,
        _resolve_label: impl FnMut(&str) -> Option<ast::DtLabel>,
    ) -> Result<Self, ValueFromAstError> {
        Ok(match ast {
            ast::PropValue::String(tok) => {
                Value::String(crate::string::interpret_escaped_string(tok.text())?)
            }
            ast::PropValue::CellList(cell_list) => Value::CellList(
                cell_list
                    .cells()
                    .map(|cell| {
                        Ok(Some(match cell {
                            ast::Cell::Phandle(phandle) => Cell::Phandle({
                                let ident = phandle
                                    .name()
                                    .ok_or(ValueFromAstError::MissingAst)?
                                    .syntax()
                                    .text()
                                    .to_owned();
                                if phandle.is_path() {
                                    PhandleTarget::Path(ident)
                                } else {
                                    PhandleTarget::Label(ident)
                                }
                            }),
                            ast::Cell::Number(token) => Cell::U32(parse_u32(token.text())?),
                            ast::Cell::Macro(_macro) => todo!(), // FIXME!!
                        }))
                    })
                    .filter_map(|v| match v {
                        Ok(None) => None,
                        Ok(Some(v)) => Some(Ok(v)),
                        Err(e) => Some(Err(e)),
                    })
                    .collect::<Result<_, ValueFromAstError>>()?,
            ),
            ast::PropValue::Phandle(phandle) => Value::Phandle({
                let ident = phandle
                    .name()
                    .ok_or(ValueFromAstError::MissingAst)?
                    .syntax()
                    .text()
                    .to_owned();
                if phandle.is_path() {
                    PhandleTarget::Path(ident)
                } else {
                    PhandleTarget::Label(ident)
                }
            }),
            ast::PropValue::Bytestring(tok) => {
                let mut bytes = Vec::new();
                let mut first_nibble = None;
                for ch in tok.text().chars() {
                    if ch == ']' {
                        break;
                    }
                    if let Some(nibble) = ch.to_digit(16) {
                        let nibble = nibble as u8;
                        if let Some(first_nibble) = first_nibble.take() {
                            bytes.push(nibble + (first_nibble << 4));
                        } else {
                            first_nibble = Some(nibble);
                        }
                    }
                }

                if first_nibble.is_some() {
                    return Err(ValueFromAstError::IncompleteBytestring);
                } else {
                    Value::Bytestring(bytes)
                }
            }
            ast::PropValue::Macro(_tok) => {
                // TODO: find macro and reparse as value
                todo!()
            }
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// A 32-bit integer
pub enum Cell {
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
impl Cell {
    pub fn into_json(self) -> serde_json::Value {
        match self {
            Self::U32(n) => serde_json::Value::Number(n.into()),
            Self::Phandle(_phandle_target) => serde_json::Value::Number((-1).into()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// A phandle's target
///
// TODO(parser, analyzer): this should work
// ```
// myprop = &{mylabel}, &mylabel, &{mylabel/bbb}, &{/aaa/bbb};
// mylabel: aaa {
//   bbb {};
// };
// ```
pub enum PhandleTarget {
    /// e.g. `&{/soc/uart}` -> `Path("/soc/uart")`
    Path(String),
    /// e.g. `&UART_1` -> `Label("UART_1")`
    Label(String),
}

#[cfg(test)]
mod tests {
    use dt_parser::ast;

    #[track_caller]
    fn parse_value(value_src: &str) -> ast::PropValue {
        let src = format!("/dts-v1/; / {{ foo = {value_src}; }};");

        let parse = ast::SourceFile::parse(&src);
        assert_eq!(
            (&parse.errors, &parse.lex_errors),
            (&Vec::new(), &Vec::new())
        );

        let file = parse.source_file();
        let node = file.nodes().next().unwrap();
        let prop = node.properties().next().unwrap();

        dbg!(&prop);

        let mut vals = prop.values();
        vals.next().unwrap()
    }

    #[test]
    fn common_test() {
        parse_value("[abcd]");
        parse_value("<1234>");
        parse_value("\"foo\"");
        //parse_value("foo");
    }
}
