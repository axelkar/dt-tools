use std::{collections::HashMap, sync::Arc};

use dt_parser::{
    ast::{self, AstNode, AstNodeOrToken, AstToken, DtPhandle, HasMacroInvocation, HasName},
    cst::RedNode,
    parser::Entrypoint,
    TextRange,
};

use crate::macros::{evaluate_macro, MacroDefinition};

#[derive(thiserror::Error, Debug, displaydoc::Display)]
pub enum ValueFromAstError {
    /// failed to parse string: {0}
    StringParseError(#[from] crate::StringParseError),
    /// failed to parse number: {0}
    ParseIntError(#[from] std::num::ParseIntError),
    /// failed to parse number: missing hex prefix
    MissingHexPrefix,
    /// AST is missing items
    MissingAst,
    /// bytestring is missing a hex digit
    IncompleteBytestring,
    /// Unrecognized macro name {0}
    UnrecognizedMacro(String),
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
    #[must_use]
    pub fn into_json(self) -> serde_json::Value {
        use serde_json::Value as JValue;
        match self {
            Self::String(s) => JValue::String(s),
            Self::CellList(cell_items) => {
                JValue::Array(cell_items.into_iter().map(Cell::into_json).collect())
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
            Self::Phandle(_phandle_target) => JValue::String(String::new()), // TODO
        }
    }
    // TODO: resolve_macro should depend on text range
    // TODO: resolve_label
    // TODO: arithmetics + macros (evaluation)
    pub(crate) fn from_ast(
        ast: &ast::PropValue,
        resolve_label: &mut impl FnMut(&str) -> Option<ast::DtLabel>,
        macro_resolver: &impl MacroResolver,
    ) -> Result<Self, ValueFromAstError> {
        Ok(match ast {
            ast::PropValue::String(tok) => {
                Value::String(crate::string::interpret_escaped_string(tok.text())?)
            }
            ast::PropValue::CellList(cell_list) => Value::CellList(
                cell_list
                    .cells()
                    .map(|cell| Cell::from_ast(&cell, resolve_label, macro_resolver))
                    .collect::<Result<_, ValueFromAstError>>()?,
            ),
            ast::PropValue::Phandle(phandle) => {
                Value::Phandle(reference_eval(phandle, macro_resolver)?)
            }
            ast::PropValue::Bytestring(tok) => {
                let mut bytes = Vec::new();
                let mut first_nibble = None;
                for ch in tok.text().chars() {
                    if ch == ']' {
                        break;
                    }
                    #[expect(
                        clippy::cast_possible_truncation,
                        reason = "to_digit returns values 0-15"
                    )]
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
                }
                Value::Bytestring(bytes)
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
    #[must_use]
    pub fn into_json(self) -> serde_json::Value {
        match self {
            Self::U32(n) => serde_json::Value::Number(n.into()),
            Self::Phandle(_phandle_target) => serde_json::Value::Number((-1).into()),
        }
    }
    // TODO: use _resolve_label
    #[expect(clippy::used_underscore_binding, reason = "not yet implemented")]
    fn from_ast(
        ast: &ast::Cell,
        _resolve_label: &mut impl FnMut(&str) -> Option<ast::DtLabel>,
        macro_resolver: &impl MacroResolver,
    ) -> Result<Self, ValueFromAstError> {
        Ok(match ast {
            ast::Cell::Phandle(phandle) => Cell::Phandle(reference_eval(phandle, macro_resolver)?),
            ast::Cell::Number(token) => Cell::U32(parse_u32(token.text())?),
            ast::Cell::Macro(macro_ast) => {
                let macro_name = &macro_ast
                    .green_ident()
                    .expect("No macro invocation without a name")
                    .text;

                let Some(macro_def) = macro_resolver.resolve(macro_name) else {
                    return Err(ValueFromAstError::UnrecognizedMacro(macro_name.to_owned()));
                };

                let s = evaluate_macro(Some(macro_ast), macro_def).expect("FIXME: no error");

                let parse = Entrypoint::Cells.parse(&s);

                let cells = RedNode::new(Arc::new(parse.green_node.clone()));

                let cell = ast::Cell::cast(cells.children().next().unwrap()).unwrap();
                // TODO: handle errors & map textranges somehow?!
                // TODO: this can return multiple cells

                Cell::from_ast(&cell, _resolve_label, macro_resolver)?
            }
        })
    }
}

// Required to decouple &str's lifetime from &MacroDefinition
pub trait MacroResolver {
    fn resolve<'r>(&'r self, s: &str) -> Option<&'r MacroDefinition>;
}
impl<K: Eq + std::hash::Hash + std::borrow::Borrow<str>, S: std::hash::BuildHasher> MacroResolver
    for HashMap<K, (TextRange, &MacroDefinition), S>
{
    fn resolve<'r>(&'r self, s: &str) -> Option<&'r MacroDefinition> {
        self.get(s).map(|a| a.1)
    }
}

fn reference_eval(
    phandle: &DtPhandle,
    macro_resolver: &impl MacroResolver,
) -> Result<PhandleTarget, ValueFromAstError> {
    let (macro_ast, macro_def) = if let Some(macro_ast) = phandle.macro_invocation() {
        let macro_name = &macro_ast
            .green_ident()
            .expect("No macro invocation without a name")
            .text;

        let Some(macro_def) = macro_resolver.resolve(macro_name) else {
            return Err(ValueFromAstError::UnrecognizedMacro(macro_name.to_owned()));
        };
        (Some(macro_ast), macro_def)
    } else {
        let ident = phandle.name().ok_or(ValueFromAstError::MissingAst)?;
        let ident = ident.syntax().text();

        match macro_resolver.resolve(ident) {
            Some(macro_def) => (None, macro_def),
            None => {
                return if phandle.is_path() {
                    Ok(PhandleTarget::Path(ident.to_owned()))
                } else {
                    Ok(PhandleTarget::Label(ident.to_owned()))
                }
            }
        }
    };

    if phandle.is_path() {
        // TODO: implement path-based references
        todo!()
    }

    let s = evaluate_macro(macro_ast.as_ref(), macro_def).expect("FIXME: no error");

    let parse = Entrypoint::ReferenceNoamp.parse(&s);
    let phandle = ast::DtPhandle::cast(RedNode::new(Arc::new(parse.green_node.clone()))).unwrap();
    // TODO: handle errors & map textranges somehow?!

    reference_eval(&phandle, macro_resolver)
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
