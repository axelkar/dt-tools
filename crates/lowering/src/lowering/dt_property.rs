use std::{num::ParseIntError, str::FromStr, sync::Arc};

use dt_tools_analyzer::string::interpret_escaped_string;
use dt_tools_diagnostic::Severity;
use dt_tools_parser::{
    TextRange,
    ast::{self, AstNode, AstToken, HasName},
    cst::RedToken,
};
use num_traits::{AsPrimitive, Num};

use super::{LowerCtx, lower_phandle, undefined_macro};
use crate::{
    diag::Diag,
    expr_eval::{self, interpret_escaped_char_tok, parse_int_tok},
    extra_num_traits::{Bits, Signedness},
    lowering::{
        dt_node::{build_path, get_name_and_unit_addr},
        item::unsupported_preprocessor,
    },
    mir::{
        MirCell32, MirCellList, MirDefinition, MirDefinitionValue, MirPhandleTarget,
        MirPropertyData, MirValue,
    },
    preprocess::PreprocessedText,
};

/// Lowers an [`ast::DtProperty`] to a [`MirDefinition`], if valid.
pub(crate) fn lower_dt_property(
    ctx: &mut LowerCtx<'_>,
    diag: &mut Diag<'_>,
    parent_node_path: &str,
    prop: &ast::DtProperty,
) -> Result<(), ()> {
    if parent_node_path.is_empty() {
        diag.emit(
            prop.syntax().text_range(),
            "Property must be defined inside a node",
            Severity::Error,
        );
        return Err(());
    }

    let mut values = Vec::new();
    for value_ast in prop.values() {
        let _ = lower_prop_value(diag, &mut values, &value_ast);
    }

    let name = get_name_and_unit_addr(diag, prop)?;
    let path = build_path(parent_node_path, &name);

    let text_range = prop.syntax().text_range();
    let provenance = text_range.within_file(PreprocessedText);
    let name_span = prop.name().map_or(provenance, |name| {
        name.syntax().text_range().within_file(PreprocessedText)
    });

    ctx.mir.definitions.push(MirDefinition {
        path,
        value: MirDefinitionValue::Property(MirPropertyData { values }),
        provenance,
        name_span,
    });
    Ok(())
}

/// Lowers an [`ast::PropValue`] to a [`MirValue`], if valid.
pub(crate) fn lower_prop_value(
    diag: &mut Diag<'_>,
    output: &mut Vec<MirValue>,
    ast: &ast::PropValue,
) -> Result<(), ()> {
    match ast {
        ast::PropValue::String(tok) => {
            output.push(MirValue::String(interpret_escaped_string_tok(tok, diag)?));
        }
        ast::PropValue::CellList(cell_list) => {
            let bits_size = BitsSize::from_token(cell_list.bits_number().as_ref(), diag)?;
            let cells = lower_cell_list(diag, cell_list, bits_size);
            output.push(MirValue::CellList(cells));
        }
        ast::PropValue::Bytestring(tok) => {
            output.push(MirValue::Bytestring(lower_bytestring(diag, tok)?));
        }
        ast::PropValue::Phandle(phandle) => {
            output.push(MirValue::Phandle(lower_phandle(diag, phandle)?));
        }
        ast::PropValue::Macro(macro_inv) => {
            undefined_macro(diag, macro_inv);
            return Err(());
        }
        ast::PropValue::DtsDirective(dir) => {
            // TODO: implement DTS directives in property values
            diag.emit(
                dir.syntax().text_range(),
                "Currently unimplemented in property values",
                Severity::Error,
            );
        }
        ast::PropValue::PreprocessorConditional(cond) => {
            unsupported_preprocessor(diag, cond.syntax().text_range());
        }
        ast::PropValue::PreprocessorDirective(dir) => {
            unsupported_preprocessor(diag, dir.syntax().text_range());
        }
    }
    Ok(())
}

/// Wraps [`interpret_escaped_string`], handling errors.
pub fn interpret_escaped_string_tok(
    tok: &Arc<RedToken>,
    diag: &mut Diag<'_>,
) -> Result<String, ()> {
    match interpret_escaped_string(tok.text()) {
        Ok(path) => Ok(path),
        Err(err) => {
            diag.emit(
                tok.text_range(),
                format!("Failed to parse string: {err}"),
                Severity::Error,
            );
            Err(())
        }
    }
}

fn lower_bytestring(diag: &mut Diag<'_>, tok: &Arc<RedToken>) -> Result<Vec<u8>, ()> {
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
        diag.emit(
            tok.text_range(),
            "Bytestring is missing a hex digit",
            Severity::Error,
        );
        Err(())
    } else {
        Ok(bytes)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum BitsSize {
    Bits8,
    Bits16,
    #[default]
    Bits32,
    Bits64,
}
impl BitsSize {
    /// Parse a number token after `/bits/` into a [`BitsSize`], defaulting to 32-bit.
    ///
    /// Returns `Err(())` on unrecoverable parse errors (invalid number, unsupported bits).
    fn from_token(
        bits_number_tok: Option<&Arc<RedToken>>,
        diag: &mut Diag<'_>,
    ) -> Result<Self, ()> {
        let Some(bits_number_tok) = bits_number_tok else {
            return Ok(Self::default());
        };

        let bits = parse_int_tok::<u32>(bits_number_tok, diag)?;

        Ok(match bits {
            8 => Self::Bits8,
            16 => Self::Bits16,
            32 => Self::Bits32,
            64 => Self::Bits64,
            _ => {
                diag.emit(
                    bits_number_tok.text_range(),
                    format!("Unsupported /bits/: {bits}. Cells must be 8, 16, 32 or 64 bits."),
                    Severity::Error,
                );
                return Err(());
            }
        })
    }
}

/// Lower a cell list into the correct [`MirCellList`] variant.
fn lower_cell_list(
    diag: &mut Diag<'_>,
    cell_list: &ast::DtCellList,
    bits_size: BitsSize,
) -> MirCellList {
    match bits_size {
        BitsSize::Bits8 => MirCellList::Bits8(collect_cells(diag, cell_list)),
        BitsSize::Bits16 => MirCellList::Bits16(collect_cells(diag, cell_list)),
        BitsSize::Bits32 => MirCellList::Bits32(collect_cells(diag, cell_list)),
        BitsSize::Bits64 => MirCellList::Bits64(collect_cells(diag, cell_list)),
    }
}

fn collect_cells<T: LowerCell>(diag: &mut Diag<'_>, cell_list: &ast::DtCellList) -> Vec<T> {
    let mut cells: Vec<T> = Vec::new();
    cell_list.cells().for_each(|c| {
        let _ = lower_cell::<T>(diag, &mut cells, &c);
    });
    cells
}

/// Trait abstracting over what a cell lowers to: `u8`, `u16`, `u64`, or [`MirCell32`].
trait LowerCell: Sized {
    /// Numeric type
    type Number: num_traits::Unsigned
        + FromStr<Err = ParseIntError>
        + Num<FromStrRadixErr = ParseIntError>
        + Bits
        + Signedness
        + TryFrom<u32>
        + TryFrom<i64>
        + Copy
        + 'static;

    /// Signed numeric type
    type Signed: num_traits::Signed
        + FromStr<Err = ParseIntError>
        + Num
        + TryFrom<i64>
        + AsPrimitive<Self::Number>;

    /// Converts [`Self::Number`] to [`Self`].
    fn from_number(number: Self::Number) -> Self;

    /// Handle a phandle reference in this cell type.
    ///
    /// Returns an error for non-32-bit cells; wraps in [`MirCell32::Phandle`] for 32-bit.
    fn from_phandle(
        target: MirPhandleTarget,
        phandle_range: TextRange,
        diag: &mut Diag<'_>,
    ) -> Result<Self, ()>;
}

/// Lower a single [`ast::Cell`] using the [`LowerCell`] trait for type-specific conversion.
fn lower_cell<T: LowerCell>(
    diag: &mut Diag<'_>,
    output: &mut Vec<T>,
    ast: &ast::Cell,
) -> Result<(), ()> {
    match ast {
        ast::Cell::Number(tok) => {
            output.push(parse_int_tok::<T::Number>(tok, diag).map(T::from_number)?);
        }
        ast::Cell::Char(tok) => {
            let ch = interpret_escaped_char_tok(tok, diag)?;

            let val = u32::from(ch);
            if let Ok(v) = T::Number::try_from(val) {
                output.push(T::from_number(v));
            } else {
                diag.emit(
                    tok.text_range(),
                    format!(
                        "character value {ch:?}={val} too large for {}-bit cell",
                        T::Number::BITS
                    ),
                    Severity::Error,
                );
                return Err(());
            }
        }
        ast::Cell::Phandle(phandle) => {
            let target = lower_phandle(diag, phandle)?;
            output.push(T::from_phandle(
                target,
                phandle.syntax().text_range(),
                diag,
            )?);
        }
        ast::Cell::DtExpr(dt_expr) => {
            let expr = dt_expr.expr().ok_or(())?;
            let num = expr_eval::eval(expr, diag)?;

            if let Ok(v) = T::Number::try_from(num) {
                output.push(T::from_number(v));
            } else if let Ok(v) = <T::Signed>::try_from(num) {
                // Two's complement bitwise reinterpretation
                output.push(T::from_number(v.as_()));
            } else {
                emit_overflow(num, dt_expr.syntax().text_range(), T::Number::BITS, diag);
                return Err(());
            }
        }
        ast::Cell::Macro(macro_inv) => {
            undefined_macro(diag, macro_inv);
            return Err(());
        }
        ast::Cell::DtsDirective(dir) => {
            // TODO: implement DTS directives in cells
            diag.emit(
                dir.syntax().text_range(),
                "Currently unimplemented in cells",
                Severity::Error,
            );
        }
        ast::Cell::PreprocessorConditional(cond) => {
            unsupported_preprocessor(diag, cond.syntax().text_range());
        }
        ast::Cell::PreprocessorDirective(dir) => {
            unsupported_preprocessor(diag, dir.syntax().text_range());
        }
    }
    Ok(())
}

/// Helper function that isn't monomorphized
fn emit_overflow(num: i64, range: TextRange, bits: u32, diag: &mut Diag<'_>) {
    let cmp = if num < 0 { "small" } else { "large" };
    diag.emit(
        range,
        format!(
            "number {num} too {cmp} to fit in {bits}-bit signed integer \
             (using two's complement) or {bits}-bit unsigned integer"
        ),
        Severity::Error,
    );
}

/// Helper function that isn't monomorphized
fn emit_phandle_rejected(phandle_range: TextRange, bits: u32, diag: &mut Diag<'_>) {
    diag.emit(
        phandle_range,
        format!(
            "phandle reference not valid in {bits}-bit cells (only 32-bit cell lists support phandles)"
        ),
        Severity::Error,
    );
}

macro_rules! impl_lower_cell_int {
    ($T:ty, $signed:ty) => {
        impl LowerCell for $T {
            type Number = Self;
            type Signed = $signed;

            fn from_number(number: Self::Number) -> Self {
                number
            }

            fn from_phandle(
                _target: MirPhandleTarget,
                phandle_range: TextRange,
                diag: &mut Diag<'_>,
            ) -> Result<Self, ()> {
                emit_phandle_rejected(phandle_range, <$T>::BITS, diag);
                Err(())
            }
        }
    };
}

impl_lower_cell_int!(u8, i8);
impl_lower_cell_int!(u16, i16);
impl_lower_cell_int!(u64, i64);

impl LowerCell for MirCell32 {
    type Number = u32;
    type Signed = i32;

    fn from_number(number: u32) -> Self {
        MirCell32::Number(number)
    }

    fn from_phandle(
        target: MirPhandleTarget,
        _phandle_range: TextRange,
        _diag: &mut Diag<'_>,
    ) -> Result<Self, ()> {
        Ok(MirCell32::Phandle(target))
    }
}

#[cfg(test)]
#[expect(
    clippy::needless_raw_string_hashes,
    reason = "expect-test auto update adds r#"
)]
mod tests {
    use expect_test::expect;

    use crate::lowering::tests::check_mir;

    #[test]
    fn mir_properties() {
        check_mir(
            r#"
/dts-v1/;
/ { foo = "bar"; baz = <1 2 3>; qux = [ab cd]; };
"#,
            &[],
            expect![[r#"
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L3:1-L3:50
                property /foo = "bar"; /main.dts L3:5-L3:17
                property /baz = <1 2 3>; /main.dts L3:18-L3:32
                property /qux = [abcd]; /main.dts L3:33-L3:47
            "#]],
        );
    }

    /// C integer suffixes from dt-bindings headers are accepted.
    #[test]
    fn mir_number_suffix() {
        check_mir(
            r#"
/dts-v1/;
/ { prop = <18U 0x10UL 5LL>; };
"#,
            &[],
            expect![[r#"
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L3:1-L3:32
                property /prop = <18 16 5>; /main.dts L3:5-L3:29
            "#]],
        );
    }

    #[test]
    fn mir_bits() {
        check_mir(
            r#"
/dts-v1/;
/ { value = /bits/ 64 <1>; };
"#,
            &[],
            expect![[r#"
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L3:1-L3:30
                property /value = /bits/ 64 <1>; /main.dts L3:5-L3:27
            "#]],
        );
    }

    #[test]
    fn mir_undefined_label_phandle() {
        check_mir(
            r#"
/dts-v1/;
/ { foo = &BOGUS; };
"#,
            &[],
            expect![[r#"
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L3:1-L3:21
                property /foo = &BOGUS; /main.dts L3:5-L3:18

                --- errors ---
                Error L3:5-L3:8: Label not found: BOGUS [dt_tools_lowering::check_mir_post]
            "#]],
        );
    }

    #[test]
    fn mir_undefined_path_phandle() {
        check_mir(
            r#"
/dts-v1/;
/ { foo = &{/bar}; };
"#,
            &[],
            expect![[r#"
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L3:1-L3:22
                property /foo = &{/bar}; /main.dts L3:5-L3:19

                --- errors ---
                Error L3:5-L3:8: Node at path not found: /bar [dt_tools_lowering::check_mir_post]
            "#]],
        );
    }

    /// A label-relative path resolves in cells and values.
    #[test]
    fn mir_label_path_phandle() {
        check_mir(
            r#"
/dts-v1/;
/ { LBL: node { sub { }; }; };
/ { prop = <&{LBL/sub}>; value = &{LBL/sub}; };
"#,
            &[],
            expect![[r#"
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L3:1-L3:31
                node /node labels=[LBL] /main.dts L3:5-L3:28
                node /node/sub /main.dts L3:17-L3:25
                node / /main.dts L4:1-L4:48
                property /prop = <&{LBL/sub}>; /main.dts L4:5-L4:25
                property /value = &{LBL/sub}; /main.dts L4:26-L4:45
            "#]],
        );
    }

    /// A label-relative path reports a missing label and a missing node.
    #[test]
    fn mir_label_path_phandle_errors() {
        check_mir(
            r#"
/dts-v1/;
/ { LBL: node { sub { }; }; };
/ { a = <&{NOPE/sub}>; b = <&{LBL/nope}>; };
"#,
            &[],
            expect![[r#"
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L3:1-L3:31
                node /node labels=[LBL] /main.dts L3:5-L3:28
                node /node/sub /main.dts L3:17-L3:25
                node / /main.dts L4:1-L4:45
                property /a = <&{NOPE/sub}>; /main.dts L4:5-L4:23
                property /b = <&{LBL/nope}>; /main.dts L4:24-L4:42

                --- errors ---
                Error L4:24: Node at path not found: /node/nope [dt_tools_lowering::check_mir_post]
                Error L4:5: Label not found: NOPE [dt_tools_lowering::check_mir_post]
            "#]],
        );
    }

    #[test]
    fn mir_num_out_of_bounds() {
        check_mir(
            r#"
/dts-v1/;
/ { prop = <(1 << 32)>; };
"#,
            &[],
            expect![[r#"
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L3:1-L3:27
                property /prop = <>; /main.dts L3:5-L3:24

                --- errors ---
                Error L3:13-L3:22: number 4294967296 too large to fit in 32-bit signed integer (using two's complement) or 32-bit unsigned integer
            "#]],
        );
    }

    #[test]
    fn mir_num_negative() {
        check_mir(
            r#"
/dts-v1/;
/ { prop = <-1 (-1)>; };
"#,
            &[],
            expect![[r#"
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L3:1-L3:25
                property /prop = <1 4294967295>; /main.dts L3:5-L3:22

                --- errors ---
                Error L3:13: Expected cell, ‘/include/‘, preprocessor directive or ‘>’, but found ‘-’ [dt-tools(syntax-error)]
            "#]],
        );
    }

    #[test]
    fn mir_char() {
        check_mir(
            r#"
/dts-v1/;
/ { prop = <'\0' 'a'>; };
"#,
            &[],
            expect![[r#"
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L3:1-L3:26
                property /prop = <0 97>; /main.dts L3:5-L3:23
            "#]],
        );
    }

    #[test]
    fn mir_err_property_outside_node() {
        check_mir(
            r#"
/dts-v1/;
/ { };

#if 1
foo = "baz";
#endif
bar = "baz";
"#,
            &[],
            expect![[r#"
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L3:1-L3:7

                --- errors ---
                Error L6:1-L6:13: Property must be defined inside a node
                Error L8:1-L8:13: Property must be defined inside a node
            "#]],
        );
    }

    mod mir_bare_property_macro {
        use super::*;

        #[test]
        fn no_macro() {
            check_mir(
                r#"
/dts-v1/;
/ { bare; };
"#,
                &[],
                expect![[r#"
                    dts-v1 /main.dts L2:1-L2:10
                    node / /main.dts L3:1-L3:13
                    property /bare; /main.dts L3:5-L3:10
                "#]],
            );
        }

        #[test]
        fn alias() {
            check_mir(
                r#"
/dts-v1/;
#define MACRO bare
/ { MACRO; };
"#,
                &[],
                expect![[r#"
                    dts-v1 /main.dts L2:1-L2:10
                    node / /main.dts L4:1-L4:14
                    property /bare; /main.dts L3:15-L3:19
                "#]],
            );
        }

        #[test]
        fn node() {
            check_mir(
                r#"
/dts-v1/;
#define MACRO LBL: foo { bar = "baz"; }
/ { MACRO; };
"#,
                &[],
                expect![[r#"
                    dts-v1 /main.dts L2:1-L2:10
                    node / /main.dts L4:1-L4:14
                    node /foo labels=[LBL] /main.dts L3:15-L3:40
                    property /foo/bar = "baz"; /main.dts L3:26-L3:38
                "#]],
            );
        }

        #[test]
        fn property() {
            check_mir(
                r#"
/dts-v1/;
#define MACRO bar = <42>
/ { MACRO; };
"#,
                &[],
                expect![[r#"
                    dts-v1 /main.dts L2:1-L2:10
                    node / /main.dts L4:1-L4:14
                    property /bar = <42>; /main.dts L3:15-L3:25
                "#]],
            );
        }

        #[test]
        fn multiple_items() {
            check_mir(
                r#"
/dts-v1/;
#define MACRO a {}; b {}
/ { MACRO; };
"#,
                &[],
                expect![[r#"
                    dts-v1 /main.dts L2:1-L2:10
                    node / /main.dts L4:1-L4:14
                    node /a /main.dts L3:15-L3:20
                    node /b /main.dts L3:21-L3:25
                "#]],
            );
        }

        #[test]
        fn top_level() {
            check_mir(
                r#"
/dts-v1/;
#define MACRO / { foo {}; }
MACRO;
"#,
                &[],
                expect![[r#"
                    dts-v1 /main.dts L2:1-L2:10
                    node / /main.dts L3:15-L3:28
                    node /foo /main.dts L3:19-L3:26
                "#]],
            );
        }

        #[test]
        fn extra_semicolon_error() {
            check_mir(
                r#"
/dts-v1/;
#define MACRO bar = <42>;
/ { MACRO; };
"#,
                &[],
                expect![[r#"
                    dts-v1 /main.dts L2:1-L2:10
                    node / /main.dts L4:1-L4:14
                    property /bar = <42>; /main.dts L3:15-L3:26

                    --- errors ---
                    Error L4:10: Unmatched `;` [dt-tools(syntax-error)]
                "#]],
            );
        }
    }
}
