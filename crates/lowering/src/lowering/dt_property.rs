use std::{borrow::Cow, num::ParseIntError, str::FromStr, sync::Arc};

use dt_tools_analyzer::string::interpret_escaped_string;
use dt_tools_diagnostic::Severity;
use dt_tools_parser::{
    TextRange,
    ast::{self, AstNode, AstToken},
    cst::RedToken,
    parser::Entrypoint,
};
use num_traits::{AsPrimitive, Num};

use super::{IntraFileCtx, lower_phandle, resolve_macro_to_ast};
use crate::{
    diag::{Diag, SourceMap},
    expr_eval::{self, interpret_escaped_char_tok, parse_int_tok},
    extra_num_traits::{Bits, Signedness},
    lowering::{
        dt_node::{build_path, get_name_and_unit_addr},
        item::handle_preprocessor_conditional,
    },
    macros::MacroCtx,
    mir::{
        MirCell32, MirCellList, MirDefinition, MirDefinitionValue, MirPhandleTarget,
        MirPropertyData, MirProvenance, MirValue,
    },
};

/// Lowers an [`ast::DtProperty`] to a [`MirDefinition`], if valid.
pub(crate) fn lower_dt_property(
    ctx: &mut IntraFileCtx<'_, '_>,
    diag: &mut Diag<'_, '_>,
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
    let mut expansion_stack = Vec::new();
    for value_ast in prop.values() {
        let _ = lower_prop_value(ctx, diag, &mut values, &value_ast, &mut expansion_stack);
    }

    let name = get_name_and_unit_addr(ctx.db, ctx.env, diag, prop)?;
    let path = build_path(parent_node_path, &name);

    let text_range = prop.syntax().text_range();
    let provenance = MirProvenance {
        span: diag.resolve(text_range),
    };

    ctx.mir.definitions.push(MirDefinition {
        path,
        value: MirDefinitionValue::Property(MirPropertyData { values }),
        provenance,
    });
    Ok(())
}

/// Lowers an [`ast::PropValue`] to a [`MirValue`], if valid.
pub(crate) fn lower_prop_value(
    ctx: &mut IntraFileCtx<'_, '_>,
    diag: &mut Diag<'_, '_>,
    output: &mut Vec<MirValue>,
    ast: &ast::PropValue,
    expansion_stack: &mut Vec<String>,
) -> Result<(), ()> {
    match ast {
        ast::PropValue::String(tok) => {
            output.push(MirValue::String(interpret_escaped_string_tok(tok, diag)?));
        }
        ast::PropValue::CellList(cell_list) => {
            let bits_size = BitsSize::from_token(cell_list.bits_number().as_ref(), diag)?;
            let cells = lower_cell_list(ctx, diag, cell_list, bits_size);
            output.push(MirValue::CellList(cells));
        }
        ast::PropValue::Bytestring(tok) => {
            output.push(MirValue::Bytestring(lower_bytestring(diag, tok)?));
        }
        ast::PropValue::Phandle(phandle) => {
            output.push(MirValue::Phandle(lower_phandle(
                ctx.db, ctx.env, diag, phandle,
            )?));
        }
        ast::PropValue::Macro(macro_inv) => {
            let name = macro_inv
                .green_ident()
                .map_or(String::new(), |t| t.text.to_string());
            if expansion_stack.contains(&name) {
                diag.emit(
                    macro_inv.syntax().text_range(),
                    format!("macro `{name}` expanded recursively"),
                    Severity::Error,
                );
                return Err(());
            }
            let (ast, expansion) = resolve_macro_to_ast(
                ctx.db,
                ctx.env,
                diag,
                &MacroCtx::Explicit(macro_inv),
                Entrypoint::PropValues,
            )?
            .expect("resolve_macro_to_value should not return Ok(None) with an explicit macro");
            expansion_stack.push(name);
            let child_map = SourceMap::Macro {
                parent: diag.map,
                expansion: &expansion,
            };
            let result = lower_prop_value(
                ctx,
                &mut Diag::new(&mut *diag.sink, &child_map),
                output,
                &ast,
                expansion_stack,
            );
            expansion_stack.pop();
            result?;
        }
        ast::PropValue::DtsDirective(dir) => {
            //handle_dts_directive(ctx, parent_node_path, &dir, files);
            // TODO: implement
            diag.emit(
                dir.syntax().text_range(),
                Cow::Borrowed("Currently unimplemented in property values"),
                Severity::Error,
            );
        }
        ast::PropValue::PreprocessorConditional(cond) => {
            let Some(branch) = handle_preprocessor_conditional(ctx, diag, cond) else {
                return Ok(());
            };
            for ast in branch.prop_values() {
                let _ = lower_prop_value(ctx, diag, output, &ast, expansion_stack);
            }
        }
        ast::PropValue::PreprocessorDirective(dir) => {
            //handle_preprocessor_directive(ctx, parent_node_path, files, text_range, &dir);
            // TODO: implement
            diag.emit(
                dir.syntax().text_range(),
                Cow::Borrowed("Currently unimplemented in property values"),
                Severity::Error,
            );
        }
    }
    Ok(())
}

/// Wraps [`interpret_escaped_string`], handling errors.
pub fn interpret_escaped_string_tok(
    tok: &Arc<RedToken>,
    diag: &mut Diag<'_, '_>,
) -> Result<String, ()> {
    match interpret_escaped_string(tok.text()) {
        Ok(path) => Ok(path),
        Err(err) => {
            diag.emit(
                tok.text_range(),
                Cow::Owned(format!("Failed to parse string: {err}")),
                Severity::Error,
            );
            Err(())
        }
    }
}

fn lower_bytestring(diag: &mut Diag<'_, '_>, tok: &Arc<RedToken>) -> Result<Vec<u8>, ()> {
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
            Cow::Borrowed("Bytestring is missing a hex digit"),
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
        diag: &mut Diag<'_, '_>,
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
                    Cow::Owned(format!(
                        "Unsupported /bits/: {bits}. Cells must be 8, 16, 32 or 64 bits."
                    )),
                    Severity::Error,
                );
                return Err(());
            }
        })
    }
}

/// Lower a cell list into the correct [`MirCellList`] variant.
fn lower_cell_list(
    ctx: &mut IntraFileCtx<'_, '_>,
    diag: &mut Diag<'_, '_>,
    cell_list: &ast::DtCellList,
    bits_size: BitsSize,
) -> MirCellList {
    match bits_size {
        BitsSize::Bits8 => MirCellList::Bits8(collect_cells(ctx, diag, cell_list)),
        BitsSize::Bits16 => MirCellList::Bits16(collect_cells(ctx, diag, cell_list)),
        BitsSize::Bits32 => MirCellList::Bits32(collect_cells(ctx, diag, cell_list)),
        BitsSize::Bits64 => MirCellList::Bits64(collect_cells(ctx, diag, cell_list)),
    }
}

fn collect_cells<T: LowerCell>(
    ctx: &mut IntraFileCtx<'_, '_>,
    diag: &mut Diag<'_, '_>,
    cell_list: &ast::DtCellList,
) -> Vec<T> {
    let mut cells: Vec<T> = Vec::new();
    cell_list.cells().for_each(|c| {
        let _ = lower_cell::<T>(ctx, diag, &mut cells, &c);
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
        diag: &mut Diag<'_, '_>,
    ) -> Result<Self, ()>;
}

/// Lower a single [`ast::Cell`] using the [`LowerCell`] trait for type-specific conversion.
fn lower_cell<T: LowerCell>(
    ctx: &mut IntraFileCtx<'_, '_>,
    diag: &mut Diag<'_, '_>,
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
                    Cow::Owned(format!(
                        "character value {ch:?}={val} too large for {}-bit cell",
                        T::Number::BITS
                    )),
                    Severity::Error,
                );
                return Err(());
            }
        }
        ast::Cell::Phandle(phandle) => {
            let target = lower_phandle(ctx.db, ctx.env, diag, phandle)?;
            output.push(T::from_phandle(
                target,
                phandle.syntax().text_range(),
                diag,
            )?);
        }
        ast::Cell::DtExpr(dt_expr) => {
            let expr = dt_expr.expr().ok_or(())?;
            let num = expr_eval::eval(ctx.db, ctx.env, expr, diag)?;

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
            let (ast, expansion) = resolve_macro_to_ast(
                ctx.db,
                ctx.env,
                diag,
                &MacroCtx::Explicit(macro_inv),
                Entrypoint::Cells,
            )?
            .expect("resolve_macro_to_value should not return Ok(None) with an explicit macro");

            let child_map = SourceMap::Macro {
                parent: diag.map,
                expansion: &expansion,
            };
            lower_cell::<T>(
                ctx,
                &mut Diag::new(&mut *diag.sink, &child_map),
                output,
                &ast,
            )?;
        }
        ast::Cell::DtsDirective(dir) => {
            //handle_dts_directive(ctx, parent_node_path, &dir, files);
            // TODO: implement
            diag.emit(
                dir.syntax().text_range(),
                Cow::Borrowed("Currently unimplemented in cells"),
                Severity::Error,
            );
        }
        ast::Cell::PreprocessorConditional(cond) => {
            let Some(branch) = handle_preprocessor_conditional(ctx, diag, cond) else {
                return Ok(());
            };
            for ast in branch.cells() {
                let _ = lower_cell::<T>(ctx, diag, output, &ast);
            }
        }
        ast::Cell::PreprocessorDirective(dir) => {
            //handle_preprocessor_directive(ctx, parent_node_path, files, text_range, &dir);
            // TODO: implement
            diag.emit(
                dir.syntax().text_range(),
                Cow::Borrowed("Currently unimplemented in cells"),
                Severity::Error,
            );
        }
    }
    Ok(())
}

/// Helper function that isn't monomorphized
fn emit_overflow(num: i64, range: TextRange, bits: u32, diag: &mut Diag<'_, '_>) {
    let cmp = if num < 0 { "small" } else { "large" };
    diag.emit(
        range,
        Cow::Owned(format!(
            "number {num} too {cmp} to fit in {bits}-bit signed integer \
             (using two's complement) or {bits}-bit unsigned integer"
        )),
        Severity::Error,
    );
}

/// Helper function that isn't monomorphized
fn emit_phandle_rejected(phandle_range: TextRange, bits: u32, diag: &mut Diag<'_, '_>) {
    diag.emit(
        phandle_range,
        Cow::Owned(format!(
            "phandle reference not valid in {bits}-bit cells (only 32-bit cell lists support phandles)"
        )),
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
                diag: &mut Diag<'_, '_>,
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
        _diag: &mut Diag<'_, '_>,
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
                dts-v1  /main.dts L2:1-L2:10
                node   / /main.dts L3:1-L3:50
                property = CellList(Bits32([Number(1), Number(2), Number(3)])) /baz /main.dts L3:18-L3:32
                property = String("bar") /foo /main.dts L3:5-L3:17
                property = Bytestring([171, 205]) /qux /main.dts L3:33-L3:47
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
                dts-v1  /main.dts L2:1-L2:10
                node   / /main.dts L3:1-L3:30
                property = CellList(Bits64([1])) /value /main.dts L3:5-L3:27
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
                dts-v1  /main.dts L2:1-L2:10
                node   / /main.dts L3:1-L3:21
                property = Phandle(Label("BOGUS")) /foo /main.dts L3:5-L3:18

                --- errors ---
                Error L3:5-L3:18: Label not found: BOGUS [dt_tools_lowering::check_mir_post]
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
                dts-v1  /main.dts L2:1-L2:10
                node   / /main.dts L3:1-L3:22
                property = Phandle(Path("/bar")) /foo /main.dts L3:5-L3:19

                --- errors ---
                Error L3:5-L3:19: Node at path not found: /bar [dt_tools_lowering::check_mir_post]
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
                dts-v1  /main.dts L2:1-L2:10
                node   / /main.dts L3:1-L3:27
                property = CellList(Bits32([])) /prop /main.dts L3:5-L3:24

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
                dts-v1  /main.dts L2:1-L2:10
                node   / /main.dts L3:1-L3:25
                property = CellList(Bits32([Number(1), Number(4294967295)])) /prop /main.dts L3:5-L3:22

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
                dts-v1  /main.dts L2:1-L2:10
                node   / /main.dts L3:1-L3:26
                property = CellList(Bits32([Number(0), Number(97)])) /prop /main.dts L3:5-L3:23
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
                dts-v1  /main.dts L2:1-L2:10
                node   / /main.dts L3:1-L3:7

                --- errors ---
                Error L6:1-L6:13: Property must be defined inside a node
                Error L8:1-L8:13: Property must be defined inside a node
            "#]],
        );
    }
}
