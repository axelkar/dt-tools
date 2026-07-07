use std::{borrow::Cow, num::ParseIntError, str::FromStr, sync::Arc};

use dt_tools_analyzer::string::interpret_escaped_string;
use dt_tools_diagnostic::{Diagnostic, DiagnosticCollector, Severity, Span};
use dt_tools_parser::{
    TextRange,
    ast::{self, AstNode},
    cst::RedToken,
    parser::Entrypoint,
};
use num_traits::{AsPrimitive, Num};

use super::{IntraFileCtx, lower_phandle, resolve_macro_to_value};
use crate::{
    db::BaseDb,
    expr_eval::{self, interpret_escaped_char_tok, parse_int_tok},
    extra_num_traits::{Bits, Signedness},
    file::File,
    lowering::dt_node::{build_path, get_name_and_unit_addr},
    macros::{MacroCtx, env::TrackedMapEnvMut},
    mir::{
        MirCell32, MirCellList, MirDefinition, MirDefinitionValue, MirPhandleTarget,
        MirPropertyData, MirProvenance, MirValue,
    },
};

// TODO: options to results here
// TODO: get rid of `impl DiagnosticCollector` and `impl FnMut(TextRange) -> Span<File>` because they cause monomorphization

/// Lowers an [`ast::DtProperty`] to a [`MirDefinition`], if valid.
///
/// Returns `None` if the property has no name or can't be processed.
pub(crate) fn lower_dt_property(
    ctx: &mut IntraFileCtx<'_, '_, impl DiagnosticCollector<File>>,
    parent_node_path: &str,
    prop: &ast::DtProperty,
) -> Result<MirDefinition, ()> {
    let mut values = Vec::new();
    for value_ast in prop.values() {
        if let Ok(value) = lower_prop_value(
            ctx.db,
            ctx.env,
            ctx.diag,
            &mut |tr| tr.within_file(ctx.file),
            &value_ast,
        ) {
            values.push(value);
        }
    }

    let name = get_name_and_unit_addr(
        ctx.db,
        ctx.env,
        ctx.diag,
        &mut |tr| tr.within_file(ctx.file),
        prop,
    )?;
    let path = build_path(parent_node_path, &name);

    let text_range = prop.syntax().text_range();
    let provenance = MirProvenance {
        file: ctx.file,
        text_range,
    };

    Ok(MirDefinition {
        path,
        value: MirDefinitionValue::Property(MirPropertyData { values }),
        provenance,
    })
}

/// Lowers an [`ast::PropValue`] to a [`MirValue`], if valid.
pub(crate) fn lower_prop_value<'db>(
    db: &'db dyn BaseDb,
    env: &mut TrackedMapEnvMut<'db>,
    diag: &impl DiagnosticCollector<File>,
    spanner: &mut impl FnMut(TextRange) -> Span<File>,
    value: &ast::PropValue,
) -> Result<MirValue, ()> {
    match value {
        ast::PropValue::String(tok) => match interpret_escaped_string(tok.text()) {
            Ok(path) => Ok(MirValue::String(path)),
            Err(err) => {
                diag.emit(Diagnostic::new(
                    spanner(tok.text_range()),
                    Cow::Owned(format!("Failed to parse string: {err}")),
                    Severity::Error,
                ));
                Err(())
            }
        },
        ast::PropValue::CellList(cell_list) => {
            let bits_size = BitsSize::from_token(cell_list.bits_number().as_ref(), diag, spanner)?;
            let cells = lower_cell_list(db, env, diag, spanner, cell_list, bits_size);
            Ok(MirValue::CellList(cells))
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
                diag.emit(Diagnostic::new(
                    spanner(tok.text_range()),
                    Cow::Borrowed("Bytestring is missing a hex digit"),
                    Severity::Error,
                ));
                return Err(());
            }

            Ok(MirValue::Bytestring(bytes))
        }
        ast::PropValue::Phandle(phandle) => {
            let target = lower_phandle(db, env, diag, spanner, phandle)?;
            Ok(MirValue::Phandle(target))
        }
        ast::PropValue::Macro(macro_inv) => Ok(resolve_macro_to_value(
            db,
            env,
            diag,
            spanner,
            &MacroCtx::Explicit(macro_inv),
            Entrypoint::PropValues,
            lower_prop_value,
        )?
        .expect("resolve_macro_to_value should not return Ok(None) with an explicit macro")),
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
    /// Returns `None` on unrecoverable parse errors (invalid number, unsupported bits).
    fn from_token(
        bits_number_tok: Option<&Arc<RedToken>>,
        diag: &impl DiagnosticCollector<File>,
        spanner: &mut impl FnMut(TextRange) -> Span<File>,
    ) -> Result<Self, ()> {
        let Some(bits_number_tok) = bits_number_tok else {
            return Ok(Self::default());
        };

        let bits = parse_int_tok::<u32>(bits_number_tok, diag, spanner)?;

        Ok(match bits {
            8 => Self::Bits8,
            16 => Self::Bits16,
            32 => Self::Bits32,
            64 => Self::Bits64,
            _ => {
                diag.emit(Diagnostic::new(
                    spanner(bits_number_tok.text_range()),
                    Cow::Owned(format!(
                        "Unsupported /bits/: {bits}. Cells must be 8, 16, 32 or 64 bits."
                    )),
                    Severity::Error,
                ));
                return Err(());
            }
        })
    }
}

/// Lower a cell list into the correct [`MirCellList`] variant.
fn lower_cell_list<'db>(
    db: &'db dyn BaseDb,
    env: &mut TrackedMapEnvMut<'db>,
    diag: &impl DiagnosticCollector<File>,
    spanner: &mut impl FnMut(TextRange) -> Span<File>,
    cell_list: &ast::DtCellList,
    bits_size: BitsSize,
) -> MirCellList {
    match bits_size {
        BitsSize::Bits8 => {
            // TODO: return Err on any Err, but run lower_cell on all child cells
            let cells: Vec<u8> = cell_list
                .cells()
                .filter_map(|c| lower_cell::<u8>(db, env, diag, spanner, &c).ok())
                .collect();
            MirCellList::Bits8(cells)
        }
        BitsSize::Bits16 => {
            let cells: Vec<u16> = cell_list
                .cells()
                .filter_map(|c| lower_cell::<u16>(db, env, diag, spanner, &c).ok())
                .collect();
            MirCellList::Bits16(cells)
        }
        BitsSize::Bits32 => {
            let cells: Vec<MirCell32> = cell_list
                .cells()
                .filter_map(|c| lower_cell::<MirCell32>(db, env, diag, spanner, &c).ok())
                .collect();
            MirCellList::Bits32(cells)
        }
        BitsSize::Bits64 => {
            let cells: Vec<u64> = cell_list
                .cells()
                .filter_map(|c| lower_cell::<u64>(db, env, diag, spanner, &c).ok())
                .collect();
            MirCellList::Bits64(cells)
        }
    }
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
        diag: &impl DiagnosticCollector<File>,
        spanner: &mut impl FnMut(TextRange) -> Span<File>,
    ) -> Result<Self, ()>;
}

/// Lower a single [`ast::Cell`] using the [`LowerCell`] trait for type-specific conversion.
fn lower_cell<'db, T: LowerCell>(
    db: &'db dyn BaseDb,
    env: &mut TrackedMapEnvMut<'db>,
    diag: &impl DiagnosticCollector<File>,
    spanner: &mut impl FnMut(TextRange) -> Span<File>,
    cell: &ast::Cell,
) -> Result<T, ()> {
    match cell {
        ast::Cell::Number(tok) => {
            parse_int_tok::<T::Number>(tok, diag, spanner).map(T::from_number)
        }
        ast::Cell::Char(tok) => {
            let ch = interpret_escaped_char_tok(tok, diag, spanner)?;

            let val = u32::from(ch);
            if let Ok(v) = T::Number::try_from(val) {
                Ok(T::from_number(v))
            } else {
                diag.emit(Diagnostic::new(
                    spanner(tok.text_range()),
                    Cow::Owned(format!(
                        "character value {ch:?}={val} too large for {}-bit cell",
                        T::Number::BITS
                    )),
                    Severity::Error,
                ));
                Err(())
            }
        }
        ast::Cell::Phandle(phandle) => {
            let target = lower_phandle(db, env, diag, spanner, phandle)?;
            T::from_phandle(target, phandle.syntax().text_range(), diag, spanner)
        }
        ast::Cell::DtExpr(dt_expr) => {
            let expr = dt_expr.expr().ok_or(())?;
            let num = expr_eval::eval(db, env, expr, diag, spanner)?;

            if let Ok(v) = T::Number::try_from(num) {
                Ok(T::from_number(v))
            } else if let Ok(v) = <T::Signed>::try_from(num) {
                // Two's complement bitwise reinterpretation
                Ok(T::from_number(v.as_()))
            } else {
                emit_overflow(
                    num,
                    dt_expr.syntax().text_range(),
                    T::Number::BITS,
                    diag,
                    spanner,
                );
                Err(())
            }
        }
        ast::Cell::Macro(macro_inv) => Ok(resolve_macro_to_value(
            db,
            env,
            diag,
            spanner,
            &MacroCtx::Explicit(macro_inv),
            Entrypoint::Cells,
            |db, env, diag, spanner, cell| lower_cell::<T>(db, env, diag, spanner, cell),
        )?
        .expect("resolve_macro_to_value should not return Ok(None) with an explicit macro")),
    }
}

/// Helper function that isn't monomorphized
fn emit_overflow(
    num: i64,
    range: TextRange,
    bits: u32,
    diag: &impl DiagnosticCollector<File>,
    spanner: &mut impl FnMut(TextRange) -> Span<File>,
) {
    let cmp = if num < 0 { "small" } else { "large" };
    diag.emit(Diagnostic::new(
        spanner(range),
        Cow::Owned(format!(
            "number {num} too {cmp} to fit in {bits}-bit signed integer \
             (using two's complement) or {bits}-bit unsigned integer"
        )),
        Severity::Error,
    ));
}

/// Helper function that isn't monomorphized
fn emit_phandle_rejected(
    phandle_range: TextRange,
    bits: u32,
    diag: &impl DiagnosticCollector<File>,
    spanner: &mut impl FnMut(TextRange) -> Span<File>,
) {
    diag.emit(Diagnostic::new(
        spanner(phandle_range),
        Cow::Owned(format!(
            "phandle reference not valid in {bits}-bit cells (only 32-bit cell lists support phandles)"
        )),
        Severity::Error,
    ));
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
                diag: &impl DiagnosticCollector<File>,
                spanner: &mut impl FnMut(TextRange) -> Span<File>,
            ) -> Result<Self, ()> {
                emit_phandle_rejected(phandle_range, <$T>::BITS, diag, spanner);
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
        _diag: &impl DiagnosticCollector<File>,
        _spanner: &mut impl FnMut(TextRange) -> Span<File>,
    ) -> Result<Self, ()> {
        Ok(MirCell32::Phandle(target))
    }
}
