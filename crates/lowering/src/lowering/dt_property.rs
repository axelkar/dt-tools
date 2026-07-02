use std::borrow::Cow;

use dt_tools_diagnostic::{Diagnostic, DiagnosticCollector, Severity, Span};
use dt_tools_parser::{
    TextRange,
    ast::{self, AstNode, AstNodeOrToken},
    parser::Entrypoint,
};

use super::{IntraFileCtx, lower_phandle, resolve_macro_to_value};
use crate::{
    db::BaseDb,
    expr_eval::{self, interpret_escaped_char_tok, parse_int_tok},
    file::File,
    lowering::dt_node::{build_path, get_name_and_unit_addr},
    macros::{MacroCtx, env::TrackedMapEnvMut},
    mir::{MirCell, MirDefinition, MirDefinitionValue, MirPropertyData, MirProvenance, MirValue},
};

/// Lowers an [`ast::DtProperty`] to a [`MirDefinition`], if valid.
///
/// Returns `None` if the property has no name or can't be processed.
pub(crate) fn process_dt_property(
    ctx: &mut IntraFileCtx<'_, '_, impl DiagnosticCollector<File>>,
    parent_node_path: &str,
    prop: &ast::DtProperty,
) -> Option<MirDefinition> {
    let mut values = Vec::new();
    for value_ast in prop.values() {
        if let Some(value) = lower_prop_value(
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

    Some(MirDefinition {
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
) -> Option<MirValue> {
    match value {
        ast::PropValue::String(tok) => {
            match dt_tools_analyzer::string::interpret_escaped_string(tok.text()) {
                Ok(path) => Some(MirValue::String(path)),
                Err(err) => {
                    diag.emit(Diagnostic::new(
                        spanner(tok.text_range()),
                        Cow::Owned(format!("Failed to parse string: {err}")),
                        Severity::Error,
                    ));
                    None
                }
            }
        }
        ast::PropValue::CellList(cell_list) => {
            let cells: Vec<MirCell> = cell_list
                .cells()
                .filter_map(|cell| lower_cell(db, env, diag, spanner, &cell))
                .collect();
            Some(MirValue::CellList(cells))
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
                return None;
            }

            Some(MirValue::Bytestring(bytes))
        }
        ast::PropValue::Phandle(phandle) => {
            let target = lower_phandle(db, env, diag, spanner, phandle)?;
            Some(MirValue::Phandle(target))
        }
        ast::PropValue::Macro(macro_inv) => resolve_macro_to_value(
            db,
            env,
            diag,
            spanner,
            &MacroCtx::Explicit(macro_inv),
            Entrypoint::PropValues,
            lower_prop_value,
        ),
    }
}

/// Lowers an [`ast::Cell`] to a [`MirCell`], if valid.
pub(crate) fn lower_cell<'db>(
    db: &'db dyn BaseDb,
    env: &mut TrackedMapEnvMut<'db>,
    diag: &impl DiagnosticCollector<File>,
    spanner: &mut impl FnMut(TextRange) -> Span<File>,
    cell: &ast::Cell,
) -> Option<MirCell> {
    match cell {
        ast::Cell::Number(tok) => parse_int_tok::<u32>(
            tok,
            diag,
            spanner,
            u32::from_str_radix,
            "32-bit unsigned integer",
        )
        .map(MirCell::U32),
        ast::Cell::Char(tok) => {
            let val = interpret_escaped_char_tok(tok, diag, spanner)?;

            Some(MirCell::U32(val as u32))
        }
        ast::Cell::Phandle(phandle) => Some(MirCell::Phandle(lower_phandle(
            db, env, diag, spanner, phandle,
        )?)),
        ast::Cell::Macro(macro_inv) => resolve_macro_to_value(
            db,
            env,
            diag,
            spanner,
            &MacroCtx::Explicit(macro_inv),
            Entrypoint::Cells,
            lower_cell,
        ),
        ast::Cell::DtExpr(dt_expr) => {
            let expr = dt_expr.expr()?;
            let num = expr_eval::eval(db, env, expr, diag, spanner)?;
            if let Ok(num) = u32::try_from(num) {
                Some(MirCell::U32(num))
            } else if let Ok(num) = i32::try_from(num) {
                // Encoded using two's complement
                Some(MirCell::U32(num.cast_unsigned()))
            } else {
                let cmp = if num < 0 { "small" } else { "large" };

                diag.emit(Diagnostic::new(
                    spanner(cell.syntax_item().text_range()),
                    Cow::Owned(format!(
                        "number {num} too {cmp} to fit in 32-bit signed integer (using two's complement) or 32-bit unsigned integer"
                    )),
                    Severity::Error,
                ));
                None
            }
        }
    }
}
