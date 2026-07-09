use std::{num::ParseIntError, str::FromStr, sync::Arc};

use dt_tools_analyzer::string::interpret_escaped_char;
use dt_tools_diagnostic::Severity;
use dt_tools_parser::{
    ast::{self, AstNode},
    cst::RedToken,
    lexer::TokenKind,
    parser::Entrypoint,
};
use num_traits::Num;

use crate::{
    db::BaseDb,
    diag::{Diag, SourceMap},
    extra_num_traits::{Bits, Signedness},
    lowering::resolve_macro_to_ast,
    macros::{MacroCtx, env::TrackedMapEnvMut},
};

/// Parses an integer and emits errors.
pub fn parse_int_tok<
    T: FromStr<Err = ParseIntError> + Num<FromStrRadixErr = ParseIntError> + Bits + Signedness,
>(
    tok: &Arc<RedToken>,
    diag: &mut Diag<'_, '_>,
) -> Result<T, ()> {
    /// Helper function that isn't monomorphized
    fn emit_err(
        err: &ParseIntError,
        diag: &mut Diag<'_, '_>,
        tok: &Arc<RedToken>,
        bits: u32,
        signedness: &'static str,
    ) {
        let msg = err
            .to_string()
            .replace("target type", &format!("{bits}-bit {signedness} integer"));
        diag.emit(tok.text_range(), msg, Severity::Error);
    }

    let src = tok.text().as_str();
    if src.starts_with("0x") || src.starts_with("0X") {
        // Hexadecimal
        T::from_str_radix(&src[2..], 16)
    } else if src.starts_with("0b") || src.starts_with("0B") {
        // Binary
        T::from_str_radix(&src[2..], 2)
    } else if src.starts_with("0o") || src.starts_with("0O") {
        // Octal
        T::from_str_radix(&src[2..], 8)
    } else {
        // Decimal
        src.parse()
    }
    .map_err(|err| emit_err(&err, diag, tok, T::BITS, T::SIGNEDNESS))
}

/// Wraps [`interpret_escaped_char`], handling errors.
pub fn interpret_escaped_char_tok(
    tok: &Arc<RedToken>,
    diag: &mut Diag<'_, '_>,
) -> Result<char, ()> {
    interpret_escaped_char(tok.text()).map_err(|err| {
        diag.emit(tok.text_range(), err.to_string(), Severity::Error);
    })
}

// TODO: in preprocessor conditional mode, undefined macros that are used as object-like macros evaluate to zero as defined here: https://gcc.gnu.org/onlinedocs/cpp/If.html
#[expect(clippy::too_many_lines, reason = "It's pretty enough as is")]
pub fn eval(
    db: &dyn BaseDb,
    env: &TrackedMapEnvMut,
    ast: ast::Expr,
    diag: &mut Diag<'_, '_>,
) -> Result<i64, ()> {
    match ast {
        ast::Expr::PrefixExpr(prefix_expr) => {
            let mut value = || eval(db, env, prefix_expr.expr().ok_or(())?, diag);
            match prefix_expr.op().ok_or(())? {
                TokenKind::Plus => value(),
                TokenKind::Minus => Ok(-value()?),
                TokenKind::Ident => {
                    // `defined` operator
                    let inner_expr = prefix_expr.expr().ok_or(())?;

                    let inner_expr = match inner_expr.into_paren_expr() {
                        Ok(paren_expr) => paren_expr.expr().ok_or(())?,
                        Err(inner_expr) => inner_expr,
                    };

                    let macro_ast = inner_expr.into_macro_invocation().ok().ok_or(())?;
                    let name: &str = &macro_ast.green_ident().ok_or(())?.text;
                    Ok(i64::from(env.get_macro_def(db, name).is_some()))
                }
                TokenKind::BitwiseNot => Ok(!value()?),
                TokenKind::LogicalNot => Ok(i64::from(value()? == 0)),
                _ => Err(()),
            }
        }
        ast::Expr::ParenExpr(paren_expr) => eval(db, env, paren_expr.expr().ok_or(())?, diag),
        ast::Expr::MacroInvocation(macro_invocation) => {
            let (expr_ast, expansion) = resolve_macro_to_ast::<ast::Expr>(
                db,
                env,
                diag,
                &MacroCtx::Explicit(&macro_invocation),
                Entrypoint::PreprocessorConditional,
            )?
            .expect("resolve_macro_to_value should not return Ok(None) with an explicit macro");

            // TODO: prevent infinite recursion

            let child_map = SourceMap::Macro {
                parent: diag.map,
                expansion: &expansion,
            };
            eval(
                db,
                env,
                expr_ast,
                &mut Diag::new(&mut *diag.sink, &child_map),
            )
        }
        ast::Expr::LiteralExpr(literal_expr) => {
            if let Some(number) = literal_expr
                .syntax()
                .child_tokens()
                .find(|tok| tok.green.kind == TokenKind::Number)
            {
                parse_int_tok::<i64>(&number, diag)
            } else if let Some(char) = literal_expr
                .syntax()
                .child_tokens()
                .find(|tok| tok.green.kind == TokenKind::Char)
            {
                let val = interpret_escaped_char_tok(&char, diag)?;

                Ok(val as i64)
            } else {
                Err(())
            }
        }
        ast::Expr::InfixExpr(infix_expr) => {
            let mut iter = infix_expr.syntax().children();
            let lhs = iter
                .find_map(|child| ast::Expr::cast(child.as_node()?.clone()))
                .ok_or(())?;

            let op = iter
                .by_ref()
                .filter_map(dt_tools_parser::cst::TreeItem::into_token)
                .find(|tok| !tok.green.kind.is_trivia())
                .ok_or(())?;

            let mhs = if op.green.kind == TokenKind::QuestionMark {
                let mhs = iter
                    .find_map(|child| ast::Expr::cast(child.as_node()?.clone()))
                    .ok_or(())?;
                iter.by_ref()
                    .filter_map(dt_tools_parser::cst::TreeItem::into_token)
                    .find(|tok| tok.green.kind == TokenKind::Colon)
                    .ok_or(())?;
                Some(mhs)
            } else {
                None
            };

            let rhs = iter
                .find_map(|child| ast::Expr::cast(child.as_node()?.clone()))
                .ok_or(())?;

            let lhs_value = eval(db, env, lhs, diag)?;
            let rhs_value = || eval(db, env, rhs, diag);

            Ok(match op.green.kind {
                TokenKind::Plus => lhs_value + rhs_value()?,
                TokenKind::Minus => lhs_value - rhs_value()?,
                TokenKind::Asterisk => lhs_value * rhs_value()?,
                TokenKind::Slash => lhs_value / rhs_value()?,
                TokenKind::Modulo => lhs_value % rhs_value()?,
                TokenKind::Ampersand => lhs_value & rhs_value()?,
                TokenKind::BitwiseOr => lhs_value | rhs_value()?,
                TokenKind::BitwiseXor => lhs_value ^ rhs_value()?,
                TokenKind::BitwiseShl => lhs_value << rhs_value()?,
                TokenKind::BitwiseShr => lhs_value >> rhs_value()?,
                TokenKind::LogicalAnd => i64::from(lhs_value != 0 && rhs_value()? != 0),
                TokenKind::LogicalOr => i64::from(lhs_value != 0 || rhs_value()? != 0),
                TokenKind::LAngle => i64::from(lhs_value < rhs_value()?),
                TokenKind::RAngle => i64::from(lhs_value > rhs_value()?),
                TokenKind::RelationalLtEq => i64::from(lhs_value <= rhs_value()?),
                TokenKind::RelationalGtEq => i64::from(lhs_value >= rhs_value()?),
                TokenKind::RelationalEq => i64::from(lhs_value == rhs_value()?),
                TokenKind::RelationalNeq => i64::from(lhs_value != rhs_value()?),
                TokenKind::QuestionMark => {
                    let cond = lhs_value != 0;
                    if cond {
                        let mhs = mhs.ok_or(())?;
                        eval(db, env, mhs, diag)?
                    } else {
                        rhs_value()?
                    }
                }
                _ => return Err(()),
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use dt_tools_analyzer::macros::MacroDefinition;
    use dt_tools_parser::{TextRange, ast::AstNode, parser::Entrypoint};

    use crate::{db::BaseDb, macros::env::TrackedMapEnvMut};

    fn check(input: &str, env: &TrackedMapEnvMut) -> Result<i64, ()> {
        let db = crate::db::BaseDatabase::default();

        let parse = Entrypoint::PreprocessorConditional.parse(input);
        let ast = dt_tools_parser::ast::Expr::cast(parse.red_node().child_nodes().next().unwrap())
            .unwrap();

        let mut diagnostics = Vec::new();
        let file = db
            .get_files()
            .add_virtual(&db, "/main.dts".into(), String::new());
        let map = crate::diag::SourceMap::File(file);

        let num = super::eval(
            &db,
            env,
            ast,
            &mut crate::diag::Diag::new(&mut diagnostics, &map),
        );

        assert!(
            diagnostics.is_empty(),
            "There should be no diagnostics: {diagnostics:#?}"
        );

        num
    }

    #[test]
    fn test_literal() {
        let env = TrackedMapEnvMut::default();
        assert_eq!(check("42", &env), Ok(42));
    }

    #[test]
    fn test_paren() {
        let env = TrackedMapEnvMut::default();
        assert_eq!(check("(1 + 2) * 3", &env), Ok(9));
    }

    #[test]
    fn test_infix() {
        let env = TrackedMapEnvMut::default();
        assert_eq!(check("21 + 21", &env), Ok(42));
        assert_eq!(check("21 * 2", &env), Ok(42));
        assert_eq!(check("1 + 2 * 3", &env), Ok(7));
    }

    #[test]
    fn test_prefix() {
        let env = TrackedMapEnvMut::default();
        assert_eq!(check("!42", &env), Ok(0));
        assert_eq!(check("~42", &env), Ok(!42));
    }

    #[test]
    fn test_ternary() {
        let env = TrackedMapEnvMut::default();
        assert_eq!(check("1 ? 2 : 3", &env), Ok(2));
        assert_eq!(check("0 ? 2 : 3", &env), Ok(3));
    }

    #[test]
    fn test_prefix_defined() {
        let db = crate::db::BaseDatabase::default();
        let file = db
            .get_files()
            .add_virtual(&db, "/main.dts".into(), String::new());
        let span = TextRange::new(0, 0).within_file(file);

        let mut env = TrackedMapEnvMut::default();
        let def = MacroDefinition::parse("#define FOO").unwrap();
        env.insert_macro(def, span);

        assert_eq!(check("defined FOO", &env), Ok(1));
        assert_eq!(check("defined BAR", &env), Ok(0));
        assert_eq!(check("defined(FOO)", &env), Ok(1));
        assert_eq!(check("defined(BAR)", &env), Ok(0));
    }

    #[test]
    fn test_macro() {
        let db = crate::db::BaseDatabase::default();
        let file = db
            .get_files()
            .add_virtual(&db, "/main.dts".into(), String::new());
        let span = TextRange::new(0, 0).within_file(file);

        let mut env = TrackedMapEnvMut::default();

        let def = MacroDefinition::parse("#define FOO BAR").unwrap();
        env.insert_macro(def, span);

        let def = MacroDefinition::parse("#define BAR 42").unwrap();
        env.insert_macro(def, span);

        assert_eq!(check("FOO", &env), Ok(42));
    }

    // TODO: shouldn't ever overflow stack?
    #[test]
    #[ignore = "overflows stack"]
    fn test_macro_self_recursion() {
        let db = crate::db::BaseDatabase::default();
        let file = db
            .get_files()
            .add_virtual(&db, "/main.dts".into(), String::new());
        let span = TextRange::new(0, 0).within_file(file);

        let mut env = TrackedMapEnvMut::default();

        let def = MacroDefinition::parse("#define FOO FOO").unwrap();
        env.insert_macro(def, span);

        assert_eq!(check("FOO", &env), Ok(42));
    }
}
