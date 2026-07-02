use std::{borrow::Cow, num::ParseIntError, str::FromStr, sync::Arc};

use dt_tools_analyzer::{macros::substitute_macro_ast, string::interpret_escaped_char};
use dt_tools_diagnostic::{Diagnostic, DiagnosticCollector, Severity, Span};
use dt_tools_parser::{
    TextRange,
    ast::{self, AstNode},
    cst::RedToken,
    lexer::TokenKind,
};

use crate::salsa::{db::BaseDb, file::File, macros::env::TrackedMapEnvMut};

/// Parses an integer and emits errors.
///
/// # Example
///
/// ```ignore
/// parse_int::<i64>(
///     src,
///     i64::from_str_radix,
///     text_range,
///     diag,
///     "64-bit signed integer"
/// )
/// ```
pub fn parse_int_tok<T: FromStr<Err = ParseIntError>>(
    tok: &Arc<RedToken>,
    diag: &impl DiagnosticCollector<File>,
    spanner: &mut impl FnMut(TextRange) -> Span<File>,
    from_str_radix: impl FnOnce(&str, u32) -> Result<T, ParseIntError>,
    type_description: &str,
) -> Option<T> {
    let emit_err = |err: &ParseIntError| {
        let msg = err.to_string().replace("target type", type_description);
        diag.emit(Diagnostic::new(
            spanner(tok.text_range()),
            msg.into(),
            Severity::Error,
        ));
    };

    let src = tok.text().as_str();
    if src.starts_with("0x") || src.starts_with("0X") {
        // Hexadecimal
        from_str_radix(&src[2..], 16).inspect_err(emit_err).ok()
    } else {
        src.parse().inspect_err(emit_err).ok()
    }
}

pub fn interpret_escaped_char_tok(
    tok: &Arc<RedToken>,
    diag: &impl DiagnosticCollector<File>,
    spanner: &mut impl FnMut(TextRange) -> Span<File>,
) -> Option<char> {
    interpret_escaped_char(tok.text())
        .inspect_err(|err| {
            diag.emit(Diagnostic::new(
                spanner(tok.text_range()),
                err.to_string().into(),
                Severity::Error,
            ));
        })
        .ok()
}

// TODO: in preprocessor conditional mode, undefined macros that are used as object-like macros evaluate to zero as defined here: https://gcc.gnu.org/onlinedocs/cpp/If.html
#[expect(clippy::too_many_lines, reason = "It's pretty enough as is")]
pub fn eval(
    db: &dyn BaseDb,
    env: &TrackedMapEnvMut,
    ast: ast::Expr,
    diag: &impl DiagnosticCollector<File>,
    spanner: &mut impl FnMut(TextRange) -> Span<File>,
) -> Option<i64> {
    match ast {
        ast::Expr::PrefixExpr(prefix_expr) => {
            let mut value = || eval(db, env, prefix_expr.expr()?, diag, spanner);
            match prefix_expr.op()? {
                TokenKind::Plus => value(),
                TokenKind::Minus => Some(-value()?),
                TokenKind::Ident => {
                    // `defined` operator
                    let inner_expr = prefix_expr.expr()?;

                    let inner_expr = match inner_expr.into_paren_expr() {
                        Ok(paren_expr) => paren_expr.expr()?,
                        Err(inner_expr) => inner_expr,
                    };

                    let macro_ast = inner_expr.into_macro_invocation().ok()?;
                    let name: &str = &macro_ast.green_ident()?.text;
                    Some(i64::from(env.get_macro_def(db, name).is_some()))
                }
                TokenKind::BitwiseNot => Some(!value()?),
                TokenKind::LogicalNot => Some(i64::from(value()? == 0)),
                _ => None,
            }
        }
        ast::Expr::ParenExpr(paren_expr) => eval(db, env, paren_expr.expr()?, diag, spanner),
        ast::Expr::MacroInvocation(macro_invocation) => {
            let ident = macro_invocation.ident()?;
            let name: &str = ident.text();
            let Some(def) = env.get_macro_def(db, name) else {
                diag.emit(Diagnostic::new(
                    spanner(ident.text_range()),
                    Cow::Owned(format!("Macro `{name}` is not defined")),
                    Severity::Error,
                ));
                return None;
            };

            // TODO: error handling
            let (_trmaps, s) = substitute_macro_ast(Some(&macro_invocation), def).ok()?;

            let parse = dt_tools_parser::parser::Entrypoint::PreprocessorConditional.parse(&s);

            if !parse.lex_errors.is_empty() || !parse.errors.is_empty() {
                diag.emit(Diagnostic::new(
                    spanner(macro_invocation.syntax().text_range()),
                    Cow::Borrowed("Failed to parse the result of this macro invocation"),
                    Severity::Error,
                ));
                // TODO: trmaps and error handling
                return None;
            }

            // TODO: prevent infinite recursion...

            let expr_ast = parse.red_node().child_nodes().find_map(ast::Expr::cast)?;

            // FIXME: diag needs trmaps
            // TODO: wrap spanner!
            eval(db, env, expr_ast, diag, spanner)
        }
        ast::Expr::LiteralExpr(literal_expr) => {
            if let Some(number) = literal_expr
                .syntax()
                .child_tokens()
                .find(|tok| tok.green.kind == TokenKind::Number)
            {
                parse_int_tok::<i64>(
                    &number,
                    diag,
                    spanner,
                    i64::from_str_radix,
                    "64-bit signed integer",
                )
            } else if let Some(char) = literal_expr
                .syntax()
                .child_tokens()
                .find(|tok| tok.green.kind == TokenKind::Char)
            {
                let val = interpret_escaped_char_tok(&char, diag, spanner)?;

                Some(val as i64)
            } else {
                None
            }
        }
        ast::Expr::InfixExpr(infix_expr) => {
            let mut iter = infix_expr.syntax().children();
            let lhs = iter.find_map(|child| ast::Expr::cast(child.as_node()?.clone()))?;

            let op = iter
                .by_ref()
                .filter_map(dt_tools_parser::cst::TreeItem::into_token)
                .find(|tok| !tok.green.kind.is_trivia())?;

            let mhs = if op.green.kind == TokenKind::QuestionMark {
                let mhs = iter.find_map(|child| ast::Expr::cast(child.as_node()?.clone()))?;
                iter.by_ref()
                    .filter_map(dt_tools_parser::cst::TreeItem::into_token)
                    .find(|tok| tok.green.kind == TokenKind::Colon)?;
                Some(mhs)
            } else {
                None
            };

            let rhs = iter.find_map(|child| ast::Expr::cast(child.as_node()?.clone()))?;

            let lhs_value = eval(db, env, lhs, diag, spanner)?;
            let rhs_value = || eval(db, env, rhs, diag, spanner);

            Some(match op.green.kind {
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
                        let mhs = mhs?;
                        eval(db, env, mhs, diag, spanner)?
                    } else {
                        rhs_value()?
                    }
                }
                _ => return None,
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use dt_tools_analyzer::macros::MacroDefinition;
    use dt_tools_parser::{TextRange, ast::AstNode, parser::Entrypoint};

    use crate::salsa::{db::BaseDb, macros::env::TrackedMapEnvMut};

    fn check(input: &str, env: &TrackedMapEnvMut) -> Option<i64> {
        let db = crate::salsa::db::BaseDatabase::default();

        let parse = Entrypoint::PreprocessorConditional.parse(input);
        let ast = dt_tools_parser::ast::Expr::cast(parse.red_node().child_nodes().next().unwrap())
            .unwrap();

        let mut diagnostics = Vec::new();
        let diag = parking_lot::Mutex::new(&mut diagnostics);
        let file = db
            .get_files()
            .add_virtual(&db, "/main.dts".into(), String::new());

        let num = super::eval(&db, env, ast, &diag, &mut |text_range| {
            text_range.within_file(file)
        });

        assert!(
            diagnostics.is_empty(),
            "There should be no diagnostics: {diagnostics:#?}"
        );

        num
    }

    #[test]
    fn test_literal() {
        let env = TrackedMapEnvMut::default();
        assert_eq!(check("42", &env), Some(42));
    }

    #[test]
    fn test_paren() {
        let env = TrackedMapEnvMut::default();
        assert_eq!(check("(1 + 2) * 3", &env), Some(9));
    }

    #[test]
    fn test_infix() {
        let env = TrackedMapEnvMut::default();
        assert_eq!(check("21 + 21", &env), Some(42));
        assert_eq!(check("21 * 2", &env), Some(42));
        assert_eq!(check("1 + 2 * 3", &env), Some(7));
    }

    #[test]
    fn test_prefix() {
        let env = TrackedMapEnvMut::default();
        assert_eq!(check("!42", &env), Some(0));
        assert_eq!(check("~42", &env), Some(!42));
    }

    #[test]
    fn test_ternary() {
        let env = TrackedMapEnvMut::default();
        assert_eq!(check("1 ? 2 : 3", &env), Some(2));
        assert_eq!(check("0 ? 2 : 3", &env), Some(3));
    }

    #[test]
    fn test_prefix_defined() {
        let db = crate::salsa::db::BaseDatabase::default();
        let file = db
            .get_files()
            .add_virtual(&db, "/main.dts".into(), String::new());
        let span = TextRange::new(0, 0).within_file(file);

        let mut env = TrackedMapEnvMut::default();
        let def = MacroDefinition::parse("#define FOO").unwrap();
        env.insert_macro(def, span);

        assert_eq!(check("defined FOO", &env), Some(1));
        assert_eq!(check("defined BAR", &env), Some(0));
        assert_eq!(check("defined(FOO)", &env), Some(1));
        assert_eq!(check("defined(BAR)", &env), Some(0));
    }

    #[test]
    fn test_macro() {
        let db = crate::salsa::db::BaseDatabase::default();
        let file = db
            .get_files()
            .add_virtual(&db, "/main.dts".into(), String::new());
        let span = TextRange::new(0, 0).within_file(file);

        let mut env = TrackedMapEnvMut::default();

        let def = MacroDefinition::parse("#define FOO BAR").unwrap();
        env.insert_macro(def, span);

        let def = MacroDefinition::parse("#define BAR 42").unwrap();
        env.insert_macro(def, span);

        assert_eq!(check("FOO", &env), Some(42));
    }

    // TODO: shouldn't ever overflow stack?
    #[test]
    #[ignore = "overflows stack"]
    fn test_macro_self_recursion() {
        let db = crate::salsa::db::BaseDatabase::default();
        let file = db
            .get_files()
            .add_virtual(&db, "/main.dts".into(), String::new());
        let span = TextRange::new(0, 0).within_file(file);

        let mut env = TrackedMapEnvMut::default();

        let def = MacroDefinition::parse("#define FOO FOO").unwrap();
        env.insert_macro(def, span);

        assert_eq!(check("FOO", &env), Some(42));
    }
}
