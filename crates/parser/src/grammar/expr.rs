//! Note that binding power is often abbreviated as `bp` here.
//!
//! See <https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html> for how the algorithm works.

use std::borrow::Cow;

use crate::{
    cst::NodeKind,
    grammar::macro_invocation,
    lexer::TokenKind,
    parser::{Expected, Parser},
};

#[derive(PartialEq)]
enum InfixOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseShl,
    BitwiseShr,
    LogicalAnd,
    LogicalOr,
    Lt,
    Gt,
    LtEq,
    GtEq,
    Eq,
    Neq,
    TernaryQuestionMark,
}

impl InfixOp {
    fn at(p: &mut Parser) -> Option<Self> {
        p.add_expected(Expected::InfixOperator);
        Some(match p.peek()? {
            TokenKind::Plus => Self::Add,
            TokenKind::Minus => Self::Sub,
            TokenKind::Asterisk => Self::Mul,
            TokenKind::Slash => Self::Div,
            TokenKind::Modulo => Self::Mod,
            TokenKind::Ampersand => Self::BitwiseAnd,
            TokenKind::BitwiseOr => Self::BitwiseOr,
            TokenKind::BitwiseXor => Self::BitwiseXor,
            TokenKind::BitwiseShl => Self::BitwiseShl,
            TokenKind::BitwiseShr => Self::BitwiseShr,
            TokenKind::LogicalAnd => Self::LogicalAnd,
            TokenKind::LogicalOr => Self::LogicalOr,
            TokenKind::LAngle => Self::Lt,
            TokenKind::RAngle => Self::Gt,
            TokenKind::RelationalLtEq => Self::LtEq,
            TokenKind::RelationalGtEq => Self::GtEq,
            TokenKind::RelationalEq => Self::Eq,
            TokenKind::RelationalNeq => Self::Neq,
            TokenKind::QuestionMark => Self::TernaryQuestionMark,
            _ => return None,
        })
    }
    /// See also [`PrefixOp::binding_power`].
    fn binding_power(&self) -> (u8, u8) {
        // Order taken from here: https://en.cppreference.com/w/c/language/operator_precedence.html
        match self {
            Self::TernaryQuestionMark => (2, 1),
            Self::LogicalOr => (3, 4),
            Self::LogicalAnd => (5, 6),
            Self::BitwiseOr => (7, 8),
            Self::BitwiseXor => (9, 10),
            Self::BitwiseAnd => (11, 12),
            Self::Eq | Self::Neq => (13, 14),
            Self::Lt | Self::Gt | Self::LtEq | Self::GtEq => (15, 16),
            Self::BitwiseShl | Self::BitwiseShr => (17, 18),

            Self::Add | Self::Sub => (19, 20),
            Self::Mul | Self::Div | Self::Mod => (21, 22),
        }
    }
}

#[derive(PartialEq)]
enum PrefixOp {
    UnaryPlus,
    Neg,
    // A prefix operator like `sizeof`
    Defined,
    BitwiseNot,
    LogicalNot,
}

impl PrefixOp {
    fn at(p: &mut Parser, defined_operator: bool) -> Option<Self> {
        p.add_expected(Expected::PrefixOperator);
        if defined_operator {
            p.add_expected(Expected::PreprocessorDefinedOperator);
        }
        Some(match p.peek()? {
            TokenKind::Plus => Self::UnaryPlus,
            TokenKind::Minus => Self::Neg,
            TokenKind::Ident if defined_operator && p.text() == Some("defined") => Self::Defined,
            TokenKind::BitwiseNot => Self::BitwiseNot,
            TokenKind::LogicalNot => Self::LogicalNot,
            _ => return None,
        })
    }
    /// See also [`InfixOp::binding_power`].
    fn binding_power() -> ((), u8) {
        // Order taken from here: https://en.cppreference.com/w/c/language/operator_precedence.html
        ((), 23)
    }
}

/// Parses an expression that should evaluate to an integer.
pub fn expr(p: &mut Parser, defined_operator: bool) -> Result<(), ()> {
    expr_binding_power(p, defined_operator, 0)
}

fn expr_binding_power(p: &mut Parser, defined_operator: bool, min_bp: u8) -> Result<(), ()> {
    let mut m_infix = p.start();

    lhs(p, defined_operator)?;

    // If None: We’re not at an operator; we don’t know what to do next, so we return and let the caller decide.
    while let Some(op) = InfixOp::at(p) {
        let (left_bp, right_bp) = op.binding_power();

        if left_bp < min_bp {
            break;
        }

        // Eat the operator’s token.
        p.bump();

        if op == InfixOp::TernaryQuestionMark {
            if expr_binding_power(p, defined_operator, 0).is_err() {
                m_infix = m_infix.complete(p, NodeKind::InfixExpr).precede(p);
                break;
            }

            if !p.expect(TokenKind::Colon) {
                // FIXME: return Err?
                m_infix = m_infix.complete(p, NodeKind::InfixExpr).precede(p);
                break;
            }
        }

        let parsed_rhs = expr_binding_power(p, defined_operator, right_bp).is_ok();

        m_infix = m_infix.complete(p, NodeKind::InfixExpr).precede(p);

        if !parsed_rhs {
            break;
        }
    }

    // m.ignore() instead of (lhs as CompletedMarker).precede() so all lhses don't have to be CST nodes
    m_infix.ignore(p);

    Ok(())
}

fn lhs(p: &mut Parser, defined_operator: bool) -> Result<(), ()> {
    if let Some(_op) = PrefixOp::at(p, defined_operator) {
        let m = p.start();

        let ((), right_bp) = PrefixOp::binding_power();

        // Eat the operator's token
        p.bump();

        let res = expr_binding_power(p, defined_operator, right_bp);

        m.complete(p, NodeKind::PrefixExpr);
        res
    } else if p.at(TokenKind::LParen) {
        let m = p.start();
        p.bump();

        let res = expr_binding_power(p, defined_operator, 0);

        p.expect(TokenKind::RParen);

        m.complete(p, NodeKind::ParenExpr);
        res
    } else if p.at(TokenKind::Ident) {
        macro_invocation(p);
        Ok(())
    } else if p.at(&[TokenKind::Number, TokenKind::Char]) {
        // TODO: no node!
        let m = p.start();
        p.bump();
        m.complete(p, NodeKind::LiteralExpr);
        Ok(())
    } else if let Some(op) = InfixOp::at(p)
        && op != InfixOp::TernaryQuestionMark
    {
        // Error recovery: missing lhs but existing op

        p.error()
            .msg_expected()
            .add_hint(Cow::Borrowed(
                "Recovered as expression with missing left-hand-side expression",
            ))
            .emit();
        let m = p.start();
        p.bump();
        let _ = expr_binding_power(p, defined_operator, 0);
        m.complete(p, NodeKind::ParseError);
        Err(())
    } else if p.check(TokenKind::RParen).silent().at() {
        // Error recovery: don't bump RParen as a parent may be expecting it

        p.error().msg_expected().emit();
        Err(())
    } else {
        p.error().msg_expected().bump_wrap_err().emit();
        Err(())
    }
}

/// Syntax documentation: <https://gcc.gnu.org/onlinedocs/cpp/If.html>
pub(crate) fn entry_preprocessor_conditional(p: &mut Parser) {
    let _ = expr(p, true);
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::{grammar::tests::check_ep, parser::Entrypoint};

    #[test]
    fn parse_math() {
        check_ep(
            Entrypoint::PreprocessorConditional,
            r"1 * 2 + 3",
            expect![[r#"
                Errors: []

                Tree:
                EntryPreprocessorConditional@0..9
                  InfixExpr@0..9
                    InfixExpr@0..5
                      LiteralExpr@0..1
                        Number@0..1 "1"
                      Whitespace@1..2 " "
                      Asterisk@2..3 "*"
                      Whitespace@3..4 " "
                      LiteralExpr@4..5
                        Number@4..5 "2"
                    Whitespace@5..6 " "
                    Plus@6..7 "+"
                    Whitespace@7..8 " "
                    LiteralExpr@8..9
                      Number@8..9 "3"
            "#]],
        );
    }

    #[test]
    fn parse_ternary() {
        check_ep(
            Entrypoint::PreprocessorConditional,
            r"CONDITION ? 1 : 2",
            expect![[r#"
                Errors: []

                Tree:
                EntryPreprocessorConditional@0..17
                  InfixExpr@0..17
                    MacroInvocation@0..9
                      Ident@0..9 "CONDITION"
                    Whitespace@9..10 " "
                    QuestionMark@10..11 "?"
                    Whitespace@11..12 " "
                    LiteralExpr@12..13
                      Number@12..13 "1"
                    Whitespace@13..14 " "
                    Colon@14..15 ":"
                    Whitespace@15..16 " "
                    LiteralExpr@16..17
                      Number@16..17 "2"
            "#]],
        );
    }

    #[test]
    fn parse_defined() {
        check_ep(
            Entrypoint::PreprocessorConditional,
            r"defined FOO",
            expect![[r#"
                Errors: []

                Tree:
                EntryPreprocessorConditional@0..11
                  PrefixExpr@0..11
                    Ident@0..7 "defined"
                    Whitespace@7..8 " "
                    MacroInvocation@8..11
                      Ident@8..11 "FOO"
            "#]],
        );
        check_ep(
            Entrypoint::PreprocessorConditional,
            r"defined(FOO)",
            expect![[r#"
                Errors: []

                Tree:
                EntryPreprocessorConditional@0..12
                  PrefixExpr@0..12
                    Ident@0..7 "defined"
                    ParenExpr@7..12
                      LParen@7..8 "("
                      MacroInvocation@8..11
                        Ident@8..11 "FOO"
                      RParen@11..12 ")"
            "#]],
        );
    }

    #[test]
    fn parse_pp_cond() {
        // TODO: e2e test with evaluator: binary operators should evaluate to 1 or 0

        // TODO: e2e test with evaluator: should re-parse whole expression...
        // #define three 1 + 2
        // example = three / 2;
        // example = 1 + 2 / 2; // what it should be!
        // example = (1 + 2) / 2; // what a naive reparser would do

        check_ep(
            Entrypoint::PreprocessorConditional,
            r"0 || 2",
            expect![[r#"
                Errors: []

                Tree:
                EntryPreprocessorConditional@0..6
                  InfixExpr@0..6
                    LiteralExpr@0..1
                      Number@0..1 "0"
                    Whitespace@1..2 " "
                    LogicalOr@2..4 "||"
                    Whitespace@4..5 " "
                    LiteralExpr@5..6
                      Number@5..6 "2"
            "#]],
        );

        check_ep(
            Entrypoint::PreprocessorConditional,
            r"defined FOO",
            expect![[r#"
                Errors: []

                Tree:
                EntryPreprocessorConditional@0..11
                  PrefixExpr@0..11
                    Ident@0..7 "defined"
                    Whitespace@7..8 " "
                    MacroInvocation@8..11
                      Ident@8..11 "FOO"
            "#]],
        );

        // Extra
        check_ep(
            Entrypoint::PreprocessorConditional,
            r"0 0",
            expect![[r#"
                Errors: [
                    ParseError {
                        message: "Expected infix operator or end-of-file, but found number literal",
                        primary_text_range: TextRange {
                            start: 2,
                            end: 3,
                        },
                        span_labels: [],
                    },
                ]

                Tree:
                EntryPreprocessorConditional@0..3
                  LiteralExpr@0..1
                    Number@0..1 "0"
                  Whitespace@1..2 " "
                  ParseError@2..3
                    Number@2..3 "0"
            "#]],
        );
    }
}
