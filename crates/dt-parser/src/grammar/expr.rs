//! Note that binding power is often abbreviated as `bp` here.
//!
//! See <https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html> for how the algorithm works.

use std::borrow::Cow;

use crate::{
    cst::NodeKind,
    grammar::macro_invocation,
    lexer::TokenKind,
    parser::{CompletedMarker, Expected, Parser},
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
        Some(if p.at(TokenKind::Plus) {
            Self::Add
        } else if p.at(TokenKind::Minus) {
            Self::Sub
        } else if p.at(TokenKind::Asterisk) {
            Self::Mul
        } else if p.at(TokenKind::Slash) {
            Self::Div
        } else if p.at(TokenKind::Modulo) {
            Self::Mod
        } else if p.at(TokenKind::Ampersand) {
            Self::BitwiseAnd
        } else if p.at(TokenKind::BitwiseOr) {
            Self::BitwiseOr
        } else if p.at(TokenKind::BitwiseXor) {
            Self::BitwiseXor
        } else if p.at(TokenKind::BitwiseShl) {
            Self::BitwiseShl
        } else if p.at(TokenKind::BitwiseShr) {
            Self::BitwiseShr
        } else if p.at(TokenKind::LogicalAnd) {
            Self::LogicalAnd
        } else if p.at(TokenKind::LogicalOr) {
            Self::LogicalOr
        } else if p.at(TokenKind::LAngle) {
            Self::Lt
        } else if p.at(TokenKind::RAngle) {
            Self::Gt
        } else if p.at(TokenKind::RelationalLtEq) {
            Self::LtEq
        } else if p.at(TokenKind::RelationalGtEq) {
            Self::GtEq
        } else if p.at(TokenKind::RelationalEq) {
            Self::Eq
        } else if p.at(TokenKind::RelationalNeq) {
            Self::Neq
        } else if p.at(TokenKind::QuestionMark) {
            Self::TernaryQuestionMark
        } else {
            return None;
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
        Some(if p.at(TokenKind::Plus) {
            Self::UnaryPlus
        } else if p.at(TokenKind::Minus) {
            Self::Neg
        } else if defined_operator && {
            p.add_expected(Expected::PreprocessorDefinedOperator);
            p.silent_at(TokenKind::Ident) && p.text() == Some("defined")
        } {
            Self::Defined
        } else if p.at(TokenKind::BitwiseNot) {
            Self::BitwiseNot
        } else if p.at(TokenKind::LogicalNot) {
            Self::LogicalNot
        } else {
            return None;
        })
    }
    /// See also [`InfixOp::binding_power`].
    fn binding_power() -> ((), u8) {
        // Order taken from here: https://en.cppreference.com/w/c/language/operator_precedence.html
        ((), 23)
    }
}

/// Parses an expression that should evaluate to an integer.
pub fn expr(p: &mut Parser, defined_operator: bool) -> Option<CompletedMarker> {
    expr_binding_power(p, defined_operator, 0)
}

fn expr_binding_power(
    p: &mut Parser,
    defined_operator: bool,
    min_bp: u8,
) -> Option<CompletedMarker> {
    let mut lhs = lhs(p, defined_operator)?;

    loop {
        let Some(op) = InfixOp::at(p) else {
            // We’re not at an operator; we don’t know what to do next, so we return and let the
            // caller decide.
            break;
        };

        let (left_bp, right_bp) = op.binding_power();

        if left_bp < min_bp {
            break;
        }

        // Eat the operator’s token.
        p.bump();

        let m = lhs.precede(p);

        if op == InfixOp::TernaryQuestionMark {
            if expr_binding_power(p, defined_operator, 0).is_none() {
                lhs = m.complete(p, NodeKind::InfixExpr);
                break;
            }

            if !p.expect(TokenKind::Colon) {
                lhs = m.complete(p, NodeKind::InfixExpr);
                break;
            }
        }

        let parsed_rhs = expr_binding_power(p, defined_operator, right_bp).is_some();
        lhs = m.complete(p, NodeKind::InfixExpr);

        if !parsed_rhs {
            break;
        }
    }

    Some(lhs)
}

fn lhs(p: &mut Parser, defined_operator: bool) -> Option<CompletedMarker> {
    Some(if let Some(_op) = PrefixOp::at(p, defined_operator) {
        let m = p.start();

        let ((), right_bp) = PrefixOp::binding_power();

        // Eat the operator's token
        p.bump();

        expr_binding_power(p, defined_operator, right_bp);

        m.complete(p, NodeKind::PrefixExpr)
    } else if p.at(TokenKind::LParen) {
        let m = p.start();
        p.bump();

        expr_binding_power(p, defined_operator, 0);

        p.expect(TokenKind::RParen);

        m.complete(p, NodeKind::ParenExpr)
    } else if p.at(TokenKind::Ident) {
        macro_invocation(p.start(), p)
    } else if p.at_set(&[TokenKind::Number, TokenKind::Char]) {
        let m = p.start();
        p.bump();
        m.complete(p, NodeKind::LiteralExpr)
    } else if InfixOp::at(p).is_some_and(|op| op != InfixOp::TernaryQuestionMark) {
        // Error recovery: missing lhs but existing op

        p.error()
            .msg_expected()
            .add_hint(Cow::Borrowed(
                "Recovered as expression with missing left-hand-side expression",
            ))
            .emit();
        let m = p.start();
        p.bump();
        expr_binding_power(p, defined_operator, 0);
        m.complete(p, NodeKind::ParseError);
        return None;
    } else if p.silent_at(TokenKind::RParen) {
        // Error recovery: don't bump RParen as a parent may be expecting it

        p.error().msg_expected().emit();
        return None;
    } else {
        p.error().msg_expected().bump_wrap_err().emit();
        return None;
    })
}

/// Syntax documentation: <https://gcc.gnu.org/onlinedocs/cpp/If.html>
pub(crate) fn entry_preprocessor_conditional(p: &mut Parser) {
    expr(p, true);
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
                        message: "Expected ‘+’, ‘-’, ‘*’, ‘/’, ‘%’, ‘&’, ‘|’, ‘^‘, ‘<<’, ‘>>’, ‘&&‘, ‘||‘, ‘<’, ‘>’, ‘<=‘, ‘>=‘, ‘==‘, ‘!=‘, ‘?‘ or end-of-file, but found number literal",
                        primary_span: TextRange {
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
