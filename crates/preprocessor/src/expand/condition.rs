//! Condition evaluation.

use std::sync::Arc;

use dt_tools_diagnostic::{Severity, text_range::TextRange};

use super::{Expander, MacroToken};
use crate::{lexer::PpTokenKind, token::PpToken};

/// A binary operator.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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
    /// Returns the operator for `text`.
    fn at(text: &str) -> Option<Self> {
        Some(match text {
            "+" => Self::Add,
            "-" => Self::Sub,
            "*" => Self::Mul,
            "/" => Self::Div,
            "%" => Self::Mod,
            "&" => Self::BitwiseAnd,
            "|" => Self::BitwiseOr,
            "^" => Self::BitwiseXor,
            "<<" => Self::BitwiseShl,
            ">>" => Self::BitwiseShr,
            "&&" => Self::LogicalAnd,
            "||" => Self::LogicalOr,
            "<" => Self::Lt,
            ">" => Self::Gt,
            "<=" => Self::LtEq,
            ">=" => Self::GtEq,
            "==" => Self::Eq,
            "!=" => Self::Neq,
            "?" => Self::TernaryQuestionMark,
            _ => return None,
        })
    }

    /// Returns the left and right binding powers.
    fn binding_power(self) -> (u8, u8) {
        // Order taken from here: https://en.cppreference.com/c/language/operator_precedence
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

/// A prefix operator.
#[derive(Clone, Copy)]
enum PrefixOp {
    Plus,
    Minus,
    BitwiseNot,
    LogicalNot,
}

impl PrefixOp {
    /// Returns the operator for `text`.
    fn at(text: &str) -> Option<Self> {
        Some(match text {
            "+" => Self::Plus,
            "-" => Self::Minus,
            "~" => Self::BitwiseNot,
            "!" => Self::LogicalNot,
            _ => return None,
        })
    }

    /// Returns the operand's binding power.
    fn binding_power() -> u8 {
        23
    }
}

impl Expander<'_> {
    /// Expands a condition's tokens.
    pub(super) fn expand_condition(&mut self, tokens: &[PpToken]) -> Vec<MacroToken> {
        let mut replaced = Vec::with_capacity(tokens.len());
        let mut index = 0;

        while index < tokens.len() {
            let token = tokens[index];
            if token.kind == PpTokenKind::Ident && token.text(self.cleaned_text) == "defined" {
                let (value, consumed) = self.eval_defined(tokens, index);
                replaced.push(MacroToken {
                    token: PpToken {
                        kind: PpTokenKind::Number,
                        cleaned: token.cleaned,
                        orig: token.orig,
                        has_space_before: token.has_space_before,
                        line_start: token.line_start,
                    },
                    text: Some(Arc::from(if value { "1" } else { "0" })),
                    hide: None,
                    depth: 0,
                });
                index += consumed;
                continue;
            }

            replaced.push(MacroToken::source(token));
            index += 1;
        }

        self.prescan(replaced)
    }

    /// Evaluates `defined` at `index`, returning its value and consumed token count.
    fn eval_defined(&mut self, tokens: &[PpToken], index: usize) -> (bool, usize) {
        debug_assert_eq!(tokens[index].kind, PpTokenKind::Ident);
        debug_assert_eq!(tokens[index].text(self.cleaned_text), "defined");

        let (name, consumed) = match tokens.get(index + 1) {
            Some(lparen)
                if self.is_punct(Some(lparen), "(")
                    && let (Some(name), Some(rparen)) =
                        (tokens.get(index + 2), tokens.get(index + 3))
                    && name.kind == PpTokenKind::Ident
                    && self.is_punct(Some(rparen), ")") =>
            {
                (*name, 4)
            }
            Some(name) if name.kind == PpTokenKind::Ident => (*name, 2),
            _ => {
                self.report(
                    Severity::Error,
                    "`defined` requires a macro name".to_owned(),
                    tokens[index].orig,
                );
                return (false, 1);
            }
        };

        let value = self.env.get(name.text(self.cleaned_text)).is_some();
        (value, consumed)
    }
}

/// Evaluates a condition, returning its value and diagnostics.
pub(super) fn evaluate(
    tokens: &[MacroToken],
    cleaned: &str,
    origin: TextRange,
) -> (i64, Vec<(String, TextRange)>) {
    let mut parser = Parser {
        tokens,
        index: 0,
        cleaned,
        origin,
        diagnostics: Vec::new(),
    };
    let value = parser.parse();

    (value, parser.diagnostics)
}

/// A condition parser.
struct Parser<'a> {
    tokens: &'a [MacroToken],
    index: usize,
    cleaned: &'a str,
    /// Range of the whole directive.
    origin: TextRange,
    diagnostics: Vec<(String, TextRange)>,
}

impl Parser<'_> {
    /// Parses and evaluates the expression.
    fn parse(&mut self) -> i64 {
        let value = self.expr_bp(0, true);

        if let Some(token) = self.tokens.get(self.index) {
            self.error("unexpected token in condition".to_owned(), token.token.orig);
        }

        value
    }

    /// Parses an expression with the given minimum binding power.
    fn expr_bp(&mut self, min_bp: u8, live: bool) -> i64 {
        let mut lhs = self.prefix_expr(live);

        while let Some((op, origin)) = self.peek_infix() {
            let (left_bp, right_bp) = op.binding_power();
            if left_bp < min_bp {
                break;
            }
            self.index += 1;

            if op == InfixOp::TernaryQuestionMark {
                let then = self.expr_bp(0, live && lhs != 0);
                self.expect_punct(":");
                let otherwise = self.expr_bp(right_bp, live && lhs == 0);

                lhs = if !live {
                    0
                } else if lhs != 0 {
                    then
                } else {
                    otherwise
                };
                continue;
            }

            let rhs_live = live
                && match op {
                    InfixOp::LogicalAnd => lhs != 0,
                    InfixOp::LogicalOr => lhs == 0,
                    _ => true,
                };
            let rhs = self.expr_bp(right_bp, rhs_live);

            lhs = if live {
                self.apply(op, lhs, rhs, origin)
            } else {
                0
            };
        }

        lhs
    }

    /// Parses a prefix expression.
    fn prefix_expr(&mut self, live: bool) -> i64 {
        let Some(token) = self.tokens.get(self.index) else {
            self.error("expected a value in condition".to_owned(), self.origin);
            return 0;
        };

        let op = if token.token.kind == PpTokenKind::Punct {
            PrefixOp::at(token.text(self.cleaned))
        } else {
            None
        };
        let Some(op) = op else {
            return self.primary(live);
        };
        self.index += 1;

        let operand = self.expr_bp(PrefixOp::binding_power(), live);
        if !live {
            return 0;
        }

        match op {
            PrefixOp::Plus => operand,
            PrefixOp::Minus => operand.wrapping_neg(),
            PrefixOp::BitwiseNot => !operand,
            PrefixOp::LogicalNot => i64::from(operand == 0),
        }
    }

    /// Parses a primary expression.
    fn primary(&mut self, live: bool) -> i64 {
        let Some(token) = self.tokens.get(self.index) else {
            self.error("expected a value in condition".to_owned(), self.origin);
            return 0;
        };
        self.index += 1;
        let text = token.text(self.cleaned);

        match token.token.kind {
            PpTokenKind::Number => {
                if let Some(value) = parse_integer(text) {
                    if live { value } else { 0 }
                } else {
                    self.error(
                        format!("invalid integer literal `{text}`"),
                        token.token.orig,
                    );
                    0
                }
            }
            PpTokenKind::Char => {
                if let Some(value) = parse_char(text) {
                    if live { value } else { 0 }
                } else {
                    self.error(
                        format!("invalid character constant `{text}`"),
                        token.token.orig,
                    );
                    0
                }
            }
            // Unexpanded identifiers are undefined and evaluate to 0
            PpTokenKind::Ident => 0,
            PpTokenKind::Punct if text == "(" => {
                let value = self.expr_bp(0, live);
                self.expect_punct(")");
                value
            }
            PpTokenKind::Punct => {
                self.error(
                    format!("unsupported operator `{text}` in condition"),
                    token.token.orig,
                );
                0
            }
            _ => {
                self.error(
                    "unsupported token in condition".to_owned(),
                    token.token.orig,
                );
                0
            }
        }
    }

    /// Applies a binary operator to two live values.
    fn apply(&mut self, op: InfixOp, lhs: i64, rhs: i64, origin: TextRange) -> i64 {
        match op {
            InfixOp::Mul => lhs.wrapping_mul(rhs),
            InfixOp::Div => {
                if rhs == 0 {
                    self.error("division by zero in condition".to_owned(), origin);
                    0
                } else {
                    lhs.wrapping_div(rhs)
                }
            }
            InfixOp::Mod => {
                if rhs == 0 {
                    self.error("division by zero in condition".to_owned(), origin);
                    0
                } else {
                    lhs.wrapping_rem(rhs)
                }
            }
            InfixOp::Add => lhs.wrapping_add(rhs),
            InfixOp::Sub => lhs.wrapping_sub(rhs),
            InfixOp::BitwiseShl => lhs.wrapping_shl(shift_amount(rhs)),
            InfixOp::BitwiseShr => lhs.wrapping_shr(shift_amount(rhs)),
            InfixOp::Lt => i64::from(lhs < rhs),
            InfixOp::Gt => i64::from(lhs > rhs),
            InfixOp::LtEq => i64::from(lhs <= rhs),
            InfixOp::GtEq => i64::from(lhs >= rhs),
            InfixOp::Eq => i64::from(lhs == rhs),
            InfixOp::Neq => i64::from(lhs != rhs),
            InfixOp::BitwiseAnd => lhs & rhs,
            InfixOp::BitwiseOr => lhs | rhs,
            InfixOp::BitwiseXor => lhs ^ rhs,
            InfixOp::LogicalAnd => i64::from(lhs != 0 && rhs != 0),
            InfixOp::LogicalOr => i64::from(lhs != 0 || rhs != 0),
            InfixOp::TernaryQuestionMark => unreachable!("ternary is not a binary operator"),
        }
    }

    /// Returns the next infix operator and its range.
    fn peek_infix(&self) -> Option<(InfixOp, TextRange)> {
        let token = self.tokens.get(self.index)?;
        if token.token.kind != PpTokenKind::Punct {
            return None;
        }
        let op = InfixOp::at(token.text(self.cleaned))?;
        Some((op, token.token.orig))
    }

    /// Consumes the punctuator `text`.
    fn expect_punct(&mut self, text: &str) {
        let Some(token) = self.tokens.get(self.index) else {
            self.error(format!("expected `{text}`"), self.origin);
            return;
        };
        if token.token.kind == PpTokenKind::Punct && token.text(self.cleaned) == text {
            self.index += 1;
        } else {
            self.error(format!("expected `{text}`"), token.token.orig);
        }
    }

    /// Records a diagnostic.
    fn error(&mut self, message: String, range: TextRange) {
        self.diagnostics.push((message, range));
    }
}

/// Returns the shift count, masked like [`i64::wrapping_shl`].
fn shift_amount(rhs: i64) -> u32 {
    u32::try_from(rhs & 63).expect("masked to 6 bits")
}

/// Parses an integer constant.
fn parse_integer(text: &str) -> Option<i64> {
    let digits = text.trim_end_matches(['u', 'U', 'l', 'L']);
    if !is_valid_suffix(&text[digits.len()..]) {
        return None;
    }

    let (radix, digits) = if let Some(digits) = digits
        .strip_prefix("0x")
        .or_else(|| digits.strip_prefix("0X"))
    {
        (16, digits)
    } else if let Some(digits) = digits
        .strip_prefix("0b")
        .or_else(|| digits.strip_prefix("0B"))
    {
        (2, digits)
    } else if let Some(digits) = digits.strip_prefix("0")
        && !digits.is_empty()
    {
        (8, digits)
    } else {
        (10, digits)
    };

    if digits.is_empty() {
        return None;
    }

    i64::from_str_radix(digits, radix).ok()
}

/// Returns true if `suffix` is a valid integer suffix.
fn is_valid_suffix(suffix: &str) -> bool {
    matches!(
        suffix.to_ascii_lowercase().as_str(),
        "" | "u" | "l" | "ll" | "ul" | "ull" | "lu" | "llu"
    )
}

/// Parses a character constant.
fn parse_char(text: &str) -> Option<i64> {
    let inner = text.strip_prefix('\'')?.strip_suffix('\'')?;
    let mut chars = inner.chars();

    let value = match chars.next()? {
        '\\' => parse_escape(&mut chars)?,
        ch => u32::from(ch),
    };

    // Multi-character constants are implementation-defined.
    if chars.next().is_some() {
        return None;
    }

    // A character constant has the value of a `char` converted to `int`.
    let byte = u8::try_from(value & 0xff).ok()?;
    Some(i64::from(byte.cast_signed()))
}

/// Parses one escape sequence after the backslash.
fn parse_escape(chars: &mut std::str::Chars<'_>) -> Option<u32> {
    let ch = chars.next()?;
    Some(match ch {
        'a' => 0x07,
        'b' => 0x08,
        'f' => 0x0c,
        'n' => 0x0a,
        'r' => 0x0d,
        't' => 0x09,
        'v' => 0x0b,
        '\\' => u32::from('\\'),
        '\'' => u32::from('\''),
        '"' => u32::from('"'),
        '?' => u32::from('?'),
        'x' => {
            let mut value = 0;
            let mut digits = 0;
            while let Some(digit) = chars.clone().next().and_then(|ch| ch.to_digit(16)) {
                chars.next();
                value = value * 16 + digit;
                digits += 1;
            }
            if digits == 0 {
                return None;
            }
            value
        }
        '0'..='7' => {
            let mut value = ch.to_digit(8)?;
            for _ in 0..2 {
                let Some(digit) = chars.clone().next().and_then(|ch| ch.to_digit(8)) else {
                    break;
                };
                chars.next();
                value = (value << 3) + digit;
            }
            value
        }
        _ => return None,
    })
}
