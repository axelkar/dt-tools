use dt_tools_diagnostic::Severity;

use super::{Expander, condition::evaluate};
use crate::{
    directive::{Directive, DirectiveKind},
    env::{MacroDef, MacroKind},
    lexer::PpTokenKind,
    output::MacroDefSite,
    token::PpToken,
};

pub(crate) struct DirectiveHandler<'a, 'b, 'c> {
    pub(crate) expander: &'a mut Expander<'b>,
    pub(crate) directive: Directive<'c>,
    pub(crate) argument_pos: usize,
}

impl<'c> DirectiveHandler<'_, '_, 'c> {
    pub(crate) fn peek_arg(&self) -> Option<&'c PpToken> {
        self.directive.argument.get(self.argument_pos)
    }
    pub(crate) fn next_arg(&mut self) -> Option<&'c PpToken> {
        let arg = self.peek_arg()?;
        self.argument_pos += 1;
        Some(arg)
    }
    pub(crate) fn next_arg_or_err(&mut self, msg: &str) -> Option<&'c PpToken> {
        self.next_arg().or_else(|| {
            self.expander
                .report(Severity::Error, msg.to_owned(), self.directive.text_range);
            None
        })
    }

    /// Handles a directive.
    pub(crate) fn handle(&mut self) {
        if self.directive.kind.is_conditional() {
            self.conditional();
            return;
        }

        if !self.expander.is_active() {
            return;
        }

        match self.directive.kind {
            DirectiveKind::Define => self.define(),
            DirectiveKind::Undef => self.undef(),
            DirectiveKind::Error => self.message(Severity::Error),
            DirectiveKind::Warning => self.message(Severity::Warn),
            DirectiveKind::Unsupported => self.expander.report(
                Severity::Warn,
                format!("directive `{}` is not supported yet", self.directive.name),
                self.directive.text_range,
            ),
            _ => unreachable!("conditional directives are handled above"),
        }
    }

    /// Handles a conditional directive.
    fn conditional(&mut self) {
        match self.directive.kind {
            DirectiveKind::If | DirectiveKind::Ifdef | DirectiveKind::Ifndef => self.open(),
            DirectiveKind::Elif | DirectiveKind::Elifdef | DirectiveKind::Elifndef => self.elif(),
            DirectiveKind::Else => self.else_branch(),
            DirectiveKind::Endif => self.endif(),
            _ => unreachable!("not a conditional directive"),
        }
    }

    /// Opens a conditional group.
    fn open(&mut self) {
        let condition = self.expander.is_active() && self.condition();
        self.expander.conditionals.open(
            self.directive.text_range,
            self.directive.text_range.end,
            condition,
        );
    }

    /// Starts an `#elif` branch.
    fn elif(&mut self) {
        if self.expander.conditionals.is_empty() {
            self.expander.report(
                Severity::Error,
                format!("`#{}` without `#if`", self.directive.name),
                self.directive.text_range,
            );
            return;
        }

        if self.expander.conditionals.else_seen() {
            self.expander.report(
                Severity::Error,
                format!("`#{}` after `#else`", self.directive.name),
                self.directive.text_range,
            );
            return;
        }

        let condition = self.expander.conditionals.should_evaluate_elif() && self.condition();
        self.expander.conditionals.elif(
            self.directive.text_range.start,
            self.directive.text_range.end,
            condition,
        );
    }

    /// Starts an `#else` branch.
    fn else_branch(&mut self) {
        if self.expander.conditionals.is_empty() {
            self.expander.report(
                Severity::Error,
                "`#else` without `#if`".to_owned(),
                self.directive.text_range,
            );
            return;
        }

        if self.expander.conditionals.else_seen() {
            self.expander.report(
                Severity::Error,
                "`#else` after `#else`".to_owned(),
                self.directive.text_range,
            );
            return;
        }

        self.expander.conditionals.else_branch(
            self.directive.text_range.start,
            self.directive.text_range.end,
        );
    }

    /// Closes the current conditional group.
    fn endif(&mut self) {
        if !self
            .expander
            .conditionals
            .close(self.directive.text_range.start)
        {
            self.expander.report(
                Severity::Error,
                "`#endif` without `#if`".to_owned(),
                self.directive.text_range,
            );
        }
    }

    /// Evaluates a condition.
    fn condition(&mut self) -> bool {
        match self.directive.kind {
            DirectiveKind::If | DirectiveKind::Elif => self.eval_condition(),
            DirectiveKind::Ifdef | DirectiveKind::Elifdef => self.is_defined(),
            DirectiveKind::Ifndef | DirectiveKind::Elifndef => !self.is_defined(),
            _ => unreachable!("not a condition"),
        }
    }

    /// Evaluates a condition.
    fn eval_condition(&mut self) -> bool {
        let tokens = self.expander.expand_condition(self.directive.argument);
        let (value, diagnostics) = evaluate(
            &tokens,
            self.expander.cleaned_text,
            self.directive.text_range,
        );
        for (message, range) in diagnostics {
            self.expander.report(Severity::Error, message, range);
        }
        value != 0
    }

    /// Returns true if the `#ifdef` operand names a defined macro.
    fn is_defined(&mut self) -> bool {
        let Some(name) = self.next_arg() else {
            self.expander.report(
                Severity::Error,
                "macro name missing".to_owned(),
                self.directive.text_range,
            );
            return false;
        };

        if name.kind != PpTokenKind::Ident {
            self.expander.report(
                Severity::Error,
                "macro name must be an identifier".to_owned(),
                name.orig,
            );
            return false;
        }

        self.expander
            .env
            .get(name.text(self.expander.cleaned_text))
            .is_some()
    }

    /// Handles a `#define` directive.
    pub(crate) fn define(&mut self) {
        let Some(name_token) = self.macro_name_token() else {
            return;
        };
        let name = name_token.text(self.expander.cleaned_text).to_owned();

        let Some(kind) = self.parse_params() else {
            return;
        };
        let replacement_start = self.argument_pos;
        let replacement = self.directive.argument[replacement_start..].to_vec();

        let previous = self.expander.env.get(&name).cloned();
        if let Some(previous) = previous
            && (previous.kind != kind
                || !same_replacement(
                    &previous.replacement,
                    &replacement,
                    self.expander.cleaned_text,
                ))
        {
            self.expander.report(
                Severity::Warn,
                format!("macro `{name}` redefined"),
                name_token.orig,
            );
        }

        self.expander
            .env
            .define(name.clone(), MacroDef { kind, replacement });
        self.expander.macro_defs.push(MacroDefSite {
            name,
            span: self.directive.text_range.within_file(self.expander.file),
        });
    }

    /// Parses a macro's optional parameter list.
    ///
    /// The macro may be an object-like macro or a function-like macro.
    ///
    /// Returns the macro kind.
    pub(crate) fn parse_params(&mut self) -> Option<MacroKind> {
        let is_function_like = self.peek_arg().is_some_and(|lparen| {
            !lparen.has_space_before && self.expander.is_punct(Some(lparen), "(")
        });

        if is_function_like {
            self.argument_pos += 1;
        } else {
            return Some(MacroKind::Object);
        }

        let mut params = Vec::new();
        let mut is_variadic = false;

        if self.expander.is_punct(self.peek_arg(), ")") {
            self.argument_pos += 1;
            return Some(MacroKind::Function {
                params,
                is_variadic,
            });
        }

        loop {
            let token = self.next_arg_or_err("unterminated macro parameter list")?;

            if self.expander.is_punct(Some(token), "...") {
                is_variadic = true;
            } else if token.kind == PpTokenKind::Ident {
                let name = token.text(self.expander.cleaned_text);
                if params.iter().any(|param| param == name) {
                    self.expander.report(
                        Severity::Error,
                        format!("duplicate macro parameter `{name}`"),
                        token.orig,
                    );
                    return None;
                }
                params.push(name.to_owned());
            } else {
                self.expander.report(
                    Severity::Error,
                    "expected a macro parameter or `...`".to_owned(),
                    token.orig,
                );
                return None;
            }

            let separator = self.next_arg_or_err("unterminated macro parameter list")?;

            if self.expander.is_punct(Some(separator), ")") {
                return Some(MacroKind::Function {
                    params,
                    is_variadic,
                });
            } else if self.expander.is_punct(Some(separator), ",") {
                if is_variadic {
                    self.expander.report(
                        Severity::Error,
                        "`...` must be the last parameter".to_owned(),
                        separator.orig,
                    );
                    return None;
                }
            } else {
                self.expander.report(
                    Severity::Error,
                    "expected `,` or `)` in macro parameter list".to_owned(),
                    separator.orig,
                );
                return None;
            }
        }
    }

    /// Handles an `#undef` directive.
    pub(crate) fn undef(&mut self) {
        let Some(name_token) = self.macro_name_token() else {
            return;
        };
        if let Some(extra) = self.next_arg() {
            self.expander.report(
                Severity::Warn,
                "extra tokens at end of #undef directive".to_owned(),
                extra.orig,
            );
        }
        self.expander
            .env
            .undef(name_token.text(self.expander.cleaned_text));
    }

    /// Gets a macro name token.
    pub(crate) fn macro_name_token(&mut self) -> Option<&'c PpToken> {
        let name_token = self.next_arg_or_err("macro name missing")?;
        if name_token.kind != PpTokenKind::Ident {
            self.expander.report(
                Severity::Error,
                "macro name must be an identifier".to_owned(),
                name_token.orig,
            );
            return None;
        }
        Some(name_token)
    }

    /// Handles a message directive.
    pub(crate) fn message(&mut self, severity: Severity) {
        let message = match (
            self.directive.argument.first(),
            self.directive.argument.last(),
        ) {
            (Some(first), Some(last)) => self.expander.cleaned_text
                [first.cleaned.start..last.cleaned.end]
                .trim()
                .to_owned(),
            _ => match severity {
                Severity::Error => "error directive".to_owned(),
                Severity::Warn => "warning directive".to_owned(),
            },
        };
        self.expander
            .report(severity, message, self.directive.text_range);
    }
}

/// Returns true if two replacement lists are identical.
pub(crate) fn same_replacement(left: &[PpToken], right: &[PpToken], cleaned: &str) -> bool {
    left.len() == right.len()
        && left.iter().zip(right).all(|(left, right)| {
            left.kind == right.kind
                && left.has_space_before == right.has_space_before
                && left.text(cleaned) == right.text(cleaned)
        })
}
