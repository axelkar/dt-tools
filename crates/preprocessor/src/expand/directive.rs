use dt_tools_diagnostic::Severity;

use super::Expander;
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
        }
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
        let mut variadic = false;

        if self.expander.is_punct(self.peek_arg(), ")") {
            self.argument_pos += 1;
            return Some(MacroKind::Function { params, variadic });
        }

        loop {
            let token = self.next_arg_or_err("unterminated macro parameter list")?;

            if self.expander.is_punct(Some(token), "...") {
                variadic = true;
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
                return Some(MacroKind::Function { params, variadic });
            } else if self.expander.is_punct(Some(separator), ",") {
                if variadic {
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
