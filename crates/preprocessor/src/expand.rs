//! Directive handling and object-like macro expansion.

use std::borrow::Cow;

use dt_tools_diagnostic::{Diagnostic, Severity, text_range::TextRange};
use rustc_hash::FxHashSet;

use crate::config::PreprocessorConfig;
use crate::directive::{self, Directive, DirectiveKind};
use crate::env::{MacroDef, MacroEnv};
use crate::files::SourceId;
use crate::lexer::PpTokenKind;
use crate::output::MacroDefSite;
use crate::token::PpToken;

/// The token stream after directives are handled and object-like macros are expanded.
pub(crate) struct Expansion {
    pub(crate) tokens: Vec<PpToken>,
    pub(crate) macro_defs: Vec<MacroDefSite>,
    pub(crate) diagnostics: Vec<Diagnostic<SourceId>>,
}

/// Handles directives and expands object-like macros of a file in source order.
pub(crate) fn expand(
    tokens: &[PpToken],
    cleaned: &str,
    file: SourceId,
    config: &PreprocessorConfig,
) -> Expansion {
    let mut expander = Expander {
        env: MacroEnv::default(),
        cleaned_text: cleaned,
        file,
        config,
        tokens: Vec::new(),
        macro_defs: Vec::new(),
        diagnostics: Vec::new(),
        expanded_bytes: 0,
        byte_cap_reported: false,
    };

    let mut index = 0;
    while index < tokens.len() {
        if let Some(directive) = directive::parse(&tokens[index..], cleaned) {
            expander.directive(directive);
            index += directive.len_tokens;
        } else {
            expander.expand_token(tokens[index], &mut FxHashSet::default(), 0);
            index += 1;
        }
    }

    Expansion {
        tokens: expander.tokens,
        macro_defs: expander.macro_defs,
        diagnostics: expander.diagnostics,
    }
}

struct Expander<'a> {
    env: MacroEnv,
    cleaned_text: &'a str,
    file: SourceId,
    config: &'a PreprocessorConfig,
    tokens: Vec<PpToken>,
    macro_defs: Vec<MacroDefSite>,
    diagnostics: Vec<Diagnostic<SourceId>>,
    expanded_bytes: usize,
    byte_cap_reported: bool,
}

impl Expander<'_> {
    /// Handles a directive.
    fn directive(&mut self, directive: Directive<'_>) {
        match directive.kind {
            DirectiveKind::Define => self.define(directive),
            DirectiveKind::Undef => self.undef(directive),
            DirectiveKind::Error => self.message(directive, Severity::Error),
            DirectiveKind::Warning => self.message(directive, Severity::Warn),
            DirectiveKind::Unsupported => self.report(
                Severity::Warn,
                format!("directive `{}` is not supported yet", directive.name),
                directive.text_range,
            ),
        }
    }

    /// Handles a `#define` directive.
    fn define(&mut self, directive: Directive<'_>) {
        let Some(name_token) = self.macro_name_token(directive) else {
            return;
        };

        if let Some(lparen) = directive.argument.get(1)
            && !lparen.has_space_before
            && lparen.kind == PpTokenKind::Punct
            && lparen.text(self.cleaned_text) == "("
        {
            self.report(
                Severity::Warn,
                "function-like macros are not supported yet".to_owned(),
                lparen.orig,
            );
            return;
        }

        let name = name_token.text(self.cleaned_text).to_owned();
        let replacement = directive.argument[1..].to_vec();

        let previous = self
            .env
            .get(&name)
            .map(|definition| definition.replacement.clone());
        if let Some(previous) = previous
            && !same_replacement(&previous, &replacement, self.cleaned_text)
        {
            self.report(
                Severity::Warn,
                format!("macro `{name}` redefined"),
                name_token.orig,
            );
        }

        self.env.define(name.clone(), MacroDef { replacement });
        self.macro_defs.push(MacroDefSite {
            name,
            span: directive.text_range.within_file(self.file),
        });
    }

    /// Handles an `#undef` directive.
    fn undef(&mut self, directive: Directive<'_>) {
        let Some(name_token) = self.macro_name_token(directive) else {
            return;
        };
        if let Some(extra) = directive.argument.get(1) {
            self.report(
                Severity::Warn,
                "extra tokens at end of #undef directive".to_owned(),
                extra.orig,
            );
        }
        self.env.undef(name_token.text(self.cleaned_text));
    }

    /// Gets a macro name token from a [`Directive`].
    fn macro_name_token<'a>(&mut self, directive: Directive<'a>) -> Option<&'a PpToken> {
        let Some(name_token) = directive.argument.first() else {
            self.report(
                Severity::Error,
                "macro name missing".to_owned(),
                directive.text_range,
            );
            return None;
        };
        if name_token.kind != PpTokenKind::Ident {
            self.report(
                Severity::Error,
                "macro name must be an identifier".to_owned(),
                name_token.orig,
            );
            return None;
        }
        Some(name_token)
    }

    /// Handles a message directive.
    fn message(&mut self, directive: Directive<'_>, severity: Severity) {
        let message = match (directive.argument.first(), directive.argument.last()) {
            (Some(first), Some(last)) => self.cleaned_text[first.cleaned.start..last.cleaned.end]
                .trim()
                .to_owned(),
            _ => match severity {
                Severity::Error => "error directive".to_owned(),
                Severity::Warn => "warning directive".to_owned(),
            },
        };
        self.report(severity, message, directive.text_range);
    }

    /// Expands a token and performs partial rescan on its replacement.
    fn expand_token(&mut self, token: PpToken, active: &mut FxHashSet<String>, depth: usize) {
        if self.expanded_bytes <= self.config.max_expanded_bytes
            && token.kind == PpTokenKind::Ident
            && let name = token.text(self.cleaned_text)
            && !active.contains(name)
            && let Some(replacement) = self
                .env
                .get(name)
                .map(|definition| definition.replacement.clone())
        {
            if depth >= self.config.max_expansion_depth {
                self.report(
                    Severity::Error,
                    "macro expansion depth exceeded".to_owned(),
                    token.orig,
                );
                self.push(token);
                return;
            }

            active.insert(name.to_owned());

            let mut first = true;
            for mut replacement_token in replacement {
                if first {
                    replacement_token.has_space_before = token.has_space_before;
                    replacement_token.line_start = token.line_start;
                    first = false;
                }

                // TODO: full rescan, this is only partial, per-token
                self.expand_token(replacement_token, active, depth + 1);
            }

            active.remove(name);
            return;
        }

        self.push(token);
    }

    /// Pushes a token to the output.
    fn push(&mut self, token: PpToken) {
        self.expanded_bytes += token.text(self.cleaned_text).len();
        if self.expanded_bytes > self.config.max_expanded_bytes && !self.byte_cap_reported {
            self.byte_cap_reported = true;
            self.report(
                Severity::Error,
                "expanded text exceeds the byte limit".to_owned(),
                token.orig,
            );
        }
        self.tokens.push(token);
    }

    /// Reports a diagnostic.
    fn report(&mut self, severity: Severity, message: String, range: TextRange) {
        self.diagnostics.push(Diagnostic::new(
            range.within_file(self.file),
            Cow::Owned(message),
            severity,
        ));
    }
}

/// Returns true if two replacement lists are identical.
fn same_replacement(left: &[PpToken], right: &[PpToken], cleaned: &str) -> bool {
    left.len() == right.len()
        && left.iter().zip(right).all(|(left, right)| {
            left.kind == right.kind
                && left.has_space_before == right.has_space_before
                && left.text(cleaned) == right.text(cleaned)
        })
}
