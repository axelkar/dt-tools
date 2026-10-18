use camino::Utf8Path;
use dt_tools_diagnostic::{Severity, Span};

use super::{
    Expander, MacroToken, condition::evaluate, conditional::Conditionals, expand_file,
    is_punct_token,
};
use crate::{
    directive::{Directive, DirectiveKind},
    env::{MacroDef, MacroKind},
    files::{IncludeSearch, LoadedFiles, SourceId, resolve_file},
    lexer::PpTokenKind,
    output::{IncludeEdge, MacroDefSite},
    token::PpToken,
};

pub(crate) struct DirectiveHandler<'a, 'b, 'c, R> {
    pub(crate) expander: &'a mut Expander<'b>,
    pub(crate) conditionals: &'a mut Conditionals,
    pub(crate) files: &'a mut LoadedFiles,
    pub(crate) read_file: &'a mut R,
    pub(crate) file: SourceId,
    /// The number of includes that led to the current file.
    pub(crate) depth: usize,
    pub(crate) directive: Directive<'c>,
    pub(crate) argument_pos: usize,
}

impl<'c, R> DirectiveHandler<'_, '_, 'c, R>
where
    R: FnMut(&Utf8Path) -> Option<String>,
{
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
                .report(Severity::Error, msg.to_owned(), self.span());
            None
        })
    }

    /// Returns the directive's span in the current file.
    fn span(&self) -> Span<SourceId> {
        self.directive.text_range.within_file(self.file)
    }

    /// Handles a directive.
    pub(crate) fn handle(&mut self) {
        if self.directive.kind.is_conditional() {
            self.conditional();
            return;
        }

        if !self.conditionals.is_active() {
            return;
        }

        match self.directive.kind {
            DirectiveKind::Define => self.define(),
            DirectiveKind::Undef => self.undef(),
            DirectiveKind::Include => self.include(),
            DirectiveKind::Error => self.message(Severity::Error),
            DirectiveKind::Warning => self.message(Severity::Warn),
            DirectiveKind::Unsupported => self.expander.report(
                Severity::Warn,
                format!("directive `{}` is not supported yet", self.directive.name),
                self.span(),
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
        let condition = self.conditionals.is_active() && self.condition();
        self.conditionals.open(
            self.directive.text_range,
            self.directive.text_range.end,
            condition,
        );
    }

    /// Starts an `#elif` branch.
    fn elif(&mut self) {
        if self.conditionals.is_empty() {
            self.expander.report(
                Severity::Error,
                format!("`#{}` without `#if`", self.directive.name),
                self.span(),
            );
            return;
        }

        if self.conditionals.else_seen() {
            self.expander.report(
                Severity::Error,
                format!("`#{}` after `#else`", self.directive.name),
                self.span(),
            );
            return;
        }

        let condition = self.conditionals.should_evaluate_elif() && self.condition();
        self.conditionals.elif(
            self.directive.text_range.start,
            self.directive.text_range.end,
            condition,
        );
    }

    /// Starts an `#else` branch.
    fn else_branch(&mut self) {
        if self.conditionals.is_empty() {
            self.expander.report(
                Severity::Error,
                "`#else` without `#if`".to_owned(),
                self.span(),
            );
            return;
        }

        if self.conditionals.else_seen() {
            self.expander.report(
                Severity::Error,
                "`#else` after `#else`".to_owned(),
                self.span(),
            );
            return;
        }

        self.conditionals.else_branch(
            self.directive.text_range.start,
            self.directive.text_range.end,
        );
    }

    /// Closes the current conditional group.
    fn endif(&mut self) {
        if !self.conditionals.close(self.directive.text_range.start) {
            self.expander.report(
                Severity::Error,
                "`#endif` without `#if`".to_owned(),
                self.span(),
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
        let tokens = self
            .expander
            .expand_condition(self.directive.argument, &self.files.cleaned);
        let (value, diagnostics) = evaluate(&tokens, &self.files.cleaned, self.span());
        for (message, span) in diagnostics {
            self.expander.report(Severity::Error, message, span);
        }
        value != 0
    }

    /// Returns true if the `#ifdef` operand names a defined macro.
    fn is_defined(&mut self) -> bool {
        let Some(name) = self.next_arg() else {
            self.expander.report(
                Severity::Error,
                "macro name missing".to_owned(),
                self.span(),
            );
            return false;
        };

        if name.kind != PpTokenKind::Ident {
            self.expander.report(
                Severity::Error,
                "macro name must be an identifier".to_owned(),
                name.orig.within_file(name.file),
            );
            return false;
        }

        self.expander
            .env
            .get(name.text(&self.files.cleaned))
            .is_some()
    }

    /// Handles a `#define` directive.
    pub(crate) fn define(&mut self) {
        let Some(name_token) = self.macro_name_token() else {
            return;
        };
        let name = name_token.text(&self.files.cleaned).to_owned();

        let Some(kind) = self.parse_params() else {
            return;
        };
        let replacement_start = self.argument_pos;
        let replacement = self.directive.argument[replacement_start..].to_vec();

        let previous = self.expander.env.get(&name).cloned();
        if let Some(previous) = previous
            && (previous.kind != kind
                || !same_replacement(&previous.replacement, &replacement, &self.files.cleaned))
        {
            self.expander.report(
                Severity::Warn,
                format!("macro `{name}` redefined"),
                name_token.orig.within_file(name_token.file),
            );
        }

        let definition = name_token.orig.within_file(name_token.file);
        self.expander.env.define(
            name.clone(),
            MacroDef {
                kind,
                replacement,
                definition,
            },
        );
        self.expander.macro_defs.push(MacroDefSite {
            name,
            span: self.span(),
        });
    }

    /// Parses a macro's optional parameter list.
    ///
    /// The macro may be an object-like macro or a function-like macro.
    ///
    /// Returns the macro kind.
    pub(crate) fn parse_params(&mut self) -> Option<MacroKind> {
        let is_function_like = self.peek_arg().is_some_and(|lparen| {
            !lparen.has_space_before && is_punct_token(Some(lparen), "(", &self.files.cleaned)
        });

        if is_function_like {
            self.argument_pos += 1;
        } else {
            return Some(MacroKind::Object);
        }

        let mut params = Vec::new();
        let mut variadic_param = None;

        if is_punct_token(self.peek_arg(), ")", &self.files.cleaned) {
            self.argument_pos += 1;
            return Some(MacroKind::Function {
                params,
                variadic_param,
            });
        }

        loop {
            let token = self.next_arg_or_err("unterminated macro parameter list")?;

            if is_punct_token(Some(token), "...", &self.files.cleaned) {
                variadic_param = Some("__VA_ARGS__".to_owned());
            } else if token.kind == PpTokenKind::Ident {
                let name = token.text(&self.files.cleaned);
                if params.iter().any(|param| param == name) {
                    self.expander.report(
                        Severity::Error,
                        format!("duplicate macro parameter `{name}`"),
                        token.orig.within_file(token.file),
                    );
                    return None;
                }
                params.push(name.to_owned());
            } else {
                self.expander.report(
                    Severity::Error,
                    "expected a macro parameter or `...`".to_owned(),
                    token.orig.within_file(token.file),
                );
                return None;
            }

            let separator = self.next_arg_or_err("unterminated macro parameter list")?;

            if is_punct_token(Some(separator), ")", &self.files.cleaned) {
                return Some(MacroKind::Function {
                    params,
                    variadic_param,
                });
            } else if is_punct_token(Some(separator), "...", &self.files.cleaned)
                && variadic_param.is_none()
            {
                // GNU named variadic: the identifier before `...` is the variadic parameter.
                let name = params.pop()?;
                if params.contains(&name) {
                    self.expander.report(
                        Severity::Error,
                        format!("duplicate macro parameter `{name}`"),
                        separator.orig.within_file(separator.file),
                    );
                    return None;
                }
                variadic_param = Some(name);

                let rparen = self.next_arg_or_err("expected `)` after `...`")?;
                if !is_punct_token(Some(rparen), ")", &self.files.cleaned) {
                    self.expander.report(
                        Severity::Error,
                        "expected `)` after `...`".to_owned(),
                        rparen.orig.within_file(rparen.file),
                    );
                    return None;
                }

                return Some(MacroKind::Function {
                    params,
                    variadic_param,
                });
            } else if is_punct_token(Some(separator), ",", &self.files.cleaned) {
                if variadic_param.is_some() {
                    self.expander.report(
                        Severity::Error,
                        "`...` must be the last parameter".to_owned(),
                        separator.orig.within_file(separator.file),
                    );
                    return None;
                }
            } else {
                self.expander.report(
                    Severity::Error,
                    "expected `,` or `)` in macro parameter list".to_owned(),
                    separator.orig.within_file(separator.file),
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
                extra.orig.within_file(extra.file),
            );
        }
        self.expander
            .env
            .undef(name_token.text(&self.files.cleaned));
    }

    /// Gets a macro name token.
    pub(crate) fn macro_name_token(&mut self) -> Option<&'c PpToken> {
        let name_token = self.next_arg_or_err("macro name missing")?;
        if name_token.kind != PpTokenKind::Ident {
            self.expander.report(
                Severity::Error,
                "macro name must be an identifier".to_owned(),
                name_token.orig.within_file(name_token.file),
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
            (Some(first), Some(last)) => self.files.cleaned[first.cleaned.start..last.cleaned.end]
                .trim()
                .to_owned(),
            _ => match severity {
                Severity::Error => "error directive".to_owned(),
                Severity::Warn => "warning directive".to_owned(),
            },
        };
        self.expander.report(severity, message, self.span());
    }

    /// Handles an `#include` directive.
    fn include(&mut self) {
        if self.depth >= self.expander.config.max_include_depth {
            self.expander.report(
                Severity::Error,
                "`#include` nested too deeply".to_owned(),
                self.span(),
            );
            return;
        }

        let Some(header) = self.header_name() else {
            return;
        };
        let search = if header.quoted {
            IncludeSearch::IncluderFirst
        } else {
            IncludeSearch::IncludePaths
        };
        let Some((path, contents)) = resolve_file(
            &self.expander.config.include_dirs,
            self.files.path(self.file),
            &header.name,
            search,
            self.read_file,
        ) else {
            self.expander.report(
                Severity::Error,
                format!("include file not found: `{}`", header.name),
                self.span(),
            );
            return;
        };

        let loaded = self.files.add(&path, &contents);
        self.expander.includes.push(IncludeEdge {
            file: loaded.id,
            span: self.span(),
        });

        for error in &loaded.errors {
            self.expander.report(
                Severity::Error,
                error.error.to_string(),
                error.orig_range.within_file(loaded.id),
            );
        }

        expand_file(
            &mut *self.expander,
            &mut *self.files,
            &mut *self.read_file,
            loaded.id,
            &loaded.tokens,
            loaded.orig_len,
            self.depth + 1,
        );
    }

    /// Parses the header name after `#include`.
    fn header_name(&mut self) -> Option<HeaderName> {
        let raw = self.directive.argument;
        let Some(first) = raw.first() else {
            self.expander.report(
                Severity::Error,
                HEADER_NAME_EXPECTED.to_owned(),
                self.span(),
            );
            return None;
        };

        let is_raw = is_punct_token(Some(first), "<", &self.files.cleaned)
            || first.kind == PpTokenKind::String;
        let tokens: Vec<MacroToken> = raw.iter().copied().map(MacroToken::source).collect();
        let (tokens, source) = if is_raw {
            (tokens, HeaderTokens::Raw)
        } else {
            (
                self.expander.prescan(tokens, &self.files.cleaned),
                HeaderTokens::Expanded,
            )
        };

        self.scan_header_name(&tokens, source)
    }

    /// Parses a header name from its tokens.
    fn scan_header_name(
        &mut self,
        tokens: &[MacroToken],
        source: HeaderTokens,
    ) -> Option<HeaderName> {
        let Some(first) = tokens.first() else {
            self.expander.report(
                Severity::Error,
                HEADER_NAME_EXPECTED.to_owned(),
                self.span(),
            );
            return None;
        };
        let text = first.text(&self.files.cleaned);

        if first.kind == PpTokenKind::String {
            let Some(name) = text
                .strip_prefix('"')
                .and_then(|text| text.strip_suffix('"'))
            else {
                self.expander.report(
                    Severity::Error,
                    "unterminated header name".to_owned(),
                    first.primary_span(),
                );
                return None;
            };
            let name = name.to_owned();
            self.warn_extra(tokens.get(1).map(MacroToken::primary_span));

            return Some(HeaderName { name, quoted: true });
        }

        let is_left_angle = first.kind == PpTokenKind::Punct && text == "<";
        if is_left_angle {
            let Some(rangle) = tokens.iter().position(|token| {
                token.kind == PpTokenKind::Punct && token.text(&self.files.cleaned) == ">"
            }) else {
                self.expander.report(
                    Severity::Error,
                    "unterminated header name".to_owned(),
                    first.primary_span(),
                );
                return None;
            };
            let name = match source {
                HeaderTokens::Raw => {
                    let start = first
                        .cleaned_range()
                        .expect("raw header tokens are from source")
                        .end;
                    let end = tokens[rangle]
                        .cleaned_range()
                        .expect("raw header tokens are from source")
                        .start;

                    self.files.cleaned[start..end].to_owned()
                }
                HeaderTokens::Expanded => {
                    let mut name = render_tokens(&tokens[1..rangle], &self.files.cleaned);
                    if tokens[rangle].has_space_before {
                        name.push(' ');
                    }
                    name
                }
            };
            self.warn_extra(tokens.get(rangle + 1).map(MacroToken::primary_span));

            return Some(HeaderName {
                name,
                quoted: false,
            });
        }

        self.expander.report(
            Severity::Error,
            HEADER_NAME_EXPECTED.to_owned(),
            first.primary_span(),
        );
        None
    }

    /// Warns about the first token after a header name.
    fn warn_extra(&mut self, span: Option<Span<SourceId>>) {
        let Some(span) = span else {
            return;
        };
        self.expander.report(
            Severity::Warn,
            "extra tokens at end of `#include` directive".to_owned(),
            span,
        );
    }
}

/// The message for a computed include that is not a header name.
const HEADER_NAME_EXPECTED: &str = "`#include` expects a file name in quotes or angle brackets";

/// Renders `tokens` with a space wherever the source had one.
fn render_tokens(tokens: &[MacroToken], cleaned: &str) -> String {
    let mut out = String::new();

    for token in tokens {
        if token.has_space_before {
            out.push(' ');
        }
        out.push_str(token.text(cleaned));
    }

    out
}

/// How a header name's tokens were produced.
#[derive(Clone, Copy)]
enum HeaderTokens {
    /// Copied from the directive without macro expansion.
    Raw,
    /// The result of macro expansion.
    Expanded,
}

/// A header name written after `#include`.
struct HeaderName {
    /// The name without quotes or angle brackets.
    name: String,
    /// Whether the name was quoted, so the includer's directory is searched.
    quoted: bool,
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
