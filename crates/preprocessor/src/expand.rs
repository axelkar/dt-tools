//! Directive handling and macro expansion.

mod directive;
mod rescan;

use std::borrow::Cow;

use dt_tools_diagnostic::{Diagnostic, Severity, text_range::TextRange};

use crate::{
    config::PreprocessorConfig,
    env::{MacroEnv, MacroKind},
    expand::rescan::{HideSet, MacroToken, TokenStream, extend_hide, union_hide},
    files::SourceId,
    lexer::PpTokenKind,
    output::MacroDefSite,
    token::PpToken,
};

/// The token stream after directives are handled and macros are expanded.
pub(crate) struct Expansion {
    pub(crate) tokens: Vec<PpToken>,
    pub(crate) macro_defs: Vec<MacroDefSite>,
    pub(crate) diagnostics: Vec<Diagnostic<SourceId>>,
}

/// Handles directives and expands macros of a file in source order.
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

    let mut stream = TokenStream::new(tokens);

    loop {
        if stream.is_rescan_empty()
            && let Some(directive) = crate::directive::parse(stream.remaining_source(), cleaned)
        {
            stream.skip_source(directive.len_tokens);
            directive::DirectiveHandler {
                expander: &mut expander,
                directive,
                argument_pos: 0,
            }
            .handle();
            continue;
        }

        let Some(token) = stream.next() else {
            break;
        };

        expander.expand_token(&token, &mut stream);
    }

    Expansion {
        tokens: expander
            .tokens
            .into_iter()
            .map(|token| token.token)
            .collect(),
        macro_defs: expander.macro_defs,
        diagnostics: expander.diagnostics,
    }
}

struct Expander<'a> {
    env: MacroEnv,
    cleaned_text: &'a str,
    file: SourceId,
    config: &'a PreprocessorConfig,
    tokens: Vec<MacroToken>,
    macro_defs: Vec<MacroDefSite>,
    diagnostics: Vec<Diagnostic<SourceId>>,
    expanded_bytes: usize,
    byte_cap_reported: bool,
}

/// The arguments of a function-like macro invocation.
struct Arguments {
    /// Argument token lists in order.
    lists: Vec<Vec<MacroToken>>,
    /// Commas between consecutive arguments, in source order.
    ///
    /// `commas[i]` separates `lists[i]` and `lists[i + 1]`.
    commas: Vec<PpToken>,
}

impl Expander<'_> {
    /// Returns true if `token` is the punctuator `text`.
    fn is_punct(&self, token: Option<&PpToken>, text: &str) -> bool {
        token.is_some_and(|token| {
            token.kind == PpTokenKind::Punct && token.text(self.cleaned_text) == text
        })
    }

    /// Expands one token, pushing its replacement for rescan.
    ///
    /// Expansion = pre-expansion (prescan) + argument substitution + rescan.
    ///
    /// Reference: [WG14 N3220 (C23 draft) § 6.10.5 Macro replacement](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3220.pdf#subsection.6.10.5).
    fn expand_token(&mut self, token: &MacroToken, stream: &mut TokenStream<'_>) {
        let cleaned = self.cleaned_text;

        if self.expanded_bytes <= self.config.max_expanded_bytes
            && token.token.kind == PpTokenKind::Ident
            && let name = token.token.text(cleaned)
            && !token.is_hidden(name)
            && let Some(definition) = self.env.get(name).cloned()
        {
            match &definition.kind {
                MacroKind::Object => {
                    self.expand_object_like(token, name, &definition.replacement, stream);
                    return;
                }
                MacroKind::Function {
                    params,
                    is_variadic,
                } => {
                    let next = stream.peek();

                    // Only a macro invocation when followed by left parenthesis: WG14 N3220 § 6.10.5 Macro replacement, para 10
                    if !next.is_some_and(|next| self.is_punct(Some(&next), "(")) {
                        self.push(token.clone());
                        return;
                    }

                    self.expand_function_like(
                        token,
                        name,
                        &definition.replacement,
                        params,
                        *is_variadic,
                        stream,
                    );
                    return;
                }
            }
        }

        self.push(token.clone());
    }

    /// Returns true if the expansion depth cap was reached.
    ///
    /// Reports a diagnostic and emits `token` when it was.
    fn at_depth_limit(&mut self, token: &MacroToken) -> bool {
        if token.depth < self.config.max_expansion_depth {
            return false;
        }

        self.report(
            Severity::Error,
            "macro expansion depth exceeded".to_owned(),
            token.token.orig,
        );
        self.push(token.clone());
        true
    }

    /// Expands an object-like macro invocation.
    ///
    /// Reference: <https://en.cppreference.com/c/preprocessor/replace#Object-like_macros>
    fn expand_object_like(
        &mut self,
        instance_token: &MacroToken,
        name: &str,
        replacement: &[PpToken],
        stream: &mut TokenStream<'_>,
    ) {
        if self.at_depth_limit(instance_token) {
            return;
        }

        let hide = extend_hide(instance_token.hide.as_ref(), name);
        let depth = instance_token.depth + 1;
        let mut out = Vec::with_capacity(replacement.len());

        for (index, &replacement) in replacement.iter().enumerate() {
            let mut replacement = replacement;
            if index == 0 {
                replacement.has_space_before = instance_token.token.has_space_before;
                replacement.line_start = instance_token.token.line_start;
            }
            out.push(MacroToken {
                token: replacement,
                hide: Some(hide.clone()),
                depth,
            });
        }

        stream.push_replacement(out);
    }

    /// Expands a function-like macro invocation.
    ///
    /// Reference: <https://en.cppreference.com/c/preprocessor/replace#Function-like_macros>
    fn expand_function_like(
        &mut self,
        instance_token: &MacroToken,
        name: &str,
        replacement: &[PpToken],
        params: &[String],
        is_variadic: bool,
        stream: &mut TokenStream<'_>,
    ) {
        if self.at_depth_limit(instance_token) {
            return;
        }

        // Consume the opening parenthesis.
        let Some(lparen) = stream.next() else {
            return;
        };

        let Some(args) = self.collect_arguments(stream, lparen.token) else {
            return;
        };

        self.substitute_function_like(
            instance_token,
            name,
            replacement,
            params,
            is_variadic,
            args,
            stream,
        );
    }

    /// Collects a function-like macro invocation's arguments.
    ///
    /// Reference: [WG14 N3220 (C23 draft) § 6.10.5 Macro replacement](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3220.pdf#subsection.6.10.5).
    fn collect_arguments(
        &mut self,
        stream: &mut TokenStream<'_>,
        lparen: PpToken,
    ) -> Option<Arguments> {
        let mut lists = vec![Vec::new()];
        let mut commas = Vec::new();
        let mut depth = 1;

        loop {
            let Some(token) = stream.next() else {
                self.report(
                    Severity::Error,
                    "unterminated macro invocation".to_owned(),
                    lparen.orig,
                );
                return None;
            };

            // Comma and inner parentheses handling: WG14 N3220 § 6.10.5 Macro replacement, para 11
            if token.token.kind == PpTokenKind::Punct {
                let text = token.token.text(self.cleaned_text);
                if text == "(" {
                    depth += 1;
                } else if text == ")" {
                    depth -= 1;
                    if depth == 0 {
                        // Closing parenthesis
                        break;
                    }
                } else if depth == 1 && text == "," {
                    commas.push(token.token);
                    lists.push(Vec::new());
                    continue;
                }
            }

            lists.last_mut().expect("lists is never empty").push(token);
        }

        Some(Arguments { lists, commas })
    }

    /// Substitutes a function-like macro invocation's arguments.
    ///
    /// Reference: [WG14 N3220 (C23 draft) § 6.10.5.1 Argument substitution](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3220.pdf#subsubsection.6.10.5.1).
    #[expect(clippy::too_many_arguments, reason = "internal substitution step")]
    fn substitute_function_like(
        &mut self,
        invocation: &MacroToken,
        name: &str,
        replacement: &[PpToken],
        params: &[String],
        is_variadic: bool,
        args: Arguments,
        stream: &mut TokenStream<'_>,
    ) {
        let Arguments { mut lists, commas } = args;
        let fixed = params.len();

        if fixed == 0 && lists.len() == 1 && lists[0].is_empty() {
            lists.clear();
        }

        if is_variadic {
            if lists.len() < fixed {
                self.report(
                    Severity::Error,
                    format!("macro `{name}` requires at least {fixed} arguments"),
                    invocation.token.orig,
                );
                return;
            }
        } else if lists.len() != fixed {
            self.report(
                Severity::Error,
                format!(
                    "macro `{name}` requires {fixed} arguments, but {} were given",
                    lists.len()
                ),
                invocation.token.orig,
            );
            return;
        }

        let expanded_args: Vec<Vec<MacroToken>> = lists
            .into_iter()
            .map(|argument| self.prescan(argument))
            .collect();

        let varargs = if is_variadic {
            let mut varargs = Vec::new();
            for (offset, argument) in expanded_args[fixed..].iter().enumerate() {
                if offset > 0 {
                    varargs.push(MacroToken {
                        token: commas[fixed + offset - 1],
                        hide: None,
                        depth: 0,
                    });
                }
                varargs.extend(argument.iter().cloned());
            }
            varargs
        } else {
            Vec::new()
        };

        let hide = extend_hide(invocation.hide.as_ref(), name);
        let depth = invocation.depth + 1;
        let mut out = Vec::new();
        let mut first_output = true;

        for &token in replacement {
            let text = token.text(self.cleaned_text);
            if let Some(index) = params.iter().position(|param| param == text) {
                push_argument(
                    &mut out,
                    &expanded_args[index],
                    token,
                    &hide,
                    depth,
                    &mut first_output,
                    invocation,
                );
            } else if is_variadic && text == "__VA_ARGS__" {
                push_argument(
                    &mut out,
                    &varargs,
                    token,
                    &hide,
                    depth,
                    &mut first_output,
                    invocation,
                );
            } else {
                let mut token = MacroToken {
                    token,
                    hide: Some(hide.clone()),
                    depth,
                };
                if first_output {
                    token.token.has_space_before = invocation.token.has_space_before;
                    token.token.line_start = invocation.token.line_start;
                    first_output = false;
                }
                out.push(token);
            }
        }

        stream.push_replacement(out);
    }

    /// Expands an argument before substitution.
    fn prescan(&mut self, argument: Vec<MacroToken>) -> Vec<MacroToken> {
        let saved = std::mem::take(&mut self.tokens);
        let mut stream = TokenStream::new(&[]);
        stream.push_replacement(argument);

        while let Some(token) = stream.next() {
            self.expand_token(&token, &mut stream);
        }

        std::mem::replace(&mut self.tokens, saved)
    }

    /// Pushes a token to the output.
    fn push(&mut self, token: MacroToken) {
        self.expanded_bytes += token.token.text(self.cleaned_text).len();
        if self.expanded_bytes > self.config.max_expanded_bytes && !self.byte_cap_reported {
            self.byte_cap_reported = true;
            self.report(
                Severity::Error,
                "expanded text exceeds the byte limit".to_owned(),
                token.token.orig,
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

/// Appends a substituted argument to the expansion.
fn push_argument(
    out: &mut Vec<MacroToken>,
    argument: &[MacroToken],
    parameter: PpToken,
    hide: &HideSet,
    depth: usize,
    first_output: &mut bool,
    invocation: &MacroToken,
) {
    for (index, token) in argument.iter().enumerate() {
        let mut token = token.clone();
        if index == 0 {
            token.token.has_space_before = parameter.has_space_before;
            token.token.line_start = parameter.line_start;
        }
        token.hide = Some(union_hide(token.hide.as_ref(), hide));
        token.depth = depth;
        if *first_output {
            token.token.has_space_before = invocation.token.has_space_before;
            token.token.line_start = invocation.token.line_start;
            *first_output = false;
        }
        out.push(token);
    }
}
