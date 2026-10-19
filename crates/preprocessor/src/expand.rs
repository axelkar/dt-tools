//! Directive handling, conditionals, and macro expansion.

mod condition;
mod conditional;
mod directive;
mod rescan;
mod substitution;

use std::{borrow::Cow, sync::Arc};

use camino::{Utf8Path, Utf8PathBuf};
use dt_tools_diagnostic::{Diagnostic, Severity, Span};
pub(crate) use rescan::{MacroToken, TokenSpelling};

use crate::{
    config::{Define, PreprocessorConfig},
    env::{MacroDef, MacroEnv, MacroKind},
    expand::{
        conditional::Conditionals,
        rescan::{TokenStream, extend_hide},
        substitution::substitute_object_like,
    },
    files::{COMMAND_LINE_SOURCE_PREFIX, LoadedFiles, SourceId},
    lexer::PpTokenKind,
    origin::MacroExpansion,
    output::{IncludeEdge, MacroDefSite},
    token::PpToken,
};

/// The token stream after preprocessing.
pub(crate) struct Expansion {
    pub(crate) tokens: Vec<MacroToken>,
    pub(crate) macro_defs: Vec<MacroDefSite>,
    pub(crate) includes: Vec<IncludeEdge>,
    pub(crate) diagnostics: Vec<Diagnostic<SourceId>>,
    pub(crate) skipped: Vec<Span<SourceId>>,
}

/// Handles directives, conditionals, and macros starting at a root file.
pub(crate) fn expand<R>(
    tokens: &[PpToken],
    files: &mut LoadedFiles,
    orig_len: usize,
    file: SourceId,
    config: &PreprocessorConfig,
    read_file: &mut R,
) -> Expansion
where
    R: FnMut(&Utf8Path) -> Option<String>,
{
    profiling::scope!("preprocessor::expand", files.path(file).as_str());

    let mut expander = Expander {
        env: MacroEnv::default(),
        config,
        tokens: Vec::new(),
        macro_defs: Vec::new(),
        includes: Vec::new(),
        diagnostics: Vec::new(),
        skipped: Vec::new(),
        expanded_bytes: 0,
        byte_cap_reported: false,
    };

    for define in &config.defines {
        define_predefined(&mut expander, files, define);
    }

    expand_file(&mut expander, files, read_file, file, tokens, orig_len, 0);

    let Expander {
        tokens,
        macro_defs,
        includes,
        diagnostics,
        skipped,
        ..
    } = expander;

    Expansion {
        tokens,
        macro_defs,
        includes,
        diagnostics,
        skipped,
    }
}

/// Defines a macro from [`PreprocessorConfig::defines`].
fn define_predefined(expander: &mut Expander<'_>, files: &mut LoadedFiles, define: &Define) {
    let path = Utf8PathBuf::from(format!("{COMMAND_LINE_SOURCE_PREFIX}{}>", define.name));
    let contents = format!("{} {}", define.name, define.value);
    let loaded = files.add(&path, &contents);

    let Some((name_token, replacement)) = loaded.tokens.split_first() else {
        return;
    };

    if name_token.kind != PpTokenKind::Ident || name_token.text(&files.cleaned) != define.name {
        expander.report(
            Severity::Error,
            format!("invalid macro name `{}`", define.name),
            name_token.orig.within_file(loaded.id),
        );
        return;
    }

    expander.env.define(
        define.name.clone(),
        MacroDef {
            kind: MacroKind::Object,
            replacement: replacement.to_vec(),
            definition: name_token.orig.within_file(loaded.id),
        },
    );
}

/// Handles directives, conditionals, and macros of one file in source order.
///
/// Each file has its own conditional group stack: conditional directives cannot span files.
pub(super) fn expand_file<R>(
    expander: &mut Expander<'_>,
    files: &mut LoadedFiles,
    read_file: &mut R,
    file: SourceId,
    tokens: &[PpToken],
    orig_len: usize,
    depth: usize,
) where
    R: FnMut(&Utf8Path) -> Option<String>,
{
    profiling::scope!("preprocessor::expand_file", files.path(file).as_str());

    let mut conditionals = Conditionals::default();
    let mut stream = TokenStream::new(tokens);

    loop {
        if stream.is_rescan_empty()
            && let Some(directive) =
                crate::directive::parse(stream.remaining_source(), &files.cleaned)
        {
            stream.skip_source(directive.len_tokens);
            directive::DirectiveHandler {
                expander,
                conditionals: &mut conditionals,
                files,
                read_file,
                file,
                depth,
                directive,
                argument_pos: 0,
            }
            .handle();
            continue;
        }

        let Some(token) = stream.next() else {
            break;
        };

        if conditionals.is_active() {
            expander.expand_token(&token, &mut stream, &files.cleaned);
        }
    }

    for opening in conditionals.finish(orig_len) {
        expander.report(
            Severity::Error,
            "unterminated conditional directive".to_owned(),
            opening.within_file(file),
        );
    }

    expander.skipped.extend(
        conditionals
            .skipped()
            .iter()
            .map(|range| range.within_file(file)),
    );
}

pub(crate) struct Expander<'a> {
    env: MacroEnv,
    config: &'a PreprocessorConfig,
    tokens: Vec<MacroToken>,
    macro_defs: Vec<MacroDefSite>,
    includes: Vec<IncludeEdge>,
    diagnostics: Vec<Diagnostic<SourceId>>,
    skipped: Vec<Span<SourceId>>,
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
    commas: Vec<MacroToken>,
}

/// Returns true if `token` is the punctuator `text`.
pub(super) fn is_punct_token(token: Option<&PpToken>, text: &str, cleaned: &str) -> bool {
    token.is_some_and(|token| token.kind == PpTokenKind::Punct && token.text(cleaned) == text)
}

impl Expander<'_> {
    /// Expands one token, pushing its replacement for rescan.
    ///
    /// Expansion = pre-expansion (prescan) + argument substitution + rescan.
    ///
    /// Reference: [WG14 N3220 (C23 draft) § 6.10.5 Macro replacement](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3220.pdf#subsection.6.10.5).
    fn expand_token(&mut self, token: &MacroToken, stream: &mut TokenStream<'_>, cleaned: &str) {
        if self.expanded_bytes <= self.config.max_expanded_bytes
            && token.kind == PpTokenKind::Ident
            && let name = token.text(cleaned)
            && !token.is_hidden(name)
            && let Some(definition) = self.env.get(name).cloned()
        {
            match &definition.kind {
                MacroKind::Object => {
                    self.expand_object_like(
                        token,
                        name,
                        definition.definition,
                        &definition.replacement,
                        stream,
                        cleaned,
                    );
                    return;
                }
                MacroKind::Function {
                    params,
                    variadic_param,
                } => {
                    let next = stream.peek();

                    // Only a macro invocation when followed by left parenthesis: WG14 N3220 § 6.10.5 Macro replacement, para 10
                    if !next.is_some_and(|next| {
                        next.kind == PpTokenKind::Punct && next.text(cleaned) == "("
                    }) {
                        self.push(token.clone(), cleaned);
                        return;
                    }

                    self.expand_function_like(
                        token,
                        name,
                        definition.definition,
                        &definition.replacement,
                        params,
                        variadic_param.as_deref(),
                        stream,
                        cleaned,
                    );
                    return;
                }
            }
        }

        self.push(token.clone(), cleaned);
    }

    /// Returns true if the expansion depth cap was reached.
    ///
    /// Reports a diagnostic and emits `token` when it was.
    fn at_depth_limit(&mut self, token: &MacroToken, cleaned: &str) -> bool {
        if token.depth < self.config.max_expansion_depth {
            return false;
        }

        self.report(
            Severity::Error,
            "macro expansion depth exceeded".to_owned(),
            token.primary_span(),
        );
        self.push(token.clone(), cleaned);
        true
    }

    /// Expands an object-like macro invocation.
    ///
    /// Reference: <https://en.cppreference.com/c/preprocessor/replace#Object-like_macros>
    fn expand_object_like(
        &mut self,
        instance_token: &MacroToken,
        name: &str,
        definition: Span<SourceId>,
        replacement: &[PpToken],
        stream: &mut TokenStream<'_>,
        cleaned: &str,
    ) {
        profiling::scope!("preprocessor::expand_object_like", name);

        if self.at_depth_limit(instance_token, cleaned) {
            return;
        }

        let expansion = Arc::new(MacroExpansion {
            name: Arc::from(name),
            invocation: instance_token.primary_span(),
            definition,
            parent: instance_token.expansion.clone(),
        });
        let hide = extend_hide(instance_token.hide.as_ref(), name);
        let depth = instance_token.depth + 1;
        let (mut out, errors) =
            substitute_object_like(cleaned, hide, depth, replacement, expansion);
        for (message, span) in errors {
            self.report(Severity::Error, message, span);
        }

        if let Some(first) = out.first_mut() {
            first.has_space_before = instance_token.has_space_before;
            first.line_start = instance_token.line_start;
        }

        stream.push_replacement(out);
    }

    /// Expands a function-like macro invocation.
    ///
    /// Reference: <https://en.cppreference.com/c/preprocessor/replace#Function-like_macros>
    #[expect(clippy::too_many_arguments, reason = "recursive expansion step")]
    fn expand_function_like(
        &mut self,
        instance_token: &MacroToken,
        name: &str,
        definition: Span<SourceId>,
        replacement: &[PpToken],
        params: &[String],
        variadic_param: Option<&str>,
        stream: &mut TokenStream<'_>,
        cleaned: &str,
    ) {
        profiling::scope!("preprocessor::expand_function_like", name);

        if self.at_depth_limit(instance_token, cleaned) {
            return;
        }

        // Consume the opening parenthesis.
        let Some(lparen) = stream.next() else {
            return;
        };

        let Some(args) = self.collect_arguments(stream, lparen.primary_span(), cleaned) else {
            return;
        };

        let expansion = Arc::new(MacroExpansion {
            name: Arc::from(name),
            invocation: instance_token.primary_span(),
            definition,
            parent: instance_token.expansion.clone(),
        });
        self.substitute_function_like(
            instance_token,
            name,
            replacement,
            params,
            variadic_param,
            args,
            stream,
            cleaned,
            expansion,
        );
    }

    /// Collects a function-like macro invocation's arguments.
    ///
    /// Reference: [WG14 N3220 (C23 draft) § 6.10.5 Macro replacement](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3220.pdf#subsection.6.10.5).
    fn collect_arguments(
        &mut self,
        stream: &mut TokenStream<'_>,
        lparen: Span<SourceId>,
        cleaned: &str,
    ) -> Option<Arguments> {
        let mut lists = vec![Vec::new()];
        let mut commas = Vec::new();
        let mut depth = 1;

        loop {
            let Some(token) = stream.next() else {
                self.report(
                    Severity::Error,
                    "unterminated macro invocation".to_owned(),
                    lparen,
                );
                return None;
            };

            // Comma and inner parentheses handling: WG14 N3220 § 6.10.5 Macro replacement, para 11
            if token.kind == PpTokenKind::Punct {
                let text = token.text(cleaned);
                if text == "(" {
                    depth += 1;
                } else if text == ")" {
                    depth -= 1;
                    if depth == 0 {
                        // Closing parenthesis
                        break;
                    }
                } else if depth == 1 && text == "," {
                    commas.push(token);
                    lists.push(Vec::new());
                    continue;
                }
            }

            lists.last_mut().expect("lists is never empty").push(token);
        }

        Some(Arguments { lists, commas })
    }

    /// Expands an argument before substitution.
    fn prescan(&mut self, argument: Vec<MacroToken>, cleaned: &str) -> Vec<MacroToken> {
        let saved = std::mem::take(&mut self.tokens);
        let mut stream = TokenStream::new(&[]);
        stream.push_replacement(argument);

        while let Some(token) = stream.next() {
            self.expand_token(&token, &mut stream, cleaned);
        }

        std::mem::replace(&mut self.tokens, saved)
    }

    /// Pushes a token to the output.
    fn push(&mut self, token: MacroToken, cleaned: &str) {
        self.expanded_bytes += token.text(cleaned).len();
        if self.expanded_bytes > self.config.max_expanded_bytes && !self.byte_cap_reported {
            self.byte_cap_reported = true;
            self.report(
                Severity::Error,
                "expanded text exceeds the byte limit".to_owned(),
                token.primary_span(),
            );
        }
        self.tokens.push(token);
    }

    /// Reports a diagnostic.
    fn report(&mut self, severity: Severity, message: String, span: Span<SourceId>) {
        self.diagnostics
            .push(Diagnostic::new(span, Cow::Owned(message), severity));
    }
}
