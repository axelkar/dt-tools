//! Function-like macro argument substitution.

use std::sync::Arc;

use dt_tools_diagnostic::{Severity, Span, text_range::TextRange};
use logos::Logos;

use super::{Arguments, Expander, MacroToken, TokenSpelling, is_punct_token};
use crate::{
    expand::rescan::{HideSet, TokenStream, extend_hide, union_hide},
    files::SourceId,
    lexer::PpTokenKind,
    origin::{Argument, MacroExpansion, Origin},
    token::PpToken,
};

/// One piece of a substituted replacement list.
enum Substituted {
    /// A zero-width operand of `##`.
    Placemarker,
    /// A real token.
    Token(MacroToken),
}

/// The replacement list and prepared arguments of an invocation.
#[derive(Clone, Copy)]
struct SubstitutionInfo<'a> {
    /// Replacement tokens.
    replacement: &'a [PpToken],
    /// Parameter names in order.
    params: &'a [String],
    /// Raw arguments, including variadic ones.
    raw_arguments: &'a [Vec<MacroToken>],
    /// Prescanned arguments, including variadic ones.
    expanded_arguments: &'a [Vec<MacroToken>],
    /// Raw variadic arguments joined with their commas.
    raw_varargs: &'a [MacroToken],
    /// Prescanned variadic arguments joined with their commas.
    expanded_varargs: &'a [MacroToken],
    /// Whether the macro is function-like.
    is_function_like: bool,
    /// The variadic parameter's name, or `None` if the macro is not variadic.
    variadic_param: Option<&'a str>,
}

impl SubstitutionInfo<'_> {
    /// Returns the index of the parameter with name `name`.
    fn parameter(&self, name: &str) -> Option<usize> {
        self.params.iter().position(|param| param == name)
    }

    /// Returns true if a variadic argument was written.
    fn has_varargs(&self) -> bool {
        self.raw_arguments.len() > self.params.len()
    }

    /// Returns true if the replacement token at `index` is an operand of `##`.
    fn adjacent_to_paste(&self, index: usize, cleaned: &str) -> bool {
        adjacent_to_paste(self.replacement, index, cleaned)
    }
}

/// Returns true if the replacement token at `index` is an operand of `##`.
fn adjacent_to_paste(replacement: &[PpToken], index: usize, cleaned: &str) -> bool {
    let previous = index
        .checked_sub(1)
        .and_then(|index| replacement.get(index));
    let next = replacement.get(index + 1);
    is_punct_token(previous, "##", cleaned) || is_punct_token(next, "##", cleaned)
}

/// Returns which arguments need macro expansion.
///
/// An argument is expanded only when its parameter is used outside `#` and `##`.
fn arguments_to_expand(
    replacement: &[PpToken],
    params: &[String],
    fixed: usize,
    variadic_param: Option<&str>,
    cleaned: &str,
) -> (Vec<bool>, bool) {
    let mut params_to_expand = vec![false; fixed];
    let mut varargs_to_expand = false;

    for (index, token) in replacement.iter().enumerate() {
        let text = token.text(cleaned);
        let param = params.iter().position(|param| param == text);
        let is_varargs = variadic_param == Some(text);
        if param.is_none() && !is_varargs {
            continue;
        }

        let stringified = index > 0 && is_punct_token(Some(&replacement[index - 1]), "#", cleaned);
        if stringified || adjacent_to_paste(replacement, index, cleaned) {
            continue;
        }

        if let Some(param) = param {
            params_to_expand[param] = true;
        } else {
            varargs_to_expand = true;
        }
    }

    (params_to_expand, varargs_to_expand)
}

/// Builds a function-like macro's substituted replacement list.
struct SubstitutionBuilder<'a> {
    substitution_info: SubstitutionInfo<'a>,
    cleaned: &'a str,
    hide: HideSet,
    depth: usize,
    expansion: Arc<MacroExpansion>,
    /// Substituted replacement tokens.
    out: Vec<Substituted>,
    errors: Vec<(String, Span<SourceId>)>,
    pending_paste: Option<Span<SourceId>>,
}

impl<'a> SubstitutionBuilder<'a> {
    /// Creates a builder for one macro invocation.
    fn new(
        cleaned: &'a str,
        hide: HideSet,
        depth: usize,
        substitution_info: SubstitutionInfo<'a>,
        expansion: Arc<MacroExpansion>,
    ) -> Self {
        Self {
            substitution_info,
            cleaned,
            hide,
            depth,
            expansion,
            out: Vec::new(),
            errors: Vec::new(),
            pending_paste: None,
        }
    }

    /// Pushes a token, applying a pending `##`.
    fn token(&mut self, token: MacroToken) {
        if let Some(origin) = self.pending_paste.take() {
            self.paste_right(origin, Some(token));
        } else {
            self.out.push(Substituted::Token(token));
        }
    }

    /// Pushes a source token with the macro's hide set and depth.
    fn plain(&mut self, token: PpToken) {
        let mut token = MacroToken::source(token);
        token.hide = Some(Arc::clone(&self.hide));
        token.depth = self.depth;
        token.expansion = Some(Arc::clone(&self.expansion));
        self.token(token);
    }

    /// Pushes a placemarker, applying a pending `##`.
    fn placemarker(&mut self) {
        if let Some(origin) = self.pending_paste.take() {
            self.paste_right(origin, None);
        } else {
            self.out.push(Substituted::Placemarker);
        }
    }

    /// Starts a `##` operation.
    fn paste(&mut self, origin: Span<SourceId>) {
        if self.pending_paste.replace(origin).is_some() {
            self.error("`##` cannot appear next to another `##`".to_owned(), origin);
        }
    }

    /// Substitutes an argument, applying a pending `##` to its first token.
    fn argument(
        &mut self,
        argument: &[MacroToken],
        parameter: PpToken,
        raw_span: Option<Span<SourceId>>,
    ) {
        if argument.is_empty() {
            return;
        }

        let span = raw_span.unwrap_or_else(|| parameter.orig.within_file(parameter.file));

        for (index, token) in argument.iter().enumerate() {
            let mut token = token.clone();
            if index == 0 {
                token.has_space_before = parameter.has_space_before;
                token.line_start = parameter.line_start;
            }
            token.hide = Some(union_hide(token.hide.as_ref(), &self.hide));
            token.depth = self.depth;

            token.expansion = Some(match token.expansion.take() {
                None => Arc::clone(&self.expansion),
                Some(existing) if chain_contains(&existing, &self.expansion) => existing,
                Some(existing) => attach_parent(&existing, Arc::clone(&self.expansion)),
            });
            let argument = Arc::new(Argument {
                parameter: Arc::from(parameter.text(self.cleaned)),
                macro_name: Arc::clone(&self.expansion.name),
                span,
            });
            if !token.arguments.contains(&argument) {
                token.arguments.push(argument);
            }

            if index == 0 {
                self.token(token);
            } else {
                self.out.push(Substituted::Token(token));
            }
        }
    }

    /// Substitutes an operand of `##`, which may be empty.
    fn paste_operand(
        &mut self,
        argument: &[MacroToken],
        parameter: PpToken,
        raw_span: Option<Span<SourceId>>,
    ) {
        if argument.is_empty() {
            self.placemarker();
        } else {
            self.argument(argument, parameter, raw_span);
        }
    }

    /// Substitutes a `#` operator, returning the number of replacement tokens consumed.
    fn stringify(&mut self, index: usize) -> Result<usize, String> {
        let (token, consumed) = self.stringify_operand(index)?;
        self.token(token);
        Ok(consumed)
    }

    /// Substitutes the replacement list into the output.
    fn substitute(&mut self) {
        let info = self.substitution_info;
        let mut index = 0;

        while index < info.replacement.len() {
            let token = info.replacement[index];
            let text = token.text(self.cleaned);

            if token.kind == PpTokenKind::Punct && text == "##" {
                self.paste(token.orig.within_file(token.file));
                index += 1;
                continue;
            }

            // GNU comma elision: `, ## __VA_ARGS__`.
            if token.kind == PpTokenKind::Punct
                && text == ","
                && info.variadic_param.is_some()
                && is_punct_token(info.replacement.get(index + 1), "##", self.cleaned)
                && info.replacement.get(index + 2).is_some_and(|operand| {
                    operand.kind == PpTokenKind::Ident
                        && Some(operand.text(self.cleaned)) == info.variadic_param
                })
            {
                let operand = info.replacement[index + 2];
                // The comma is elided only when no variadic argument was written.
                if !info.has_varargs() {
                    index += 3;
                    continue;
                }
                self.plain(token);
                self.argument(info.raw_varargs, operand, argument_span(info.raw_varargs));
                index += 3;
                continue;
            }

            if info.is_function_like && token.kind == PpTokenKind::Punct && text == "#" {
                match self.stringify(index) {
                    Ok(consumed) => index += consumed,
                    Err(message) => {
                        self.error(message, token.orig.within_file(token.file));
                        index += 2;
                    }
                }
                continue;
            }

            if let Some(param) = info.parameter(text) {
                let raw_span = argument_span(&info.raw_arguments[param]);
                if info.adjacent_to_paste(index, self.cleaned) {
                    self.paste_operand(&info.raw_arguments[param], token, raw_span);
                } else {
                    self.argument(&info.expanded_arguments[param], token, raw_span);
                }
            } else if Some(text) == info.variadic_param {
                let raw_span = argument_span(info.raw_varargs);
                if info.adjacent_to_paste(index, self.cleaned) {
                    self.paste_operand(info.raw_varargs, token, raw_span);
                } else {
                    self.argument(info.expanded_varargs, token, raw_span);
                }
            } else {
                self.plain(token);
            }
            index += 1;
        }
    }

    /// Returns the stringified token for the `#` at `index` and the number of replacement tokens it
    /// consumes.
    fn stringify_operand(&self, index: usize) -> Result<(MacroToken, usize), String> {
        let substitution = self.substitution_info;
        let hash = substitution.replacement[index];
        let Some(operand) = substitution.replacement.get(index + 1) else {
            return Err("`#` must be followed by a macro parameter".to_owned());
        };
        let operand_text = operand.text(self.cleaned);

        let argument = if let Some(param) = substitution.parameter(operand_text) {
            &substitution.raw_arguments[param]
        } else if Some(operand_text) == substitution.variadic_param {
            substitution.raw_varargs
        } else {
            return Err("`#` must be followed by a macro parameter".to_owned());
        };

        let mut spelling = String::from("\"");
        let mut first = true;
        for token in argument {
            if !first && token.has_space_before {
                spelling.push(' ');
            }
            for ch in token.text(self.cleaned).chars() {
                if ch == '"' || ch == '\\' {
                    spelling.push('\\');
                }
                spelling.push(ch);
            }
            first = false;
        }
        spelling.push('"');

        let operator = Span {
            file: hash.file,
            text_range: hash.orig,
        };
        let argument_span = argument_span(argument).unwrap_or(operator);

        Ok((
            MacroToken {
                kind: PpTokenKind::String,
                spelling: TokenSpelling::Synthetic(Arc::from(spelling)),
                has_space_before: hash.has_space_before,
                line_start: hash.line_start,
                hide: Some(Arc::clone(&self.hide)),
                depth: self.depth,
                origin: Origin::Stringify {
                    operator,
                    argument: argument_span,
                },
                expansion: Some(Arc::clone(&self.expansion)),
                arguments: vec![Arc::new(Argument {
                    parameter: Arc::from(operand_text),
                    macro_name: Arc::clone(&self.expansion.name),
                    span: argument_span,
                })],
            },
            2,
        ))
    }

    /// Applies a pending `##` with `right`.
    fn paste_right(&mut self, origin: Span<SourceId>, right: Option<MacroToken>) {
        let left = match self.out.pop() {
            None => {
                self.error(
                    "`##` cannot appear at the start of a macro expansion".to_owned(),
                    origin,
                );
                None
            }
            Some(Substituted::Placemarker) => None,
            Some(Substituted::Token(token)) => Some(token),
        };

        match paste_pair(
            left.as_ref(),
            right.as_ref(),
            self.cleaned,
            &self.expansion,
            origin,
        ) {
            Paste::Token(token) => self.out.push(Substituted::Token(token)),
            Paste::Placemarker => self.out.push(Substituted::Placemarker),
            Paste::Invalid => {
                if let Some(left) = left {
                    self.out.push(Substituted::Token(left));
                }
                if let Some(right) = right {
                    self.out.push(Substituted::Token(right));
                }
            }
        }
    }

    /// Finishes the list, returning its tokens and substitution errors.
    fn finish(mut self) -> (Vec<MacroToken>, Vec<(String, Span<SourceId>)>) {
        if let Some(origin) = self.pending_paste.take() {
            self.error(
                "`##` cannot appear at the end of a macro expansion".to_owned(),
                origin,
            );
        }

        let tokens = self
            .out
            .into_iter()
            .filter_map(|substituted| match substituted {
                Substituted::Placemarker => None,
                Substituted::Token(token) => Some(token),
            })
            .collect();

        (tokens, self.errors)
    }

    /// Records a substitution error.
    fn error(&mut self, message: String, span: Span<SourceId>) {
        self.errors.push((message, span));
    }
}

impl Expander<'_> {
    /// Substitutes a function-like macro invocation's arguments.
    ///
    /// Reference: [WG14 N3220 (C23 draft) § 6.10.5.1 Argument substitution](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3220.pdf#subsubsection.6.10.5.1).
    #[expect(clippy::too_many_arguments, reason = "internal substitution step")]
    pub(super) fn substitute_function_like(
        &mut self,
        invocation: &MacroToken,
        name: &str,
        replacement: &[PpToken],
        params: &[String],
        variadic_param: Option<&str>,
        args: Arguments,
        stream: &mut TokenStream<'_>,
        cleaned: &str,
        expansion: Arc<MacroExpansion>,
    ) {
        let Arguments {
            lists: mut raw_arguments,
            commas,
        } = args;
        let fixed = params.len();

        if fixed == 0 && raw_arguments.len() == 1 && raw_arguments[0].is_empty() {
            raw_arguments.clear();
        }

        if variadic_param.is_some() {
            if raw_arguments.len() < fixed {
                self.report(
                    Severity::Error,
                    format!("macro `{name}` requires at least {fixed} arguments"),
                    invocation.primary_span(),
                );
                return;
            }
        } else if raw_arguments.len() != fixed {
            self.report(
                Severity::Error,
                format!(
                    "macro `{name}` requires {fixed} arguments, but {} were given",
                    raw_arguments.len()
                ),
                invocation.primary_span(),
            );
            return;
        }

        let (params_to_expand, varargs_to_expand) =
            arguments_to_expand(replacement, params, fixed, variadic_param, cleaned);
        let expanded_arguments: Vec<Vec<MacroToken>> = raw_arguments
            .iter()
            .enumerate()
            .map(|(index, argument)| {
                let expand = if index < fixed {
                    params_to_expand[index]
                } else {
                    varargs_to_expand
                };
                if expand {
                    self.prescan(argument.clone(), cleaned)
                } else {
                    argument.clone()
                }
            })
            .collect();

        // `#` and `##` operands are not prescanned, so they use the raw argument tokens.
        let raw_varargs = if variadic_param.is_some() {
            join_varargs(&raw_arguments[fixed..], &commas, fixed)
        } else {
            Vec::new()
        };
        let expanded_varargs = if variadic_param.is_some() {
            join_varargs(&expanded_arguments[fixed..], &commas, fixed)
        } else {
            Vec::new()
        };

        let substitution_info = SubstitutionInfo {
            replacement,
            params,
            raw_arguments: &raw_arguments,
            expanded_arguments: &expanded_arguments,
            raw_varargs: &raw_varargs,
            expanded_varargs: &expanded_varargs,
            is_function_like: true,
            variadic_param,
        };

        let hide = extend_hide(invocation.hide.as_ref(), name);
        let depth = invocation.depth + 1;
        let mut builder =
            SubstitutionBuilder::new(cleaned, hide, depth, substitution_info, expansion);
        builder.substitute();

        let (mut out, errors) = builder.finish();
        for (message, span) in errors {
            self.report(Severity::Error, message, span);
        }

        if let Some(first) = out.first_mut() {
            first.has_space_before = invocation.has_space_before;
            first.line_start = invocation.line_start;
        }

        stream.push_replacement(out);
    }
}

/// Substitutes an object-like macro's replacement list.
pub(super) fn substitute_object_like(
    cleaned: &str,
    hide: HideSet,
    depth: usize,
    replacement: &[PpToken],
    expansion: Arc<MacroExpansion>,
) -> (Vec<MacroToken>, Vec<(String, Span<SourceId>)>) {
    let substitution_info = SubstitutionInfo {
        replacement,
        params: &[],
        raw_arguments: &[],
        expanded_arguments: &[],
        raw_varargs: &[],
        expanded_varargs: &[],
        is_function_like: false,
        variadic_param: None,
    };
    let mut builder = SubstitutionBuilder::new(cleaned, hide, depth, substitution_info, expansion);
    builder.substitute();
    builder.finish()
}

/// The result of pasting two operands.
enum Paste {
    /// One token formed from the operands.
    Token(MacroToken),
    /// Both operands were empty.
    Placemarker,
    /// The concatenation is not one token, so the operands are kept separately.
    Invalid,
}

/// Pastes two paste operands into one token.
fn paste_pair(
    left: Option<&MacroToken>,
    right: Option<&MacroToken>,
    cleaned: &str,
    expansion: &Arc<MacroExpansion>,
    operator: Span<SourceId>,
) -> Paste {
    match (left, right) {
        (None, None) => Paste::Placemarker,
        (Some(token), None) | (None, Some(token)) => Paste::Token(token.clone()),
        (Some(left), Some(right)) => {
            let mut text = String::new();
            text.push_str(left.text(cleaned));
            text.push_str(right.text(cleaned));

            // Assembler mode keeps the operands when they do not form one token.
            let mut lexer = PpTokenKind::lexer(&text);
            let Some(Ok(kind)) = lexer.next() else {
                return Paste::Invalid;
            };
            if lexer.slice() != text || lexer.next().is_some() {
                return Paste::Invalid;
            }

            let hide = match (left.hide.as_ref(), right.hide.as_ref()) {
                (Some(left_hide), Some(right_hide)) => {
                    Some(union_hide(Some(left_hide), right_hide))
                }
                (Some(hide), None) | (None, Some(hide)) => Some(Arc::clone(hide)),
                (None, None) => None,
            };

            Paste::Token(MacroToken {
                kind,
                spelling: TokenSpelling::Synthetic(Arc::from(text)),
                has_space_before: left.has_space_before,
                line_start: left.line_start,
                hide,
                depth: left.depth.max(right.depth),
                origin: Origin::Concat {
                    left: token_span(left),
                    right: token_span(right),
                    operator,
                },
                expansion: Some(Arc::clone(expansion)),
                arguments: {
                    let mut arguments = left.arguments.clone();
                    for argument in &right.arguments {
                        if !arguments.contains(argument) {
                            arguments.push(Arc::clone(argument));
                        }
                    }
                    arguments
                },
            })
        }
    }
}

/// Returns `expansion` with `parent` attached at the end of its chain.
fn attach_parent(
    expansion: &Arc<MacroExpansion>,
    parent: Arc<MacroExpansion>,
) -> Arc<MacroExpansion> {
    let parent = match &expansion.parent {
        Some(existing) => Some(attach_parent(existing, parent)),
        None => Some(parent),
    };

    Arc::new(MacroExpansion {
        name: Arc::clone(&expansion.name),
        invocation: expansion.invocation,
        definition: expansion.definition,
        parent,
    })
}

/// Returns true if `needle` is in `expansion`'s chain.
fn chain_contains(expansion: &MacroExpansion, needle: &MacroExpansion) -> bool {
    (expansion.name == needle.name && expansion.invocation == needle.invocation)
        || expansion
            .parent
            .as_deref()
            .is_some_and(|parent| chain_contains(parent, needle))
}

/// Returns the source span of `token`.
fn token_span(token: &MacroToken) -> Span<SourceId> {
    token.primary_span()
}

/// Returns the source span of a macro argument.
fn argument_span(argument: &[MacroToken]) -> Option<Span<SourceId>> {
    let first = token_span(argument.first()?);
    let last = token_span(argument.last()?);
    let range = if first.file == last.file {
        TextRange::new(first.text_range.start, last.text_range.end)
    } else {
        first.text_range
    };

    Some(Span {
        file: first.file,
        text_range: range,
    })
}

/// Joins variadic argument lists with their original commas.
fn join_varargs(lists: &[Vec<MacroToken>], commas: &[MacroToken], fixed: usize) -> Vec<MacroToken> {
    let mut out = Vec::new();

    for (offset, argument) in lists.iter().enumerate() {
        if offset > 0 {
            let mut comma = commas[fixed + offset - 1].clone();
            comma.hide = None;
            comma.depth = 0;
            comma.expansion = None;
            comma.arguments = Vec::new();
            out.push(comma);
        }
        out.extend(argument.iter().cloned());
    }

    out
}
