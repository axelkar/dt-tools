use std::borrow::Cow;

use dt_tools_analyzer::macros::MacroDefinition;
use dt_tools_diagnostic::Severity;
use dt_tools_parser::{
    TextRange,
    ast::{self, AstNode, AstToken},
    lexer::{self, TokenKind},
};
use rustc_hash::FxHashSet;

use crate::{
    db::BaseDb,
    diag::{Diag, MacroSubstitutionProvenance},
    macros::env::TrackedMapEnvMut,
};

/// A macro expansion that happened during argument prescan.
#[derive(Debug, Clone)]
pub(crate) struct PrescanExpansion {
    /// The macro substitution provenance.
    pub substitution: MacroSubstitutionProvenance,
    /// This expansion's output within the prescan output text.
    pub output_range: TextRange,
}

pub mod env;

pub enum MacroCtx<'a> {
    Explicit(&'a ast::MacroInvocation),
    Implicit(&'a ast::Name),
}
impl MacroCtx<'_> {
    /// Returns the name of the macro.
    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Explicit(invoc) => invoc
                .green_ident()
                .map(|ident| ident.text.as_str())
                .unwrap_or_default(),
            Self::Implicit(name) => name.syntax().green.text.as_str(),
        }
    }

    /// Returns the text range of the macro invocation.
    #[must_use]
    pub fn text_range(&self) -> TextRange {
        match self {
            Self::Explicit(invoc) => invoc.syntax().text_range(),
            Self::Implicit(name) => name.syntax().text_range(),
        }
    }

    /// Returns the text range of each argument.
    #[must_use]
    pub fn arg_ranges(&self) -> Vec<TextRange> {
        match self {
            Self::Explicit(invoc) => invoc.arguments().map(|arg| arg.text_range()).collect(),
            Self::Implicit(_) => Vec::new(),
        }
    }
}

/// Collects the trimmed argument string and its non-empty token range from a span of tokens.
fn collect_arg(tokens: &[lexer::Token<'_>]) -> (String, TextRange) {
    let arg = tokens.iter().map(|t| t.text).collect::<String>();
    let trimmed = arg.trim().to_owned();
    let mut first = None;
    let mut last = None;
    for t in tokens {
        if !t.text.trim().is_empty() {
            first.get_or_insert(t.text_range.start);
            last = Some(t.text_range.end);
        }
    }
    let range = match (first, last) {
        (Some(s), Some(e)) => TextRange::new(s, e),
        _ => tokens
            .first()
            .map_or(TextRange::new(0, 0), |t| t.text_range),
    };
    (trimmed, range)
}

/// Split a `MACRO(a, b, c)` token stream into argument strings plus range metadata.
///
/// `lparen_idx` is the index of the `(` token. `name_range` is the range of the macro name token.
fn extract_args_from_tokens(
    tokens: &[lexer::Token<'_>],
    lparen_idx: usize,
    name_range: TextRange,
) -> (usize, Vec<String>, Vec<TextRange>, TextRange) {
    let mut level = 0u32;
    let mut start = lparen_idx + 1;
    let mut args = Vec::new();
    let mut arg_ranges = Vec::new();

    for (i, tok) in tokens.iter().enumerate().skip(lparen_idx + 1) {
        match tok.kind {
            Ok(TokenKind::LParen) => level += 1,
            Ok(TokenKind::RParen) if level == 0 => {
                let (arg, range) = collect_arg(&tokens[start..i]);
                if !arg.is_empty() || !args.is_empty() {
                    args.push(arg);
                    arg_ranges.push(range);
                }
                let invoc = TextRange::new(name_range.start, tok.text_range.end);
                return (i, args, arg_ranges, invoc);
            }
            Ok(TokenKind::RParen) => level -= 1,
            Ok(TokenKind::Comma) if level == 0 => {
                let (arg, range) = collect_arg(&tokens[start..i]);
                args.push(arg);
                arg_ranges.push(range);
                start = i + 1;
            }
            _ => {}
        }
    }

    // No matching RParen found
    (tokens.len().saturating_sub(1), args, arg_ranges, name_range)
}

/// Single-pass macro expansion with inline replacement.
fn expand_macros_in_text<'db>(
    db: &'db dyn BaseDb,
    env: &TrackedMapEnvMut<'db>,
    text: &str,
    disabled: &mut FxHashSet<String>,
) -> String {
    let tokens = lexer::lex(text);
    let mut out = String::with_capacity(text.len());
    let mut i = 0;

    while let Some(tok) = tokens.get(i) {
        if tok.kind != Ok(TokenKind::Ident) || env.get_macro_def(db, tok.text).is_none() {
            out.push_str(tok.text);
            i += 1;
            continue;
        }

        let macro_name = tok.text.to_owned();
        let def = env.get_macro_def(db, &macro_name).unwrap();

        if disabled.contains(&macro_name) {
            out.push_str(tok.text);
            i += 1;
            continue;
        }

        // Gather arguments (empty vec for object-like macros).
        let (args, advance_to) = if def.param_count() == 0 {
            (Vec::new(), i + 1)
        } else {
            let mut j = i + 1;
            while let Some(tok) = tokens.get(j)
                && tok.kind.is_ok_and(TokenKind::is_trivia)
            {
                j += 1;
            }
            if let Some(tok) = tokens.get(j)
                && tok.kind == Ok(TokenKind::LParen)
            {
                let (rparen_idx, args, _arg_ranges, _invoc) =
                    extract_args_from_tokens(&tokens, j, tok.text_range);
                (args, rparen_idx + 1)
            } else {
                // Names a function-like macro but isn't followed by `(`.
                out.push_str(tok.text);
                i += 1;
                continue;
            }
        };

        // Expand = prescan + substitute + rescan.
        let prescanned = prescan_arguments(db, env, &args, def, disabled);
        let sub = def
            .substitute(&prescanned)
            .expect("argument count should be validated during parse");
        // Disable only for rescanning, not prescanning.
        disabled.insert(macro_name.clone());
        let expanded = expand_macros_in_text(db, env, &sub.substituted_text, disabled);
        out.push_str(&expanded);
        disabled.remove(&macro_name);

        i = advance_to;
    }

    out
}

/// Prescan macro arguments before substitution.
///
/// See <https://gcc.gnu.org/onlinedocs/cpp/Argument-Prescan.html> for details.
fn prescan_arguments<'db>(
    db: &'db dyn BaseDb,
    env: &TrackedMapEnvMut<'db>,
    arguments: &[String],
    def: &MacroDefinition,
    disabled: &mut FxHashSet<String>,
) -> Vec<String> {
    let non_prescan_vararg_idx = def
        .variadic_param_idx()
        .filter(|i| def.dont_prescan_indices().contains(i));

    arguments
        .iter()
        .enumerate()
        .map(|(i, arg)| {
            if def.dont_prescan_indices().contains(&i)
                || non_prescan_vararg_idx.is_some_and(|k| i >= k)
            {
                arg.clone()
            } else {
                expand_macros_in_text(db, env, arg, disabled)
            }
        })
        .collect()
}

/// Resolves and substitutes a macro.
///
/// Returns `Ok(None)` if the macro doesn't exist and the macro reference is implicit.
pub(crate) fn substitute_macro_tok<'db>(
    db: &'db dyn BaseDb,
    env: &TrackedMapEnvMut<'db>,
    diag: &mut Diag<'_, '_>,
    macro_ctx: &MacroCtx,
) -> Result<Option<(MacroSubstitutionProvenance, String)>, ()> {
    let name = macro_ctx.name();

    let Some((def, def_span)) = env.get_macro(db, name).map(|(def, span)| (def, *span)) else {
        let is_explicit_macro = matches!(macro_ctx, MacroCtx::Explicit(_));
        return if is_explicit_macro {
            diag.emit(
                macro_ctx.text_range(),
                Cow::Owned(format!("Macro `{name}` is not defined")),
                Severity::Error,
            );
            Err(())
        } else {
            Ok(None)
        };
    };

    // Extract raw argument strings from the AST.
    let raw_arguments: Vec<String> = match macro_ctx {
        MacroCtx::Explicit(inv) => inv
            .arguments()
            .map(|arg| {
                arg.green
                    .child_tokens()
                    .map(|tok| tok.text.as_str())
                    .collect::<String>()
            })
            .collect(),
        MacroCtx::Implicit(_) => Vec::new(),
    };

    let mut disabled: FxHashSet<String> = FxHashSet::default();

    let arguments = prescan_arguments(db, env, &raw_arguments, def, &mut disabled);
    // TODO: map back prescanned arguments too (PrescanExpansion). currently maps to raw_arguments...

    match def.substitute(&arguments) {
        Ok(substituted_body) => Ok(Some((
            MacroSubstitutionProvenance {
                name: macro_ctx.name().to_owned(),
                source_mappings: substituted_body.source_mappings,
                substituted_text_len: substituted_body.substituted_text.len(),
                def: def.clone(),
                def_span,
                args: macro_ctx.arg_ranges(),
                invocation: macro_ctx.text_range(),
            },
            substituted_body.substituted_text,
        ))),
        Err(err) => {
            diag.emit(macro_ctx.text_range(), err.to_string(), Severity::Error);
            Err(())
        }
    }
}

#[cfg(test)]
#[expect(
    clippy::needless_raw_string_hashes,
    reason = "expect-test auto update adds r#"
)]
mod tests {
    use dt_tools_analyzer::macros::MacroDefinition;
    use dt_tools_parser::{
        TextRange,
        ast::{self, AstNodeOrToken},
        parser::Entrypoint,
    };
    use expect_test::{Expect, expect};

    use crate::{
        db::BaseDb,
        diag::{Diag, SourceMap},
        emit_parse_errors,
        lowering::{dt_node::resolve_name_or_macro, tests::fmt_diags},
        macros::env::TrackedMapEnvMut,
    };

    #[expect(clippy::needless_pass_by_value, reason = "ergonomics")]
    fn check(macros: &[&str], input: &str, expect: Expect) {
        let db = crate::db::BaseDatabase::default();
        let file = db
            .get_files()
            .add_virtual(&db, "/main.dts".into(), String::new());

        let mut env = TrackedMapEnvMut::default();

        for macro_src in macros.iter().copied() {
            let def = MacroDefinition::parse(macro_src).unwrap();

            let file = db.get_files().add_virtual(
                &db,
                format!("/macro_{}.h", def.name).into(),
                macro_src.to_owned(),
            );

            let span = TextRange::new(0, macro_src.len()).within_file(file);

            env.insert_macro(def, span);
        }

        let mut diags = Vec::new();
        let map = SourceMap::File(file);
        let mut diag = Diag::new(&mut diags, &map);

        // --- parse input ---
        let parse = Entrypoint::Name.parse(input);
        emit_parse_errors(&parse, &mut diag);

        let red_node = parse.red_node();
        let ast = ast::EntryName::cast_node(red_node.clone()).unwrap();

        let Ok(text) = resolve_name_or_macro(&db, &env, &mut diag, &ast) else {
            let mut out = "Failed to resolve\n".to_owned();
            fmt_diags(&db, &mut out, &diags);

            expect.assert_eq(&out);
            return;
        };

        let mut out = String::new();
        out.push_str("--- substituted ---\n");
        out.push_str(&text);
        out.push('\n');

        if !diags.is_empty() {
            out.push('\n');
            fmt_diags(&db, &mut out, &diags);
        }
        expect.assert_eq(&out);
    }

    #[test]
    fn no_macro() {
        check(
            &[],
            "foo",
            expect![[r#"
                --- substituted ---
                foo
            "#]],
        );
    }

    #[test]
    fn basic() {
        check(
            &["#define FOO bar"],
            "FOO",
            expect![[r#"
                --- substituted ---
                bar
            "#]],
        );
    }

    #[test]
    fn marvell_dt_example() {
        check(
            &[
                "#define CP11X_NAME cp0",
                // also known as CAT
                "#define PASTER(x, y) x ## y",
                // also known as XCAT
                "#define EVALUATOR(x, y) PASTER(x, y)",
                "#define CP11X_LABEL(name) EVALUATOR(CP11X_NAME, EVALUATOR(_, name))",
            ],
            "CP11X_LABEL(thermal_ic)",
            expect![[r#"
                --- substituted ---
                cp0_thermal_ic
            "#]],
        );
    }

    #[test]
    fn prescan_recursion() {
        // allow prescanning the inner MACRO in MACRO(MACRO(...))
        check(
            &["#define CAT(x, y) x ## y", "#define XCAT(x, y) CAT(x, y)"],
            "XCAT(foo, XCAT(bar, baz))",
            expect![[r#"
                --- substituted ---
                foobarbaz
            "#]],
        );
    }
}
