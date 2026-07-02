use std::borrow::Cow;

use dt_tools_analyzer::macros::{TextRangeMap, substitute_macro_ast};
use dt_tools_diagnostic::{Diagnostic, DiagnosticCollector, Severity, Span};
use dt_tools_parser::{
    TextRange,
    ast::{self, AstNode, AstToken},
};

use crate::{db::BaseDb, file::File, macros::env::TrackedMapEnvMut};

pub mod env;

pub enum MacroCtx<'a> {
    Explicit(&'a ast::MacroInvocation),
    Implicit(&'a ast::Name),
}

/// Resolves and substitutes a macro.
///
/// Returns `None` if the macro doesn't exist or there is some other error.
pub fn substitute_macro_tok<'db>(
    db: &'db dyn BaseDb,
    env: &mut TrackedMapEnvMut<'db>,
    diag: &impl DiagnosticCollector<File>,
    spanner: &mut impl FnMut(TextRange) -> Span<File>,
    macro_ctx: &MacroCtx,
) -> Option<(Span<File>, Vec<TextRangeMap>, String)> {
    let (name, span) = match &macro_ctx {
        MacroCtx::Explicit(inv) => (
            inv.green_ident()?.text.as_str(),
            spanner(inv.syntax().text_range()),
        ),
        MacroCtx::Implicit(tok) => (
            tok.syntax().green.text.as_str(),
            spanner(tok.syntax().text_range()),
        ),
    };

    let Some(def) = env.get_macro_def(db, name) else {
        let is_explicit_macro = matches!(macro_ctx, MacroCtx::Explicit(_));
        if is_explicit_macro {
            diag.emit(Diagnostic::new(
                span,
                Cow::Owned(format!("Macro `{name}` is not defined")),
                Severity::Error,
            ));
        }

        return None;
    };

    match substitute_macro_ast(
        match macro_ctx {
            MacroCtx::Explicit(inv) => Some(inv),
            _ => None,
        },
        def,
    ) {
        Ok((trmaps, expanded)) => Some((span, trmaps, expanded)),
        Err(err) => {
            diag.emit(Diagnostic::new(span, Cow::Owned(err), Severity::Error));
            None
        }
    }
}
