use std::borrow::Cow;

use dt_tools_analyzer::macros::{SubstitutedBody, substitute_macro};
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
/// Returns `Ok(None)` if the macro doesn't exist and the macro reference is implicit.
pub(crate) fn substitute_macro_tok<'db>(
    db: &'db dyn BaseDb,
    env: &mut TrackedMapEnvMut<'db>,
    diag: &impl DiagnosticCollector<File>,
    spanner: &mut impl FnMut(TextRange) -> Span<File>,
    macro_ctx: &MacroCtx,
) -> Result<Option<(Span<File>, SubstitutedBody)>, ()> {
    let (name, span) = match &macro_ctx {
        MacroCtx::Explicit(inv) => (
            inv.green_ident().ok_or(())?.text.as_str(),
            spanner(inv.syntax().text_range()),
        ),
        MacroCtx::Implicit(tok) => (
            tok.syntax().green.text.as_str(),
            spanner(tok.syntax().text_range()),
        ),
    };

    let Some(def) = env.get_macro_def(db, name) else {
        let is_explicit_macro = matches!(macro_ctx, MacroCtx::Explicit(_));
        return if is_explicit_macro {
            diag.emit(Diagnostic::new(
                span,
                Cow::Owned(format!("Macro `{name}` is not defined")),
                Severity::Error,
            ));
            Err(())
        } else {
            Ok(None)
        };
    };

    match substitute_macro(
        match macro_ctx {
            MacroCtx::Explicit(inv) => Some(inv),
            _ => None,
        },
        def,
    ) {
        Ok(substituted_body) => Ok(Some((span, substituted_body))),
        Err(err) => {
            diag.emit(Diagnostic::new(span, Cow::Owned(err), Severity::Error));
            Err(())
        }
    }
}
