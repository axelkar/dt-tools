use std::borrow::Cow;

use dt_tools_analyzer::macros::{SubstitutedBody, substitute_macro};
use dt_tools_diagnostic::Severity;
use dt_tools_parser::{
    TextRange,
    ast::{self, AstNode, AstToken},
};

use crate::{db::BaseDb, diag::Diag, macros::env::TrackedMapEnvMut};

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
}

/// Resolves and substitutes a macro.
///
/// Returns `Ok(None)` if the macro doesn't exist and the macro reference is implicit.
pub(crate) fn substitute_macro_tok<'db>(
    db: &'db dyn BaseDb,
    env: &TrackedMapEnvMut<'db>,
    diag: &mut Diag<'_, '_>,
    macro_ctx: &MacroCtx,
) -> Result<Option<SubstitutedBody>, ()> {
    let name = macro_ctx.name();

    let Some(def) = env.get_macro_def(db, name) else {
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

    match substitute_macro(
        match macro_ctx {
            MacroCtx::Explicit(inv) => Some(inv),
            _ => None,
        },
        def,
    ) {
        Ok(substituted_body) => Ok(Some(substituted_body)),
        Err(err) => {
            diag.emit(macro_ctx.text_range(), Cow::Owned(err), Severity::Error);
            Err(())
        }
    }
}
