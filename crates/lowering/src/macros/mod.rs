use std::borrow::Cow;

use dt_tools_analyzer::macros::{SubstitutedBody, substitute_macro};
use dt_tools_diagnostic::{Severity, Span};
use dt_tools_parser::{
    TextRange,
    ast::{self, AstNode, AstToken},
};

use crate::{db::BaseDb, diag::Diag, file::File, macros::env::TrackedMapEnvMut};

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

/// Resolves and substitutes a macro.
///
/// Returns `Ok(None)` if the macro doesn't exist and the macro reference is implicit.
pub(crate) fn substitute_macro_tok<'db>(
    db: &'db dyn BaseDb,
    env: &TrackedMapEnvMut<'db>,
    diag: &mut Diag<'_, '_>,
    macro_ctx: &MacroCtx,
) -> Result<Option<(SubstitutedBody, Span<File>)>, ()> {
    let name = macro_ctx.name();

    let Some((def, macro_def)) = env.get_macro(db, name).map(|(def, span)| (def, *span)) else {
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
        Ok(substituted_body) => Ok(Some((substituted_body, macro_def))),
        Err(err) => {
            diag.emit(macro_ctx.text_range(), err.to_string(), Severity::Error);
            Err(())
        }
    }
}
