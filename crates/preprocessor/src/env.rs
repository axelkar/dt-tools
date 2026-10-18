//! Macro environment.

use rustc_hash::FxHashMap;

use crate::token::PpToken;

/// The kind of a macro.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum MacroKind {
    /// An object-like macro.
    Object,
    /// A function-like macro.
    Function {
        /// Parameter names in order.
        params: Vec<String>,
        /// Whether the macro takes `...`.
        variadic: bool,
    },
}

/// A macro definition.
#[derive(Debug, Clone)]
pub(crate) struct MacroDef {
    /// The macro kind.
    pub(crate) kind: MacroKind,
    /// Replacement tokens.
    pub(crate) replacement: Vec<PpToken>,
}

/// Macros during one preprocessing run.
#[derive(Debug, Default)]
pub(crate) struct MacroEnv {
    macros: FxHashMap<String, MacroDef>,
}

impl MacroEnv {
    /// Defines or redefines `name`.
    pub(crate) fn define(&mut self, name: String, def: MacroDef) {
        self.macros.insert(name, def);
    }

    /// Removes `name` if it is defined.
    pub(crate) fn undef(&mut self, name: &str) {
        self.macros.remove(name);
    }

    /// Returns the definition of `name`.
    #[must_use]
    pub(crate) fn get(&self, name: &str) -> Option<&MacroDef> {
        self.macros.get(name)
    }
}
