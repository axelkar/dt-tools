use dt_analyzer::macros::MacroDefinition;
use rustc_hash::FxHashMap;

use crate::salsa::db::BaseDb;

/// Interned string for a macro's name, for use in [`MacroEnv`].
#[salsa::interned]
pub struct MacroName {
    #[returns(deref)]
    pub text: String,
}

#[salsa::tracked]
struct MacroEnv<'db> {
    // TODO: multiple parents?
    parent: Option<MacroEnv<'db>>,
    #[tracked]
    #[returns(ref)]
    own_map: FxHashMap<String, MacroDefinition>,
}

#[salsa::tracked]
impl<'db> MacroEnv<'db> {
    /// Salsa ingredients like [`MacroEnv`] changing won't cause a tracked function to get
    /// recomputed but the results of queries like [`MacroEnv::own_map()`] or [`MacroEnv::get()`] will.
    ///
    /// We can use backdating and a wrapping tracked function (this `get`) to our advantage. The
    /// depending tracked function only gets recomputed if the output of `get` changes.
    ///
    /// We have to clone the output value because it has to be stored in the Salsa database without
    /// referring to [`MacroEnv::own_map`].
    #[salsa::tracked(returns(as_ref))]
    fn get(self, db: &'db dyn BaseDb, name: MacroName<'db>) -> Option<MacroDefinition> {
        self.own_map(db)
            .get(name.text(db))
            .or_else(|| self.parent(db)?.get(db, name))
            .cloned()
    }
}

// TODO: do we need something similar for DTS references and extensions?
