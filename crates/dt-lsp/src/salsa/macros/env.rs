use dt_analyzer::macros::MacroDefinition;
use rustc_hash::FxHashMap;

use crate::salsa::db::BaseDb;

/// Interned string for a macro's name, for use in [`MacroEnv`].
#[salsa::interned]
pub struct MacroName {
    #[returns(deref)]
    pub text: String,
}

/// Map of defined macros.
#[salsa::tracked]
pub struct MacroEnv<'db> {
    parent: Option<MacroEnv<'db>>,
    /// Inner `Option` so it can be overriden over `parent` to `None`.
    #[tracked]
    #[returns(ref)]
    own_map: FxHashMap<String, Option<MacroDefinition>>,
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
    pub fn get_macro(self, db: &'db dyn BaseDb, name: MacroName<'db>) -> Option<MacroDefinition> {
        self.own_map(db)
            .get(name.text(db))
            .map(Option::as_ref)
            .or_else(|| Some(self.parent(db)?.get_macro(db, name)))
            .flatten() // flattening at the end makes sure own_map can override None
            .cloned()
    }
}

#[derive(Clone)]
pub struct MacroEnvMut<'db> {
    pub parent: Option<MacroEnv<'db>>,
    /// Inner `Option` so it can be overriden over `parent` to `None`.
    pub own_map: FxHashMap<String, Option<MacroDefinition>>,
}

impl<'db> MacroEnvMut<'db> {
    pub fn get_macro(&self, db: &'db dyn BaseDb, name: &str) -> Option<&MacroDefinition> {
        self.own_map
            .get(name)
            .map(Option::as_ref)
            .or_else(|| {
                Some(
                    self.parent
                        .as_ref()?
                        .get_macro(db, MacroName::new(db, name)),
                )
            })
            .flatten() // flattening at the end makes sure own_map can override None
    }
    pub fn into_salsa_tracked_struct(self, db: &'db dyn BaseDb) -> MacroEnv<'db> {
        if let (true, Some(parent)) = (self.own_map.is_empty(), self.parent) {
            parent
        } else {
            MacroEnv::new(db, self.parent, self.own_map)
        }
    }
    pub fn to_salsa_tracked_struct_mut(&mut self, db: &'db dyn BaseDb) -> MacroEnv<'db> {
        if let (true, Some(parent)) = (self.own_map.is_empty(), self.parent) {
            parent
        } else {
            let own_map = std::mem::take(&mut self.own_map);
            let new_env = MacroEnv::new(db, self.parent, own_map);
            self.parent = Some(new_env);
            new_env
        }
    }
}

// TODO: do we need something similar for DTS references and extensions?
