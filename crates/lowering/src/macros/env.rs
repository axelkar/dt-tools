// TODO: not MacroEnv anymore; move file.
use dt_tools_analyzer::macros::MacroDefinition;
use dt_tools_diagnostic::Span;
use rustc_hash::FxHashMap;

use crate::{db::BaseDb, file::File};

/// Interned string for a key in [`TrackedMapEnv`].
#[salsa::interned]
pub struct InternedKey {
    #[returns(deref)]
    pub text: String,
}

#[salsa::tracked]
pub struct TrackedMapEnv<'db> {
    parent: Option<TrackedMapEnv<'db>>,

    /// Map of defined macros. Inner `Option` so it can be overriden over `parent` to `None`.
    #[tracked]
    #[returns(ref)]
    own_macro_map: FxHashMap<String, Option<(MacroDefinition, Span<File>)>>,

    /// Map of defined labels to paths. Inner `Option` so it can be overriden over `parent` to `None`.
    #[tracked]
    #[returns(ref)]
    own_label_map: FxHashMap<String, Option<(String, Span<File>)>>,
}

#[salsa::tracked]
impl<'db> TrackedMapEnv<'db> {
    /// Salsa ingredients like [`TrackedMapEnv`] changing won't directly cause a tracked function to
    /// get recomputed but the results of queries like [`TrackedMapEnv::own_map()`] or
    /// [`TrackedMapEnv::get()`] will.
    ///
    /// We can use backdating and a wrapping tracked function (this `get`) to our advantage. The
    /// depending tracked function only gets recomputed if the output of `get` changes.
    ///
    /// We have to clone the output value because it has to be stored in the Salsa database without
    /// referring to [`TrackedMapEnv::own_map`].
    #[salsa::tracked(returns(as_ref), lru = 512)]
    pub fn get_macro(
        self,
        db: &'db dyn BaseDb,
        name: InternedKey<'db>,
    ) -> Option<(MacroDefinition, Span<File>)> {
        let span = profiling::tracy_client::span!("TrackedMapEnv::get_macro");
        span.emit_text(name.text(db));

        tracing::trace!(
            name = name.text(db),
            self = ?self.0,
            self.parent = ?self.parent(db).map(|parent| parent.0),
            "get_macro",
        );

        self.own_macro_map(db)
            .get(name.text(db))
            .map(Option::as_ref)
            .or_else(|| Some(self.parent(db)?.get_macro(db, name)))
            .flatten() // flattening at the end makes sure own_map can override None
            .cloned()
    }

    /// See [`Self::get_macro`] for details about Salsa integration.
    #[salsa::tracked(returns(as_ref), lru = 512)]
    pub fn get_macro_def(
        self,
        db: &'db dyn BaseDb,
        name: InternedKey<'db>,
    ) -> Option<MacroDefinition> {
        let span = profiling::tracy_client::span!("TrackedMapEnv::get_macro_def");
        span.emit_text(name.text(db));

        tracing::trace!(
            name = name.text(db),
            self = ?self.0,
            self.parent = ?self.parent(db).map(|parent| parent.0),
            "get_macro",
        );

        self.own_macro_map(db)
            .get(name.text(db))
            .map(|opt| opt.as_ref().map(|(def, _span)| def))
            .or_else(|| Some(self.parent(db)?.get_macro_def(db, name)))
            .flatten() // flattening at the end makes sure own_map can override None
            .cloned()
    }

    /// See [`Self::get_macro`] for details about Salsa integration.
    #[salsa::tracked(returns(as_ref), lru = 512)]
    pub fn get_label(
        self,
        db: &'db dyn BaseDb,
        name: InternedKey<'db>,
    ) -> Option<(String, Span<File>)> {
        let span = profiling::tracy_client::span!("TrackedMapEnv::get_label");
        span.emit_text(name.text(db));

        tracing::trace!(
            name = name.text(db),
            self = ?self.0,
            self.parent = ?self.parent(db).map(|parent| parent.0),
            "get_label",
        );

        self.own_label_map(db)
            .get(name.text(db))
            .map(Option::as_ref)
            .or_else(|| Some(self.parent(db)?.get_label(db, name)))
            .flatten() // flattening at the end makes sure own_label_map can override None
            .cloned()
    }

    /// See [`Self::get_macro`] for details about Salsa integration.
    #[salsa::tracked(returns(as_ref), lru = 512)]
    pub fn get_label_path(self, db: &'db dyn BaseDb, name: InternedKey<'db>) -> Option<String> {
        let span = profiling::tracy_client::span!("TrackedMapEnv::get_label_path");
        span.emit_text(name.text(db));

        tracing::trace!(
            name = name.text(db),
            self = ?self.0,
            self.parent = ?self.parent(db).map(|parent| parent.0),
            "get_label_path",
        );

        self.own_label_map(db)
            .get(name.text(db))
            .map(|opt| opt.as_ref().map(|(path, _span)| path))
            .or_else(|| Some(self.parent(db)?.get_label_path(db, name)))
            .flatten() // flattening at the end makes sure own_label_map can override None
            .cloned()
    }

    fn clone_to_maps(
        self,
        db: &'db dyn BaseDb,
        macro_map: &mut FxHashMap<String, Option<(MacroDefinition, Span<File>)>>,
        label_map: &mut FxHashMap<String, Option<(String, Span<File>)>>,
    ) {
        if let Some(parent) = self.parent(db) {
            parent.clone_to_maps(db, macro_map, label_map);
        }
        for (k, v) in self.own_macro_map(db) {
            macro_map.insert(k.clone(), v.clone());
        }
        for (k, v) in self.own_label_map(db) {
            label_map.insert(k.clone(), v.clone());
        }
    }

    /// Forks a mutable environment from this immutable environment.
    #[must_use]
    pub fn to_mut(self) -> TrackedMapEnvMut<'db> {
        TrackedMapEnvMut::from_parent(Some(self))
    }
}

#[derive(Clone, Default)]
pub struct TrackedMapEnvMut<'db> {
    pub parent: Option<TrackedMapEnv<'db>>,
    /// Map of defined macros. Inner `Option` so it can be overriden over `parent` to `None`.
    pub own_macro_map: FxHashMap<String, Option<(MacroDefinition, Span<File>)>>,
    /// Map of defined labels to paths. Inner `Option` so it can be overriden over `parent` to `None`.
    pub own_label_map: FxHashMap<String, Option<(String, Span<File>)>>,
}

impl<'db> TrackedMapEnvMut<'db> {
    /// Forks a mutable environment from an optional immutable environment.
    #[must_use]
    pub fn from_parent(parent: Option<TrackedMapEnv<'db>>) -> Self {
        Self {
            parent,
            own_macro_map: FxHashMap::default(),
            own_label_map: FxHashMap::default(),
        }
    }

    // Gets a macro by name
    pub fn get_macro(
        &self,
        db: &'db dyn BaseDb,
        name: &str,
    ) -> Option<&(MacroDefinition, Span<File>)> {
        let span = profiling::tracy_client::span!("TrackedMapEnvMut::get_macro");
        span.emit_text(name);

        self.own_macro_map
            .get(name)
            .map(Option::as_ref)
            .or_else(|| {
                Some(
                    self.parent
                        .as_ref()?
                        .get_macro(db, InternedKey::new(db, name)),
                )
            })
            .flatten() // flattening at the end makes sure own_map can override None
    }

    // Gets a macro definition by name
    pub fn get_macro_def(&self, db: &'db dyn BaseDb, name: &str) -> Option<&MacroDefinition> {
        let span = profiling::tracy_client::span!("TrackedMapEnvMut::get_macro_def");
        span.emit_text(name);

        self.own_macro_map
            .get(name)
            .map(|opt| opt.as_ref().map(|(def, _span)| def))
            .or_else(|| {
                Some(
                    self.parent
                        .as_ref()?
                        .get_macro_def(db, InternedKey::new(db, name)),
                )
            })
            .flatten() // flattening at the end makes sure own_map can override None
    }

    // Gets a label by name
    pub fn get_label(&self, db: &'db dyn BaseDb, name: &str) -> Option<&(String, Span<File>)> {
        let span = profiling::tracy_client::span!("TrackedMapEnvMut::get_label");
        span.emit_text(name);

        self.own_label_map
            .get(name)
            .map(Option::as_ref)
            .or_else(|| {
                Some(
                    self.parent
                        .as_ref()?
                        .get_label(db, InternedKey::new(db, name)),
                )
            })
            .flatten() // flattening at the end makes sure own_map can override None
    }

    // Gets a label's path by name
    pub fn get_label_path(&self, db: &'db dyn BaseDb, name: &str) -> Option<&String> {
        let span = profiling::tracy_client::span!("TrackedMapEnvMut::get_label_path");
        span.emit_text(name);

        self.own_label_map
            .get(name)
            .map(|opt| opt.as_ref().map(|(path, _span)| path))
            .or_else(|| {
                Some(
                    self.parent
                        .as_ref()?
                        .get_label_path(db, InternedKey::new(db, name)),
                )
            })
            .flatten() // flattening at the end makes sure own_map can override None
    }

    pub fn insert_macro(&mut self, def: MacroDefinition, span: Span<File>) {
        // Helper function because this clones def.name
        self.own_macro_map
            .insert(def.name.clone(), Some((def, span)));
    }

    /// Flatten ancestors to `self`, removing any indirection.
    pub fn flatten_ancestors(&mut self, db: &'db dyn BaseDb) {
        let _span = profiling::tracy_client::span!("TrackedMapEnvMut::flatten_ancestors");

        let mut macro_map = FxHashMap::default();
        let mut label_map = FxHashMap::default();

        if let Some(parent) = std::mem::take(&mut self.parent) {
            parent.clone_to_maps(db, &mut macro_map, &mut label_map);
        }

        for (k, v) in std::mem::take(&mut self.own_macro_map) {
            macro_map.insert(k.clone(), v.clone());
        }
        for (k, v) in std::mem::take(&mut self.own_label_map) {
            label_map.insert(k.clone(), v.clone());
        }
        self.own_macro_map = macro_map;
        self.own_label_map = label_map;
    }

    pub fn into_immut(self, db: &'db dyn BaseDb) -> TrackedMapEnv<'db> {
        if self.own_macro_map.is_empty()
            && self.own_label_map.is_empty()
            && let Some(parent) = self.parent
        {
            parent
        } else {
            TrackedMapEnv::new(db, self.parent, self.own_macro_map, self.own_label_map)
        }
    }

    pub fn commit_to_immut(&mut self, db: &'db dyn BaseDb) -> TrackedMapEnv<'db> {
        if self.own_macro_map.is_empty()
            && self.own_label_map.is_empty()
            && let Some(parent) = self.parent
        {
            parent
        } else {
            let new_env = std::mem::take(self).into_immut(db);
            self.parent = Some(new_env);
            new_env
        }
    }
}
