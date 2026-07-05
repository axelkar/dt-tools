use std::{collections::BTreeSet, fmt};

use dt_tools_parser::TextRange;

use crate::file::File;

/// Mid-level intermediate representation of devicetrees.
///
/// Uses a flat list of definitions keyed by full path. Tree assembly and merging happens at the end.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Mir {
    // TODO: use BTreeMap<String, MirDefinition> for performance.. except them we lose history
    // information, which is important in LSP hover
    pub definitions: Vec<MirDefinition>,
    /// Extensions that couldn't be resolved (overlay mode only).
    pub unresolved_extensions: Vec<UnresolvedExtension>,
}

impl Mir {
    /// Merge another `Mir` into this one (e.g., from an included file).
    pub fn merge(&mut self, other: &Self) {
        let _span = profiling::tracy_client::span!("lsp::salsa::Mir::merge");

        self.definitions.extend(other.definitions.iter().cloned());
        self.unresolved_extensions
            .extend(other.unresolved_extensions.iter().cloned());
    }

    /// Format MIR for display.
    #[must_use]
    pub fn display(&self, db: &dyn crate::db::BaseDb) -> String {
        use std::fmt::Write;

        let mut defs: Vec<_> = self.definitions.iter().collect();
        defs.sort_by_key(|d| &d.path);

        let mut out = String::new();
        for def in &defs {
            let kind = match &def.value {
                MirDefinitionValue::Node(n) => {
                    let labels = if n.labels.is_empty() {
                        String::new()
                    } else {
                        format!(" labels=[{}]", n.labels.join(", "))
                    };
                    format!("node{labels}")
                }
                MirDefinitionValue::Property(p) => {
                    let vals: Vec<String> = p.values.iter().map(|v| format!("{v:?}")).collect();
                    format!("property = {}", vals.join(", "))
                }
                MirDefinitionValue::DeletedNode => "delete-node".to_owned(),
                MirDefinitionValue::DeletedProperty => "delete-property".to_owned(),
            };
            let _ = writeln!(
                out,
                "{:6} {} {} {}",
                kind,
                def.path,
                def.provenance.file.path(db),
                def.provenance.text_range
            );
        }
        if !self.unresolved_extensions.is_empty() {
            out.push_str("--- unresolved ---\n");
            for ext in &self.unresolved_extensions {
                let _ = writeln!(
                    out,
                    "  label={} ({} definitions)",
                    ext.label,
                    ext.body.len()
                );
            }
        }
        out
    }

    /// Returns an iterator over live/effective nodes and properties that are either `path_base` or children of it.
    ///
    /// Pass `""` to iterate all live definitions in the tree.
    pub fn iter_live_defs_under(&self, path_base: &str) -> impl Iterator<Item = &MirDefinition> {
        fn strip_base<'a>(this: &'a str, base: &str) -> Option<&'a str> {
            if this == base {
                Some("")
            } else {
                this.strip_prefix(base)
                    .filter(|stripped| stripped.starts_with('/'))
            }
        }
        fn btreeset_find_base_of<'a>(set: &BTreeSet<&'a str>, needle: &'a str) -> Option<&'a str> {
            set.range(..=needle)
                .rfind(|possible_base| strip_base(needle, possible_base).is_some())
                .map(|v| &**v)
        }

        let mut deleted = BTreeSet::<&str>::new();

        self.definitions
            .iter()
            .rev()
            .take_while(|def| {
                // break immediately if a parent of path_base or path_base itself is deleted
                !(matches!(def.value, MirDefinitionValue::DeletedNode)
                    && strip_base(path_base, &def.path).is_some())
            })
            .filter_map(move |def| {
                // filter to children only
                let stripped = strip_base(&def.path, path_base)?;

                match def.value {
                    MirDefinitionValue::Node(_) | MirDefinitionValue::Property(_)
                        if btreeset_find_base_of(&deleted, stripped).is_none() =>
                    {
                        if let MirDefinitionValue::Property(_) = def.value {
                            // Overwritten
                            deleted.insert(stripped);
                        }

                        Some(def)
                    }
                    MirDefinitionValue::DeletedNode | MirDefinitionValue::DeletedProperty => {
                        deleted.insert(stripped);
                        None
                    }
                    _ => None,
                }
            })
    }
}

/// One source location that contributed to a definition.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MirProvenance {
    pub file: File,
    pub text_range: TextRange,
}

/// Flat MIR definition with a path
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MirDefinition {
    /// Absolute path, containing the leading slash. E.g. `"/soc"`, `"/soc/uart@ff000000"`, `"/soc/uart@ff000000/compatible"`.
    pub path: String,
    pub value: MirDefinitionValue,
    pub provenance: MirProvenance,
}

/// What kind of definition this is.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MirDefinitionValue {
    /// Node definition. Labels are stored here; children are separate
    /// `MirDefinition`s with longer paths.
    Node(MirNodeData),
    /// Property definition with resolved values.
    Property(MirPropertyData),
    /// `/delete-node/` removes the node at this path.
    DeletedNode,
    /// `/delete-property/` removes the property at this path.
    DeletedProperty,
}

/// Metadata for a node definition.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MirNodeData {
    /// Label names defined on this node.
    pub labels: Vec<String>,
}

/// Resolved property values.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MirPropertyData {
    pub values: Vec<MirValue>,
}

/// Resolved property value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MirValue {
    /// String value (`"hello"`).
    String(String),
    /// Cell list (`<1 2 3>`).
    CellList(Vec<MirCell>),
    /// Bytestring (`[ab cd ef]`).
    Bytestring(Vec<u8>),
    /// Phandle reference (`&label` or `&{/path}`).
    Phandle(MirPhandleTarget),
}
impl fmt::Display for MirValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MirValue::String(val) => fmt::Debug::fmt(val, f),
            MirValue::CellList(val) => {
                f.write_str("<")?;
                for (i, cell) in val.iter().enumerate() {
                    if i != 0 {
                        f.write_str(" ")?;
                    }
                    cell.fmt(f)?;
                }
                f.write_str(">")
            }
            MirValue::Bytestring(val) => todo!(),
            MirValue::Phandle(val) => val.fmt(f),
        }
    }
}

/// Single cell inside a cell list.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MirCell {
    /// Numeric value.
    U32(u32),
    /// Phandle reference.
    Phandle(MirPhandleTarget),
}
impl fmt::Display for MirCell {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MirCell::U32(val) => val.fmt(f),
            MirCell::Phandle(val) => val.fmt(f),
        }
    }
}

/// Symbolic phandle target reference. MIR does not resolve these to numeric phandle values.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MirPhandleTarget {
    /// Label reference like `&UART_1`.
    Label(String),
    /// Absolute path reference like `&{/soc/uart}`.
    Path(String),
}
impl fmt::Display for MirPhandleTarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Label(s) => write!(f, "&{s}"),
            Self::Path(s) => write!(f, "&{{{s}}}"),
        }
    }
}

/// Extension node (`&label { ... }`) that couldn't be resolved because the label hasn't been
/// defined yet. Only produced in overlay mode (`/plugin/;`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnresolvedExtension {
    /// The label that the extension targets.
    pub label: String,
    /// Definitions from the extension body (paths relative to target).
    pub body: Vec<MirDefinition>,
    pub provenance: MirProvenance,
}

#[cfg(test)]
#[expect(
    clippy::needless_raw_string_hashes,
    reason = "expect-test auto update adds r#"
)]
mod tests {
    use expect_test::expect;

    use crate::{db::BaseDb, includes::IncludeDirs, lowering::lower_root_file};

    use super::*;

    #[test]
    fn iter_live_defs_under() {
        let contents = r"/dts-v1/;

/ {
    qux {
        quux {
            foo {
                prop1 = <1>;
            };
        };
    };
};
/ {
    /delete-node/ qux;
};
/ {
    qux {
        quux {
            bar {
                prop2 = <2>;
            };
        };
    };
};
";
        let path_base = "/qux";

        let db = crate::db::BaseDatabase::default();
        IncludeDirs::new(&db, vec![]);

        let root_file = db
            .get_files()
            .add_virtual(&db, "/main.dts".into(), contents.to_owned());

        let result = lower_root_file(&db, root_file).expect("Should be a readable file");
        let mir = result.mir(&db);

        let new_mir = Mir {
            definitions: Mir::iter_live_defs_under(mir, path_base).cloned().collect(),
            unresolved_extensions: Vec::new(),
        };

        expect![[r#"
            node   /qux /main.dts 161..261
            node   /qux/quux /main.dts 175..254
            node   /qux/quux/bar /main.dts 194..243
            property = CellList([U32(2)]) /qux/quux/bar/prop2 /main.dts 216..228
        "#]]
        .assert_eq(&new_mir.display(&db));
    }
}
