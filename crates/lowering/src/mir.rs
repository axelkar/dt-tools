use std::{borrow::Cow, collections::BTreeSet, fmt};

use dt_tools_diagnostic::Span;

use crate::file::{DisplaySpanLineColumn, File};

fn strip_base<'a>(this: &'a str, base: &str) -> Option<&'a str> {
    if this == base {
        Some("")
    } else if base == "/" {
        this.starts_with('/').then_some(this)
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

        let mut out = String::new();
        for def in &self.definitions {
            let kind = match &def.value {
                MirDefinitionValue::Node(_) => "node",
                MirDefinitionValue::Property(_) => "property",
                MirDefinitionValue::DeletedNode => "delete-node",
                MirDefinitionValue::DeletedProperty => "delete-property",
                MirDefinitionValue::V1Directive => "dts-v1",
                MirDefinitionValue::PluginDirective => "plugin",
            };

            let after_path = match &def.value {
                MirDefinitionValue::Node(data) => {
                    let labels = if data.labels.is_empty() {
                        String::new()
                    } else {
                        format!(" labels=[{}]", data.labels.join(", "))
                    };

                    let omit_if_no_ref = if data.omit_if_no_ref {
                        " [omit-if-no-ref]"
                    } else {
                        ""
                    };

                    Cow::Owned(format!("{omit_if_no_ref}{labels}"))
                }
                MirDefinitionValue::Property(data) => {
                    let values: Vec<String> = data.values.iter().map(ToString::to_string).collect();

                    if values.is_empty() {
                        ";".into()
                    } else {
                        Cow::Owned(format!(" = {};", values.join(", ")))
                    }
                }
                _ => "".into(),
            };

            let path_part = if def.path.is_empty() {
                String::new()
            } else {
                format!(" {}", def.path)
            };

            let _ = writeln!(
                out,
                "{}{}{} {} {}",
                kind,
                path_part,
                after_path,
                def.provenance.span.file.path(db),
                DisplaySpanLineColumn(&def.provenance.span, db)
            );
        }
        if !self.unresolved_extensions.is_empty() {
            out.push_str("--- unresolved extensions ---\n");
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
    ///
    /// Note that the order is reversed.
    pub fn iter_live_defs_under(&self, path_base: &str) -> impl Iterator<Item = &MirDefinition> {
        let mut deleted = BTreeSet::<&str>::new();

        self.definitions
            .iter()
            .rev()
            .take_while(|def| {
                // break immediately if an ancestor of path_base or path_base itself is deleted
                !(matches!(def.value, MirDefinitionValue::DeletedNode)
                    && strip_base(path_base, &def.path).is_some())
            })
            .filter_map(move |def| {
                // filter to children only
                let stripped = strip_base(&def.path, path_base)?;

                match def.value {
                    MirDefinitionValue::Node(_)
                    | MirDefinitionValue::Property(_)
                    | MirDefinitionValue::V1Directive
                    | MirDefinitionValue::PluginDirective => {
                        if btreeset_find_base_of(&deleted, stripped).is_none() {
                            if let MirDefinitionValue::Property(_) = def.value {
                                // Overwritten
                                deleted.insert(stripped);
                            }

                            Some(def)
                        } else {
                            None
                        }
                    }
                    MirDefinitionValue::DeletedNode | MirDefinitionValue::DeletedProperty => {
                        deleted.insert(stripped);
                        None
                    }
                }
            })
    }

    /// Returns the history of a path.
    ///
    /// May include [`MirDefinition`]s at this path and ancestor paths.
    pub fn history(&self, path: &str) -> impl Iterator<Item = &MirDefinition> {
        let mut currently_defined = false;

        self.definitions.iter().filter_map(move |def| {
            // filter to this or ancestors only
            let stripped = strip_base(path, &def.path)?;
            let is_self = stripped.is_empty();

            if is_self {
                match def.value {
                    MirDefinitionValue::Node(_) | MirDefinitionValue::Property(_) => {
                        currently_defined = true;
                    }
                    MirDefinitionValue::DeletedNode | MirDefinitionValue::DeletedProperty => {
                        currently_defined = false;
                    }
                    _ => {}
                }

                Some(def)
            } else if currently_defined && let MirDefinitionValue::DeletedNode = def.value {
                // Deleted as a child
                currently_defined = false;
                Some(def)
            } else {
                None
            }
        })
    }

    /// Returns true if `path` currently exists as a live node.
    #[must_use]
    pub fn contains_node(&self, path: &str) -> bool {
        self.iter_live_defs_under(path)
            .any(|def| def.path == path && matches!(def.value, MirDefinitionValue::Node(_)))
    }

    /// Returns true if this switches to overlay mode.
    #[must_use]
    pub fn sets_overlay(&self) -> bool {
        self.definitions
            .iter()
            .any(|def| matches!(def.value, MirDefinitionValue::PluginDirective))
    }
}

/// One source location that contributed to a definition.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MirProvenance {
    pub span: Span<File>,
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

    /// `/dts-v1/;` begins a DTS document.
    V1Directive,
    /// `/plugin/;` specifies an "overlay" document.
    ///
    /// It must come immediately after `/dts-v1/;`.
    PluginDirective,
}

/// Metadata for a node definition.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MirNodeData {
    /// Label names defined on this node.
    pub labels: Vec<String>,
    /// Whether this has `/omit-if-no-ref/`.
    pub omit_if_no_ref: bool,
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
    CellList(MirCellList),
    /// Bytestring (`[ab cd ef]`).
    Bytestring(Vec<u8>),
    /// Phandle reference (`&label` or `&{/path}`).
    Phandle(MirPhandleTarget),
}
/// Formats the MIR property value into a working DTS property value fragment.
impl fmt::Display for MirValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MirValue::String(val) => fmt::Debug::fmt(val, f),
            MirValue::CellList(val) => val.fmt(f),
            MirValue::Bytestring(val) => {
                f.write_str("[")?;
                for byte in val {
                    write!(f, "{byte:02x}")?;
                }
                f.write_str("]")
            }
            MirValue::Phandle(val) => val.fmt(f),
        }
    }
}

/// Cell list.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MirCellList {
    Bits8(Vec<u8>),
    Bits16(Vec<u16>),
    /// Note: may contain phandles
    Bits32(Vec<MirCell32>),
    Bits64(Vec<u64>),
}
impl MirCellList {
    /// Returns the bit size.
    #[must_use]
    pub fn bits(&self) -> u8 {
        match self {
            Self::Bits8(_) => 8,
            Self::Bits16(_) => 16,
            Self::Bits32(_) => 32,
            Self::Bits64(_) => 64,
        }
    }
}
/// Formats the MIR cell list into a working DTS property value fragment.
impl fmt::Display for MirCellList {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let bits = self.bits();
        if bits != 32 {
            write!(f, "/bits/ {bits} ")?;
        }

        f.write_str("<")?;
        match self {
            Self::Bits8(val) => {
                for (i, cell) in val.iter().enumerate() {
                    if i != 0 {
                        f.write_str(" ")?;
                    }
                    cell.fmt(f)?;
                }
            }
            Self::Bits16(val) => {
                for (i, cell) in val.iter().enumerate() {
                    if i != 0 {
                        f.write_str(" ")?;
                    }
                    cell.fmt(f)?;
                }
            }
            Self::Bits32(val) => {
                for (i, cell) in val.iter().enumerate() {
                    if i != 0 {
                        f.write_str(" ")?;
                    }
                    cell.fmt(f)?;
                }
            }
            Self::Bits64(val) => {
                for (i, cell) in val.iter().enumerate() {
                    if i != 0 {
                        f.write_str(" ")?;
                    }
                    cell.fmt(f)?;
                }
            }
        }
        f.write_str(">")
    }
}

/// Cell inside a [`MirCellList`] with 32-bit cells.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MirCell32 {
    /// Numeric value.
    Number(u32),
    /// Phandle reference.
    Phandle(MirPhandleTarget),
}
impl fmt::Display for MirCell32 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Number(val) => val.fmt(f),
            Self::Phandle(val) => val.fmt(f),
        }
    }
}
impl From<u32> for MirCell32 {
    fn from(value: u32) -> Self {
        Self::Number(value)
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
    use expect_test::{Expect, expect};

    use super::*;
    use crate::{db::BaseDb, includes::IncludeDirs, lowering::lower_root_file};

    #[expect(clippy::needless_pass_by_value, reason = "ergonomics")]
    fn check_iter_live_defs_under(input: &str, path_base: &str, expect: Expect) {
        let db = crate::db::BaseDatabase::default();
        IncludeDirs::new(&db, vec![]);

        let root_file = db
            .get_files()
            .add_virtual(&db, "/main.dts".into(), input.to_owned());

        let result = lower_root_file(&db, root_file).expect("Should be a readable file");
        let mir = result.mir(&db);

        let new_mir = Mir {
            definitions: Mir::iter_live_defs_under(mir, path_base).cloned().collect(),
            unresolved_extensions: Vec::new(),
        };

        expect.assert_eq(&new_mir.display(&db));
    }

    #[test]
    fn iter_live_defs_under_node() {
        check_iter_live_defs_under(
            r"/dts-v1/;

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
",
            "/qux",
            expect![[r#"
                property /qux/quux/bar/prop2 = <2>; /main.dts L19:17-L19:29
                node /qux/quux/bar /main.dts L18:13-L20:15
                node /qux/quux /main.dts L17:9-L21:11
                node /qux /main.dts L16:5-L22:7
            "#]],
        );
    }

    #[test]
    fn iter_live_defs_under_root() {
        check_iter_live_defs_under(
            r#"/dts-v1/;

/ {
    foo = "bar";
};
"#,
            "/",
            expect![[r#"
                property /foo = "bar"; /main.dts L4:5-L4:17
                node / /main.dts L3:1-L5:3
            "#]],
        );
    }

    #[test]
    fn iter_live_defs_under_nothing() {
        check_iter_live_defs_under(
            r#"/dts-v1/;

/ {
    foo = "bar";
};
"#,
            "",
            expect![[r#"
                property /foo = "bar"; /main.dts L4:5-L4:17
                node / /main.dts L3:1-L5:3
                dts-v1 /main.dts L1:1-L1:10
            "#]],
        );
    }
}
