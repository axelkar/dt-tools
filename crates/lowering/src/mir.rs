use std::fmt;

use dt_tools_parser::TextRange;

use crate::file::File;

/// Mid-level intermediate representation of devicetrees.
///
/// Uses a flat list of definitions keyed by full path. Tree assembly and merging happens at the end.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Mir {
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
