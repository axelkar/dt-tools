use compact_str::CompactString;
use core::ops::Range;
use smallstr::SmallString;
use smallvec::SmallVec;

// TODO: somehow use references (&str) to the file instead of Arc<str>. I think this requires
// self-referencing. Maybe I could do something similar to tl::VDomGuard

//pub type NodeName = SmallString<[u8; 15]>;
//pub type NodeLabel = SmallString<[u8; 15]>;
pub type NodeName = CompactString;
pub type NodeLabel = CompactString;
pub type Whitespace = CompactString;

#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub struct Node {
    pub label: Option<NodeLabel>,
    pub name: NodeName,
    /// Portion after an @
    pub unit_address: Option<NodeName>,
    pub props: Vec<Spanned<Property>>,
    pub nodes: Vec<Spanned<Node>>,
}

#[derive(Clone, PartialEq, Eq, Debug, Default)]
/// TODO: add whitespace before and after equals sign
/// from PropertyValue
pub struct Property {
    pub name: String,
    /// TODO: add whitespace after and before comma
    pub values: SmallVec<[Spanned<PropertyValue>; 2]>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Cell {
    Number(u32),
    Phandle(NodeLabel),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum PropertyValue {
    Cells(SmallVec<[Cell; 4]>),
    /// TODO: add whitespace and whether they have 0x for reconstruction
    Bytestring(SmallVec<[u8; 8]>),
    String(SmallString<[u8; 30]>),
}

#[derive(Clone, PartialEq, Eq, Debug, Default)]
/// TODO: add includes, file-wide whitespace and references
pub struct Document {
    pub props: Vec<Spanned<Property>>,
    pub nodes: Vec<Spanned<Node>>,
}

pub type Span = Range<usize>;
pub type Spanned<T> = (T, Span);

