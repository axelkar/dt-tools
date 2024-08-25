//! Preprocessor macro implementation
//!
//! Macro substitution, parameters, ternary operator are evaluated here

use dt_parser::cst2::parser::Entrypoint;

pub(crate) enum MacroContext {
    /// In value context or extension name context, after ampersand: `foo = &BAR(baz);`
    Reference,
    /// Node, property or label name, e.g. `FOO(baz): BAR(baz) {};`
    NameDefinition,
    /// Property value context, e.g. `foo = BAR(baz);`
    Value,
    /// Cell value context, e.g. `foo = <BAR(baz)>;`
    Cell,
    /// Item context, e.g. `/ { FOO(bar); };`
    Item,
}
impl MacroContext {
    /// Returns the entrypoint for parsing the macro's output.
    fn entrypoint(&self) -> Entrypoint {
        match self {
            Self::Reference => Entrypoint::ReferenceNoamp,
            Self::NameDefinition => Entrypoint::Name,
            Self::Value => Entrypoint::PropValues,
            Self::Cell => Entrypoint::Cells,
            Self::Item => Entrypoint::SourceFile,
        }
    }
}
