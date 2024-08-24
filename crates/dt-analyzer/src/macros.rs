//! Preprocessor macro implementation
//!
//! Macro substitution, parameters, ternary operator are evaluated here

pub(crate) enum MacroContext {
    /// Label definition context, e.g. `FOO(bar): baz {};`
    LabelDefinition,
    /// In value context, after ampersand: `foo = &BAR(baz);`
    Reference,
    /// Item meaning node or property, e.g. `FOO(bar) {};`
    ItemNameDefinition,
    /// Property value context, e.g. `foo = BAR(baz);`
    Value,
    /// Cell value context, e.g. `foo = <BAR(baz)>;`
    Cell,
}
