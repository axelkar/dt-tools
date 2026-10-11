//! Preprocessor configuration.

use camino::Utf8PathBuf;

/// A predefined macro, like a compiler `-D` flag.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Define {
    pub name: String,
    /// The replacement text for the macro.
    pub value: String,
}

/// Configuration for [`preprocess`](crate::preprocess).
#[derive(Debug, Clone)]
pub struct PreprocessorConfig {
    /// Search path for `#include <...>`. Quoted includes check the includer's directory first.
    pub include_dirs: Vec<Utf8PathBuf>,
    /// Predefined macros.
    pub defines: Vec<Define>,
    /// Maximum include nesting depth. GCC and dtc both cap at 200.
    pub max_include_depth: usize,
    /// Maximum macro expansion nesting depth.
    pub max_expansion_depth: usize,
    /// Maximum size of the expanded text in bytes.
    pub max_expanded_bytes: usize,
}

/// The default include nesting cap, matching GCC and dtc.
const DEFAULT_MAX_INCLUDE_DEPTH: usize = 200;
/// The default macro expansion depth cap.
const DEFAULT_MAX_EXPANSION_DEPTH: usize = 256;
/// The default cap on expanded text size in bytes.
const DEFAULT_MAX_EXPANDED_BYTES: usize = 64 * 1024 * 1024;

impl Default for PreprocessorConfig {
    fn default() -> Self {
        Self {
            include_dirs: Vec::new(),
            defines: Vec::new(),
            max_include_depth: DEFAULT_MAX_INCLUDE_DEPTH,
            max_expansion_depth: DEFAULT_MAX_EXPANSION_DEPTH,
            max_expanded_bytes: DEFAULT_MAX_EXPANDED_BYTES,
        }
    }
}
