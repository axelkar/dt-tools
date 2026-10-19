//! Preprocessing settings.

use camino::Utf8PathBuf;

/// Global preprocessing settings.
#[salsa::input(singleton)]
pub struct PreprocessSettings {
    #[returns(ref)]
    pub include_dirs: Vec<Utf8PathBuf>,

    /// Predefined macros as `(name, value)` pairs.
    #[returns(ref)]
    pub defines: Vec<(String, String)>,
}
