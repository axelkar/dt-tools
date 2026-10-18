//! Include-path settings.

/// Global setting singleton.
#[salsa::input(singleton)]
pub struct IncludeDirs {
    #[returns(ref)]
    pub include_dirs: Vec<camino::Utf8PathBuf>,
}
