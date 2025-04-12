use std::path::{Path, PathBuf};

/// Looks for `filename` in `start` directory's ancestors
///
/// Returns the `filename` path and matching ancestor if found
pub(crate) fn search<'a, F: FnMut(&PathBuf) -> bool>(
    start: &'a Path,
    filename: &str,
    mut f: F,
) -> Option<(&'a Path, PathBuf)> {
    start
        .ancestors()
        .map(|p| (p, p.join(filename)))
        .find(|(_, arg)| f(arg))
}
