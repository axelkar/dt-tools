use std::path::{Path, PathBuf};

/// Searches for a file with the given `filename` in the given `start` directory
/// and its ancestors, applying a `f` to each candidate path.
///
/// Returns a reference to the directory where the match was found and a full path
/// constructed by joining the ancestor with `filename`
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
