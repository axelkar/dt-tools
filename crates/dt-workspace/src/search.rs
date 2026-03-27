use camino::{Utf8Path, Utf8PathBuf};

/// Searches for a file with the given `filename` in the given `start` directory
/// and its ancestors, applying a `f` to each candidate path.
///
/// Returns a reference to the directory where the match was found and a full path
/// constructed by joining the ancestor with `filename`
pub(crate) fn search<'a, F: FnMut(&Utf8PathBuf) -> bool>(
    start: &'a Utf8Path,
    filename: &str,
    mut f: F,
) -> Option<(&'a Utf8Path, Utf8PathBuf)> {
    start
        .ancestors()
        .map(|p| (p, p.join(filename)))
        .find(|(_, arg)| f(arg))
}
