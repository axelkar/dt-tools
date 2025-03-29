use std::{
    env::current_dir,
    path::{Path, PathBuf},
};

pub(crate) fn search<F: FnMut(&PathBuf) -> bool>(
    start: &Path,
    filename: &str,
    mut f: F,
    // Option<(original path passed to fn, path with marker)>
) -> Option<(PathBuf, PathBuf)> {
    start
        .ancestors()
        .map(|p| (p, p.join(filename)))
        .find(|(_, arg)| f(arg))
        .map(|(o, p)| (o.to_path_buf(), p))
}

pub(crate) fn search_cwd<F: FnMut(&PathBuf) -> bool>(
    filename: &str,
    f: F,
) -> Option<(PathBuf, PathBuf)> {
    search(&current_dir().unwrap(), filename, f)
}

pub(crate) fn is_exists_and_file(p: &PathBuf) -> bool {
    p.exists() && p.is_file()
}
