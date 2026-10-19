//! Source file identities.

use camino::{Utf8Path, Utf8PathBuf};
use dt_tools_diagnostic::text_range::TextRange;
use rustc_hash::FxHashMap;

use crate::{
    lexer::{LexErrorAt, lex},
    token::PpToken,
};

/// Prefix of synthetic sources that hold command-line macro definitions.
pub(crate) const COMMAND_LINE_SOURCE_PREFIX: &str = "/<command-line:";

/// An identifier for a source file within one preprocessing run.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SourceId(pub u32);

/// A source file participating in a preprocessing run.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceFile {
    pub id: SourceId,
    pub path: Utf8PathBuf,
}

impl SourceFile {
    /// Returns true for synthetic sources that hold command-line macro definitions.
    #[must_use]
    pub fn is_command_line(&self) -> bool {
        self.path.as_str().starts_with(COMMAND_LINE_SOURCE_PREFIX)
    }
}

/// Source files for one preprocessing run.
#[derive(Debug, Default)]
pub struct SourceSet {
    files: Vec<SourceFile>,
    by_path: FxHashMap<Utf8PathBuf, SourceId>,
}

impl SourceSet {
    /// Registers a path, returning its existing or new id.
    ///
    /// # Panics
    ///
    /// Panics if more than [`u32::MAX`] files are registered.
    pub fn add(&mut self, path: &Utf8Path) -> SourceId {
        if let Some(id) = self.by_path.get(path) {
            return *id;
        }
        let id = SourceId(u32::try_from(self.files.len()).expect("too many source files"));
        self.files.push(SourceFile {
            id,
            path: path.to_owned(),
        });
        self.by_path.insert(path.to_owned(), id);
        id
    }

    /// Returns the file with `id`.
    ///
    /// # Panics
    ///
    /// Panics if `id` is out of range for the set.
    #[must_use]
    pub fn get(&self, id: SourceId) -> &SourceFile {
        &self.files[id.0 as usize]
    }

    /// Consumes the set, returning the files in first-encounter order.
    #[must_use]
    pub fn into_files(self) -> Vec<SourceFile> {
        self.files
    }
}

/// Where a relative include is searched.
#[derive(Clone, Copy)]
pub(crate) enum IncludeSearch {
    /// The includer's directory, then the include paths.
    IncluderFirst,
    /// Only the include paths.
    IncludePaths,
}

/// Returns the first readable candidate for `name`.
pub(crate) fn resolve_file<R>(
    include_dirs: &[Utf8PathBuf],
    includer: &Utf8Path,
    name: &str,
    search: IncludeSearch,
    read_file: &mut R,
) -> Option<(Utf8PathBuf, String)>
where
    R: FnMut(&Utf8Path) -> Option<String>,
{
    let name = Utf8Path::new(name);
    let mut candidates = Vec::new();

    if name.is_absolute() {
        candidates.push(name.to_owned());
    } else {
        if matches!(search, IncludeSearch::IncluderFirst)
            && let Some(dir) = includer.parent()
        {
            candidates.push(dir.join(name));
        }
        candidates.extend(include_dirs.iter().map(|dir| dir.join(name)));
    }

    candidates
        .into_iter()
        .find_map(|candidate| read_file(&candidate).map(|contents| (candidate, contents)))
}

/// Files loaded during one preprocessing run.
#[derive(Debug, Default)]
pub(crate) struct LoadedFiles {
    /// File identities, in first-encounter order.
    pub(crate) sources: SourceSet,
    /// Concatenated cleaned text of every loaded file.
    pub(crate) cleaned: String,
}

/// A file added to the cleaned buffer.
pub(crate) struct LoadedFile {
    /// The file's id.
    pub(crate) id: SourceId,
    /// The file's tokens, with `cleaned` ranges indexed into the buffer.
    pub(crate) tokens: Vec<PpToken>,
    /// Lexical errors.
    pub(crate) errors: Vec<LexErrorAt>,
    /// Length of the original file in bytes.
    pub(crate) orig_len: usize,
}

impl LoadedFiles {
    /// Lexes `contents` as `path` and appends it to the cleaned buffer.
    pub(crate) fn add(&mut self, path: &Utf8Path, contents: &str) -> LoadedFile {
        let id = self.sources.add(path);
        let mut lexed = lex(contents, id);
        let base = self.cleaned.len();

        for token in &mut lexed.tokens {
            token.cleaned = TextRange::new(token.cleaned.start + base, token.cleaned.end + base);
        }
        self.cleaned.push_str(&lexed.cleaned_text);

        LoadedFile {
            id,
            tokens: lexed.tokens,
            errors: lexed.errors,
            orig_len: lexed.orig_len,
        }
    }

    /// Returns the path of the file with `id`.
    ///
    /// # Panics
    ///
    /// Panics if `id` is out of range for the set.
    pub(crate) fn path(&self, id: SourceId) -> &Utf8Path {
        self.sources.get(id).path.as_path()
    }
}
