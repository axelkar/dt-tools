use std::borrow::Cow;

use dt_analyzer::new::stage1::AnalyzedToplevel;
use dt_diagnostic::{Diagnostic, Severity};

pub mod db;
pub mod file;

#[salsa::tracked]
pub struct Parse<'db> {
    #[returns(ref)]
    pub parse: dt_parser::parser::Parse<'static>,
}

// `no_eq`: Probably faster to compare the inputs than the whole AST
#[salsa::tracked(no_eq)]
pub fn parse_file<'db>(db: &'db dyn db::BaseDb, file: file::File) -> Option<Parse<'db>> {
    if !file.is_readable_file(db) {
        return None;
    }

    let contents: &str = file.contents(db);
    let parse = dt_parser::parser::parse(contents).into_static();
    Some(Parse::new(db, parse))
}

#[salsa::tracked]
pub struct Outline<'db> {
    #[tracked]
    #[returns(ref)]
    pub toplevels: Vec<AnalyzedToplevel>,

    #[tracked]
    #[returns(ref)]
    pub diagnostics: Vec<Diagnostic>,
}

// `no_eq`: Probably faster to compare the inputs than the whole AST
#[salsa::tracked(no_eq)]
pub fn outline<'db>(db: &'db dyn db::BaseDb, file: file::File) -> Option<Outline<'db>> {
    let parse = parse_file(db, file)?;
    let file_ast = parse.parse(db).source_file();

    let mut diagnostics = Vec::new();
    let diag = std::sync::Mutex::new(&mut diagnostics);

    let toplevels = dt_analyzer::new::stage1::analyze_file(&file_ast, file.contents(db), &diag);

    Some(Outline::new(db, toplevels, diagnostics))
}

#[salsa::tracked]
pub struct AlsoDeps<'db> {
    #[tracked]
    #[returns(ref)]
    pub included_files: Vec<file::File>,

    #[tracked]
    #[returns(ref)]
    pub diagnostics: Vec<Diagnostic>,
}

// TODO: per-AnalyzedInclude??
#[salsa::tracked]
pub fn also_deps<'db>(db: &'db dyn db::BaseDb, file: file::File, outline: Outline<'db>) -> Result<AlsoDeps<'db>, ()> {
    let mut included_files = Vec::new();
    let mut diagnostics = Vec::new();

    let parent_path = file.path(db).parent().ok_or(())?;
    // FIXME: actual config singleton!
    let include_dirs: &[&camino::Utf8Path] = &[];

    let files = db.get_files();
    for include in outline.toplevels(db).iter().filter_map(AnalyzedToplevel::as_include) {
        for path in include.possible_paths_utf8(parent_path, include_dirs) {
            let file = files.get_file(db, path);
            included_files.push(file);

            if !file.is_readable_file(db) {
                diagnostics.push(Diagnostic::new(include.text_range, Cow::Borrowed("Couldn't find file to include"), Severity::Error));
            }
        }
    }

    Ok(AlsoDeps::new(db, included_files, diagnostics))
}

#[cfg(test)]
mod tests {
    use camino::Utf8PathBuf;

    use crate::salsa::db::BaseDb;

    use super::*;

    #[test]
    fn test_parse_file() {
        let db = crate::salsa::db::BaseDatabase::default();
        let file_path = Utf8PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_data/basic.dts");
        let file = db.get_files().get_file(&db, file_path);

        let parse = parse_file(&db, file).expect("Should be a readable file");
        let parse = parse.parse(&db);

        assert_eq!(&parse.lex_errors, &[]);
        assert_eq!(&parse.errors, &[]);
    }

    #[test]
    fn test_outline() {
        let db = crate::salsa::db::BaseDatabase::default();
        let file_path = Utf8PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_data/including.dts");
        let file = db.get_files().get_file(&db, file_path);

        let outline = outline(&db, file).expect("Should be a readable file");

        assert_eq!(outline.diagnostics(&db), &[]);
        let toplevels = outline.toplevels(&db);
        assert!(
            matches!(toplevels.as_slice(), &[AnalyzedToplevel::Include(_), AnalyzedToplevel::Include(_)]),
            "toplevels are not as expected: {toplevels:#?}"
        );
    }

    #[test]
    fn test_also_deps() {
        let db = crate::salsa::db::BaseDatabase::default();
        let file_path = Utf8PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_data/including.dts");
        let file = db.get_files().get_file(&db, file_path);

        let outline = outline(&db, file).expect("Should be a readable file");

        let also_deps = also_deps(&db, file, outline).expect("File should have a parent");
        assert_eq!(also_deps.included_files(&db).len(), 2);

        let diagnostics = also_deps.diagnostics(&db);
        assert_eq!(diagnostics.len(), 1, "diagnostics are not as expected: {diagnostics:#?}");
    }
}
