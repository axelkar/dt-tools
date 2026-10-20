//! Preprocessed CST -> MIR lowering.

use dt_tools_diagnostic::{Diagnostic, Severity, Span};
use dt_tools_parser::ast::{self, AstNode, AstToken, HasMacroInvocation, HasName};
use rustc_hash::FxHashMap;

use crate::{
    db::BaseDb,
    diag::Diag,
    file::File,
    mir::{Mir, MirPhandleTarget},
    preprocess::{PreprocessedText, preprocess_root, preprocessed_parse},
};

pub(crate) mod dt_node;
mod dt_property;
#[cfg(test)]
mod dtc_tests;
mod item;

/// A label definition.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Label {
    /// Path of the labeled node.
    pub path: String,
    /// The label's span in the preprocessed text.
    pub span: Span<PreprocessedText>,
}

/// Result of lowering a root file and its includes.
#[salsa::tracked]
pub struct LoweredFile<'db> {
    /// Diagnostics emitted by lowering.
    ///
    /// Their spans are offsets into the root's preprocessed text, not file positions. Consumers
    /// resolve them with the root's [`crate::preprocess::PreprocessedRoot`].
    #[tracked]
    #[returns(ref)]
    pub diagnostics: Vec<Diagnostic<PreprocessedText>>,

    /// List of files processed, for publishing diagnostics.
    #[tracked]
    #[returns(ref)]
    pub processed_files: Vec<File>,

    /// Mid-level intermediate representation.
    #[tracked]
    #[returns(ref)]
    pub mir: Mir,

    /// Labels by name. `None` marks a deleted label.
    #[tracked]
    #[returns(ref)]
    pub labels: FxHashMap<String, Option<Label>>,
}

/// Lowers the preprocessed CST to [`Mir`], starting from a root file.
///
/// [`LoweredFile`] also contains a lot of additional metadata, in addition to the MIR.
///
/// # Errors
///
/// Returns `None` if the file doesn't exist.
///
/// # Panics
///
/// Will panic if [`PreprocessSettings`] hasn't been defined.
#[salsa::tracked(returns(copy), lru = 64)]
pub fn lower_root_file(db: &dyn BaseDb, root_file: File) -> Option<LoweredFile<'_>> {
    profiling::scope!("lowering::lower_root_file", root_file.path(db).as_str());

    let preprocessed = preprocess_root(db, root_file)?;
    let parse = preprocessed_parse(db, root_file)?;
    let file_ast = parse.parse(db).source_file();

    let mut diagnostics = Vec::new();
    let mut labels = FxHashMap::default();
    let mut mir = Mir::default();

    {
        let mut diag = Diag::new(&mut diagnostics);
        let mut ctx = LowerCtx {
            mir: &mut mir,
            labels: &mut labels,
        };

        for item in file_ast.items() {
            item::lower_item(&mut ctx, &mut diag, "", item);
        }
    }

    let processed_files = preprocessed
        .sources(db)
        .iter()
        .filter(|source| !source.is_command_line())
        .map(|source| db.get_files().get_file(db, &source.path))
        .collect();

    Some(LoweredFile::new(
        db,
        diagnostics,
        processed_files,
        mir,
        labels,
    ))
}

/// Context threaded through the tree traversal.
pub(crate) struct LowerCtx<'a> {
    pub(crate) mir: &'a mut Mir,
    pub(crate) labels: &'a mut FxHashMap<String, Option<Label>>,
}
impl LowerCtx<'_> {
    /// Returns true if this is currently in overlay mode.
    #[must_use]
    fn is_overlay(&self) -> bool {
        self.mir.sets_overlay()
    }
}

/// Emits a diagnostic for a macro invocation left in the preprocessed text.
pub(crate) fn undefined_macro(diag: &mut Diag<'_>, invocation: &ast::MacroInvocation) {
    let name = invocation
        .green_ident()
        .map_or_else(|| "<unknown>".to_owned(), |token| token.text.to_string());

    diag.emit(
        invocation.syntax().text_range(),
        format!("Macro `{name}` is not defined"),
        Severity::Error,
    );
}

/// Lowers an [`ast::DtPhandle`] to a [`MirPhandleTarget`], if valid.
fn lower_phandle(diag: &mut Diag<'_>, phandle: &ast::DtPhandle) -> Result<MirPhandleTarget, ()> {
    if let Some(macro_inv) = phandle.macro_invocation() {
        // Defined macros were already expanded.
        undefined_macro(diag, &macro_inv);
        return Err(());
    }

    if phandle.is_path() {
        // &{/path/to/node}, &{/}, or &{label/path}.
        // FIXME: very naive, just strips the prefix/suffix from the source text.
        // A proper implementation would use the CST structure.
        let raw = phandle.syntax().green.text();
        let inner = raw
            .strip_prefix("&{")
            .ok_or(())?
            .strip_suffix('}')
            .ok_or(())?;

        if inner == "/" || inner.starts_with('/') {
            Ok(MirPhandleTarget::Path(inner.to_owned()))
        } else {
            Ok(match inner.split_once('/') {
                Some((label, path)) => MirPhandleTarget::LabelPath {
                    label: label.to_owned(),
                    path: path.to_owned(),
                },
                None => MirPhandleTarget::Label(inner.to_owned()),
            })
        }
    } else {
        let name_ast = phandle.name().ok_or(())?;
        Ok(MirPhandleTarget::Label(name_ast.syntax().text().to_owned()))
    }
}

#[cfg(test)]
#[expect(
    clippy::needless_raw_string_hashes,
    reason = "expect-test auto update adds r#"
)]
pub(crate) mod tests {
    use expect_test::{Expect, expect};

    use super::*;
    use crate::{db::BaseDb, file::DisplaySpanLineColumn, settings::PreprocessSettings};

    pub(crate) fn fmt_diags(db: &dyn BaseDb, out: &mut String, diags: &[Diagnostic<File>]) {
        out.push_str("--- errors ---\n");
        for d in diags {
            use std::fmt::Write;
            let span = d
                .span
                .primary_spans
                .first()
                .expect("Should have at least one primary span");

            let _ = writeln!(
                out,
                "{:?} {}: {}",
                d.severity,
                DisplaySpanLineColumn(span, db),
                d.msg
            );
        }
    }

    /// Run the preprocessor on virtual files and snapshot MIR + diagnostics.
    ///
    /// The root file is named "/main.dts".
    #[expect(clippy::needless_pass_by_value, reason = "ergonomics")]
    pub(crate) fn check_mir(
        root_file_contents: &str,
        other_files: &[(&str, &str)],
        expect: Expect,
    ) {
        let db = crate::db::BaseDatabase::default();
        PreprocessSettings::new(&db, vec![], vec![]);

        let root_file =
            db.get_files()
                .add_virtual(&db, "/main.dts".into(), root_file_contents.to_owned());

        for &(path, contents) in other_files {
            db.get_files()
                .add_virtual(&db, path.into(), contents.to_owned());
        }

        let (diags, _included_files) = crate::compute_diagnostics(&db, root_file);

        let result = lower_root_file(&db, root_file).expect("Should be a readable file");
        let mir = result.mir(&db);
        let preprocessed = preprocess_root(&db, root_file).expect("Should be a readable file");

        let mut out = mir.display(&db, preprocessed);
        if !diags.is_empty() {
            out.push('\n');
            fmt_diags(&db, &mut out, diags);
        }
        expect.assert_eq(&out);
    }

    #[test]
    fn mir_empty() {
        check_mir(
            r#"
/dts-v1/;
"#,
            &[],
            expect![[r#"
                dts-v1 /main.dts L2:1-L2:10
            "#]],
        );
    }

    #[test]
    fn mir_empty_root() {
        check_mir(
            r#"
/dts-v1/;
/ {};
"#,
            &[],
            expect![[r#"
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L3:1-L3:6
            "#]],
        );
    }

    #[test]
    fn mir_macro() {
        check_mir(
            r#"
/dts-v1/;
#define VAL 42
#define VAL2 "example"

/ { prop = <VAL>, VAL2; };
"#,
            &[],
            expect![[r#"
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L6:1-L6:27
                property /prop = <42>, "example"; /main.dts L6:5-L6:24
            "#]],
        );
    }

    #[test]
    fn mir_macro_label() {
        check_mir(
            r#"
/dts-v1/;
#define MACRO FOO

/ { MACRO: foo {}; };
/ { prop = <&FOO>; };
"#,
            &[],
            expect![[r#"
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L5:1-L5:22
                node /foo labels=[FOO] /main.dts L3:15-L3:18
                node / /main.dts L6:1-L6:22
                property /prop = <&FOO>; /main.dts L6:5-L6:19
            "#]],
        );
    }

    #[test]
    fn mir_macro_reference() {
        check_mir(
            r#"
/dts-v1/;
#define MACRO FOO

/ { FOO: foo {}; };
/ { prop = <&MACRO>; };
"#,
            &[],
            expect![[r#"
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L5:1-L5:20
                node /foo labels=[FOO] /main.dts L5:5-L5:17
                node / /main.dts L6:1-L6:24
                property /prop = <&FOO>; /main.dts L6:5-L6:21
            "#]],
        );
    }

    #[test]
    fn mir_macro_node() {
        check_mir(
            r#"
/dts-v1/;
#define FOO SUBSTITUTED

/ { FOO@bar {}; };
/ { bar@FOO {}; };
"#,
            &[],
            expect![[r#"
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L5:1-L5:19
                node /SUBSTITUTED@bar /main.dts L3:13-L3:24
                node / /main.dts L6:1-L6:19
                node /bar@SUBSTITUTED /main.dts L6:5-L6:16
            "#]],
        );
    }

    #[test]
    fn mir_macro_node_twice() {
        check_mir(
            r#"
/dts-v1/;
#define SUBSTITUTED BAR
#define FOO SUBSTITUTED

/ { FOO {}; };
"#,
            &[],
            expect![[r#"
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L6:1-L6:15
                node /BAR /main.dts L3:21-L3:24
            "#]],
        );
    }

    #[test]
    fn mir_undefined_macro() {
        check_mir(
            r#"
/dts-v1/;
/ { prop = <VAL>; prop2 = VAL; };
"#,
            &[],
            expect![[r#"
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L3:1-L3:34
                property /prop = <>; /main.dts L3:5-L3:18
                property /prop2; /main.dts L3:19-L3:31

                --- errors ---
                Error L3:13-L3:16: Macro `VAL` is not defined
                Error L3:27-L3:30: Macro `VAL` is not defined
            "#]],
        );
    }

    #[test]
    fn mir_macro_error_propagation() {
        check_mir(
            r#"
/dts-v1/;
#define BIG (1 << 40)
#define CELL(x) <x 5>

/ { prop = <BIG>, CELL((1 << 40)); };
"#,
            &[],
            expect![[r#"
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L6:1-L6:38
                property /prop = <>, <5>; /main.dts L6:5-L6:35

                --- errors ---
                Error L3:13-L3:22: number 1099511627776 too large to fit in 32-bit signed integer (using two's complement) or 32-bit unsigned integer
                Error L6:24-L6:33: number 1099511627776 too large to fit in 32-bit signed integer (using two's complement) or 32-bit unsigned integer
            "#]],
        );
    }

    /// When a `#` parameter is used directly, the argument isn't prescanned.
    /// With an indirection macro, the argument is expanded first, then stringified.
    #[test]
    fn mir_macro_prescan_stringify() {
        check_mir(
            r#"
/dts-v1/;

#define STRINGIFY(x...) #x
#define XSTRINGIFY(x...) STRINGIFY(x)

#define FOO bar

/ {
    no_prescan = STRINGIFY(FOO);
    prescanned = XSTRINGIFY(FOO);
};
"#,
            &[],
            expect![[r#"
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L9:1-L12:3
                property /no_prescan = "FOO"; /main.dts L10:5-L10:33
                property /prescanned = "bar"; /main.dts L11:5-L11:34
            "#]],
        );
    }

    /// When a `##` parameter is used directly, the arguments aren't prescanned.
    /// With an indirection macro, the arguments are expanded first, then concatenated.
    #[test]
    fn mir_macro_prescan_concat() {
        check_mir(
            r#"
/dts-v1/;

#define CAT(x, y...) x ## y
#define XCAT(x, y...) CAT(x, y)

#define PREFIX foo

/ {
    CAT(PREFIX, _suffix) {};
    XCAT(PREFIX, _suffix) {};
};
"#,
            &[],
            expect![[r#"
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L9:1-L12:3
                node /PREFIX_suffix /main.dts L10:9-L10:15
                node /foo_suffix /main.dts L7:16-L7:19
            "#]],
        );
    }

    /// The preprocessor stops recursion and leaves `SELF` in the preprocessed text,
    /// so lowering reports it as undefined.
    #[test]
    fn mir_macro_recursive() {
        check_mir(
            r#"
/dts-v1/;
#define SELF SELF

/ { prop = SELF; };
"#,
            &[],
            expect![[r#"
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L5:1-L5:20
                property /prop; /main.dts L5:5-L5:17

                --- errors ---
                Error L3:14-L3:18: Macro `SELF` is not defined
            "#]],
        );
    }

    /// Prescanning does not change the recursion behavior.
    #[test]
    fn mir_macro_recursive_with_prescan() {
        check_mir(
            r#"
/dts-v1/;
#define SELF SELF
#define ID(x) x

/ { prop = ID(SELF); };
"#,
            &[],
            expect![[r#"
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L6:1-L6:24
                property /prop; /main.dts L6:5-L6:21

                --- errors ---
                Error L3:14-L3:18: Macro `SELF` is not defined
            "#]],
        );
    }

    /// Skipped conditional branches produce neither MIR nor diagnostics.
    #[test]
    fn mir_inactive_branch() {
        check_mir(
            r#"
/dts-v1/;
#if 0
broken { prop = <1>;
#endif
/ {};
"#,
            &[],
            expect![[r#"
                dts-v1 /main.dts L2:1-L2:10
                node / /main.dts L6:1-L6:6
            "#]],
        );
    }

    /// Lowering diagnostics keep the macro provenance labels.
    #[test]
    fn lowering_diagnostic_has_macro_labels() {
        let db = crate::db::BaseDatabase::default();
        PreprocessSettings::new(&db, vec![], vec![]);
        let root_file = db.get_files().add_virtual(
            &db,
            "/main.dts".into(),
            "#define BIG (1 << 40)\n/ { prop = <BIG>; };\n".to_owned(),
        );

        let (diagnostics, _) = crate::compute_diagnostics(&db, root_file);
        let diagnostic = diagnostics
            .iter()
            .find(|diagnostic| diagnostic.msg.contains("too large"))
            .expect("overflow diagnostic");

        assert!(
            diagnostic
                .span
                .span_labels
                .iter()
                .any(|label| label.msg.contains("in this expansion of `BIG`")),
            "{diagnostic:#?}"
        );
        assert!(
            diagnostic
                .span
                .span_labels
                .iter()
                .any(|label| label.msg.contains("`BIG` defined here")),
            "{diagnostic:#?}"
        );
    }
}
