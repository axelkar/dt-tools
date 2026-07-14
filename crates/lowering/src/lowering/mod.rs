//! CST -> MIR lowering.
//!
//! Preprocessor macros, conditionals and includes are all tied very much together, which means they must be evaluated in one pass. For simplicity, everything else is also evaluated in one pass.
// TODO: handle recursive macros?

use std::borrow::Cow;

use camino::{Utf8Path, Utf8PathBuf};
use dt_tools_analyzer::macros::SubstitutedBody;
use dt_tools_diagnostic::{Diagnostic, Severity, Span};
use dt_tools_parser::{
    ast::{self, AstNode, AstNodeOrToken, AstToken, HasMacroInvocation, HasName},
    parser::Entrypoint,
};

use crate::{
    db::BaseDb,
    diag::{Diag, MacroExpansion, SourceMap},
    emit_parse_errors,
    file::File,
    includes::IncludeDirs,
    macros::{
        MacroCtx,
        env::{TrackedMapEnv, TrackedMapEnvMut},
        substitute_macro_tok,
    },
    mir::{Mir, MirPhandleTarget},
};

pub(crate) mod dt_node;
mod dt_property;
#[cfg(test)]
mod dtc_tests;
mod item;
mod preprocessor;

// TODO: how to communicate back prop values and cells? type-safe or generic?
enum LowerFileCtx {
    SourceFile,
    DtNode { parent_node_path: String },
    PropValues,
    Cells,
}

/// Result of lowering a single file and its includes.
#[salsa::tracked]
pub struct LoweredFile<'db> {
    /// Macro and label environment after processing this file.
    #[tracked]
    pub env_after: TrackedMapEnv<'db>,

    /// List of diagnostics emitted by lowering.
    #[tracked]
    #[returns(ref)]
    pub diagnostics: Vec<Diagnostic<File>>,

    /// List of files processed, for publishing diagnostics.
    #[tracked]
    #[returns(ref)]
    pub processed_files: Vec<File>,

    /// List of includes.
    #[tracked]
    #[returns(ref)]
    pub includes: Vec<(File, Span<File>)>,

    /// Mid-level intermediate representation.
    #[tracked]
    #[returns(ref)]
    pub mir: Mir,

    /// Whether this file is part of a overlay (`/plugin/;`).
    pub is_overlay: bool,
}

/// Lowers CST to [`Mir`], starting from a root file.
///
/// [`LoweredFile`] also contains a lot of additional metadata, in addition to the MIR.
///
/// # Errors
///
/// Returns `None` if the file doesn't exist.
///
/// # Panics
///
/// Will panic if [`IncludeDirs`] hasn't been defined.
pub fn lower_root_file(db: &dyn BaseDb, root_file: File) -> Option<LoweredFile<'_>> {
    lower_file(db, root_file, None, false, String::new())
}

#[allow(clippy::allow_attributes, reason = "expect doesn't work properly here")]
#[allow(
    clippy::needless_pass_by_value,
    reason = "Salsa doesn't allow &str as a tracked function parameter"
)]
/// Lowers the CST of a single file to [`Mir`], recursing into its includes.
#[salsa::tracked(lru = 128)]
pub(crate) fn lower_file<'db>(
    db: &'db dyn BaseDb,
    file: File,
    parent_env: Option<TrackedMapEnv<'db>>,
    parent_is_overlay: bool,
    parent_node_path: String,
) -> Option<LoweredFile<'db>> {
    let span = profiling::tracy_client::span!("lsp::salsa::lowering::lower_file");
    span.emit_text(file.path(db).as_str());

    let mut env = TrackedMapEnvMut::from_parent(parent_env);

    // TODO: parse entrypoint?
    let parse = super::parse_file(db, file)?;
    let file_ast = parse.parse(db).source_file();

    let mut diagnostics = Vec::new();
    let base_map = SourceMap::File(file);
    let mut diag = Diag::new(&mut diagnostics, &base_map);

    // Path of the current file's parent directory.
    let parent_dir_path = file.path(db).parent()?;
    let include_dirs = IncludeDirs::get(db).include_dirs(db);

    let mut processed_files = Vec::new();
    let mut includes = Vec::new();
    let mut mir = Mir::default();
    let mut ctx = IntraFileCtx {
        db,
        file,
        env: &mut env,
        mir: &mut mir,
        parent_is_overlay,
        parent_dir_path,
        include_dirs,
        processed_files: &mut processed_files,
        includes: &mut includes,
    };

    // TODO: PERF: split into phases with includes and after includes for Salsa tracking
    // TODO: PERF: flatten includes only at the root file boundary?

    // TODO: custom lowering function for non-SourceFile includes
    for item in file_ast.items() {
        item::lower_item(&mut ctx, &mut diag, &parent_node_path, item);
    }

    let is_overlay = ctx.is_overlay();

    Some(LoweredFile::new(
        db,
        env.into_immut(db),
        diagnostics,
        processed_files,
        includes,
        mir,
        is_overlay,
    ))
}

/// Mutable context threaded through the tree traversal in a single [file](File).
pub(crate) struct IntraFileCtx<'a, 'db> {
    db: &'db dyn BaseDb,
    file: File,
    env: &'a mut TrackedMapEnvMut<'db>,
    mir: &'a mut Mir,
    parent_is_overlay: bool,
    parent_dir_path: &'a Utf8Path,
    include_dirs: &'a [Utf8PathBuf],
    processed_files: &'a mut Vec<File>,
    includes: &'a mut Vec<(File, Span<File>)>,
}
impl IntraFileCtx<'_, '_> {
    /// Returns true if this is currently in overlay mode.
    #[must_use]
    fn is_overlay(&self) -> bool {
        self.parent_is_overlay || self.mir.sets_overlay()
    }
}

/// Resolves and substitutes a macro and reparses the result.
///
/// Returns `Ok(None)` if the macro doesn't exist and the macro reference is implicit.
pub(crate) fn resolve_macro_to_ast<'db, AstType: AstNodeOrToken>(
    db: &'db dyn BaseDb,
    env: &TrackedMapEnvMut<'db>,
    diag: &mut Diag<'_, '_>,
    macro_ctx: &MacroCtx,
    entrypoint: Entrypoint,
) -> Result<Option<(AstType, MacroExpansion)>, ()> {
    let Some((
        SubstitutedBody {
            source_mappings,
            substituted_text,
        },
        macro_def,
    )) = substitute_macro_tok(db, env, diag, macro_ctx)?
    else {
        return Ok(None);
    };

    let expansion = MacroExpansion {
        source_mappings,
        macro_def,
        args: macro_ctx.arg_ranges(),
        invocation: macro_ctx.text_range(),
    };

    let parse = entrypoint.parse(&substituted_text);

    {
        let child_map = SourceMap::Macro {
            parent: diag.map,
            expansion: &expansion,
        };
        emit_parse_errors(&parse, &mut Diag::new(&mut *diag.sink, &child_map));
    }

    if !parse.lex_errors.is_empty() || !parse.errors.is_empty() {
        diag.emit(
            macro_ctx.text_range(),
            format!("Failed to parse the result of this macro invocation ({substituted_text:?}) as {entrypoint:?}"),
            Severity::Error,
        );
    }

    let red_node = parse.red_node();
    let Some(ast) = AstType::cast_node(red_node.clone())
        .or_else(|| red_node.children().find_map(AstType::cast_either))
    else {
        let msg = format!(
            "Internal compiler error: Couldn't find {} as child of parse's root node, found node kinds {:?} and token kinds {:?}",
            std::any::type_name::<AstType>(),
            parse
                .red_node()
                .child_nodes()
                .map(|red| red.green.kind)
                .collect::<Vec<_>>(),
            parse
                .red_node()
                .child_tokens()
                .map(|red| red.green.kind)
                .collect::<Vec<_>>()
        );
        tracing::error!("{}", msg);
        diag.emit(macro_ctx.text_range(), Cow::Owned(msg), Severity::Error);
        return Err(());
    };

    Ok(Some((ast, expansion)))
}

/// Lowers an [`ast::DtPhandle`] to a [`MirPhandleTarget`], if valid.
fn lower_phandle<'db>(
    db: &'db dyn BaseDb,
    env: &mut TrackedMapEnvMut<'db>,
    diag: &mut Diag<'_, '_>,
    phandle: &ast::DtPhandle,
) -> Result<MirPhandleTarget, ()> {
    use dt_tools_parser::parser::Entrypoint;

    if let Some(macro_inv) = phandle.macro_invocation() {
        // If the phandle has a macro invocation (e.g. `&MACRO(...)`), resolve it.
        let (ast, expansion) = resolve_macro_to_ast(
            db,
            env,
            diag,
            &MacroCtx::Explicit(&macro_inv),
            Entrypoint::ReferenceNoamp,
        )?
        .expect("resolve_macro_to_value should not return Ok(None) with an explicit macro");

        let child_map = SourceMap::Macro {
            parent: diag.map,
            expansion: &expansion,
        };
        Ok(lower_phandle(
            db,
            env,
            &mut Diag::new(&mut *diag.sink, &child_map),
            &ast,
        )?)
    } else if phandle.is_path() {
        // &{/path/to/node}
        // FIXME: very naive, just strips the prefix/suffix from the source text.
        // A proper implementation would use the CST structure.
        let raw = phandle.syntax().green.text();
        let inner = raw
            .strip_prefix("&{")
            .ok_or(())?
            .strip_suffix('}')
            .ok_or(())?;

        Ok(MirPhandleTarget::Path(inner.to_owned()))
    } else {
        // No explicit macro invocation, but the name itself might still be a macro
        // (e.g. `#define UART_1 soc/uart` and `&UART_1`).
        let name_ast = phandle.name().ok_or(())?;

        // dtc wants extensions to be resolved from items above/before the extensions in
        // non-overlay mode, but phandles are fine in any order.

        Ok(resolve_macro_to_ast(
            db,
            env,
            diag,
            &MacroCtx::Implicit(&name_ast),
            Entrypoint::ReferenceNoamp,
        )?
        .map(|(ast, expansion)| {
            let child_map = SourceMap::Macro {
                parent: diag.map,
                expansion: &expansion,
            };
            lower_phandle(db, env, &mut Diag::new(&mut *diag.sink, &child_map), &ast)
        })
        .transpose()?
        .unwrap_or(MirPhandleTarget::Label(name_ast.syntax().text().to_owned())))
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
    use crate::{db::BaseDb, file::DisplaySpanLineColumn};

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
        IncludeDirs::new(&db, vec![]);

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

        let mut out = mir.display(&db);
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
                node /foo labels=[FOO] /main.dts L5:5-L5:19
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
                node /SUBSTITUTED@bar /main.dts L5:5-L5:16
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
                node /BAR /main.dts L6:5-L6:12
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
                node /PREFIX_suffix /main.dts L10:5-L10:29
                node /foo_suffix /main.dts L11:5-L11:30
            "#]],
        );
    }

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
                Error L3:14-L3:18: macro `SELF` expanded recursively
            "#]],
        );
    }

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
                Error L3:14-L3:18: macro `SELF` expanded recursively
            "#]],
        );
    }
}
