//! CST -> MIR lowering.
//!
//! Preprocessor macros, conditionals and includes are all tied very much together, which means they must be evaluated in one pass. For simplicity, everything else is also evaluated in one pass.
// TODO: handle recursive macros?

use std::borrow::Cow;

use dt_tools_diagnostic::{Diagnostic, DiagnosticCollector, Severity, Span};
use dt_tools_parser::{
    TextRange,
    ast::{self, AstNode, AstNodeOrToken, AstToken, HasMacroInvocation, HasName},
    lexer::TokenKind,
    parser::Entrypoint,
};

use crate::{
    db::BaseDb,
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

mod dt_node;
mod dt_property;
#[cfg(test)]
mod dtc_tests;
mod preprocessor;
mod toplevel;

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

    /// Whether this file is part of a plugin/overlay (`/plugin/;`).
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
    // Detect /plugin/; in the root file.
    let is_overlay = crate::parse_file(db, root_file).is_some_and(|p| {
        p.parse(db).source_file().directives().any(|dir| {
            dir.syntax()
                .child_tokens()
                .any(|tok| tok.green.kind == TokenKind::PluginDirective)
        })
    });

    lower_file(db, root_file, None, is_overlay)
}

/// Lowers the CST of a single file to [`Mir`], recursing into its includes.
#[salsa::tracked(lru = 128)]
pub(crate) fn lower_file<'db>(
    db: &'db dyn BaseDb,
    file: File,
    parent_env: Option<TrackedMapEnv<'db>>,
    is_overlay: bool,
) -> Option<LoweredFile<'db>> {
    let span = profiling::tracy_client::span!("lsp::salsa::lowering::lower_file");
    span.emit_text(file.path(db).as_str());

    let mut env = TrackedMapEnvMut::from_parent(parent_env);

    let parse = super::parse_file(db, file)?;
    let file_ast = parse.parse(db).source_file();

    let mut diagnostics = Vec::new();
    let diag = parking_lot::Mutex::new(&mut diagnostics);

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
        diag: &diag,
        mir: &mut mir,
        is_overlay,
    };

    // TODO: PERF: split into phases with includes and after includes for Salsa tracking

    for item in file_ast.items() {
        toplevel::handle_toplevel_item(
            &mut ctx,
            parent_dir_path,
            include_dirs,
            &mut processed_files,
            &mut includes,
            item,
        );
    }

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
struct IntraFileCtx<'a, 'db, D: DiagnosticCollector<File>> {
    db: &'db dyn BaseDb,
    file: File,
    env: &'a mut TrackedMapEnvMut<'db>,
    diag: &'a D,
    mir: &'a mut Mir,
    is_overlay: bool,
}

/// Resolves and substitutes a macro and reparses the result.
///
/// Returns `Ok(None)` if the macro doesn't exist and the macro reference is implicit.
fn resolve_macro_to_value<
    'db,
    AstType: AstNodeOrToken,
    MirType,
    D: DiagnosticCollector<File>,
    Spanner: FnMut(TextRange) -> Span<File>,
>(
    db: &'db dyn BaseDb,
    env: &mut TrackedMapEnvMut<'db>,
    diag: &D,
    spanner: &mut Spanner,
    macro_ctx: &MacroCtx,
    entrypoint: Entrypoint,
    lower: impl FnOnce(
        &'db dyn BaseDb,
        &mut TrackedMapEnvMut<'db>,
        &D,
        &mut Spanner,
        &AstType,
    ) -> Result<MirType, ()>,
) -> Result<Option<MirType>, ()> {
    let Some((span, _trmaps, expanded)) = substitute_macro_tok(db, env, diag, spanner, macro_ctx)?
    else {
        return Ok(None);
    };

    let parse = entrypoint.parse(&expanded);

    // TODO: use trmaps to map error ranges back to the original macro invocation site.
    emit_parse_errors(&parse, &diag, spanner);

    let red_node = parse.red_node();
    let Some(ast) = AstType::cast_node(red_node.clone())
        .or_else(|| red_node.children().find_map(AstType::cast_either))
    else {
        diag.emit(Diagnostic::new(
            span,
            Cow::Owned(format!(
                "Internal compiler error: Couldn't find {} as child of parse's root node, found node kinds {:?} and token kinds {:?}",
                std::any::type_name::<AstType>(),
                parse.red_node().child_nodes().map(|red| red.green.kind).collect::<Vec<_>>(),
                parse.red_node().child_tokens().map(|red| red.green.kind).collect::<Vec<_>>()
            )),
            Severity::Error,
        ));
        return Err(());
    };

    // TODO: remove this closure
    Ok(Some(lower(db, env, diag, spanner, &ast)?))
}

/// Lowers an [`ast::DtPhandle`] to a [`MirPhandleTarget`], if valid.
fn lower_phandle<'db>(
    db: &'db dyn BaseDb,
    env: &mut TrackedMapEnvMut<'db>,
    diag: &impl DiagnosticCollector<File>,
    spanner: &mut impl FnMut(TextRange) -> Span<File>,
    phandle: &ast::DtPhandle,
) -> Result<MirPhandleTarget, ()> {
    use dt_tools_parser::parser::Entrypoint;

    if let Some(macro_inv) = phandle.macro_invocation() {
        // If the phandle has a macro invocation (e.g. `&MACRO(...)`), resolve it.
        Ok(resolve_macro_to_value(
            db,
            env,
            diag,
            spanner,
            &MacroCtx::Explicit(&macro_inv),
            Entrypoint::ReferenceNoamp,
            lower_phandle,
        )?
        .expect("resolve_macro_to_value should not return Ok(None) with an explicit macro"))
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

        Ok(resolve_macro_to_value(
            db,
            env,
            diag,
            spanner,
            &MacroCtx::Implicit(&name_ast),
            Entrypoint::ReferenceNoamp,
            lower_phandle,
        )?
        .unwrap_or(MirPhandleTarget::Label(name_ast.syntax().text().to_owned())))
    }
}

#[cfg(test)]
#[expect(
    clippy::needless_raw_string_hashes,
    reason = "expect-test auto update adds r#"
)]
mod tests {
    use expect_test::{Expect, expect};

    use super::*;
    use crate::db::BaseDb;

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
            out.push_str("\n--- errors ---\n");
            for d in diags {
                use std::fmt::Write;
                let range = d
                    .span
                    .primary_spans
                    .first()
                    .expect("Should have at least one primary span")
                    .text_range;
                let _ = writeln!(out, "{:?} {range}: {}", d.severity, d.msg);
            }
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
            expect![""],
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
                node   / /main.dts 11..16
            "#]],
        );
    }

    #[test]
    fn mir_properties() {
        check_mir(
            r#"
/dts-v1/;
/ { foo = "bar"; baz = <1 2 3>; qux = [ab cd]; };
"#,
            &[],
            expect![[r#"
                node   / /main.dts 11..60
                property = CellList(Bits32([Number(1), Number(2), Number(3)])) /baz /main.dts 28..42
                property = String("bar") /foo /main.dts 15..27
                property = Bytestring([171, 205]) /qux /main.dts 43..57
            "#]],
        );
    }

    #[test]
    fn mir_bits() {
        check_mir(
            r#"
/dts-v1/;
/ { value = /bits/ 64 <1>; };
"#,
            &[],
            expect![[r#"
                node   / /main.dts 11..40
                property = CellList(Bits64([1])) /value /main.dts 15..37
            "#]],
        );
    }

    #[test]
    fn mir_label() {
        check_mir(
            r#"
/dts-v1/;
/ { LBL: node {}; };
"#,
            &[],
            expect![[r#"
                node   / /main.dts 11..31
                node labels=[LBL] /node /main.dts 15..28
            "#]],
        );
    }

    #[test]
    fn mir_extension() {
        check_mir(
            r#"
/dts-v1/;
/ { LBL: node {}; };
&LBL { prop = <1>; };
"#,
            &[],
            expect![[r#"
                node   / /main.dts 11..31
                node labels=[LBL] /node /main.dts 15..28
                node   /node /main.dts 32..53
                property = CellList(Bits32([Number(1)])) /node/prop /main.dts 39..50
            "#]],
        );
    }

    #[test]
    fn mir_extension_unresolved() {
        check_mir(
            r#"
/dts-v1/;
/plugin/;
&UNKNOWN { prop = <1>; };
"#,
            &[],
            expect![[r#"
                --- unresolved ---
                  label=UNKNOWN (1 definitions)
            "#]],
        );
    }

    #[test]
    fn mir_include_preprocessor() {
        check_mir(
            r#"
/dts-v1/;
#include "inc.dtsi"
/ { main_prop = <1>; };
"#,
            &[("/inc.dtsi", r#"/ { inc_prop = <2>; };"#)],
            expect![[r#"
                node   / /inc.dtsi 0..22
                node   / /main.dts 31..54
                property = CellList(Bits32([Number(2)])) /inc_prop /inc.dtsi 4..19
                property = CellList(Bits32([Number(1)])) /main_prop /main.dts 35..51
            "#]],
        );
    }

    #[test]
    fn mir_include_preprocessor_error() {
        check_mir(
            r#"
/dts-v1/;
#include "inc.dtsi"
/ { main_prop = <1>; };
"#,
            &[("/inc.dtsi", r#"/ { inc_prop = <BOGUS>; };"#)],
            expect![[r#"
                node   / /inc.dtsi 0..26
                node   / /main.dts 31..54
                property = CellList(Bits32([])) /inc_prop /inc.dtsi 4..23
                property = CellList(Bits32([Number(1)])) /main_prop /main.dts 35..51

                --- errors ---
                Error 16..21: Macro `BOGUS` is not defined
            "#]],
        );
    }

    #[test]
    fn mir_delete_node_by_name() {
        check_mir(
            r#"
/dts-v1/;
/ { foo {}; /delete-node/ foo; };
"#,
            &[],
            expect![[r#"
                node   / /main.dts 11..44
                node   /foo /main.dts 15..22
                delete-node /foo /main.dts 23..41
            "#]],
        );
    }

    #[test]
    fn mir_delete_node_by_label() {
        check_mir(
            r#"
/dts-v1/;
/ { foo: bar {}; /delete-node/ &foo; };
"#,
            &[],
            expect![[r#"
                node   / /main.dts 11..50
                node labels=[foo] /bar /main.dts 15..27
                delete-node /bar /main.dts 28..47
            "#]],
        );
    }

    #[test]
    fn mir_delete_node_by_path() {
        check_mir(
            r#"
/dts-v1/;
/ { foo {}; /delete-node/ &{/foo}; };
"#,
            &[],
            expect![[r#"
                node   / /main.dts 11..48
                node   /foo /main.dts 15..22
                delete-node /foo /main.dts 23..45
            "#]],
        );
    }

    #[test]
    fn mir_delete_property() {
        check_mir(
            r#"
/dts-v1/;
/ { foo = <1>; /delete-property/ foo; };
"#,
            &[],
            expect![[r#"
                node   / /main.dts 11..51
                property = CellList(Bits32([Number(1)])) /foo /main.dts 15..25
                delete-property /foo /main.dts 26..48
            "#]],
        );
    }

    #[test]
    fn mir_conditional_ifdef() {
        check_mir(
            r#"
/dts-v1/;
#define FLAG
#ifdef FLAG
/ { yes = <1>; };
#else
/ { no = <2>; };
#endif
"#,
            &[],
            expect![[r#"
                node   / /main.dts 36..53
                property = CellList(Bits32([Number(1)])) /yes /main.dts 40..50
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
                node   / /main.dts 50..76
                property = CellList(Bits32([Number(42)])), String("example") /prop /main.dts 54..73
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
                node   / /main.dts 30..51
                node   / /main.dts 52..73
                node labels=[FOO] /foo /main.dts 34..48
                property = CellList(Bits32([Phandle(Label("FOO"))])) /prop /main.dts 56..70
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
                node   / /main.dts 30..49
                node   / /main.dts 50..73
                node labels=[FOO] /foo /main.dts 34..46
                property = CellList(Bits32([Phandle(Label("FOO"))])) /prop /main.dts 54..70
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
                node   / /main.dts 36..54
                node   / /main.dts 55..73
                node   /SUBSTITUTED@bar /main.dts 40..51
                node   /bar@SUBSTITUTED /main.dts 59..70
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
                node   / /main.dts 60..74
                node   /BAR /main.dts 64..71
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
                node   / /main.dts 11..44
                property = CellList(Bits32([])) /prop /main.dts 15..28
                property =  /prop2 /main.dts 29..41

                --- errors ---
                Error 23..26: Macro `VAL` is not defined
                Error 37..40: Macro `VAL` is not defined
            "#]],
        );
    }

    #[test]
    fn mir_undefined_label_extension() {
        check_mir(
            r#"
/dts-v1/;
&BOGUS { };
"#,
            &[],
            expect![[r#"

                --- errors ---
                Error 11..17: Label not found: BOGUS
            "#]],
        );
    }

    #[test]
    fn mir_undefined_label_phandle() {
        check_mir(
            r#"
/dts-v1/;
/ { foo = &BOGUS; };
"#,
            &[],
            expect![[r#"
                node   / /main.dts 11..31
                property = Phandle(Label("BOGUS")) /foo /main.dts 15..28

                --- errors ---
                Error 15..28: Label not found: BOGUS [dt_tools_lowering::check_mir_post]
            "#]],
        );
    }

    #[test]
    fn mir_undefined_path_phandle() {
        check_mir(
            r#"
/dts-v1/;
/ { foo = &{/bar}; };
"#,
            &[],
            expect![[r#"
                node   / /main.dts 11..32
                property = Phandle(Path("/bar")) /foo /main.dts 15..29

                --- errors ---
                Error 15..29: Node at path not found: /bar [dt_tools_lowering::check_mir_post]
            "#]],
        );
    }

    #[test]
    fn mir_duplicate_label() {
        check_mir(
            r#"
/dts-v1/;
/ { foo: bar {}; };
/ { foo: baz {}; };
"#,
            &[],
            expect![[r#"
                node   / /main.dts 11..30
                node   / /main.dts 31..50
                node labels=[foo] /bar /main.dts 15..27
                node   /baz /main.dts 35..47

                --- errors ---
                Warn 35..39: Duplicate label `foo`
            "#]],
        );
    }

    #[test]
    fn mir_num_out_of_bounds() {
        check_mir(
            r#"
/dts-v1/;
/ { prop = <(1 << 32)>; };
"#,
            &[],
            expect![[r#"
                node   / /main.dts 11..37
                property = CellList(Bits32([])) /prop /main.dts 15..34

                --- errors ---
                Error 23..32: number 4294967296 too large to fit in 32-bit signed integer (using two's complement) or 32-bit unsigned integer
            "#]],
        );
    }

    #[test]
    fn mir_num_negative() {
        check_mir(
            r#"
/dts-v1/;
/ { prop = <-1 (-1)>; };
"#,
            &[],
            expect![[r#"
                node   / /main.dts 11..35
                property = CellList(Bits32([Number(1), Number(4294967295)])) /prop /main.dts 15..32

                --- errors ---
                Error 23..24: Expected cell or ‘>’, but found ‘-’ [dt-tools(syntax-error)]
            "#]],
        );
    }

    #[test]
    fn mir_char() {
        check_mir(
            r#"
/dts-v1/;
/ { prop = <'\0' 'a'>; };
"#,
            &[],
            expect![[r#"
                node   / /main.dts 11..36
                property = CellList(Bits32([Number(0), Number(97)])) /prop /main.dts 15..33
            "#]],
        );
    }
}
