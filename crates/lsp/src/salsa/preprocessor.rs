//! Preprocessor macros, conditionals and includes are all tied very much together, which means they must be evaluated in order.
//!
//! This module also handles [`ast::DtNode`] processing and MIR emission.
// TODO: split to multiple modules!
// TODO: handle recursive macros?

use std::borrow::Cow;

use camino::{Utf8Path, Utf8PathBuf};
use dt_tools_analyzer::macros::MacroDefinition;
use dt_tools_diagnostic::{
    Diagnostic, DiagnosticCollector, MultiSpan, OffsetDiagnosticCollector, Severity,
};
use dt_tools_parser::{
    TextRange,
    ast::{
        self, AstNode, AstNodeOrToken, AstToken, HasDtPhandle, HasLabel, HasMacroInvocation,
        HasName,
    },
    lexer::TokenKind,
    parser::Entrypoint,
};

use crate::salsa::{
    db::BaseDb,
    includes::IncludeDirs,
    macros::env::{TrackedMapEnv, TrackedMapEnvMut},
    mir::{
        Mir, MirCell, MirDefinition, MirDefinitionValue, MirNodeData, MirPhandleTarget,
        MirPropertyData, MirProvenance, MirValue, UnresolvedExtension,
    },
    tag_diagnostic,
};

#[salsa::tracked]
pub struct PpEvalFileResult<'db> {
    /// Macro and label environment after processing this file.
    #[tracked]
    pub env_after: TrackedMapEnv<'db>,

    #[tracked]
    #[returns(ref)]
    pub diagnostics: Vec<Diagnostic>,

    #[tracked]
    #[returns(ref)]
    pub mir: Mir,

    /// Whether this file is part of a plugin/overlay (`/plugin/;`).
    pub is_overlay: bool,
}

/// Lists possible file paths under `parent_path` or `include_dirs`, depending on [`relative`](Self::relative).
pub fn possible_include_paths_utf8<'a, P: AsRef<Utf8Path>>(
    relative: bool,
    path: &'a str,
    parent_path: &'a Utf8Path,
    include_dirs: &'a [P],
) -> impl Iterator<Item = Utf8PathBuf> + use<'a, P> {
    relative
        .then_some(parent_path)
        .into_iter()
        .chain(include_dirs.iter().map(AsRef::as_ref))
        .map(move |base_path| base_path.join(path))
}

fn get_pp_directive_args(
    input: &str,
    directive: &str,
    text_range: TextRange,
) -> (String, TextRange) {
    debug_assert!(input.starts_with('#'));
    let s = input
        .get(1..)
        .expect("lexer safe")
        .trim_start_matches([' ', '\t']);

    debug_assert!(s.starts_with(directive));
    let s = s
        .get(directive.len()..)
        .expect("lexer safe")
        .trim_start_matches([' ', '\t']);

    let start_offset = subslice_offset(input, s).unwrap();

    // Newlines are escaped outside comments. Let's undo that to make the text parseable.
    let s = s.replace("\\\n", " \n");

    debug_assert_eq!(text_range.start + input.len(), text_range.end);
    (
        s,
        TextRange {
            start: text_range.start + start_offset,
            end: text_range.end,
        },
    )
}
#[cfg(test)]
#[test]
fn test_get_pp_directive_args() {
    let input = "#include a";
    assert_eq!(
        get_pp_directive_args(
            input,
            "include",
            TextRange {
                start: 0,
                end: input.len()
            }
        ),
        (
            "a".to_owned(),
            TextRange {
                start: "#include ".len(),
                end: input.len()
            }
        )
    );
}

fn subslice_offset(this: &str, inner: &str) -> Option<usize> {
    let self_beg = this.as_ptr() as usize;
    let inner = inner.as_ptr() as usize;
    if inner < self_beg || inner > self_beg.wrapping_add(this.len()) {
        None
    } else {
        Some(inner.wrapping_sub(self_beg))
    }
}
#[cfg(test)]
#[test]
fn test_subslice_offset() {
    let s = "foo";
    assert_eq!(subslice_offset(s, s.to_owned().as_str()), None);
    assert_eq!(subslice_offset(s, &s[0..]), Some(0));
    assert_eq!(subslice_offset(s, &s[1..]), Some(1));
}

fn get_pp_directive_arg_string(
    input: &str,
    directive: &str,
    text_range: TextRange,
    diag: &impl DiagnosticCollector,
) -> Option<(bool, String)> {
    // TODO: macro substitution in #include

    let (args, args_text_range) = get_pp_directive_args(input, directive, text_range);

    let (relative, split) = match args.as_bytes().first() {
        Some(b'<') => (false, args.get(1..).expect("safe").split_once('>')),
        Some(b'"') => (true, args.get(1..).expect("safe").split_once('"')),
        None => {
            diag.emit(Diagnostic::new(
                args_text_range,
                Cow::Borrowed("Expected an argument"),
                Severity::Error,
            ));
            return None;
        }
        _ => {
            diag.emit(Diagnostic::new(
                args_text_range,
                Cow::Borrowed("Unexpected character in argument, expected `\"` or `<`."),
                Severity::Error,
            ));
            return None;
        }
    };

    let Some((s, rest)) = split else {
        diag.emit(Diagnostic::new(
            args_text_range,
            Cow::Borrowed("Missing string terminator"),
            Severity::Error,
        ));
        return None;
    };

    if !rest.is_empty() {
        diag.emit(Diagnostic::new(
            args_text_range,
            Cow::Borrowed("Unexpected characters after include string"),
            Severity::Warn,
        ));
    }

    Some((relative, s.to_owned()))
}

fn pp_cond_directive_eval(
    db: &dyn BaseDb,
    env: &mut TrackedMapEnvMut,
    directive: &ast::PreprocessorDirective,
    diag: &impl DiagnosticCollector,
) -> Option<bool> {
    let input = directive.syntax().text();

    let directive_text_range = directive.syntax().text_range();

    let Some(directive_name) = directive.syntax().green.kind.preprocessor_directive_name() else {
        diag.emit(Diagnostic::new(
            directive_text_range,
            Cow::Borrowed("Internal compiler error: preprocessor directive should have a name"),
            Severity::Warn,
        ));
        return None;
    };

    let (mut condition, mut condition_text_range) =
        get_pp_directive_args(input, directive_name, directive_text_range);

    if directive_name == "ifdef" || directive_name == "elifdef" {
        // TODO: add a test for this and preprocessor directives in general
        condition.insert_str(0, "defined(");
        condition.push(')');

        // TODO: proper trmaps...
        condition_text_range =
            condition_text_range.offset_wrapping_signed(-"defined(".len().cast_signed());
        condition_text_range.end -= 1;
    } else if directive_name == "ifndef" || directive_name == "elifndef" {
        condition.insert_str(0, "!defined(");
        condition.push(')');

        condition_text_range =
            condition_text_range.offset_wrapping_signed(-"!defined(".len().cast_signed());
        condition_text_range.end -= 1;
    } else if directive_name == "else" {
        if !condition.is_empty() {
            diag.emit(Diagnostic::new(
                condition_text_range,
                Cow::Borrowed("Extra arguments to `#else`"),
                Severity::Warn,
            ));
        }

        // Else branch is always enabled
        return Some(true);
    }

    let parse = dt_tools_parser::parser::Entrypoint::PreprocessorConditional.parse(&condition);

    let offset_diag = OffsetDiagnosticCollector {
        inner: diag,
        offset: condition_text_range.start,
    };

    if !parse.lex_errors.is_empty() || !parse.errors.is_empty() {
        let earliest_lex_error_range = parse.lex_errors.first().map(|e| e.text_range);
        for lex_error in &parse.lex_errors {
            offset_diag.emit(Diagnostic::new(
                lex_error.text_range,
                format!("{} [lex error]", lex_error.inner).into(),
                Severity::Error,
            ));
        }

        for error in &parse.errors {
            if let Some(earliest_lex_error_range) = earliest_lex_error_range
                && error.primary_span.start >= earliest_lex_error_range.start
            {
                break;
            }

            let mut diagnostic = Diagnostic {
                span: MultiSpan {
                    primary_spans: vec![error.primary_span],
                    span_labels: error.span_labels.clone(),
                },
                msg: error.message.clone(),
                severity: Severity::Error,
            };
            super::tag_diagnostic(&mut diagnostic, "dt-tools(syntax-error)");
            offset_diag.emit(diagnostic);
        }
        return None;
    }

    let expr_ast = parse.red_node().child_nodes().find_map(ast::Expr::cast)?;

    let val = super::expr_eval::eval(db, env, expr_ast, &offset_diag)?;

    Some(val != 0)
}

// TODO: rename this function as it actually returns MIR and does everything in "one pass"
/// Walks toplevel items of a file, evaluating necessary preprocessor directives and macros.
///
/// Returns `None` if the file doesn't exist.
///
/// # Algorithm
///
/// - Keep a [`MacroEnv`].
/// - For each toplevel item:
///   - Update the macro environment as appropriate.
///   - On an include, recurse `eval_outline` because we have to know the next macro env.
///   - On a conditional, evalute the conditions of the branches and choose the correct branch, if any.
///
/// # Panics
///
/// Will panic if [`IncludeDirs`] hasn't been defined.
#[salsa::tracked]
pub fn preprocessor_eval_file<'db>(
    db: &'db dyn BaseDb,
    file: super::file::File,
    parent_env: Option<TrackedMapEnv<'db>>,
    is_overlay: bool,
) -> Option<PpEvalFileResult<'db>> {
    let mut env = TrackedMapEnvMut::from_parent(parent_env);

    let parse = super::parse_file(db, file)?;
    let file_ast = parse.parse(db).source_file();

    let mut diagnostics = Vec::new();
    let diag = parking_lot::Mutex::new(&mut diagnostics);

    // Path of the current file's parent directory.
    let parent_path = file.path(db).parent()?;
    let include_dirs = IncludeDirs::get(db).include_dirs(db);

    let mut mir = Mir::default();

    for item in file_ast.items() {
        handle_toplevel_item(
            db,
            file,
            &mut env,
            &diag,
            parent_path,
            include_dirs,
            &mut mir,
            is_overlay,
            item,
        );
    }

    super::tag_diagnostics(
        &mut diagnostics,
        concat!(module_path!(), "::preprocessor_eval_file"),
    );

    Some(PpEvalFileResult::new(
        db,
        env.into_immut(db),
        diagnostics,
        mir,
        is_overlay,
    ))
}

#[expect(clippy::too_many_lines, reason = "hard to make this shorter")]
fn handle_toplevel_item<'db>(
    db: &'db dyn BaseDb,
    file: super::file::File,
    env: &mut TrackedMapEnvMut<'db>,
    diag: &impl DiagnosticCollector,
    parent_path: &Utf8Path,
    include_dirs: &[Utf8PathBuf],
    mir: &mut Mir,
    is_overlay: bool,
    item: ast::ToplevelItem,
) {
    let files = db.get_files();

    let text_range = item.syntax_item().text_range();

    match item {
        ast::ToplevelItem::Node(dt_node) => {
            process_dt_node(db, file, env, diag, mir, is_overlay, "", &dt_node);
        }
        ast::ToplevelItem::Directive(dir) => {
            handle_directive(
                db,
                file,
                env,
                diag,
                mir,
                is_overlay,
                "",
                &dir,
                include_dirs,
                parent_path,
                files,
            );
        }
        ast::ToplevelItem::PreprocessorConditional(preprocessor_conditional) => {
            let Some((_dir, branch)) =
                preprocessor_conditional.branches().find(|(dir, _branch)| {
                    pp_cond_directive_eval(db, env, dir, &diag).is_some_and(|val| val)
                })
            else {
                return;
            };
            for item in branch.items() {
                handle_toplevel_item(
                    db,
                    file,
                    env,
                    diag,
                    parent_path,
                    include_dirs,
                    mir,
                    is_overlay,
                    item,
                );
            }
        }
        ast::ToplevelItem::PreprocessorDirective(dir) => match dir.kind() {
            TokenKind::PragmaDirective => {
                // TODO: implement #pragma
                diag.emit(Diagnostic::new(
                    text_range,
                    Cow::Borrowed("`#pragma` is unimplemented"),
                    Severity::Error,
                ));
            }
            TokenKind::DefineDirective => match MacroDefinition::parse(dir.syntax().text()) {
                Ok(parsed) => env.insert_macro(parsed),
                Err(err) => {
                    diag.emit(Diagnostic::new(
                        if let Some(local_range) = err.text_range() {
                            local_range.offset(text_range.start)
                        } else {
                            text_range
                        },
                        Cow::Owned(err.to_string()),
                        Severity::Error,
                    ));
                }
            },
            TokenKind::UndefDirective => {
                let input = dir.syntax().text();
                let (args, args_text_range) = get_pp_directive_args(input, "undef", text_range);
                if args.contains(' ') {
                    diag.emit(Diagnostic::new(
                        args_text_range,
                        Cow::Borrowed("Arguments to `#undef` should be just a macro name"),
                        Severity::Error,
                    ));
                } else {
                    env.own_macro_map.insert(args, None);
                }
            }
            TokenKind::IncludeDirective => {
                // TODO: real evaluation and macro substitution
                let input = dir.syntax().text();
                let Some((relative, path)) =
                    get_pp_directive_arg_string(input, "include", text_range, &diag)
                else {
                    return;
                };
                let Some(file) =
                    possible_include_paths_utf8(relative, &path, parent_path, include_dirs)
                        .map(|path| files.get_file(db, &path))
                        .find(|file| file.is_readable_file(db))
                else {
                    diag.emit(Diagnostic::new(
                        text_range,
                        Cow::Owned(format!(
                            "Couldn't find file to include from include_dirs={include_dirs:?}"
                        )),
                        Severity::Error,
                    ));
                    return;
                };
                let result = preprocessor_eval_file(
                    db,
                    file,
                    Some(std::mem::take(env).into_immut(db)),
                    is_overlay,
                )
                .expect("The file should exist, its existence is confirmed above");
                *env = result.env_after(db).to_mut();
                mir.merge(result.mir(db));
            }
            TokenKind::ErrorDirective => {
                let input = dir.syntax().text();
                let (args, _args_text_range) = get_pp_directive_args(input, "error", text_range);
                // TODO: remove debug thing?
                diag.emit(Diagnostic::new(
                    text_range,
                    Cow::Owned(format!(
                        "`#error`: {args:?}, defined {args:?}={}",
                        env.get_macro(db, &args).is_some()
                    )),
                    Severity::Error,
                ));
            }
            _ => {}
        },
    }

    // TODO: PERF: Salsa tracked? currently this breaks all Salsa tracking...
    env.flatten_ancestors(db);
}

/// Handle a DTS directive: `/include/`, `/delete-node/`, `/delete-property/`.
fn handle_directive<'db>(
    db: &'db dyn BaseDb,
    file: super::file::File,
    env: &mut TrackedMapEnvMut<'db>,
    diag: &impl DiagnosticCollector,
    mir: &mut Mir,
    is_overlay: bool,
    path_prefix: &str,
    dir: &ast::Directive,
    include_dirs: &[Utf8PathBuf],
    parent_path: &Utf8Path,
    files: &crate::salsa::file::Files,
) {
    let tokens: Vec<_> = dir.syntax().child_tokens().collect();
    let text_range = dir.syntax().text_range();

    if tokens
        .iter()
        .any(|tok| tok.green.kind == TokenKind::DtIncludeDirective)
    {
        // /include/ "path"
        let Some(string_tok) = tokens
            .iter()
            .find(|tok| tok.green.kind == TokenKind::String)
        else {
            return;
        };
        let include_path =
            match dt_tools_analyzer::string::interpret_escaped_string(string_tok.text()) {
                Ok(path) => path,
                Err(err) => {
                    diag.emit(Diagnostic::new(
                        string_tok.text_range(),
                        Cow::Owned(format!("Failed to parse string: {err}")),
                        Severity::Error,
                    ));
                    return;
                }
            };
        let Some(include_file) =
            possible_include_paths_utf8(true, &include_path, parent_path, include_dirs)
                .map(|path| files.get_file(db, &path))
                .find(|f| f.is_readable_file(db))
        else {
            diag.emit(Diagnostic::new(
                text_range,
                Cow::Owned(format!("Couldn't find file to include: {include_path}")),
                Severity::Error,
            ));
            return;
        };
        let result = preprocessor_eval_file(
            db,
            include_file,
            Some(std::mem::take(env).into_immut(db)),
            is_overlay,
        )
        .expect("file exists");
        *env = result.env_after(db).to_mut();
        mir.merge(result.mir(db));
    } else {
        emit_delete_directive(db, file, env, diag, mir, is_overlay, path_prefix, dir);
    }
}

/// Build a node/property path. Handles the root node `/` and avoids `//`.
fn build_path(parent: &str, name: &str) -> String {
    if name == "/" {
        "/".to_owned()
    } else if parent == "/" {
        format!("/{name}")
    } else {
        format!("{parent}/{name}")
    }
}

/// Process an [`ast::DtNode`] and its subtree into flat [`MirDefinition`]s.
fn process_dt_node<'db>(
    db: &'db dyn BaseDb,
    file: super::file::File,
    env: &mut TrackedMapEnvMut<'db>,
    diag: &impl DiagnosticCollector,
    mir: &mut Mir,
    is_overlay: bool,
    path_prefix: &str,
    dt_node: &ast::DtNode,
) {
    let text_range = dt_node.syntax().text_range();
    let provenance = MirProvenance { file, text_range };

    // Handle extensions: &label { } or &{/path} { }
    if let Some(phandle) = dt_node.extension_name() {
        let Some(target) = convert_phandle(db, env, diag, &phandle) else {
            return;
        };

        let target_path = match &target {
            MirPhandleTarget::Label(name) => {
                env.get_label(db, name).map(std::borrow::ToOwned::to_owned)
            }
            MirPhandleTarget::Path(path) => Some(path.clone()),
        };
        if let Some(ref target_path) = target_path {
            process_dt_node_body(db, file, env, diag, mir, is_overlay, target_path, dt_node);
        } else if is_overlay {
            let mut body_mir = Mir::default();
            process_dt_node_body(db, file, env, diag, &mut body_mir, is_overlay, "", dt_node);
            // Propagate any nested unresolved extensions.
            mir.unresolved_extensions
                .extend(body_mir.unresolved_extensions);
            if let MirPhandleTarget::Label(label) = target {
                mir.unresolved_extensions.push(UnresolvedExtension {
                    label,
                    body: body_mir.definitions,
                    provenance: provenance.clone(),
                });
            }
        } else {
            diag.emit(Diagnostic::new(
                text_range,
                Cow::Owned(format!("Label not found: {target}")),
                Severity::Error,
            ));
        }
        return;
    }

    // Build the node's full path.
    let name = dt_node
        .text_name("")
        .map(Cow::into_owned)
        .unwrap_or_default();
    let node_path = build_path(path_prefix, &name);

    // Collect labels defined on this node.
    let mut labels: Vec<String> = Vec::new();
    if let Some(label) = dt_node.label()
        && let Some(label_name) = label.name()
    {
        let label_text = label_name.syntax().text().to_string();
        // Check for duplicate labels (DTC: no duplicates globally).
        if env.get_macro(db, &label_text).is_some() {
            // TODO: MultiSpan & cross-file diagnostics
            diag.emit(Diagnostic::new(
                label_name.syntax().text_range(),
                Cow::Owned(format!("Duplicate label `{label_text}`")),
                Severity::Error,
            ));
        } else {
            env.own_label_map
                .insert(label_text.clone(), Some(node_path.clone()));
            labels.push(label_text);
            dbg!(&env.own_label_map);
        }
    }

    // Emit the node definition.
    mir.definitions.push(MirDefinition {
        path: node_path.clone(),
        value: MirDefinitionValue::Node(MirNodeData { labels }),
        provenance,
    });

    // Process the node body: subnodes and properties.
    process_dt_node_body(db, file, env, diag, mir, is_overlay, &node_path, dt_node);
}

/// Process the body of an [`ast::DtNode`]: subnodes and properties.
fn process_dt_node_body<'db>(
    db: &'db dyn BaseDb,
    file: super::file::File,
    env: &mut TrackedMapEnvMut<'db>,
    diag: &impl DiagnosticCollector,
    mir: &mut Mir,
    is_overlay: bool,
    parent_path: &str,
    dt_node: &ast::DtNode,
) {
    // TODO: preprocessor conditionals in nodes

    for item in dt_node.node_items() {
        match item {
            ast::NodeItem::DtProperty(prop) => {
                if let Some(def) = process_dt_property(db, file, env, diag, parent_path, &prop) {
                    mir.definitions.push(def);
                }
            }
            ast::NodeItem::DtNode(subnode) => {
                process_dt_node(db, file, env, diag, mir, is_overlay, parent_path, &subnode);
            }
            ast::NodeItem::Directive(dir) => {
                emit_delete_directive(db, file, env, diag, mir, is_overlay, parent_path, &dir);
            }
        }
    }
}

/// Emit [`MirDefinitionValue::DeletedNode`] or [`MirDefinitionValue::DeletedProperty`] from a directive.
fn emit_delete_directive<'db>(
    db: &'db dyn BaseDb,
    file: super::file::File,
    env: &mut TrackedMapEnvMut<'db>,
    diag: &impl DiagnosticCollector,
    mir: &mut Mir,
    is_overlay: bool,
    path_prefix: &str,
    dir: &ast::Directive,
) {
    let kind = dir.kind();
    let Some(args) = dir.arguments() else { return };

    let text_range = dir.syntax().text_range();
    let provenance = MirProvenance { file, text_range };

    if kind == Some(TokenKind::DeleteNodeDirective) {
        let target_path = if let Some(name_tok) = args.name() {
            build_path(path_prefix, name_tok.syntax().text().as_str())
        } else if let Some(phandle) = args.dt_phandle() {
            let Some(target) = convert_phandle(db, env, diag, &phandle) else {
                return;
            };

            match &target {
                MirPhandleTarget::Label(name) => {
                    if let Some(path) = env.get_label(db, name) {
                        path.to_owned()
                    } else {
                        // Couldn't resolve it.
                        if is_overlay {
                            // TODO: handle unresolved delete-node!
                        } else {
                            dbg!(&env.own_label_map);
                            diag.emit(Diagnostic::new(
                                text_range,
                                Cow::Owned(format!("Label not found: {name}")),
                                Severity::Error,
                            ));
                        }
                        return;
                    }
                }
                MirPhandleTarget::Path(path) => path.clone(),
            }
        } else {
            return;
        };

        mir.definitions.push(MirDefinition {
            path: target_path,
            value: MirDefinitionValue::DeletedNode,
            provenance,
        });
    } else if kind == Some(TokenKind::DeletePropertyDirective)
        && let Some(name_tok) = args.name()
    {
        let target_path = build_path(path_prefix, name_tok.syntax().text().as_str());

        mir.definitions.push(MirDefinition {
            path: target_path,
            value: MirDefinitionValue::DeletedProperty,
            provenance,
        });
    }
}

/// Convert a [`ast::DtProperty`] into a [`MirDefinition`], if valid.
///
/// Returns `None` if the property has no name or can't be processed.
fn process_dt_property<'db>(
    db: &'db dyn BaseDb,
    file: super::file::File,
    env: &mut TrackedMapEnvMut<'db>,
    diag: &impl DiagnosticCollector,
    parent_path: &str,
    prop: &ast::DtProperty,
) -> Option<MirDefinition> {
    let name_ast = prop.name()?;
    let name = name_ast.syntax().text().to_string();
    let path = build_path(parent_path, &name);

    let text_range = prop.syntax().text_range();
    let provenance = MirProvenance { file, text_range };

    let mut values = Vec::new();
    for value_ast in prop.values() {
        if let Some(value) = convert_prop_value(db, env, diag, &value_ast) {
            values.push(value);
        }
    }

    Some(MirDefinition {
        path,
        value: MirDefinitionValue::Property(MirPropertyData { values }),
        provenance,
    })
}

/// Convert an AST property value to a [`MirValue`].
fn convert_prop_value<'db>(
    db: &'db dyn BaseDb,
    env: &mut TrackedMapEnvMut<'db>,
    diag: &impl DiagnosticCollector,
    value: &ast::PropValue,
) -> Option<MirValue> {
    match value {
        ast::PropValue::String(tok) => {
            match dt_tools_analyzer::string::interpret_escaped_string(tok.text()) {
                Ok(path) => Some(MirValue::String(path)),
                Err(err) => {
                    diag.emit(Diagnostic::new(
                        tok.text_range(),
                        Cow::Owned(format!("Failed to parse string: {err}")),
                        Severity::Error,
                    ));
                    None
                }
            }
        }
        ast::PropValue::CellList(cell_list) => {
            let cells: Vec<MirCell> = cell_list
                .cells()
                .filter_map(|cell| convert_cell(db, env, diag, &cell))
                .collect();
            Some(MirValue::CellList(cells))
        }
        ast::PropValue::Bytestring(tok) => {
            let mut bytes = Vec::new();
            let mut first_nibble = None;
            for ch in tok.text().chars() {
                if ch == ']' {
                    break;
                }
                #[expect(
                    clippy::cast_possible_truncation,
                    reason = "to_digit returns values 0-15"
                )]
                if let Some(nibble) = ch.to_digit(16) {
                    let nibble = nibble as u8;
                    if let Some(first_nibble) = first_nibble.take() {
                        bytes.push(nibble + (first_nibble << 4));
                    } else {
                        first_nibble = Some(nibble);
                    }
                }
            }

            if first_nibble.is_some() {
                diag.emit(Diagnostic::new(
                    tok.text_range(),
                    Cow::Borrowed("Bytestring is missing a hex digit"),
                    Severity::Error,
                ));
                return None;
            }

            Some(MirValue::Bytestring(bytes))
        }
        ast::PropValue::Phandle(phandle) => {
            let target = convert_phandle(db, env, diag, phandle)?;
            Some(MirValue::Phandle(target))
        }
        ast::PropValue::Macro(macro_inv) => resolve_macro_to_value(
            db,
            env,
            diag,
            &MacroCtx::Explicit(macro_inv),
            Entrypoint::PropValues,
            convert_prop_value,
        ),
    }
}

/// Convert an AST cell to a [`MirCell`].
fn convert_cell<'db>(
    db: &'db dyn BaseDb,
    env: &mut TrackedMapEnvMut<'db>,
    diag: &impl DiagnosticCollector,
    cell: &ast::Cell,
) -> Option<MirCell> {
    match cell {
        ast::Cell::Number(tok) => {
            // TODO: combine with expr
            let src = tok.text();
            src.parse()
                .ok()
                .or_else(|| {
                    src.strip_prefix("0x")
                        .or_else(|| src.strip_prefix("0X"))
                        .and_then(|s| u32::from_str_radix(s, 16).ok())
                })
                .map(MirCell::U32)
            // TODO: error handling?!
        }
        ast::Cell::Phandle(phandle) => {
            Some(MirCell::Phandle(convert_phandle(db, env, diag, phandle)?))
        }
        ast::Cell::Macro(macro_inv) => resolve_macro_to_value(
            db,
            env,
            diag,
            &MacroCtx::Explicit(macro_inv),
            Entrypoint::Cells,
            convert_cell,
        ),
        // TODO: exprs / DtExpr. Remember to error when it's over u32::MAX or under u32::MIN
    }
}

enum MacroCtx<'a> {
    Explicit(&'a ast::MacroInvocation),
    Implicit {
        name: &'a str,
        text_range: TextRange,
    },
}

/// Resolve a macro and reparse the result.
///
/// Returns `None` if the macro doesn't exist or there is some other error.
fn resolve_macro_to_value<'db, AstType: AstNodeOrToken, MirType, D: DiagnosticCollector>(
    db: &'db dyn BaseDb,
    env: &mut TrackedMapEnvMut<'db>,
    diag: &D,
    macro_ctx: &MacroCtx,
    entrypoint: Entrypoint,
    convert: impl FnOnce(&'db dyn BaseDb, &mut TrackedMapEnvMut<'db>, &D, &AstType) -> Option<MirType>,
) -> Option<MirType> {
    let (name, text_range) = match &macro_ctx {
        MacroCtx::Explicit(inv) => (inv.green_ident()?.text.as_str(), inv.syntax().text_range()),
        MacroCtx::Implicit { name, text_range } => (*name, *text_range),
    };

    let Some(def) = env.get_macro(db, name) else {
        let is_explicit_macro = matches!(macro_ctx, MacroCtx::Explicit(_));
        if is_explicit_macro {
            diag.emit(Diagnostic::new(
                text_range,
                Cow::Owned(format!("Macro `{name}` is not defined")),
                Severity::Error,
            ));
        }
        return None;
    };

    // TODO: use trmaps for error range mapping.

    let (_trmaps, expanded) = match dt_tools_analyzer::macros::substitute_macro_ast(
        match macro_ctx {
            MacroCtx::Explicit(inv) => Some(inv),
            _ => None,
        },
        def,
    ) {
        Ok(result) => result,
        Err(err) => {
            diag.emit(Diagnostic::new(
                text_range,
                Cow::Owned(err),
                Severity::Error,
            ));
            return None;
        }
    };
    let parse = entrypoint.parse(&expanded);

    // TODO: use trmaps to map error ranges back to the original macro invocation site.
    // TODO: multi-file errors!!

    let earliest_lex_error_range = parse.lex_errors.first().map(|e| e.text_range);
    for lex_error in &parse.lex_errors {
        diag.emit(Diagnostic::new(
            lex_error.text_range,
            format!("{} [lex error]", lex_error.inner).into(),
            Severity::Error,
        ));
    }

    for error in &parse.errors {
        if let Some(earliest_lex_error_range) = earliest_lex_error_range
            && error.primary_span.start >= earliest_lex_error_range.start
        {
            break;
        }

        let mut diagnostic = Diagnostic {
            span: MultiSpan {
                primary_spans: vec![error.primary_span],
                span_labels: error.span_labels.clone(),
            },
            msg: error.message.clone(),
            severity: Severity::Error,
        };
        tag_diagnostic(&mut diagnostic, "dt-tools(syntax-error)");
        diag.emit(diagnostic);
    }

    let Some(ast) = parse.red_node().children().find_map(AstType::cast_either) else {
        diag.emit(Diagnostic::new(
            text_range,
            Cow::Owned(format!(
                "Couldn't find {} as child of parse's root node",
                std::any::type_name::<AstType>()
            )),
            Severity::Error,
        ));
        return None;
    };

    convert(db, env, diag, &ast)
}

/// Convert an AST phandle to a [`MirPhandleTarget`].
fn convert_phandle<'db>(
    db: &'db dyn BaseDb,
    env: &mut TrackedMapEnvMut<'db>,
    diag: &impl DiagnosticCollector,
    phandle: &ast::DtPhandle,
) -> Option<MirPhandleTarget> {
    use dt_tools_parser::parser::Entrypoint;

    if let Some(macro_inv) = phandle.macro_invocation() {
        // If the phandle has a macro invocation (e.g. `&MACRO(...)`), resolve it.
        resolve_macro_to_value(
            db,
            env,
            diag,
            &MacroCtx::Explicit(&macro_inv),
            Entrypoint::ReferenceNoamp,
            convert_phandle,
        )
    } else if phandle.is_path() {
        // &{/path/to/node}
        // NOTE: very naive, just strips the prefix/suffix from the source text.
        // A proper implementation would use the CST structure.
        let raw = phandle.syntax().green.text();
        let inner = raw.strip_prefix("&{")?.strip_suffix('}')?;

        Some(MirPhandleTarget::Path(inner.to_owned()))
    } else {
        // No explicit macro invocation, but the name itself might still be a macro
        // (e.g. `#define UART_1 soc/uart` and `&UART_1`).
        let name_ast = phandle.name()?;
        let name_text = name_ast.syntax().text().to_string();

        Some(
            resolve_macro_to_value(
                db,
                env,
                diag,
                &MacroCtx::Implicit {
                    name: name_text.as_str(),
                    text_range: name_ast.syntax().text_range(),
                },
                Entrypoint::ReferenceNoamp,
                convert_phandle,
            )
            .unwrap_or(MirPhandleTarget::Label(name_text)),
        )
    }
}

#[cfg(test)]
#[expect(
    clippy::needless_raw_string_hashes,
    reason = "expect-test auto update adds r#"
)]
mod tests {
    use crate::salsa::db::BaseDb;
    use expect_test::{Expect, expect};

    use super::*;

    /// Run the preprocessor on virtual files and snapshot MIR + diagnostics.
    ///
    /// The main file is named "/main.dts".
    #[expect(clippy::needless_pass_by_value, reason = "ergonomics")]
    fn check_mir(main_file_contents: &str, other_files: &[(&str, &str)], expect: Expect) {
        let db = crate::salsa::db::BaseDatabase::default();
        IncludeDirs::new(&db, vec![]);

        let main_file = db.get_files().add_virtual(
            &db,
            Utf8PathBuf::from("/main.dts"),
            main_file_contents.to_owned(),
        );

        for &(path, contents) in other_files {
            db.get_files()
                .add_virtual(&db, Utf8PathBuf::from(path), contents.to_owned());
        }

        let is_overlay = crate::salsa::parse_file(&db, main_file).is_some_and(|p| {
            p.parse(&db).source_file().directives().any(|dir| {
                dir.syntax()
                    .child_tokens()
                    .any(|tok| tok.green.kind == dt_tools_parser::lexer::TokenKind::PluginDirective)
            })
        });

        let result = preprocessor_eval_file(&db, main_file, None, is_overlay)
            .expect("Should be a readable file");
        let mir = result.mir(&db);
        let diags = result.diagnostics(&db);

        let mut out = mir.fmt_for_test(&db);
        if !diags.is_empty() {
            out.push_str("\n--- errors ---\n");
            for d in diags {
                use std::fmt::Write;
                let range = d
                    .span
                    .primary_spans
                    .first()
                    .expect("Should have at least one primary span");
                let _ = writeln!(out, "{:?} {range}: {}", d.severity, d.msg);
            }
        }
        expect.assert_eq(&out);
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
                property = CellList([U32(1), U32(2), U32(3)]) /baz /main.dts 28..42
                property = String("bar") /foo /main.dts 15..27
                property = Bytestring([171, 205]) /qux /main.dts 43..57
            "#]],
        );
    }

    #[test]
    fn mir_label() {
        check_mir(
            r#"
/dts-v1/;
LBL: node {};
"#,
            &[],
            expect![[r#"
                node labels=[LBL] /node /main.dts 11..24
            "#]],
        );
    }

    #[test]
    fn mir_extension() {
        check_mir(
            r#"
/dts-v1/;
LBL: node {};
&LBL { prop = <1>; };
"#,
            &[],
            expect![[r#"
                node labels=[LBL] /node /main.dts 11..24
                property = CellList([U32(1)]) /node/prop /main.dts 32..43
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
                property = CellList([U32(2)]) /inc_prop /inc.dtsi 4..19
                property = CellList([U32(1)]) /main_prop /main.dts 35..51
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
                property = CellList([U32(1)]) /foo /main.dts 15..25
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
                property = CellList([U32(1)]) /yes /main.dts 40..50
            "#]],
        );
    }

    #[test]
    fn mir_macro_in_property() {
        check_mir(
            r#"
/dts-v1/;
#define VAL 42
/ { prop = <VAL>; };
"#,
            &[],
            expect![[r#"
                node   / /main.dts 26..46
                property = CellList([U32(42)]) /prop /main.dts 30..43
            "#]],
        );
    }

    #[test]
    fn mir_undefined_label_error() {
        check_mir(
            r#"
/dts-v1/;
&BOGUS { };
"#,
            &[],
            expect![[r#"

                --- errors ---
                Error 11..22: Label not found: &BOGUS [dt_tools_lsp::salsa::preprocessor::preprocessor_eval_file]
            "#]],
        );
    }
}
