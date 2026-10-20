use std::fmt::Write;

use dt_tools_diagnostic::Span;
use dt_tools_lowering::{
    db::BaseDb,
    expand::{ExpandedRoot, expand_root, resolve_range, source_file},
    file::File,
    lowering::lower_root_file,
    mir::{MirDefinition, MirDefinitionValue},
};
use dt_tools_parser::{TextRange, ast::AstNode};
use itertools::Itertools;
use tower_lsp_server::ls_types::{
    Hover, HoverContents, HoverParams, MarkupContent, MarkupKind, Range,
};

use crate::{
    lsp_utils::{offset_to_position, position_to_offset},
    uri_to_path,
};

fn make_hover_info(
    db: &dyn BaseDb,
    file: dt_tools_lowering::file::File,
    markdown: String,
    text_range: Option<TextRange>,
) -> Hover {
    Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: markdown,
        }),
        range: text_range.and_then(|text_range| {
            dt_tools_lowering::rope(db, file).as_ref().and_then(|rope| {
                Some(Range::new(
                    offset_to_position(text_range.start, rope)?,
                    offset_to_position(text_range.end, rope)?,
                ))
            })
        }),
    }
}

pub fn hover(state: &crate::Backend, params: HoverParams) -> Option<Hover> {
    let params = params.text_document_position_params;
    let uri = params.text_document.uri;
    let path = uri_to_path(&uri).expect("Invalid document URI");
    tracing::info!(?path);

    let db = state.session.db.lock().clone();
    let db = &db;
    let file = db.get_files().get_file(db, &path);

    let root_file = state.session.root_file()?;

    let Some(rope) = dt_tools_lowering::rope(db, file) else {
        return None;
    };
    let offset = position_to_offset(params.position, rope)?;

    let cst_tree_section = cst_tree_section(db, file, offset);
    let mir_definition_section = mir_definition_section(db, root_file, file, offset);
    let macro_section = macro_section(db, root_file, file, offset);
    let label_section = label_section(db, root_file, file, offset);
    let include_section = include_section(db, root_file, file, offset);
    let phandle_section = phandle_section(db, root_file, file, offset);

    let section_strs = [
        cst_tree_section.as_ref(),
        mir_definition_section.as_ref(),
        macro_section.as_ref(),
        label_section.as_ref(),
        include_section.as_ref(),
        phandle_section.as_ref(),
    ]
    .into_iter()
    .flatten()
    .map(|tup| tup.0.trim())
    .collect_vec();

    let text_range = [
        mir_definition_section.as_ref(),
        macro_section.as_ref(),
        label_section.as_ref(),
        include_section.as_ref(),
        cst_tree_section.as_ref(),
    ]
    .into_iter()
    .find_map(|opt| opt.map(|tup| tup.1))?;

    Some(make_hover_info(
        db,
        file,
        section_strs.join("\n---\n"),
        Some(text_range),
    ))
}

fn cst_tree_section(db: &dyn BaseDb, file: File, offset: usize) -> Option<(String, TextRange)> {
    let parse = dt_tools_lowering::parse_file(db, file)?;
    let file_ast = parse.parse(db).source_file();
    let cst = file_ast.syntax();
    let token = cst.token_at_offset(offset)?;

    Some((
        format!(
            "CST Tree Info: {} / {:?}",
            {
                let mut vec = token
                    .parent_ancestors()
                    .map(|node| format!("{:?}", node.green.kind))
                    .collect_vec();
                vec.reverse();
                vec.join(" / ")
            },
            token.green.kind,
        ),
        token.text_range(),
    ))
}

fn mir_definition_section(
    db: &dyn BaseDb,
    root_file: File,
    file: File,
    offset: usize,
) -> Option<(String, TextRange)> {
    fn fmt_list(singular: &str, plural: &str, values: &[impl std::fmt::Display]) -> String {
        if values.is_empty() {
            return format!("no {plural}");
        }

        let inflected = if values.len() == 1 { singular } else { plural };

        format!("{inflected} `{}`", values.iter().join(", "))
    }

    fn fmt_mir_def<'db>(
        db: &'db dyn BaseDb,
        expanded: ExpandedRoot<'db>,
        def: &MirDefinition,
        most_precise: &MirDefinition,
        already_defined: &mut bool,
    ) -> String {
        let is_same_path = def.path == most_precise.path;
        let path_part = if is_same_path {
            String::new()
        } else {
            format!("`{}` ", def.path)
        };
        let mut defined = || {
            if *already_defined {
                "redefined"
            } else {
                *already_defined = true;
                "defined"
            }
        };

        let mut s: String = match &def.value {
            MirDefinitionValue::Node(data) => format!(
                "Node {path_part}{} with {} ",
                defined(),
                fmt_list("label", "labels", &data.labels)
            ),
            MirDefinitionValue::Property(data) => format!(
                "Property {path_part}{} with {} ",
                defined(),
                fmt_list("value", "values", &data.values)
            ),
            MirDefinitionValue::DeletedNode => format!("Node {path_part}deleted "),
            MirDefinitionValue::DeletedProperty => format!("Property {path_part}deleted "),
            MirDefinitionValue::V1Directive => "`/dts-v1/;` defined ".to_owned(),
            MirDefinitionValue::PluginDirective => "`/plugin/;` defined ".to_owned(),
        };

        let is_here = def == most_precise;
        let span = resolve_range(db, expanded, def.provenance.text_range)
            .primary_spans
            .first()
            .copied()
            .expect("provenance must have a primary span");
        fmt_span(db, &span, is_here, &mut s);

        s
    }

    let expanded = expand_root(db, root_file)?;
    let mir = lower_root_file(db, root_file)?.mir(db);

    // TODO: unresolved_extensions
    let matching_defs = mir
        .definitions
        .iter()
        .filter(|def| {
            resolve_range(db, expanded, def.provenance.text_range)
                .primary_spans
                .first()
                .is_some_and(|span| {
                    span.file == file && span.text_range.byte_range().contains(&offset)
                })
        })
        .collect_vec();

    let most_precise = *matching_defs.iter().max_by_key(|def| def.path.len())?;
    let primary_span = resolve_range(db, expanded, most_precise.provenance.text_range)
        .primary_spans
        .first()
        .copied()
        .expect("provenance must have a primary span");

    // TODO: get e.g. binding documentation using `compatible`

    let mut already_defined = false;
    Some((
        format!(
            "MIR: `{}`\n\n- {}",
            most_precise.path,
            mir.history(&most_precise.path)
                .map(|def| { fmt_mir_def(db, expanded, def, most_precise, &mut already_defined) })
                .collect_vec()
                .join("\n- ")
        ),
        primary_span.text_range,
    ))
}

fn fmt_span(db: &dyn BaseDb, span: &Span<File>, is_here: bool, s: &mut String) {
    let shorter_path = span.file.shorter_path(db);
    if is_here {
        s.push_str("_here_");
    } else if let Some((line, column)) = span.file.line_column(db, span.text_range.start) {
        // Line and column are 0-based in LSP but 1-based in text and file:// links
        let line = line + 1;
        let column = column + 1;

        let uri = crate::lsp_utils::file_uri(db, span.file);
        let uri = uri.as_str();

        write!(
            s,
            "at [`{shorter_path}:{line}:{column}`]({uri}#L{line}:{column})"
        )
        .ok();
    } else {
        write!(s, "at `{shorter_path}`").ok();
    }
}

/// Shows macro definitions from the preprocessor.
fn macro_section(
    db: &dyn BaseDb,
    root_file: File,
    file: File,
    offset: usize,
) -> Option<(String, TextRange)> {
    let expanded = expand_root(db, root_file)?;

    let def = expanded.macro_defs(db).iter().find(|def| {
        source_file(db, expanded, def.span.file) == file
            && def.span.text_range.byte_range().contains(&offset)
    })?;

    Some((format!("Macro `{}`", def.name), def.span.text_range))
}

/// Shows label definitions.
fn label_section(
    db: &dyn BaseDb,
    root_file: File,
    file: File,
    offset: usize,
) -> Option<(String, TextRange)> {
    let expanded = expand_root(db, root_file)?;
    let result = lower_root_file(db, root_file)?;

    let (name, span) = result
        .labels(db)
        .iter()
        .filter_map(|(name, label)| {
            let label = label.as_ref()?;
            let span = resolve_range(db, expanded, label.span.text_range)
                .primary_spans
                .first()
                .copied()?;
            Some((name, span))
        })
        .find(|(_, span)| span.file == file && span.text_range.byte_range().contains(&offset))?;

    Some((format!("Label `{name}`"), span.text_range))
}

/// Shows resolved includes from the preprocessor.
fn include_section(
    db: &dyn BaseDb,
    root_file: File,
    file: File,
    offset: usize,
) -> Option<(String, TextRange)> {
    let expanded = expand_root(db, root_file)?;

    let edge = expanded.includes(db).iter().find(|edge| {
        source_file(db, expanded, edge.span.file) == file
            && edge.span.text_range.byte_range().contains(&offset)
    })?;
    let target = source_file(db, expanded, edge.file);

    Some((
        format!("Resolved include: `{}`", target.path(db)),
        edge.span.text_range,
    ))
}

fn phandle_section(
    _db: &dyn BaseDb,
    _root_file: File,
    _file: File,
    _offset: usize,
) -> Option<(String, TextRange)> {
    // TODO

    None
}

#[cfg(test)]
mod tests {
    use camino::Utf8PathBuf;
    use dt_tools_lowering::{
        db::{BaseDatabase, BaseDb},
        settings::PreprocessSettings,
    };

    use super::{include_section, macro_section};

    #[test]
    fn shows_preprocessor_macros_and_includes() {
        let db = BaseDatabase::default();
        PreprocessSettings::new(&db, Vec::new(), vec![]);
        let contents = "#define FOO 1\n#include \"a.dtsi\"\n/ { x = <FOO>; };\n";
        let root =
            db.get_files()
                .add_virtual(&db, Utf8PathBuf::from("/main.dts"), contents.to_owned());
        db.get_files()
            .add_virtual(&db, Utf8PathBuf::from("/a.dtsi"), String::new());

        let macro_offset = contents.find("FOO").expect("macro name");
        let (macro_info, _) =
            macro_section(&db, root, root, macro_offset).expect("macro definition");
        assert_eq!(macro_info, "Macro `FOO`");

        let include_offset = contents.find("#include").expect("include directive");
        let (include_info, _) =
            include_section(&db, root, root, include_offset).expect("include directive");
        assert_eq!(include_info, "Resolved include: `/a.dtsi`");
    }
}
