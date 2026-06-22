use std::fmt::Write;

use crate::{
    lsp_utils::{offset_to_position, position_to_offset},
    salsa::{
        db::BaseDb,
        file::File,
        mir::{MirDefinition, MirDefinitionValue},
        preprocessor::preprocessor_eval_root_file,
    },
    uri_to_path,
};
use dt_tools_parser::{
    TextRange,
    ast::{self, AstNode},
};
use itertools::Itertools;
use tower_lsp_server::ls_types::{
    Hover, HoverContents, HoverParams, MarkupContent, MarkupKind, Range,
};

fn _node_definition(node: &ast::DtNode, src: &str) -> String {
    let path = node.path(src).join("/");

    // TODO: get documentation by `compatible`
    format!("node `/{path}`")
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

    let Some(rope) = crate::salsa::rope(db, file) else {
        return None;
    };
    let offset = position_to_offset(params.position, rope)?;

    // TODO
    let cst_tree_section = cst_tree_section(db, file, offset);
    let mir_definition_section = mir_definition_section(db, root_file, file, offset);
    let label_section = label_section(db, root_file, file, offset);
    let include_section = include_section(db, root_file, file, offset);

    if [
        cst_tree_section.as_ref(),
        mir_definition_section.as_ref(),
        label_section.as_ref(),
        include_section.as_ref(),
    ]
    .iter()
    .all(Option::is_none)
    {
        return None;
    }

    let section_strs = [
        cst_tree_section.as_ref(),
        mir_definition_section.as_ref(),
        label_section.as_ref(),
        include_section.as_ref(),
    ]
    .into_iter()
    .flatten()
    .map(|tup| tup.0.trim())
    .collect_vec();

    let text_range = [
        mir_definition_section.as_ref(),
        label_section.as_ref(),
        include_section.as_ref(),
        cst_tree_section.as_ref(),
    ]
    .into_iter()
    .find_map(|opt| opt.map(|tup| tup.1));

    Some(make_hover_info(
        db,
        file,
        section_strs.join("\n---\n"),
        text_range,
    ))
}

fn cst_tree_section(db: &dyn BaseDb, file: File, offset: usize) -> Option<(String, TextRange)> {
    let parse = crate::salsa::parse_file(db, file)?;
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
    fn fmt_mir_def(
        db: &dyn BaseDb,
        def: &MirDefinition,
        most_precise: &MirDefinition,
        already_defined: &mut bool,
    ) -> String {
        let mut s: String = match &def.value {
            MirDefinitionValue::Node(mir_node_data) => format!(
                "Node {}defined with labels `{}`",
                already_defined.then_some("re").unwrap_or_default(),
                mir_node_data.labels.join(", ")
            ),
            MirDefinitionValue::Property(mir_property_data) => format!(
                "Property {}defined with values `{}`",
                already_defined.then_some("re").unwrap_or_default(),
                mir_property_data.values.iter().join(", ")
            ),
            MirDefinitionValue::DeletedNode => "Node deleted".to_owned(),
            MirDefinitionValue::DeletedProperty => "Property deleted".to_owned(),
        };
        *already_defined = true;

        // Provenance
        let is_here = def == most_precise;
        let shorter_path = def.provenance.file.shorter_path(db);
        if is_here {
            s.push_str(" _here_");
        } else if let Some((line, column)) = def
            .provenance
            .file
            .line_column(db, def.provenance.text_range.start)
        {
            // Line and column are 0-based in LSP but 1-based in text and file:// links
            let line = line + 1;
            let column = column + 1;

            let uri = def.provenance.file.uri(db);
            let uri = uri.as_str();

            write!(
                &mut s,
                " [`{shorter_path}:{line}:{column}`]({uri}#L{line}:{column})"
            )
            .ok();
        } else {
            write!(&mut s, " `{shorter_path}`").ok();
        }

        s
    }

    let mir = preprocessor_eval_root_file(db, root_file)?.mir(db);

    // TODO: unresolved_extensions
    let matching_defs = mir
        .definitions
        .iter()
        .filter(|def| {
            def.provenance.file == file && def.provenance.text_range.byte_range().contains(&offset)
        })
        .collect_vec();

    let most_precise = *matching_defs.iter().max_by_key(|def| def.path.len())?;
    let with_same_path = mir
        .definitions
        .iter()
        .filter(|def| def.path == most_precise.path)
        .collect_vec();

    let mut already_defined = false;

    Some((
        format!(
            "MIR: {}\n\n- {}",
            most_precise.path,
            with_same_path
                .iter()
                .map(|def| fmt_mir_def(db, def, most_precise, &mut already_defined))
                .collect_vec()
                .join("\n- ")
        ),
        most_precise.provenance.text_range,
    ))
}

fn label_section(
    db: &dyn BaseDb,
    root_file: File,
    file: File,
    offset: usize,
) -> Option<(String, TextRange)> {
    let result = preprocessor_eval_root_file(db, root_file)?;

    //result.env_after(db).get_label
    None
}

fn include_section(
    db: &dyn BaseDb,
    root_file: File,
    file: File,
    offset: usize,
) -> Option<(String, TextRange)> {
    let result = preprocessor_eval_root_file(db, root_file)?;

    let include = result
        .includes(db)
        .iter()
        .find(|(_, span)| span.file == file && span.text_range.byte_range().contains(&offset))?;

    Some((
        format!("Resolved include: `{}`", include.0.path(db)),
        include.1.text_range,
    ))
}

fn make_hover_info(
    db: &dyn BaseDb,
    file: crate::salsa::file::File,
    markdown: String,
    text_range: Option<TextRange>,
) -> Hover {
    Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: markdown,
        }),
        range: text_range.and_then(|text_range| {
            crate::salsa::rope(db, file).as_ref().and_then(|rope| {
                Some(Range::new(
                    offset_to_position(text_range.start, rope)?,
                    offset_to_position(text_range.end, rope)?,
                ))
            })
        }),
    }
}
