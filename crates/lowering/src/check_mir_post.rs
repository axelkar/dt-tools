use std::borrow::Cow;

use dt_tools_diagnostic::{Diagnostic, MultiSpan, Severity};

use crate::{
    db::BaseDb,
    lowering::{LoweredFile, dt_node::build_path},
    mir,
    preprocess::PreprocessedText,
    tag_diagnostics,
};

#[salsa::tracked(lru = 64)]
pub(super) fn check_mir_post<'db>(
    db: &'db dyn BaseDb,
    result: LoweredFile<'db>,
) -> Vec<Diagnostic<PreprocessedText>> {
    profiling::scope!("lowering::check_mir_post");

    let mut diagnostics = Vec::new();

    let mir = result.mir(db);
    check_phandles(db, result, mir, &mut diagnostics);
    check_directives(mir, &mut diagnostics);

    tag_diagnostics(&mut diagnostics, module_path!());

    diagnostics
}

fn check_phandles(
    db: &dyn BaseDb,
    result: LoweredFile<'_>,
    mir: &mir::Mir,
    diagnostics: &mut Vec<Diagnostic<PreprocessedText>>,
) {
    profiling::scope!("lowering::check_mir_post::check_phandles");

    if mir.sets_overlay() {
        return;
    }
    // Labels are resolved at the end because forward references are valid.
    for def in mir.iter_live_defs_under("") {
        profiling::scope!("lowering::check_mir_post::check_phandles::in_loop");
        if let mir::MirDefinitionValue::Property(mir_property_data) = &def.value {
            for value in &mir_property_data.values {
                match value {
                    mir::MirValue::CellList(mir::MirCellList::Bits32(cells)) => {
                        for cell in cells {
                            if let mir::MirCell32::Phandle(target) = cell {
                                diagnostics.extend(check_phandle(db, mir, result, def, target));
                            }
                        }
                    }
                    mir::MirValue::Phandle(target) => {
                        diagnostics.extend(check_phandle(db, mir, result, def, target));
                    }
                    _ => {}
                }
            }
        }
    }
}

fn check_phandle(
    db: &dyn BaseDb,
    mir: &mir::Mir,
    result: LoweredFile<'_>,
    def: &mir::MirDefinition,
    target: &mir::MirPhandleTarget,
) -> Option<Diagnostic<PreprocessedText>> {
    profiling::scope!(
        "lowering::check_mir_post::check_phandle",
        format!("{target:?}").as_str()
    );

    match target {
        mir::MirPhandleTarget::Label(name) => {
            if result.labels(db).get(name).is_some_and(Option::is_some) {
                None
            } else {
                Some(Cow::Owned(format!("Label not found: {name}")))
            }
        }
        mir::MirPhandleTarget::LabelPath { label, path } => {
            let base = result
                .labels(db)
                .get(label)
                .and_then(Option::as_ref)
                .map(|label| label.path.clone());

            match base {
                None => Some(Cow::Owned(format!("Label not found: {label}"))),
                Some(base) => {
                    let resolved = build_path(&base, path);
                    if mir.contains_node(&resolved) {
                        None
                    } else {
                        Some(Cow::Owned(format!("Node at path not found: {resolved}")))
                    }
                }
            }
        }
        mir::MirPhandleTarget::Path(path) => {
            if mir.contains_node(path) {
                None
            } else {
                Some(Cow::Owned(format!("Node at path not found: {path}")))
            }
        }
    }
    .map(|msg| Diagnostic {
        span: MultiSpan::from(def.name_span),
        msg,
        severity: Severity::Error,
    })
}

/// Validates the existence and order of /dts-v1/; and /plugin/;
fn check_directives(mir: &mir::Mir, diagnostics: &mut Vec<Diagnostic<PreprocessedText>>) {
    profiling::scope!("lowering::check_mir_post::check_directives");

    // Headers are a prefix of the file, and `/plugin/;` belongs to the header
    // opened by the preceding `/dts-v1/;`.
    let mut seen_dts_v1 = false;
    let mut errored_about_dts_v1 = false;
    let mut in_headers = true;
    let mut plugin_allowed = false;

    for def in &mir.definitions {
        match def.value {
            mir::MirDefinitionValue::V1Directive => {
                if !in_headers {
                    diagnostics.push(Diagnostic {
                        span: MultiSpan::from(def.name_span),
                        msg: "`/dts-v1/;` after definitions".into(),
                        severity: Severity::Error,
                    });
                    continue;
                }
                seen_dts_v1 = true;
                plugin_allowed = true;
            }
            mir::MirDefinitionValue::PluginDirective => {
                if !plugin_allowed {
                    diagnostics.push(Diagnostic {
                        span: MultiSpan::from(def.name_span),
                        msg: "`/plugin/;` must immediately follow `/dts-v1/`".into(),
                        severity: Severity::Error,
                    });
                }
                plugin_allowed = false;
            }
            mir::MirDefinitionValue::Node(_)
            | mir::MirDefinitionValue::Property(_)
            | mir::MirDefinitionValue::DeletedNode
            | mir::MirDefinitionValue::DeletedProperty => {
                in_headers = false;
                plugin_allowed = false;

                if !seen_dts_v1 && !errored_about_dts_v1 {
                    errored_about_dts_v1 = true;
                    diagnostics.push(Diagnostic {
                        span: MultiSpan::from(def.name_span),
                        msg: "Definition before `/dts-v1/;`".into(),
                        severity: Severity::Error,
                    });
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{db::BaseDb, settings::PreprocessSettings};

    /// The order diagnostic points at the definition name, not its whole body.
    #[test]
    fn definition_before_dts_v1_points_at_the_name() {
        let db = crate::db::BaseDatabase::default();
        PreprocessSettings::new(&db, vec![], vec![]);

        let root_file = db.get_files().add_virtual(
            &db,
            "/main.dts".into(),
            "#define ROOT / { b = <1>; };\nROOT\n/dts-v1/;\n".to_owned(),
        );

        let (diagnostics, _) = crate::compute_diagnostics(&db, root_file);
        let diagnostic = diagnostics
            .iter()
            .find(|diagnostic| diagnostic.msg.contains("Definition before"))
            .expect("order diagnostic");

        assert_eq!(diagnostic.span.primary_spans.len(), 1, "{diagnostic:#?}");
        assert_eq!(
            diagnostic.span.primary_spans[0].text_range.length(),
            1,
            "{diagnostic:#?}"
        );
        assert!(
            diagnostic
                .span
                .span_labels
                .iter()
                .any(|label| label.msg.contains("in this expansion of `ROOT`")),
            "{diagnostic:#?}"
        );
    }
}
