use std::borrow::Cow;

use dt_tools_diagnostic::{Diagnostic, MultiSpan, Severity, SpanLabel};

use crate::{
    db::BaseDb, file::File, lowering::LoweredFile, macros::env::InternedKey, mir, tag_diagnostics,
};

#[salsa::tracked(returns(ref), lru = 64)]
pub(super) fn check_mir_post<'db>(
    db: &'db dyn BaseDb,
    result: LoweredFile<'db>,
) -> Vec<Diagnostic<File>> {
    let mut diagnostics = Vec::new();

    if result.is_overlay(db) {
        return Vec::new();
    }
    let mir = result.mir(db);

    // Labels are resolved at the end because forward references are valid.
    for def in mir.iter_live_defs_under("") {
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

    check_directives(mir, &mut diagnostics);

    tag_diagnostics(&mut diagnostics, module_path!());

    diagnostics
}

fn check_phandle(
    db: &dyn BaseDb,
    mir: &mir::Mir,
    result: LoweredFile<'_>,
    def: &mir::MirDefinition,
    target: &mir::MirPhandleTarget,
) -> Option<Diagnostic<File>> {
    match target {
        mir::MirPhandleTarget::Label(name) => {
            if result
                .env_after(db)
                .get_label(db, InternedKey::new(db, name))
                .is_none()
            {
                Some(Diagnostic::new(
                    def.provenance.span,
                    Cow::Owned(format!("Label not found: {name}")),
                    Severity::Error,
                ))
            } else {
                None
            }
        }
        mir::MirPhandleTarget::Path(path) => {
            if mir.contains_node(path) {
                None
            } else {
                Some(Diagnostic::new(
                    def.provenance.span,
                    Cow::Owned(format!("Node at path not found: {path}")),
                    Severity::Error,
                ))
            }
        }
    }
}

fn check_directives(mir: &mir::Mir, diagnostics: &mut Vec<Diagnostic<File>>) {
    // Validate the existence and order of /dts-v1/; and /plugin/;
    let mut dts_v1: Option<mir::MirProvenance> = None;
    let mut overlay_mode: Option<mir::MirProvenance> = None;
    for def in &mir.definitions {
        match def.value {
            mir::MirDefinitionValue::V1Directive => {
                dts_v1 = Some(def.provenance.clone());
            }
            mir::MirDefinitionValue::PluginDirective => {
                if dts_v1.is_none() {
                    diagnostics.push(Diagnostic::new(
                        def.provenance.span,
                        "`/plugin/;` before `/dts-v1/;`".into(),
                        Severity::Error,
                    ));
                } else if let Some(prev_def) = &overlay_mode {
                    diagnostics.push(Diagnostic {
                        span: MultiSpan {
                            primary_spans: vec![def.provenance.span],
                            span_labels: vec![SpanLabel {
                                span: prev_def.span,
                                msg: "Previous definition here".into(),
                            }],
                        },
                        msg: "`/plugin/;` twice".into(),
                        severity: Severity::Error,
                    });
                }
                overlay_mode = Some(def.provenance.clone());
            }
            mir::MirDefinitionValue::Node(_)
            | mir::MirDefinitionValue::Property(_)
            | mir::MirDefinitionValue::DeletedNode
            | mir::MirDefinitionValue::DeletedProperty => {
                if dts_v1.is_none() {
                    diagnostics.push(Diagnostic::new(
                        def.provenance.span,
                        "Definition before /dts-v1/;".into(),
                        Severity::Error,
                    ));
                }
            }
        }
    }
}
