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
    diagnostics: &mut Vec<Diagnostic<File>>,
) {
    if mir.sets_overlay() {
        return;
    }
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
                Some(Cow::Owned(format!("Label not found: {name}")))
            } else {
                None
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
        span: def.provenance.clone().into(),
        msg,
        severity: Severity::Error,
    })
}

/// Validates the existence and order of /dts-v1/; and /plugin/;
fn check_directives(mir: &mir::Mir, diagnostics: &mut Vec<Diagnostic<File>>) {
    let mut dts_v1: Option<mir::MirProvenance> = None;
    let mut errored_about_dts_v1 = false;
    let mut overlay_mode: Option<mir::MirProvenance> = None;

    for def in &mir.definitions {
        match def.value {
            mir::MirDefinitionValue::V1Directive => {
                // Note: /dts-v1/; is allowed to be twice
                dts_v1 = Some(def.provenance.clone());
            }
            mir::MirDefinitionValue::PluginDirective => {
                if dts_v1.is_none() {
                    diagnostics.push(Diagnostic {
                        span: def.provenance.clone().into(),
                        msg: "`/plugin/;` before `/dts-v1/`;".into(),
                        severity: Severity::Error,
                    });
                } else if let Some(prev_def) = &overlay_mode {
                    let mut multispan: MultiSpan<File> = def.provenance.clone().into();

                    // TODO: how to turn MirProvenance to span labels? MirProvenance can have multiple spans.
                    multispan.span_labels.push(SpanLabel {
                        span: prev_def.primary_span(),
                        msg: "Previous definition here".into(),
                    });
                    diagnostics.push(Diagnostic {
                        span: multispan,
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
                if dts_v1.is_none() && !errored_about_dts_v1 {
                    errored_about_dts_v1 = true;
                    diagnostics.push(Diagnostic {
                        span: def.provenance.clone().into(),
                        msg: "Definition before `/dts-v1/;`".into(),
                        severity: Severity::Error,
                    });
                }
            }
        }
    }
}
