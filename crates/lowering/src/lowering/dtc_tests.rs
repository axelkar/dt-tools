use std::{
    io::Write,
    process::{Command, Stdio},
};

use camino::Utf8PathBuf;
use expect_test::expect;

use crate::{db::BaseDb, includes::IncludeDirs, lowering::lower_root_file};

fn run_dtc(contents: &str) -> Result<String, String> {
    let dtc = std::env::var("DTC");
    let dtc = dtc.as_deref().unwrap_or("dtc");

    let mut child = Command::new(dtc)
        .args(["-I", "dts", "-O", "dts"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("dtc should exist");

    child
        .stdin
        .take()
        .unwrap()
        .write_all(contents.as_bytes())
        .unwrap();
    // `stdin` drops here, closing the pipe and sending EOF

    let output = child.wait_with_output().unwrap();

    let stdout = String::from_utf8(output.stdout).expect("dtc output should be UTF-8");
    let stderr = String::from_utf8(output.stderr).expect("dtc output should be UTF-8");

    if output.status.success() {
        assert!(
            stderr.is_empty(),
            "dtc should not output to stderr when successful. stderr={stderr:?}"
        );
        Ok(stdout)
    } else {
        assert!(
            stdout.is_empty(),
            "dtc should not output to stdout when unsuccessful. stdout={stdout:?}"
        );
        assert!(
            stderr.contains("ERROR"),
            "dtc stderr should contain an error when unsuccessful. stderr={stderr:?}"
        );
        Err(stderr)
    }
}

/// Returns true if it failed.
fn cross_check_dtc_success(contents: &str) -> bool {
    let dtc_output = run_dtc(contents);

    let db = crate::db::BaseDatabase::default();
    IncludeDirs::new(&db, vec![]);

    let root_file =
        db.get_files()
            .add_virtual(&db, Utf8PathBuf::from("/main.dts"), contents.to_owned());

    let (diags, _included_files) = crate::compute_diagnostics(&db, root_file);
    let did_fail = !diags.is_empty();
    let our_result = if did_fail { Err(()) } else { Ok(()) };

    match (our_result, dtc_output) {
        (Err(()), Ok(dtc_output)) => {
            let mut out = "--- errors ---\n".to_owned();
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

            panic!(
                "we should succeed like dtc.\n\n--- input ---\n{contents}\n\n{out}\n--- dtc DTS output ---\n{dtc_output}"
            );
        }
        (Ok(()), Err(dtc_output)) => {
            let result = lower_root_file(&db, root_file).expect("Should be a readable file");
            let mir = result.mir(&db);
            let out = mir.display(&db);

            panic!(
                "we should fail like dtc.\n\n--- input ---\n{contents}\n\n--- MIR ---\n{out}\n--- dtc errors ---\n{dtc_output}"
            );
        }
        _ => {}
    }

    did_fail
}

#[test]
fn empty() {
    cross_check_dtc_success("/dts-v1/; / {};");
}

fn generate_test_dts(
    delete_node_target: &str,
    reference_usage: &str,
    reference_target_type: &str,
    fate: &str,
) -> String {
    use std::fmt::Write;

    let reference_target = match reference_target_type {
        "label" => "&lbl",
        "path" => "&{/wrapper_node/target_node}",
        _ => unreachable!(),
    };

    let mut out: String = String::new();
    let _ = writeln!(
        out,
        "/* config:\n   delete_node_target={delete_node_target}\n   reference_usage={reference_usage}\n   reference_target_type={reference_target_type}\n   fate={fate} */"
    );
    out.push_str("/dts-v1/;\n");
    out.push_str("/ { wrapper_node { lbl: target_node { }; }; };\n");

    out.push_str(
        &match reference_usage {
            "cell" => "/ { prop_before = <&>; };\n",
            "value" => "/ { prop_before = &; };\n",
            "extension" => "& { ext_prop_before = <1>; };\n",
            _ => unreachable!(),
        }
        .replace('&', reference_target),
    );

    out.push_str(match delete_node_target {
        "label" => "/delete-node/ &lbl;\n",
        "name" => "/ { wrapper_node { /delete-node/ target_node; }; };\n",
        "path" => "/delete-node/ &{/wrapper_node/target_node};\n",
        "wrapper-path" => "/delete-node/ &{/wrapper_node};\n",
        _ => unreachable!(),
    });

    out.push_str(match fate {
        "same" => "/ { wrapper_node { lbl: target_node { }; }; };\n",
        "diff" => "/ { wrapper_node { lbl: other_node { }; }; };\n",
        "node-no-label" => "/ { wrapper_node { target_node { }; }; };\n",
        "gone" => "// leave deleted\n",
        _ => unreachable!(),
    });

    out.push_str(
        &match reference_usage {
            "cell" => "/ { prop = <&>; };\n",
            "value" => "/ { prop = &; };\n",
            "extension" => "& { ext_prop = <1>; };\n",
            _ => unreachable!(),
        }
        .replace('&', reference_target),
    );

    out
}

#[test]
fn generate_delete_tests() {
    let mut fail = Vec::new();
    let mut success = Vec::new();

    for delete_node_target in ["label", "name", "path", "wrapper-path"] {
        for reference_usage in ["cell", "value", "extension"] {
            for reference_target in ["label", "path"] {
                for fate in ["gone", "same", "diff", "node-no-label"] {
                    if reference_target == "path" && (["gone", "diff"].contains(&fate)) {
                        // TODO: check invalid path phandle references in our code
                        continue;
                    }

                    let dts = generate_test_dts(
                        delete_node_target,
                        reference_usage,
                        reference_target,
                        fate,
                    );
                    let did_fail = cross_check_dtc_success(&dts);

                    // Keep track of succeeding/failing combinations
                    if did_fail { &mut fail } else { &mut success }
                        .push(format!("{reference_target}-{fate}"));
                }
            }
        }
    }

    success.sort();
    fail.sort();
    success.dedup();
    fail.dedup();

    assert!(
        !success.is_empty(),
        "Nothing succeeded; the DTS generator is probably broken"
    );
    expect![[r#"
        Success: [
            "label-diff",
            "label-same",
            "path-node-no-label",
            "path-same",
        ]
        Fail: [
            "label-gone",
            "label-node-no-label",
        ]"#]]
    .assert_eq(&format!("Success: {success:#?}\nFail: {fail:#?}"));
}
