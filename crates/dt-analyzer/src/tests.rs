use dt_parser::ast::SourceFile;

struct PanickingDiagnosticCollector;
impl dt_diagnostic::DiagnosticCollector for PanickingDiagnosticCollector {
    fn emit(&self, diag: dt_diagnostic::Diagnostic) {
        panic!("Diagnostic emitted: {:#?}", diag);
    }
}

fn dts_to_json(filename: &str) -> serde_json::Value {
    let diag = &PanickingDiagnosticCollector;

    let src = &std::fs::read_to_string(filename).unwrap();
    let parse = SourceFile::parse(src);
    assert_eq!(parse.errors, Vec::new());
    assert_eq!(parse.lex_errors, Vec::new());

    let file = &parse.source_file();

    let analyzed = crate::new::stage1::analyze_file(file, src, diag);

    let includes = analyzed
        .iter()
        .filter_map(|a| a.as_include())
        .collect::<Vec<_>>();
    if !includes.is_empty() {
        panic!("Nonzero number of includes: {:#?}", includes);
    }

    let stage2 = crate::new::stage2::compute(&analyzed, &[], diag);

    stage2.root_node.into_json()
}

macro_rules! define_tests {
    ($($fn_name:ident, $id:literal);* $(;)?) => {$(
        #[test]
        fn $fn_name() {
            let found = dts_to_json(concat!("test_data/", $id, "/source.dts"));

            let reader = std::io::BufReader::new(std::fs::File::open(concat!("test_data/", $id, "/expected.json")).unwrap());
            let expected: serde_json::Value = serde_json::from_reader(reader).unwrap();

            assert_eq!(
                found, expected
            );
        }
    )*};
}

define_tests! {
    test_1_basic, "1";
    // test_2_include, "2"; // FIXME: nonzero number of includes
    // test_3_macros, "3"; // FIXME: macros
}
