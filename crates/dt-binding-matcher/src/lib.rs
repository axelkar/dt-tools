use std::{collections::HashSet, path::Path};

use anyhow::Context as _;
use dt_analyzer::DefinitionTreeNode;
use jsonschema::Validator;
use metaschemas::SCHEMA_VALIDATOR;
use serde_yaml::{Mapping, Value};

mod fixups;
mod metaschemas;
mod retriever;

#[derive(Debug)]
pub struct BindingSchema {
    pub raw_schema: Mapping,
    pub validator: Validator,
    pub select_validator: Validator,
    pub maintainers: Option<Value>,
}
impl BindingSchema {
    /// Compiles a binding schema
    ///
    /// # Errors
    ///
    /// Will return `Err` if any of the following conditions are met:
    ///
    /// - Loading the YAML file fails
    /// - Schema is invalid according to [the `dt-schema` validations](https://github.com/devicetree-org/dt-schema)
    /// - Full JSON Schema compilation fails
    /// - Select JSON Schema compilation fails
    #[expect(clippy::missing_panics_doc, reason = "fixups should add select key")]
    pub fn compile(path: impl AsRef<Path>) -> anyhow::Result<Self> {
        let mut yaml: Value = serde_yaml::from_str(&std::fs::read_to_string(path)?)?;
        let Some(yaml_map) = yaml.as_mapping_mut() else {
            todo!()
        };

        yaml_map.entry("type".into()).or_insert("object".into());
        fixups::fixup_node(yaml_map);
        fixups::add_select(yaml_map);
        let raw_schema = yaml_map.clone();
        let select = yaml_map
            .remove("select")
            .expect("fixed document should always have select");
        let maintainers = yaml_map.remove("maintainers");

        let json = serde_json::to_value(&yaml)?;

        // FIXME: fix errors
        let _ = SCHEMA_VALIDATOR;
        /*SCHEMA_VALIDATOR.validate(&json).map_err(|err| anyhow::anyhow!(err
            .map(|e| e.to_string())
            .fold(String::new(), |mut acc, b| {
                acc.reserve(b.len() + 3);
                acc.push_str("* ");
                acc.push_str(&b);
                acc.push_str("\n");
                acc
            }))
        ).context("Invalid schema!")?;*/

        let validator = jsonschema::options()
            //.with_dt_meta_schemas()
            // Jsonschema doesn't follow $schema references. This is declared in
            // dtschema/meta-schemas/core.yaml
            //.with_draft(jsonschema::Draft::Draft201909)
            .with_retriever(retriever::DtJsonSchemaRetriever)
            .build(&json)
            .context("Failed to compile schema")?;

        let select = serde_json::to_value(&select)?;
        let select_validator = jsonschema::options()
            .build(&select)
            .context("Failed to compile select schema")?;

        Ok(Self {
            raw_schema,
            validator,
            select_validator,
            maintainers,
        })
    }
}

/// TODO: use this in lsp
pub fn get_compatible_items(map: &Mapping) -> HashSet<String> {
    // TODO: handle $ref ? this is currently really lazy
    let mut list = HashSet::new();
    if let Some(properties) = map.get("properties").and_then(Value::as_mapping) {
        if let Some(compatible) = properties.get("compatible").and_then(Value::as_mapping) {
            if let Some(item) = compatible.get("const").and_then(Value::as_str) {
                list.insert(item.to_owned());
            }
            if let Some(items) = compatible.get("enum").and_then(Value::as_sequence) {
                for item in items.iter().filter_map(Value::as_str) {
                    list.insert(item.to_owned());
                }
            }
        }
    }
    list
}

#[must_use]
pub fn find_select(tree: DefinitionTreeNode, select_validator: &Validator) -> Option<DefinitionTreeNode> {
    // TODO: return path too?
    // TODO: compare into_json with `dtc example.dts -O yaml | yq '.[]'`
    // TODO: if select is simple, just check for equality or regex pattern (lazy into_json)
    // TODO: don't turn subtrees into JSON multiple times
    Some(
        tree.dfs_iter_nodes()
            .find(|(_path, node)| select_validator.is_valid(&node.clone().into_json()))?
            .1,
    )
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::*;
    use dt_parser::ast;

    fn compile_example(example: &str, binding_schema: &BindingSchema) {
        // Hack for examples that already have root nodes like simple-framebuffer.yaml
        let example = if example.contains("/ {") {
            example
        } else {
            &format!("/dts-v1/;\n\n/ {{\n{example}\n}};")
        };

        let parse = ast::SourceFile::parse(example);

        if !parse.lex_errors.is_empty() || !parse.errors.is_empty() {
            eprintln!("Invalid DTS!");
            std::process::exit(1);
        };

        let file = parse.source_file();
        //eprintln!("Parsed tree: {}", file.syntax().green.print_tree());

        let def = dt_analyzer::analyze_cst(&file, example).unwrap();
        println!(
            "tree: {:#?} parsed from {}, json = {}",
            def.tree,
            example,
            serde_json::to_string_pretty(&def.tree.clone().into_json()).unwrap()
        );
        let json = find_select(def.tree, &binding_schema.select_validator)
            .expect("Couldn't find selected")
            .into_json();
        eprintln!("{json:#?}");
        let errors = binding_schema.validator.iter_errors(&json).collect::<Vec<_>>();
        if !errors.is_empty() {
            eprintln!("{errors:#?}");
        };
    }

    #[test]
    #[ignore = "Custom meta schemas are not fully implemented: https://github.com/Stranger6667/jsonschema/issues/664"]
    fn test_example() {
        // https://github.com/mrcjkb/rustaceanvim/discussions/231
        let schema = BindingSchema::compile(PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("example.yaml")).unwrap();
        compile_example(schema.raw_schema["examples"][0].as_str().unwrap(), &schema);
    }

    #[test]
    #[ignore = "Custom meta schemas are not fully implemented: https://github.com/Stranger6667/jsonschema/issues/664"]
    fn test_leds() {
        let schema = BindingSchema::compile(
            "/home/axel/dev/mainlining/linux/Documentation/devicetree/bindings/leds/leds-bcm6328.yaml",
        )
        .unwrap();
        compile_example(schema.raw_schema["examples"][0].as_str().unwrap(), &schema);
    }

    #[test]
    #[ignore = "Custom meta schemas are not fully implemented: https://github.com/Stranger6667/jsonschema/issues/664"]
    fn test_simplefb() {
        let schema = BindingSchema::compile(
            "/home/axel/dev/mainlining/linux/Documentation/devicetree/bindings/display/simple-framebuffer.yaml",
        )
        .unwrap();
        compile_example(schema.raw_schema["examples"][0].as_str().unwrap(), &schema);
    }
}
