use std::{collections::HashSet, path::Path};

use anyhow::Context;
use dt_analyzer::DefinitionTreeNode;
use serde_yaml::{Mapping, Value};

mod fixups;
mod loader;

pub struct BindingSchema {
    pub raw_schema: Mapping,
    pub schemas: boon::Schemas,
    pub main_schema_index: boon::SchemaIndex,
    pub select_schema_index: boon::SchemaIndex,
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
        let mut yaml: Value = serde_yaml::from_str(
            &fs_err::read_to_string(path).context("Failed to read schema from path")?,
        )?;
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

        let select_json = serde_json::to_value(&select)?;

        let main_json = serde_json::to_value(&yaml)?;

        let mut compiler = boon::Compiler::new();

        // in 2020, items->prefixItems and additionalItems->unevaluatedItems
        compiler.set_default_draft(boon::Draft::V2019_09);

        // NOTE: boon::Error must be printed before converting to anyhow::Error because
        // it's !Send+!Sync
        compiler.use_loader(Box::new(loader::DtJsonSchemaLoader));
        compiler
            .add_resource("dt-tools:main", main_json)
            .map_err(|err| anyhow::anyhow!("{:#} {}", err, err))
            .context("Failed to add JSON schema resource dt-tools:main")?;
        compiler
            .add_resource("dt-tools:select", select_json)
            .map_err(|err| anyhow::anyhow!("{:#} {}", err, err))
            .context("Failed to add JSON schema resource dt-tools:main")?;

        let mut schemas = boon::Schemas::new();

        let main_schema_index = compiler
            .compile("dt-tools:main", &mut schemas)
            .map_err(|err| anyhow::anyhow!("{:#} {}", err, err))
            .context("Failed to compile JSON schema location dt-tools:main")?;

        let select_schema_index = compiler
            .compile("dt-tools:select", &mut schemas)
            .map_err(|err| anyhow::anyhow!("{:#} {}", err, err))
            .context("Failed to compile JSON schema location dt-tools:main")?;

        Ok(Self {
            raw_schema,
            schemas,
            main_schema_index,
            select_schema_index,
            maintainers,
        })
    }
}

/// TODO: use this in lsp
pub fn get_compatible_items(map: &Mapping) -> HashSet<String> {
    // TODO: resolve/handle $ref properties? this is currently really lazy
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
#[expect(
    clippy::missing_panics_doc,
    reason = "to_string_pretty shoudln't error"
)]
pub fn find_select(
    tree: DefinitionTreeNode,
    binding_schema: &BindingSchema,
) -> Option<DefinitionTreeNode> {
    // TODO: return path too?
    // TODO: compare into_json with `dtc example.dts -O yaml | yq '.[]'`
    // TODO: if select is simple, just check for equality or regex pattern (lazy into_json)
    // TODO: don't turn subtrees into JSON multiple times
    Some(
        tree.dfs_iter_nodes()
            .find(|(path, node)| {
                eprintln!(
                    "trying {} {}",
                    path.iter().fold(String::new(), |a, b| a + b + " . "),
                    serde_json::to_string_pretty(&node.clone().into_json()).unwrap()
                );
                let binding = node.clone().into_json();
                let res = binding_schema
                    .schemas
                    .validate(&binding, binding_schema.select_schema_index);
                if let Err(ref errors) = res {
                    eprintln!("{errors:#}");
                }
                res.is_ok()
            })?
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
        }

        let file = parse.source_file();

        let def = dt_analyzer::analyze_cst(&file, example).unwrap();
        eprintln!(
            "parsed from\n```dts\n{}\n```\njson = {}",
            example,
            serde_json::to_string_pretty(&def.tree.clone().into_json()).unwrap()
        );

        let selected = find_select(def.tree, binding_schema)
            .expect("Couldn't find selected")
            .into_json();
        eprintln!(
            "selected = {}",
            serde_json::to_string_pretty(&selected).unwrap()
        );

        if let Err(errors) = binding_schema
            .schemas
            .validate(&selected, binding_schema.main_schema_index)
        {
            eprintln!("{errors:#}");
        }
    }

    #[test]
    fn test_example() {
        // https://github.com/mrcjkb/rustaceanvim/discussions/231
        let schema =
            BindingSchema::compile(PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("example.yaml"))
                .unwrap();
        compile_example(schema.raw_schema["examples"][0].as_str().unwrap(), &schema);
    }

    #[test]
    fn test_leds() {
        let schema =
            BindingSchema::compile(loader::linux_bindings_path().join("leds/leds-bcm6328.yaml"))
                .unwrap();
        compile_example(schema.raw_schema["examples"][0].as_str().unwrap(), &schema);
    }

    #[test]
    fn test_simplefb() {
        let schema = BindingSchema::compile(
            loader::linux_bindings_path().join("display/simple-framebuffer.yaml"),
        )
        .unwrap();
        compile_example(schema.raw_schema["examples"][0].as_str().unwrap(), &schema);
    }
}
