use std::{collections::HashSet, path::Path};

use anyhow::Context as _;
use dt_analyzer::DefinitionTreeNode;
use jsonschema::JSONSchema;
use metaschemas::{CompilerMetaSchemaExt, SCHEMA_VALIDATOR};
use serde_yaml::{Mapping, Value};

pub mod fixups;
mod resolver;
mod metaschemas;

#[derive(Debug)]
pub struct BindingSchema {
    pub raw_schema: Mapping,
    pub schema: JSONSchema,
    pub select: JSONSchema,
    pub maintainers: Option<Value>,
}
impl BindingSchema {
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

        let schema = JSONSchema::options()
            .with_meta_schemas()
            .with_dt_meta_schemas()
            .with_resolver(resolver::DtJsonSchemaResolver)
            .compile(&json)
            .map_err(|e| anyhow::anyhow!(e.to_string()))
            .context("Failed to compile schema")?;

        let select = serde_json::to_value(&select)?;
        let select = JSONSchema::options()
            .compile(&select)
            .map_err(|e| anyhow::anyhow!(e.to_string()))
            .context("Failed to compile select schema")?;

        Ok(Self {
            raw_schema,
            schema,
            select,
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

pub fn find_select(tree: DefinitionTreeNode, select: &JSONSchema) -> Option<DefinitionTreeNode> {
    // TODO: return path too?
    Some(
        tree.dfs_iter_nodes()
            .find(|(_path, node)| select.is_valid(&node.clone().into_json()))?
            .1,
    )
}

#[cfg(test)]
#[test]
fn test_compile() {
    fn compile_example(example: &str, schema: &BindingSchema) {
        let example = format!("/dts-v1/;\n\n/ {{\n{example}\n}};");
        let red = dt_parser::parse(&example).unwrap();

        // TODO: use dt_lint since find_syntax_errors isn't updated
        assert!(red.find_syntax_errors(&example).next().is_none());

        let def = dt_analyzer::analyze_cst(red, &example).unwrap();
        let json = find_select(def.tree, &schema.select).unwrap().into_json();
        eprintln!("{:#?}", json);
        if let Err(e) = schema.schema.validate(&json) {
            eprintln!("{:#?}", e.collect::<Vec<_>>());
        };
    }
    let schema = BindingSchema::compile("./example.yaml").unwrap();
    if let Err(e) = schema.select.validate(&serde_json::json!({
        "compatible": ["vendor,soc4-ip"]
    })) {
        eprintln!("{:#?}", e.collect::<Vec<_>>());
    };
    compile_example(schema.raw_schema["examples"][0].as_str().unwrap(), &schema);

    // FIXME: fix errors with top-level `reg`
    /*let schema = BindingSchema::compile(
        "/home/axel/dev/mainlining/linux/Documentation/devicetree/bindings/leds/leds-bcm6328.yaml",
    )
    .unwrap();
    println!("Fixed schema: {:#?}", schema.raw_schema);
    compile_example(schema.raw_schema["examples"][0].as_str().unwrap(), &schema);*/
}
