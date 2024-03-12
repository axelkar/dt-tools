use std::{collections::HashSet, path::Path};

use jsonschema::JSONSchema;
use serde_yaml::{Mapping, Value};

pub mod fixups;

#[derive(Debug)]
pub struct BindingSchema {
    pub raw_schema: Mapping,
    pub schema: JSONSchema,
    pub select: Option<Value>,
    pub maintainers: Option<Value>,
}
impl BindingSchema {
    pub fn compile(path: impl AsRef<Path>) -> Result<Self, Box<dyn std::error::Error>> {
        let mut yaml: Value = serde_yaml::from_str(&std::fs::read_to_string(path)?)?;
        let Some(yaml_map) = yaml.as_mapping_mut() else {
            todo!()
        };

        yaml_map.entry("type".into()).or_insert("object".into());
        fixups::fixup_node(yaml_map);
        fixups::add_select(yaml_map);
        let raw_schema = yaml_map.clone();
        let select = yaml_map.remove("select");
        let maintainers = yaml_map.remove("maintainers");

        let json = serde_json::to_value(&yaml)?;
        let schema = JSONSchema::options().compile(&json).map_err(|err| err.to_string())?;
        Ok(Self {
            raw_schema,
            schema,
            select,
            maintainers
        })
    }
}

fn get_compatible_items(map: &Mapping) -> HashSet<String> {
    // TODO: handle $ref ?
    let mut list = HashSet::new();
    if let Some(properties) = map.get("properties").and_then(Value::as_mapping) {
        if let Some(compatible) = properties.get("compatible").and_then(Value::as_mapping) {
            if let Some(item) = compatible.get("const").and_then(Value::as_str) {
                list.insert(item.to_owned());
            }
            if let Some(items) = compatible.get("enum").and_then(Value::as_sequence) {
                for item in items.into_iter().filter_map(Value::as_str) {
                    list.insert(item.to_owned());
                }
            }
            if let Some(_item) = compatible.get("pattern").and_then(Value::as_str) {
                todo!() // TODO: get list of possible patterns? this may not be good for LSP
            }
        }
    }
    list
}

#[cfg(test)]
#[test]
fn test_compile() {
    fn compile_example(example: &str) {
        let example = format!("/dts-v1/;\n\n/ {{\n{example}\n}};");
        let red = dt_parser::parse(&example).unwrap();

        // TODO: use dt_lint since find_syntax_errors isn't updated
        assert!(red.find_syntax_errors(&example).next().is_none());

        let def = dt_analyzer::analyze_cst(red, &example).unwrap();
        panic!("{def:#?}");
    }
    let schema = BindingSchema::compile("./example.yaml").unwrap();
    compile_example(schema.raw_schema["examples"][0].as_str().unwrap());

    let _schema = BindingSchema::compile("/home/axel/dev/mainlining/linux/Documentation/devicetree/bindings/leds/leds-bcm6328.yaml").unwrap();
}
