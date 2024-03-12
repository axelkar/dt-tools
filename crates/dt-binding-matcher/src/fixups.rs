//! "Fix" schemas to make them work with standard JSON Schema tools
//!
//! One important fixup is `single_to_array`, which enables mapping the following schema
//! ```yaml
//! properties:
//!   compatible:
//!     const: "foo,bar"
//! ```
//! to the following
//! ```yaml
//! properties:
//!   compatible:
//!     items:
//!       - const: "foo,bar"
//! ```
//! A similar fixup is applied when converting the raw [values](dt_analyzer::CustomValue) to JSON.

use serde_yaml::{Mapping, Value};

use crate::fixups::types::ty_eq;

use self::types::{is_int, is_string};

mod types {
    use serde_yaml::{Mapping, Value};

    pub fn ty_eq(schema: &Mapping, ty: &'static str) -> bool {
        schema.get("type").and_then(Value::as_str) == Some(ty)
    }

    fn is_int_type(value: &Value) -> bool {
        match value {
            Value::Number(n) if n.is_i64() => true,
            Value::Sequence(vec) => vec.first().map(is_int_type).unwrap_or_default(),
            _ => false,
        }
    }
    fn is_string_type(value: &Value) -> bool {
        match value {
            Value::String(_) => true,
            Value::Sequence(vec) => vec.first().map(is_string_type).unwrap_or_default(),
            _ => false,
        }
    }

    pub fn is_int(schema: &Mapping) -> bool {
        ty_eq(schema, "integer")
            || schema.get("const").map(is_int_type).unwrap_or_default()
            || schema.get("enum").map(is_int_type).unwrap_or_default()
            || schema.get("minimum").map(is_int_type).unwrap_or_default()
            || schema.get("maximum").map(is_int_type).unwrap_or_default()
    }
    pub fn is_string(schema: &Mapping) -> bool {
        ty_eq(schema, "string")
            || schema.get("const").map(is_string_type).unwrap_or_default()
            || schema.get("enum").map(is_string_type).unwrap_or_default()
            || schema
                .get("pattern")
                .map(is_string_type)
                .unwrap_or_default()
    }
}

fn single_to_array(map: &mut Mapping) {
    if is_int(map) || is_string(map) {
        // Why move some keywords to items and some not in
        // https://github.com/devicetree-org/dt-schema/blob/229ba6b76e7a81f0ad4b272d71d37d6d8335a480/dtschema/fixups.py#L15 ?

        *map = [(
            "items".into(),
            Value::Sequence(vec![Value::Mapping(map.clone())]),
        )]
        .into_iter()
        .collect();
    }
}

fn walk_properties(map: &mut Mapping, prop_name: &str) {
    for cond in ["allOf", "oneOf", "anyOf"] {
        if let Some(cond) = map.get_mut(cond) {
            if let Some(cond) = cond.as_mapping_mut() {
                fixup_vals(cond, prop_name);
            }
        }
    }
    if let Some(then) = map.get_mut("then") {
        if let Some(then) = then.as_mapping_mut() {
            fixup_vals(then, prop_name);
        }
    }
    fixup_vals(map, prop_name);
}

fn fixup_reg_schema(map: &mut Mapping) {
    let mut item_schema = if let Some(items) = map.get_mut("items") {
        if let Some(items) = items.as_sequence_mut() {
            items.truncate(1);
            items
                .first_mut()
                .expect("items list should have at least one item")
        } else {
            items
        }
        .as_mapping()
        .expect("items item should always be a mapping")
        .clone()
    } else {
        map.clone()
    };
    if is_int(&item_schema) {
        single_to_array(&mut item_schema);
        map.insert("items".into(), Value::Mapping(item_schema));
    }
}

fn fixup_vals(map: &mut Mapping, prop_name: &str) {
    if prop_name == "reg" {
        fixup_reg_schema(map);
    }
    single_to_array(map);
}

pub fn fixup_node(schema: &mut Mapping) {
    let is_object = ty_eq(schema, "object");
    let Some(props) = schema.get_mut("properties").and_then(Value::as_mapping_mut) else {
        return;
    };
    debug_assert!(is_object, "node must have `type: object`");

    for (prop_name, v) in props.iter_mut() {
        let Some(prop_name) = prop_name.as_str() else {
            return;
        };
        if let Some(map) = v.as_mapping_mut() {
            walk_properties(map, prop_name);
            fixup_node(map);
        } else if !v.is_bool() {
            panic!("properties has invalid child")
        }
    }
    if schema.get("additionalProperties") != Some(&Value::Bool(true))
        && schema.get("unevaluatedProperties") != Some(&Value::Bool(true))
    {
        let Some(props) = schema.get_mut("properties").and_then(Value::as_mapping_mut) else {
            return;
        };
        props.entry("phandle".into()).or_insert(Value::Bool(true));
        props.entry("$nodename".into()).or_insert(Value::Bool(true));
        props.entry("status".into()).or_insert(Value::Bool(true));
        props
            .entry("secure-status".into())
            .or_insert(Value::Bool(true));
        if props.contains_key("ranges") {
            props
                .entry("dma-ranges".into())
                .or_insert(Value::Bool(true));
        }
        if props.contains_key("clocks") && !props.contains_key("assigned-clocks") {
            props.insert("assigned-clocks".into(), Value::Bool(true));
            props
                .entry("assigned-clock-rates".into())
                .or_insert(Value::Bool(true));
            props
                .entry("assigned-clock-parents".into())
                .or_insert(Value::Bool(true));
        }

        let has_pinctrl = props.keys().filter_map(Value::as_str).any(|k| {
            k.starts_with("pinctrl-")
                && k.chars()
                    .nth("pinctrl-".len())
                    .map(|c| c.is_ascii_digit())
                    .unwrap_or_default()
        });
        if !has_pinctrl {
            props
                .entry("pinctrl-names".into())
                .or_insert(Value::Bool(true));
            let Some(pat_props) = schema
                .entry("patternProperties".into())
                .or_insert(Value::Mapping(Default::default()))
                .as_mapping_mut()
            else {
                return;
            };
            pat_props
                .entry("pinctrl-[0-9]+".into())
                .or_insert(Value::Bool(true));
        }
        schema.entry("phandle".into()).or_insert(Value::Bool(true));
    }
}

pub fn add_select(map: &mut Mapping) {
    if map.contains_key("select") {
        return;
    }
    if let Some(properties) = map.get("properties").and_then(Value::as_mapping) {
        if let Some(compatible) = properties.get("compatible").and_then(Value::as_mapping) {
            // TODO: only accept const, enum and pattern with string items
            map.insert(
                "select".into(),
                Value::Mapping(
                    [
                        (
                            Value::from("required"),
                            Value::Sequence(vec![Value::from("compatible")]),
                        ),
                        (
                            Value::from("properties"),
                            Value::Mapping(
                                [(Value::from("compatible"), compatible.clone().into())]
                                    .into_iter()
                                    .collect(),
                            ),
                        ),
                    ]
                    .into_iter()
                    .collect(),
                ),
            );
        } else if let Some(nodename) = properties.get("$nodename").and_then(Value::as_mapping) {
            // TODO: only accept const, enum and pattern with string items
            map.insert(
                "select".into(),
                Value::Mapping(
                    [
                        (
                            Value::from("required"),
                            Value::Sequence(vec![Value::from("$nodename")]),
                        ),
                        (
                            Value::from("properties"),
                            Value::Mapping(
                                [(Value::from("$nodename"), nodename.clone().into())]
                                    .into_iter()
                                    .collect(),
                            ),
                        ),
                    ]
                    .into_iter()
                    .collect(),
                ),
            );
        } else {
            map.insert("select".into(), Value::Bool(false));
        }
    } else {
        map.insert("select".into(), Value::Bool(false));
    }
}
