use jsonschema::{CompilationOptions, JSONSchema};
use once_cell::sync::Lazy;
use std::collections::HashMap;

macro_rules! schemas {
    ($($filename:expr),+) => {
        pub static META_SCHEMAS: Lazy<HashMap<String, serde_json::Value>> = Lazy::new(|| {
            let mut store = HashMap::with_capacity([$($filename),+].len());
            $(
            store.insert(
                concat!("http://devicetree.org/meta-schemas/", $filename).to_owned(),
                serde_yaml::from_str::<serde_json::Value>(include_str!(concat!("../dt-schema/dtschema/meta-schemas/", $filename))).expect("Invalid schema")
            );
            )+
            store
        });
    };
}

schemas!(
    "base.yaml",
    "boolean.yaml",
    "cell.yaml",
    "clocks.yaml",
    "core.yaml",
    "dma.yaml",
    "gpios.yaml",
    "hwlock.yaml",
    "iio.yaml",
    "interrupts.yaml",
    "iommu.yaml",
    "items.yaml",
    "keywords.yaml",
    "mailbox.yaml",
    "nodes.yaml",
    "nvmem.yaml",
    "phy.yaml",
    "power-domain.yaml",
    "pwm.yaml",
    "reset.yaml",
    "string-array.yaml",
    "types.yaml",
    "vendor-props.yaml"
);

pub static SCHEMA_VALIDATOR: Lazy<JSONSchema> = Lazy::new(|| {
    JSONSchema::options()
        .with_dt_meta_schemas()
        .with_resolver(crate::resolver::DtJsonSchemaResolver)
        .compile(
            &serde_yaml::from_str::<serde_json::Value>(include_str!(
                "../dt-schema/dtschema/meta-schemas/core.yaml"
            ))
            .expect("Invalid schema"),
        )
        .expect("Invalid meta-schema")
});

pub trait CompilerMetaSchemaExt {
    fn with_dt_meta_schemas(&mut self) -> &mut Self;
}
impl CompilerMetaSchemaExt for CompilationOptions {
    fn with_dt_meta_schemas(&mut self) -> &mut Self {
        for (id, schema) in META_SCHEMAS.clone().into_iter() {
            self.with_document(id, schema);
        }
        self
    }
}
