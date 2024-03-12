use anyhow::{anyhow, Context as _};
use jsonschema::{SchemaResolver, SchemaResolverError};
use serde_json::Value;
use std::{path::PathBuf, sync::Arc};

// TODO: "workspace dirs", read all the $id's and use here

pub struct DtJsonSchemaResolver;

impl SchemaResolver for DtJsonSchemaResolver {
    fn resolve(
        &self,
        _root_schema: &Value,
        url: &url::Url,
        _original_reference: &str,
    ) -> Result<Arc<Value>, SchemaResolverError> {
        match url.scheme() {
            "json-schema" => Err(anyhow!("cannot resolve schema without root schema ID")),
            "http" | "https" => {
                if url.host_str() == Some("devicetree.org") {
                    let abs_path = url
                        .path()
                        .strip_prefix('/')
                        .ok_or_else(|| anyhow!("devicetree.org url has no path portion"))?;

                    let mut path_a =
                        PathBuf::from(concat!(env!("CARGO_MANIFEST_DIR"), "/dt-schema/dtschema"));
                    path_a.push(abs_path);
                    eprintln!("path: {}", path_a.display());

                    let path_b = if let Some(abs_path) = abs_path.strip_prefix("schemas/") {
                        let mut path_b = PathBuf::from(
                            "/home/axel/dev/mainlining/linux/Documentation/devicetree/bindings/",
                        );
                        path_b.push(abs_path);
                        eprintln!("path: {}", path_b.display());
                        Some(path_b)
                    } else {
                        None
                    };

                    let f = std::fs::File::open(path_a)
                        .or_else(|e| match path_b {
                            Some(path_b) => std::fs::File::open(path_b),
                            None => Err(e),
                        })
                        .context("could not read devicetee.org schema")?;
                    let yaml: serde_yaml::Value = serde_yaml::from_reader(f)
                        .context("could not read devicetee.org schema")?;
                    Ok(Arc::new(serde_json::to_value(&yaml)?))
                } else {
                    Err(anyhow!("cannot resolve online schemas"))
                }
            }
            _ => Err(anyhow!("scheme is not supported")),
        }
    }
}
