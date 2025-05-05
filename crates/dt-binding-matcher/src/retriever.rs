use anyhow::{anyhow, Context as _};
use jsonschema::Retrieve;
use serde_json::Value;
use std::path::PathBuf;

// TODO: "workspace dirs", read all the $id's and use here

pub struct DtJsonSchemaRetriever;

impl Retrieve for DtJsonSchemaRetriever {
    fn retrieve(
        &self,
        url: &jsonschema::Uri<String>,
    ) -> Result<Value, Box<dyn std::error::Error + Send + Sync>> {
        match url.scheme().as_str() {
            "json-schema" => Err(anyhow!("cannot resolve schema without root schema ID").into()),
            "http" | "https" => {
                if url.authority().map(|auth| auth.host()) == Some("devicetree.org") {
                    let abs_path = url
                        .path()
                        .as_str()
                        .strip_prefix('/')
                        .ok_or_else(|| anyhow!("devicetree.org url has no path portion"))?;
                    eprintln!("reading devicetree.org/{abs_path}");

                    let mut path_a =
                        PathBuf::from(concat!(env!("CARGO_MANIFEST_DIR"), "/dt-schema/dtschema"));
                    path_a.push(abs_path);
                    eprintln!("path_a: {}", path_a.display());

                    let path_b = if let Some(abs_path) = abs_path.strip_prefix("schemas/") {
                        let mut path_b = PathBuf::from(
                            "/home/axel/dev/mainlining/linux/Documentation/devicetree/bindings/",
                        );
                        path_b.push(abs_path);
                        eprintln!("path_b: {}", path_b.display());
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
                    Ok(serde_json::to_value(&yaml)?)
                } else {
                    Err(anyhow!("cannot resolve online schemas").into())
                }
            }
            _ => Err(anyhow!("scheme is not supported").into()),
        }
    }
}
