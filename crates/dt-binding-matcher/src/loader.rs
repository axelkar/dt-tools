use anyhow::{anyhow, Context as _};
use fluent_uri::Uri;
use serde_json::Value;
use std::path::PathBuf;

// TODO: "workspace dirs", read all the $id's and use here
// TODO(axka 2025-05-14): what does $id mean?

pub struct DtJsonSchemaLoader;

impl boon::UrlLoader for DtJsonSchemaLoader {
    fn load(&self, url_str: &str) -> Result<Value, Box<dyn std::error::Error>> {
        let uri = Uri::parse(url_str)?;
        match uri.scheme().as_str() {
            "http" | "https" => {
                if uri.authority().map(|auth| auth.host()) == Some("devicetree.org") {
                    let abs_path = uri
                        .path()
                        .as_str()
                        .strip_prefix('/')
                        .ok_or_else(|| anyhow!("devicetree.org url has no path portion"))?;

                    let mut path_a =
                        PathBuf::from(concat!(env!("CARGO_MANIFEST_DIR"), "/dt-schema/dtschema"));
                    path_a.push(abs_path);

                    let path_b = if let Some(abs_path) = abs_path.strip_prefix("schemas/") {
                        let mut path_b = PathBuf::from(
                            "/home/axel/dev/mainlining/linux/Documentation/devicetree/bindings/",
                        );
                        path_b.push(abs_path);
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
