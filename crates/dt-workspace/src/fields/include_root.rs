use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

use crate::search::{is_exists_and_file, search};

use super::Validate;

const INCLUDE_MARKER: &'static str = "dt-bindings/interrupt-controller/arm-gic.h";

#[derive(Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct IncludeRoot(pub Option<PathBuf>);

impl Validate for IncludeRoot {
    fn is_valid(&self) -> bool {
        match self.0 {
            // root is *directory*..
            Some(ref p) => p.exists() && p.is_dir(),
            None => false,
        }
    }

    fn validate(&mut self, root: &Path) {
        if !self.is_valid() {
            // ..`is_exists_and_file` because we search for marker here
            self.0 = search(&root, INCLUDE_MARKER, is_exists_and_file).map(|p| p.0);
        }
    }
}
