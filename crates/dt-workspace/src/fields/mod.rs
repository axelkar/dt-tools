use std::path::Path;

pub mod include_root;

pub trait Validate {
    /// Checks field value
    fn is_valid(&self) -> bool;
    fn validate(&mut self, root: &Path);
}
