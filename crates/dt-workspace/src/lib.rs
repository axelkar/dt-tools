use std::path::{Path, PathBuf};

use config::{CombinedConfig, toml_config};
use search::search;

pub mod config;
mod search;

/// A marker to determine the Linux kernel tree root
const LINUX_MARKER: &str = "dt-bindings/interrupt-controller/arm-gic.h";

#[derive(Debug)]
pub struct Workspace {
    pub path: PathBuf,

    pub config: CombinedConfig,
}

impl Workspace {
    /// Finds a workspace directory from a `start` directory.
    ///
    /// `start` can be any existing directory.
    /// Use the **absolute** current working directory if unsure.
    ///
    /// For documentation on the heuristics, see [`WorkspacePathFindResult`].
    #[must_use]
    pub fn find_workspace_dir(start: &Path) -> WorkspacePathFindResult {
        search(start, toml_config::CONFIG_FILENAME, |f| f.is_file())
            .map(
                |(workspace_dir, toml_file_path)| WorkspacePathFindResult::TomlConfig {
                    toml_file_path,
                    workspace_dir,
                },
            )
            .or_else(|| {
                search(start, LINUX_MARKER, |f| f.is_file()).map(|(workspace_dir, _)| {
                    WorkspacePathFindResult::LinuxMarker { workspace_dir }
                })
            })
            .unwrap_or(WorkspacePathFindResult::Fallback {
                workspace_dir: start,
            })
    }
}

/// The result given by [`Workspace::find_workspace_dir`].
///
/// Ordering of heuristics:
///
/// 1. `TomlConfig`
/// 2. `LinuxMarker`
/// 3. `Fallback`
#[derive(Debug)]
pub enum WorkspacePathFindResult<'start> {
    /// TOML config file ([`toml_config::CONFIG_FILENAME`]) is a sibling of the start directory or its ancestors.
    TomlConfig {
        toml_file_path: PathBuf,
        workspace_dir: &'start Path,
    },
    /// Linux kernel (Using the marker file [`LINUX_MARKER`]) is a sibling of the start directory or its ancestors.
    LinuxMarker { workspace_dir: &'start Path },
    /// No workspace directory found using the other heuristics. Use the start directory.
    Fallback { workspace_dir: &'start Path },
}

#[cfg(test)]
mod tests {
    use std::env::current_dir;

    use super::*;

    impl<'start> WorkspacePathFindResult<'start> {
        pub fn unwrap_toml(self) -> (PathBuf, &'start Path) {
            match self {
                Self::TomlConfig {
                    toml_file_path,
                    workspace_dir,
                } => (toml_file_path, workspace_dir),
                _ => panic!("not a TomlConfig"),
            }
        }
    }

    #[test]
    fn find() {
        // toml
        let target = current_dir().unwrap();
        assert_eq!(
            Workspace::find_workspace_dir(target.as_path()).unwrap_toml(),
            (target.join(toml_config::CONFIG_FILENAME), target.as_path())
        );
    }
}
