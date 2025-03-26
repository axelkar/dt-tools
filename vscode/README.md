# dt-tools LSP

A VSCode extension providing a Language Server Protocol client for
[dt-tools](https://github.com/axelkar/dt-tools)'s LSP server.

## Installation

Look for the latest file named `dt-tools-lsp-no-server.vsix` in the
[GitHub releases](https://github.com/axelkar/dt-tools/releases) and install it
with the `Extensions: Install from VSIX` command within VS Code.

## Development

> [!TIP]
> Using [Nix](https://nixos.org/),
> [nix-direnv](https://github.com/nix-community/nix-direnv) and
> [direnv](https://direnv.net/) you can automatically get the
> required dependencies.

1. Make sure you have VS Code or VSCodium and NodeJS.
2. Install the NPM dependencies:
   ```console
   $ npm i
   ```
3. Open [`src/extension.ts`](src/extension.ts) in VS Code or VSCodium and press <kbd>F5</kbd>.

## Packaging

1. Make sure you have [vsce](https://github.com/microsoft/vscode-vsce) installed.
2. Package the extension, while ignoring the warning about a missing license:
   ```console
   $ vsce package -o dt-tools-lsp-no-server.vsix
   ```

## Features

* User command: `dt-tools LSP: Restart dt-tools LSP server`
* Setting: `Dt-tools-lsp › Server: Path`

## Requirements

By default the extension will look for `dt-lsp` in the PATH, but here's how you
get the development version:

1. Build `dt-lsp` to get its executable. Here's how you build and use the debug version.
   ```console
   ~/dev/dt-tools $ cargo build -p dt-lsp
   ```
2. Make the extension find it by configuring the `Dt-tools-lsp › Server: Path`
   setting to the path of the executable, which may be for example
   `/home/axel/dev/dt-tools/target/debug/dt-lsp`

## License

Licensed under either of

 * Apache License, Version 2.0
   ([LICENSE-APACHE](../LICENSE-APACHE) or <http://www.apache.org/licenses/LICENSE-2.0>)
 * MIT license
   ([LICENSE-MIT](../LICENSE-MIT) or <http://opensource.org/licenses/MIT>)

at your option.

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.

The files `syntaxes/dts.tmLanguage.json` and `language-configuration.json` are licensed under the [MIT license](LICENSE-GRAMMAR) by [Pietro Lorefice](https://github.com/plorefice).
