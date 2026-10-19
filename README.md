# Devicetree tools

> 🚧 NOTE: `dt-tools` is experimental

`dt-tools` helps you to write, debug and read Devicetree files more efficiently with advanced tooling!

* [IDE integration](#lsp)
* Linting
* Devicetree binding validation
* C preprocessing matching the kernel's `cpp -x assembler-with-cpp` invocation for the constructs used by DTS.
* Extensive unit and integration tests written in Rust
* Possibly in the future:
  + Comparing DTS or DTB files
  + Comparing Android `dt` and `dtbo` partitions with `/sys/firmware/fdt`

## Screenshots

![Syntax error resiliency showcase in Neovim](screenshots/syntax_error_showcase.dts.png)

![Lint showcase in Neovim](screenshots/lint_showcase.dts.png)

## Help needed

Any kinds of contributions are welcome! Just spreading awareness helps!

See the [GitHub issues](https://github.com/axelkar/dt-tools/issues), especially the ones tagged with `help wanted` or `good first issue`.

If you can, take care of code containing `TODO`, `FIXME`, `todo!(`, `unimplemented!(` or similar

## Contributing

Please read [CONTRIBUTING.md](CONTRIBUTING.md).

## LSP

`dt-tools` implements a [language server](https://code.visualstudio.com/api/language-extensions/language-server-extension-guide#why-language-server) in the [`dt-tools-lsp`](./crates/lsp) crate.

The VSCode extension's documentation can be found [here](vscode/README.md).

The language server is also packaged for Neovim with the Lua script in [`./crates/lsp/lsp.lua`](./crates/lsp/lsp.lua).

```sh
cargo build -p dt-tools-lsp
nvim --cmd 'luafile ./crates/lsp/lsp.lua' lint_showcase.dts
```

Features:

* Fast and safe parser and linter
* (TODO) View binding documentation straight from DTS!
* (TODO) Binding errors integrated into linter

## License

Licensed under either of

 * Apache License, Version 2.0
   ([LICENSE-APACHE](LICENSE-APACHE) or <http://www.apache.org/licenses/LICENSE-2.0>)
 * MIT license
   ([LICENSE-MIT](LICENSE-MIT) or <http://opensource.org/licenses/MIT>)

at your option.

## Contributions

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
