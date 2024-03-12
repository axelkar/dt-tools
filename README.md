# Devicetree tools

* `crates/dt-parser` (W.I.P.) implements an error-resistant parser inspired by rust-analyzer
* `crates/dt-analyzer` (W.I.P.) is a source file analyzer
* `crates/dt-lsp` (W.I.P.) is a [language server](https://code.visualstudio.com/api/language-extensions/language-server-extension-guide#why-language-server) for devicetree
* `crates/dt-lint` (W.I.P.) is a linter - currently only exposed to the LSP
* `crates/dt-binding-matcher` (W.I.P.) is a crate supposed to validate and match devicetree binding YAMLs

## Screenshots

![Syntax error resiliency showcase in Neovim](screenshots/syntax_error_showcase.dts.png)

![Lint showcase in Neovim](screenshots/lint_showcase.dts.png)

## TODO

* [ ] dt-diff: Compare DTS or DTB files
* [ ] something to compare Android dt+dtbo partitions to /sys/firmware/fdt
* [ ] binding matcher with documentation support

## Testing

1. Install [Rust](https://www.rust-lang.org/learn/get-started)
2. Clone this repo:
   ```sh
   git clone https://git.axka.fi/dt-tools.git dt-tools
   cd dt-tools
   ```
3. Run some commands:
   ```sh
   # Showcase parser output
   cargo run --bin test_parser ./crates/dt-parser/a.dts
   cargo run --bin test_parser ./syntax_error_showcase.dts
   # Showcase analyzer output
   cargo run --bin test_analyzer ./syntax_error_showcase.dts
   cargo test # Run unit tests
   ```

### LSP

The language server is currently only packaged for Neovim with the `./crates/dt-lsp/lsp.lua` script. I'd appreciate efforts to make a plugin for VSCode.

```sh
cd ./crates/dt-lsp
nvim -S lsp.lua ../../lint_showcase.dts
```

## Contributing patches

You can either make a GitHub [pull request](https://github.com/axelkar/dt-tools/pulls) or email me directly:

0. Setup `git send-email`:

   <https://git-send-email.io/>

1. Commit your changes, this will open up a text editor

   `git commit`

2. Send your patches to me. The command sends the last commit

   `git send-email --to="axel@axka.fi" HEAD^`

## License

Licensed under either of

 * Apache License, Version 2.0
   ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license
   ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.


Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.

