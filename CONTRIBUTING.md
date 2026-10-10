# Contributing

Thank you for your interest in contributing to dt-tools!

We welcome and appreciate any form of contributions.

## Asking questions

If you have any questions or feedback, open a [GitHub discussion](https://github.com/axelkar/dt-tools/discussions).

## Reporting bugs

[GitHub issues](https://github.com/axelkar/dt-tools/issues) are used for
submitting bugs. Make sure that the bug you are reporting has not been reported
yet or fixed on the master branch.

## Getting started

> [!TIP]
> Using [Nix](https://nixos.org/),
> [nix-direnv](https://github.com/nix-community/nix-direnv) and
> [direnv](https://direnv.net/) you can automatically get the
> required dependencies.

1. Install [Rust](https://www.rust-lang.org/learn/get-started)
2. Clone this repo:
   ```sh
   git clone https://github.com/axelkar/dt-tools.git dt-tools
   cd dt-tools
   ```
3. Try the unit tests and examples
   ```sh
   cargo test # Run unit and integration tests
   # Showcase parser output
   cargo run --example test_parser ./syntax_error_showcase.dts
   # Showcase analyzer output
   cargo run --example test_analyzer ./syntax_error_showcase.dts
   ```
4. Explore the high-level structure of dt-tools in [ARCHITECTURE.md](ARCHITECTURE.md)

## Testing

You can run all unit and integration tests by running the follwoing command:
```sh
cargo test
```

It's recommended to add tests for all new functionality, and to bug fixes to
make sure regressions don't come back.

## Commit messages

When referencing GitHub issues or pull requests in commits, please use the URLs
instead of just a hashtag and a number.

Write a commit message's subject line using the [imperative mood](https://en.wikipedia.org/wiki/Imperative_mood). It must start with a command verb, for example (but not limited to) `Add`, `Refactor`, `Bump`, `Document`, `Fix`, `Deprecate` (based on [Common Changelog's change name convention](https://common-changelog.org/#241-change)).

## Changelog

All notable changes to the project will be documented during releases in [CHANGELOG.md](CHANGELOG.md).

## Submitting changes

1. Please first make sure that you have not introduced any regressions and
   format the code by running the following commands at the repository root.
   ```sh
   cargo fmt
   cargo clippy
   cargo test
   ```
2. Make a GitHub [pull request](https://github.com/axelkar/dt-tools/pulls).

## Releases

When making a release use `cargo-release`
