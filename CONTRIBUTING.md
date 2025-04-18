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
   git clone --recurse-submodules https://github.com/axelkar/dt-tools.git dt-tools
   cd dt-tools
   ```
3. Try the unit tests and examples
   ```sh
   cargo test # Run unit and integration tests
   # Showcase parser output
   cargo run --example test_parser ./crates/dt-parser/a.dts
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

Commit messages should be written according to the [conventional commit specification](https://www.conventionalcommits.org/en/v1.0.0/).

The following commit types are supported:

- `feat:`, New features
- `fix:`, Bug fixes
- `docs:`, Documentation-only changes
- `refactor:`, A code change that neither fixes a bug nor adds a feature
- `perf:`, Performance improvements
- `test:`, Adding missing tests or correcting existing tests (If possible, add
  tests in the same commits as the changes they're testing) 
- `chore:`, Other changes that don't modify source code or documentation (e.g. Modifying Cargo.toml)

Below are examples of well-formatted commit titles:

```
feat(lint)!: rework linting API
feat(parser): implement parsing for new type of files
fix: fix nasty unhandled error
docs: fix link to website page
test(parser): add more cases to handle invalid files
```

Please write your commit messages with regard to the [API versioning](https://doc.rust-lang.org/cargo/reference/semver.html):

- `feat:` to trigger a minor version bump at the next release
- `fix:`, `test:`, `refactor:` or `perf:` to trigger a patch version bump at the next release
- `!` before the colon in the title and `BREAKING CHANGE:` in the footer to
  trigger a major version bump at the next release

## Changelog

All notable changes to the project should be documented in [CHANGELOG.md](CHANGELOG.md).

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
