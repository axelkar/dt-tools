# Contributing

Thank you for your interest in contributing to dt-tools!

We welcome and appreciate any form of contributions.

## Asking questions

If you have any questions or feedback, open a [GitHub discussion](https://github.com/axelkar/dt-tools/discussions).

## Reporting bugs

[GitHub issues](https://github.com/axelkar/dt-tools/issues) serves as a place
for submitting bugs. Make sure that the bug you are reporting has not been
reported yet or fixed on the master branch.

If you have any questions or feedback, open a [GitHub discussion](https://github.com/axelkar/dt-tools/discussions).

## Getting started

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

It's recommended to add tests for all new functionality, and to fixes to make sure regressions don't come back.

## Commit messages

It's recommended to adhere to the [conventional commit specification](https://www.conventionalcommits.org/en/v1.0.0/).

The following commit prefixes are supported:

- `feat:`, New features
- `fix:`, Bug fixes
- `docs:`, Documentation-only changes
- `refactor:`, A code change that neither fixes a bug nor adds a feature
- `perf:`, Performance improvements
- `test:`, Adding missing tests or correcting existing tests (If possible, add tests in the same commits as the changes they're testing) 
- `chore:`, Other changes that don't modify source code or documentation (e.g. Modifying Cargo.toml)

Please write your commit names with regard to the [API versioning](https://doc.rust-lang.org/cargo/reference/semver.html):

- `feat:` to trigger a minor version bump at the next release
- `fix:`, `test:`, `refactor:` or `perf:` to trigger a patch version bump at the next release
- `!` before the colon in the title and `BREAKING CHANGE:` in the footer  to
  trigger a major version bump at the next release

Below are examples of well-formatted commit titles:

```txt
feat!(lint): rework linting API
feat(parser): implement parsing for new type of files
fix: fix nasty unhandled error
docs: fix link to website page
test(parser): add more cases to handle invalid files
```

## Changelog

All notable changes to the project should be documented in [CHANGELOG.md](CHANGELOG.md).

## Submitting changes

1. Please first make sure that you have not introduced any regressions and format the code by running the following commands at the repository root.
   ```sh
   cargo fmt
   cargo clippy
   cargo test
   ```
2. Please make a GitHub [pull request](https://github.com/axelkar/dt-tools/pulls).
