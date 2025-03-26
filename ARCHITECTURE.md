# Architecture

## Analyzer

Work in progress, see https://github.com/axelkar/dt-tools/issues/5

## Parser

The parser utilizes a neat technique called CSTs (concrete syntax trees), which
means that the syntax tree can be modified and translated back into source
code. Basically it means that 100% of the text from a source file is stored and
could be formatted, even syntax errors.

It largely takes inspiration from rust-analyzer, C#'s Roslyn and
[rowan](https://lib.rs/crates/rowan). Cstree or rowan aren't currently used
because I want to keep node and token kinds separate and avoid unsafe code, but
see https://github.com/axelkar/dt-tools/issues/9

I recommend trying the commands below and reading through the
[`test_data` folder](crates/dt-parser/test_data) to get an idea of how source
code is parsed.


```dts
// Written to ./crates/dt-parser/a.dts
/dts-v1/;

/ {};
```
```console
$ cargo run --example=test_parser ./crates/dt-parser/a.dts
    Finished `dev` profile [unoptimized + debuginfo] target(s) in 0.09s
     Running `target/debug/examples/test_parser ./crates/dt-parser/a.dts`
Parsed!
SourceFile@0..17
  Directive@0..9
    V1Directive@0..8 "/dts-v1/"
    Semicolon@8..9 ";"
  Whitespace@9..11 "\n\n"
  DtNode@11..16
    Slash@11..12 "/"
    Whitespace@12..13 " "
    LCurly@13..14 "{"
    RCurly@14..15 "}"
    Semicolon@15..16 ";"
  Whitespace@16..17 "\n"
```
```console
$ cargo run --example=test_lexer ./crates/dt-parser/a.dts
    Finished `dev` profile [unoptimized + debuginfo] target(s) in 0.10s
     Running `target/debug/examples/test_lexer ./crates/dt-parser/a.dts`
‘/dts-v1/‘ "/dts-v1/" 0..8
‘;’ ";" 8..9
whitespace "\n\n" 9..11
‘/’ "/" 11..12
whitespace " " 12..13
‘{’ "{" 13..14
‘}’ "}" 14..15
‘;’ ";" 15..16
whitespace "\n" 16..17
```
