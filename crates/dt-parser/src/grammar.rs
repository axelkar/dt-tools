use std::borrow::Cow;

use constcat::concat_slices;
#[cfg(feature = "grammar-tracing")]
use tracing::debug;

use crate::{
    cst::NodeKind,
    lexer::TokenKind,
    parser::{CompletedMarker, Expected, Marker, Parser},
};

pub(crate) mod expr;

macro_rules! vis {
    (begin) => {
        #[cfg(feature = "visualize")]
        crate::parser::visualizer::Event::GramBegin(vis!(@function_name)).visualize();
    };
    (end) => {
        #[cfg(feature = "visualize")]
        crate::parser::visualizer::Event::GramEnd(vis!(@function_name)).visualize();
    };
    (@function_name) => {{
        // Okay, this is ugly, I get it. However, this is the best we can get on a stable rust.
        fn f() {}
        fn type_name_of<T>(_: T) -> &'static str {
            std::any::type_name::<T>()
        }
        let name = type_name_of(f);
        // `3` is the length of the `::f`.
        &name[..name.len() - 3]
    }};
}
// Export the macro with path-based scoping
pub(crate) use vis;

/// Parses a macro invocation.
///
/// May or may not have arguments.
///
/// As this is used in DTS cells, DTS values and preprocessor conditionals, we know that any idents must be macros, even without arguments.
///
/// In references, labels, nodes and properties they may be just names, depending on a macro being named
/// the same.
///
/// e.g. `FOO(bar, 1234)`
fn macro_invocation(m: Marker, p: &mut Parser) -> CompletedMarker {
    assert!(p.eat(TokenKind::Ident));
    if p.at_immediate(TokenKind::LParen) {
        p.bump();
        let mut level = 0usize;
        if !p.at(TokenKind::RParen) {
            let mut param_m = p.start();
            loop {
                match p.peek() {
                    Some(TokenKind::LParen) => level += 1,
                    Some(TokenKind::RParen) => {
                        if level == 0 {
                            break;
                        }
                        level -= 1;
                    }
                    Some(TokenKind::Comma) if level == 0 => {
                        param_m.complete(p, NodeKind::MacroArgument);
                        p.bump();
                        param_m = p.start();
                        continue;
                    }
                    None => break,
                    _ => {}
                }
                p.bump();
            }
            param_m.complete(p, NodeKind::MacroArgument);
        }
        p.expect(TokenKind::RParen);
    }
    m.complete(p, NodeKind::MacroInvocation)
}

/// Parses a reference without the leading ampersand and a node.
pub(super) fn reference_noamp(p: &mut Parser) {
    if p.eat(TokenKind::LCurly) {
        while !p.at(TokenKind::RCurly)
            && !p.at_end()
            && !p.silent_at_set(CONTINUE_COND_SET)
            && !p.silent_at(TokenKind::EndifDirective)
        {
            // TODO: better recovery
            p.expect(TokenKind::Slash);
            if !p.eat_name() {
                p.error().msg_expected().emit();
            }
        }
        p.expect(TokenKind::RCurly);
    } else if p.silent_at_macro_invocation_with_args() {
        macro_invocation(p.start(), p);
    } else if p.at_label_name() {
        p.bump_label_name();
    } else {
        p.error().msg_expected().bump_wrap_err().emit();
    }
}

/// Parses a Devicetree reference.
///
/// - Form: `&foo` | `&{/path}`.
#[cfg_attr(feature = "grammar-tracing", tracing::instrument(skip_all))]
fn reference(p: &mut Parser) {
    vis!(begin);
    #[cfg(feature = "grammar-tracing")]
    debug!("reference start");

    let m = p.start();

    assert!(p.eat(TokenKind::Ampersand));

    reference_noamp(p);

    // TODO: rename everything phandle to reference
    m.complete(p, NodeKind::DtPhandle);

    #[cfg(feature = "grammar-tracing")]
    debug!("reference end");
    vis!(end);
}

/// Parses cells.
///
/// `AT_EOF`: whether a successful end of cells is determined by a `>` or end-of-file
pub(super) fn cells<const AT_EOF: bool>(p: &mut Parser) -> Result<(), ()> {
    loop {
        p.add_expected(Expected::Cell);
        if p.silent_at_set(&[TokenKind::Number, TokenKind::Char]) {
            p.bump();
        } else if p.silent_at(TokenKind::Ident) {
            macro_invocation(p.start(), p);
        } else if p.silent_at(TokenKind::Ampersand) {
            reference(p);
        } else if p.silent_at(TokenKind::LParen) {
            // Start a parantesized expression
            dt_expr(p);
        } else if (AT_EOF && {
            p.add_expected(Expected::Eof);
            p.at_end()
        }) || (!AT_EOF && p.at(TokenKind::RAngle))
        {
            break;
        } else if p.silent_at_set(&[TokenKind::Semicolon, TokenKind::LCurly, TokenKind::RCurly])
            || p.at_end()
            || p.silent_at_set(CONTINUE_COND_SET)
            || p.silent_at(TokenKind::EndifDirective)
        {
            p.error().msg_expected().emit();
            return Err(());
        } else {
            p.error().msg_expected().bump_wrap_err().emit();
        }
    }

    Ok(())
}

/// Parses a Devicetree expression within parenthesis.
///
/// [From specification]: "values may be represented as arithmetic, bitwise, or logical expressions within parenthesis"
///
/// - Form: `(1 + 2 + PREPROCESSOR_MACRO)`.
///
/// [spec]: https://devicetree-specification.readthedocs.io/en/latest/chapter6-source-language.html#node-and-property-definitions
fn dt_expr(p: &mut Parser) {
    let m = p.start();

    assert!(p.eat(TokenKind::LParen));

    expr::expr(p, false);

    p.expect(TokenKind::RParen);

    m.complete(p, NodeKind::DtExpr);
}

/// Parses a Devicetree cell list.
///
/// - Form: `<1>`.
fn dt_cell_list(p: &mut Parser) -> Result<(), ()> {
    vis!(begin);
    let m = p.start();

    assert!(p.eat(TokenKind::LAngle));

    if cells::<false>(p).is_err() {
        m.complete(p, NodeKind::ParseError);
        return Err(());
    }
    p.expect(TokenKind::RAngle);

    m.complete(p, NodeKind::DtCellList);
    vis!(end);
    Ok(())
}

const ITEM_RECOVERY_SET: &[TokenKind] = concat_slices!(
[TokenKind]:
    &[
        TokenKind::Slash,
        // Name {
        TokenKind::Ident,
        TokenKind::Number,
        TokenKind::Comma,
        TokenKind::Minus,
        // }
        TokenKind::Ampersand,
        TokenKind::Equals,
        TokenKind::LCurly,
        TokenKind::Semicolon,
        TokenKind::V1Directive,
        TokenKind::PluginDirective,
        TokenKind::DtIncludeDirective,
        TokenKind::MemreserveDirective,
        TokenKind::DeleteNodeDirective,
        TokenKind::DeletePropertyDirective,
        // RCurly should be conditionally here?
        TokenKind::RCurly,

        TokenKind::EndifDirective,
    ],
    &BEGIN_COND_SET,
    &CONTINUE_COND_SET
);

pub(super) fn propvalues(p: &mut Parser, ending_kinds: &[TokenKind]) -> Result<(), ()> {
    const PROPERTY_VALUE_RECOVERY_SET: &[TokenKind] = &[
        TokenKind::String,
        TokenKind::LAngle,
        TokenKind::DtBytestring,
        TokenKind::Ampersand,
    ];

    while !p.at_end() {
        p.add_expected(Expected::Value);
        if p.silent_at(TokenKind::String) {
            p.bump();
        } else if p.silent_at(TokenKind::LAngle) {
            dt_cell_list(p)?;
        } else if p.silent_at(TokenKind::Ampersand) {
            reference(p);
        } else if p.silent_at(TokenKind::Ident) {
            macro_invocation(p.start(), p);
        } else if p.silent_at(TokenKind::DtBytestring) {
            p.bump();
        } else {
            p.error().msg_expected().bump_wrap_err().emit();
            break;
        }

        if p.eat(TokenKind::Comma) {
        } else if p.at_set(ending_kinds) {
            // This is here and not in the while loop's head to add them to the `expected` list,
            // for the proper error message.
            break;
        } else if p.silent_at_set(PROPERTY_VALUE_RECOVERY_SET) {
            // Missing comma but can be recovered
            p.error().msg_expected().emit();
        } else {
            break;
        }
    }
    Ok(())
}

/// The caller is expected to handle the label and name.
///
/// - Form: `= "foo", <1>;` | `;`.
fn dt_property(p: &mut Parser, m: Marker) -> CompletedMarker {
    vis!(begin);
    if p.eat(TokenKind::Semicolon) {
        return m.complete(p, NodeKind::DtProperty);
    }

    assert!(p.eat(TokenKind::Equals));

    if p.eat(TokenKind::BitsDirective) {
        p.expect(TokenKind::Number);
    }

    let list_m = p.start();
    if propvalues(p, &[TokenKind::Semicolon]).is_err() {
        list_m.complete(p, NodeKind::PropValueList);
        return m.complete(p, NodeKind::ParseError);
    }
    list_m.complete(p, NodeKind::PropValueList);

    p.expect_recoverable(TokenKind::Semicolon, ITEM_RECOVERY_SET);

    vis!(end);
    m.complete(p, NodeKind::DtProperty)
}

/// The caller is expected to handle the label, ampersand, name and unit address.
///
/// - Form: `{ foo = "bar"; baz {}; };`.
#[cfg_attr(feature = "grammar-tracing", tracing::instrument(skip_all))]
fn dt_node_body(p: &mut Parser, m: Marker) {
    vis!(begin);
    #[cfg(feature = "grammar-tracing")]
    debug!("dt_node_body start");

    let lcurly_span = p
        .range()
        .expect("should not be at end-of-file with caller guarantee");

    // TODO: convert other grammars to assert eat
    assert!(p.eat(TokenKind::LCurly));

    while !p.at(TokenKind::RCurly) && !p.at_end() {
        item(p);
    }

    if p.at_end() {
        // TODO: Is there a way to combine this with `,` and `;`
        p.error()
            .msg_custom(Cow::Borrowed("Expected `}`, but found end-of-file"))
            .add_span_label(lcurly_span, Cow::Borrowed("Unclosed delimiter"))
            .emit();
        m.complete(p, NodeKind::DtNode);
        return;
    }

    p.expect(TokenKind::RCurly);

    p.expect_recoverable(TokenKind::Semicolon, ITEM_RECOVERY_SET);

    m.complete(p, NodeKind::DtNode);

    #[cfg(feature = "grammar-tracing")]
    debug!("dt_node_body end");
    vis!(end);
}

// TODO: from r-a: mod_contents: while !(p.at(EOF) || (p.at(T!['}']) && stop_on_r_curly)) {
#[cfg_attr(feature = "grammar-tracing", tracing::instrument(skip_all))]
#[expect(clippy::too_many_lines, reason = "no good way to make this shorter")]
fn item(p: &mut Parser) {
    vis!(begin);
    #[cfg(feature = "grammar-tracing")]
    debug!("item start");

    let m = p.start();
    if p.eat(TokenKind::Slash) {
        if p.at(TokenKind::LCurly) {
            dt_node_body(p, m);
            // node
        } else {
            p.error().msg_expected().complete(m).emit();
        }
    } else if p.at_name() {
        let mut m = m;
        // parse a node or a property

        if p.silent_at_macro_invocation_with_args() {
            m = macro_invocation(m, p).precede(p);
        } else {
            p.bump_name();
        }

        if p.eat(TokenKind::Colon) {
            // label
            // TODO: include this in the DtNode or DtProperty
            // Actually I think this _is_ included because of the precede
            m = m.complete(p, NodeKind::DtLabel).precede(p);

            while p.at_name() {
                if p.silent_at_macro_invocation_with_args() {
                    m = macro_invocation(m, p).precede(p);
                } else {
                    p.bump_name();
                }

                if p.eat(TokenKind::Colon) {
                    m = m.complete(p, NodeKind::DtLabel).precede(p);
                } else if p.at(TokenKind::Ampersand) {
                    // label + extension e.g. `bar: &foo {};`
                    reference(p);
                    break;
                } else {
                    break;
                }
            }
        }

        // TODO: add AtSign to NAME_SET and don't treat the unit address specially?
        if p.at(TokenKind::AtSign) {
            let m = p.start();
            // unit address
            p.bump();
            if !p.eat_name() {
                p.error().msg_expected().emit();
            }
            m.complete(p, NodeKind::UnitAddress);
        }

        if p.at(TokenKind::Equals) || p.at(TokenKind::Semicolon) {
            dt_property(p, m);
        } else if p.silent_at(TokenKind::RCurly) {
            // TODO: what if inside a node?
            // axka 2025-04-21: I think this could just be "ignored" and left to the caller of item

            p.error().bump().complete(m).msg_expected().emit();
        } else if p.at(TokenKind::LCurly) {
            dt_node_body(p, m);
        } else {
            p.error().msg_expected().emit();

            if !p.silent_at_set(ITEM_RECOVERY_SET) && !p.at_end() {
                p.bump();
            }

            m.complete(p, NodeKind::ParseError);
        }
    } else if p.at(TokenKind::Ampersand) {
        // parse a node

        reference(p);

        if p.eat(TokenKind::AtSign) {
            // unit address
            // FIXME: move to reference
            p.expect(TokenKind::Ident);
        }

        if p.at(TokenKind::Equals) || p.at(TokenKind::Semicolon) {
            dt_property(p, m);
        } else if p.at(TokenKind::LCurly) {
            dt_node_body(p, m);
        } else {
            p.error().msg_expected().complete(m).emit();
        }
    } else if p.silent_at(TokenKind::Equals) {
        p.error()
            .msg_expected()
            .add_hint(Cow::Borrowed("Recovered as unnamed property"))
            .emit();

        let m_prop = p.start();
        dt_property(p, m_prop);
        m.complete(p, NodeKind::ParseError);
    } else if p.silent_at(TokenKind::LCurly) {
        // TODO: lint & analyze unnamed nodes, remove ParseError wrap
        p.error()
            .msg_expected()
            .add_hint(Cow::Borrowed("Recovered as unnamed node"))
            .emit();

        let m_node = p.start();
        dt_node_body(p, m_node);
        m.complete(p, NodeKind::ParseError);
    } else if p.silent_at(TokenKind::Semicolon) {
        p.error().msg_custom(Cow::Borrowed("Unmatched `;`")).emit();

        p.bump();
        m.complete(p, NodeKind::ParseError);
    } else if p.at_set(&[TokenKind::V1Directive, TokenKind::PluginDirective]) {
        p.bump();
        p.expect_recoverable(TokenKind::Semicolon, ITEM_RECOVERY_SET);
        m.complete(p, NodeKind::Directive);
    } else if p.eat(TokenKind::DtIncludeDirective) {
        // TODO: only match this at root
        // When an error is emitted, hint that include directives aren't supported outside the top
        // level

        p.expect(TokenKind::String);
        m.complete(p, NodeKind::Directive);
    } else if p.eat(TokenKind::MemreserveDirective) {
        let m_params = p.start();
        p.expect(TokenKind::Number);
        p.expect(TokenKind::Number);
        m_params.complete(p, NodeKind::DirectiveArguments);

        p.expect_recoverable(TokenKind::Semicolon, ITEM_RECOVERY_SET);
        m.complete(p, NodeKind::Directive);
    } else if p.at_set(&[
        TokenKind::DeleteNodeDirective,
        TokenKind::DeletePropertyDirective,
    ]) {
        // TODO: error for delete-property outside of node
        p.bump();

        let m_params = p.start();
        if p.at(TokenKind::Ampersand) {
            reference(p);
        } else if !p.eat_name() {
            p.error().msg_expected().emit();
        }
        m_params.complete(p, NodeKind::DirectiveArguments);

        p.expect_recoverable(TokenKind::Semicolon, ITEM_RECOVERY_SET);
        m.complete(p, NodeKind::Directive);
    } else {
        p.error().bump().complete(m).msg_expected().emit();
    }

    #[cfg(feature = "grammar-tracing")]
    debug!("item end");
    vis!(end);
}

const BEGIN_COND_SET: &[TokenKind] = &[
    TokenKind::IfndefDirective,
    TokenKind::IfdefDirective,
    TokenKind::IfDirective,
];
const CONTINUE_COND_SET: &[TokenKind] = &[
    TokenKind::ElifndefDirective,
    TokenKind::ElifdefDirective,
    TokenKind::ElifDirective,
    TokenKind::ElseDirective,
];

/// Parses a preprocessor directive into a CST tree.
///
/// - `inside`: Parser function for things inside a preprocessor directive. Don't put a loop inside
///   it, don't explicitly run [`preprocessor_directive`] and don't explicitly handle tokens that
///   end a preprocessor conditional branch, as they are all done automatically.
fn preprocessor_directive(p: &mut Parser, inside: impl Fn(&mut Parser)) -> bool {
    // TODO: this inside nodes

    fn cond_branches_and_end(p: &mut Parser, inside: impl Fn(&mut Parser)) {
        let mut m_branch = p.start();

        while !p.silent_at(TokenKind::EndifDirective) && !p.at_end() {
            if p.silent_at_set(CONTINUE_COND_SET) {
                m_branch.complete(p, NodeKind::PreprocessorBranch);
                p.bump();
                m_branch = p.start();
            } else {
                inside(p);
            }
        }

        m_branch.complete(p, NodeKind::PreprocessorBranch);

        p.expect(TokenKind::EndifDirective);
    }

    p.add_expected(Expected::PreprocessorDirective);

    if p.silent_at_set(BEGIN_COND_SET) {
        let m_cond = p.start();
        p.bump();
        cond_branches_and_end(p, inside);
        m_cond.complete(p, NodeKind::PreprocessorConditional);
        true
    } else if p.silent_at(TokenKind::EndifDirective) {
        p.error()
            .bump_wrap_err()
            .msg_custom(Cow::Borrowed("Unmatched `#endif`"))
            .emit();
        true
    } else if p.silent_at_set(CONTINUE_COND_SET) {
        p.error()
            .msg_custom(Cow::Borrowed(
                "Preprocessor conditional continuation without preceding `#if` or equivalent.",
            ))
            .emit();

        let m_err = p.start();
        p.bump();
        cond_branches_and_end(p, inside);
        m_err.complete(p, NodeKind::ParseError);
        true
    } else if p.silent_at_set(&[
        TokenKind::UndefDirective,
        TokenKind::PragmaDirective,
        TokenKind::DefineDirective,
        TokenKind::IncludeDirective,
        TokenKind::ErrorDirective,
    ]) {
        p.bump();
        true
    } else {
        false
    }
}

pub(super) fn entry_sourcefile(p: &mut Parser) {
    fn toplevel_item(p: &mut Parser) {
        if preprocessor_directive(p, toplevel_item) {
        } else if p.silent_at(TokenKind::RCurly) {
            p.error().msg_custom(Cow::Borrowed("Unmatched `}`")).emit();

            let e = p.start();
            p.bump();
            e.complete(p, NodeKind::ParseError);
        } else {
            item(p);
        }
    }

    while !p.at_end() {
        toplevel_item(p);
    }
}

pub(super) fn entry_name(p: &mut Parser) {
    if p.at_name() {
        p.bump_name();
    } else {
        // This just quits parsing. Is this preferred?
        p.error().msg_expected().bump_wrap_err().emit();
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{Entrypoint, parse};

    use expect_test::{Expect, ExpectFile, expect, expect_file};

    #[track_caller]
    #[expect(clippy::needless_pass_by_value, reason = "ergonomics")]
    fn check_file(src: &str, expect: ExpectFile) {
        let parse_output = parse(src);
        expect.assert_eq(&format!(
            "Errors: {:#?}

Tree:
{}",
            parse_output.errors,
            parse_output.green_node.print_tree()
        ));
    }

    #[track_caller]
    #[expect(clippy::needless_pass_by_value, reason = "ergonomics")]
    fn check(input: &str, expect: Expect) {
        let parse_output = parse(input);
        expect.assert_eq(&format!(
            "Errors: {:#?}

Tree:
{}",
            parse_output.errors,
            parse_output.green_node.print_tree()
        ));
    }

    #[track_caller]
    #[expect(clippy::needless_pass_by_value, reason = "ergonomics")]
    pub(super) fn check_ep(ep: Entrypoint, input: &str, expect: Expect) {
        let parse_output = ep.parse(input);
        expect.assert_eq(&format!(
            "Errors: {:#?}

Tree:
{}",
            parse_output.errors,
            parse_output.green_node.print_tree()
        ));
    }

    #[test]
    fn parse_from_test_data_1() {
        check_file(
            include_str!("../test_data/1.dts"),
            expect_file!["../test_data/1.dts.expect"],
        );
    }

    #[test]
    fn parse_from_test_data_2_macros() {
        check_file(
            include_str!("../test_data/2-macro-def.dts"),
            expect_file!["../test_data/2-macro-def.dts.expect"],
        );
    }

    #[test]
    fn parse_from_test_data_3_preproc() {
        check_file(
            include_str!("../test_data/3-preproc-dir.dts"),
            expect_file!["../test_data/3-preproc-dir.dts.expect"],
        );
    }

    #[test]
    fn try_entrypoint() {
        check_ep(
            Entrypoint::Name,
            "foo",
            expect![[r#"
                Errors: []

                Tree:
                EntryName@0..3
                  Name@0..3 "foo"
            "#]],
        );
        check_ep(
            Entrypoint::ReferenceNoamp,
            "foo",
            expect![[r#"
                Errors: []

                Tree:
                DtPhandle@0..3
                  Name@0..3 "foo"
            "#]],
        );

        check_ep(
            Entrypoint::PropValues,
            "\"foo\", \"bar\"",
            expect![[r#"
                Errors: []

                Tree:
                EntryPropValues@0..12
                  String@0..5 "\"foo\""
                  Comma@5..6 ","
                  Whitespace@6..7 " "
                  String@7..12 "\"bar\""
            "#]],
        );
        check_ep(
            Entrypoint::PropValues,
            "\"foo\";",
            expect![[r#"
                Errors: [
                    ParseError {
                        message: "Expected ‘,’ or end-of-file, but found ‘;’",
                        primary_span: TextRange {
                            start: 5,
                            end: 6,
                        },
                        span_labels: [],
                    },
                ]

                Tree:
                EntryPropValues@0..6
                  String@0..5 "\"foo\""
                  ParseError@5..6
                    Semicolon@5..6 ";"
            "#]],
        );

        check_ep(
            Entrypoint::Cells,
            "1 2",
            expect![[r#"
                Errors: []

                Tree:
                EntryCells@0..3
                  Number@0..1 "1"
                  Whitespace@1..2 " "
                  Number@2..3 "2"
            "#]],
        );

        check_ep(
            Entrypoint::Cells,
            "1 2>",
            expect![[r#"
                Errors: [
                    ParseError {
                        message: "Expected cell or end-of-file, but found ‘>’",
                        primary_span: TextRange {
                            start: 3,
                            end: 4,
                        },
                        span_labels: [],
                    },
                ]

                Tree:
                EntryCells@0..4
                  Number@0..1 "1"
                  Whitespace@1..2 " "
                  Number@2..3 "2"
                  ParseError@3..4
                    RAngle@3..4 ">"
            "#]],
        );
    }

    #[test]
    fn references() {
        // https://devicetree-specification.readthedocs.io/en/latest/chapter6-source-language.html#labels
        // According to the specification, labels can only match [0-9a-zA-Z_]
        // Note how the comma is not included in the label:
        check_ep(
            Entrypoint::PropValues,
            "&foo, &123_foo",
            expect![[r#"
                Errors: []

                Tree:
                EntryPropValues@0..14
                  DtPhandle@0..4
                    Ampersand@0..1 "&"
                    Name@1..4 "foo"
                  Comma@4..5 ","
                  Whitespace@5..6 " "
                  DtPhandle@6..14
                    Ampersand@6..7 "&"
                    Name@7..14 "123_foo"
            "#]],
        );
    }

    /// Item name, extension
    #[test]
    fn macro_positions_as_item_name_extension() {
        check(
            "FOO {}; FOO(bar) {}; &FOO {}; &FOO(bar) {};",
            expect![[r#"
                Errors: []

                Tree:
                SourceFile@0..43
                  DtNode@0..7
                    Name@0..3 "FOO"
                    Whitespace@3..4 " "
                    LCurly@4..5 "{"
                    RCurly@5..6 "}"
                    Semicolon@6..7 ";"
                  Whitespace@7..8 " "
                  DtNode@8..20
                    MacroInvocation@8..16
                      Ident@8..11 "FOO"
                      LParen@11..12 "("
                      MacroArgument@12..15
                        Ident@12..15 "bar"
                      RParen@15..16 ")"
                    Whitespace@16..17 " "
                    LCurly@17..18 "{"
                    RCurly@18..19 "}"
                    Semicolon@19..20 ";"
                  Whitespace@20..21 " "
                  DtNode@21..29
                    DtPhandle@21..25
                      Ampersand@21..22 "&"
                      Name@22..25 "FOO"
                    Whitespace@25..26 " "
                    LCurly@26..27 "{"
                    RCurly@27..28 "}"
                    Semicolon@28..29 ";"
                  Whitespace@29..30 " "
                  DtNode@30..43
                    DtPhandle@30..39
                      Ampersand@30..31 "&"
                      MacroInvocation@31..39
                        Ident@31..34 "FOO"
                        LParen@34..35 "("
                        MacroArgument@35..38
                          Ident@35..38 "bar"
                        RParen@38..39 ")"
                    Whitespace@39..40 " "
                    LCurly@40..41 "{"
                    RCurly@41..42 "}"
                    Semicolon@42..43 ";"
            "#]],
        );
    }

    /// Label definition
    #[test]
    fn macro_positions_as_label_def() {
        check(
            "FOO: bar {}; FOO(bar): bar {};",
            expect![[r#"
                Errors: []

                Tree:
                SourceFile@0..30
                  DtNode@0..12
                    DtLabel@0..4
                      Name@0..3 "FOO"
                      Colon@3..4 ":"
                    Whitespace@4..5 " "
                    Name@5..8 "bar"
                    Whitespace@8..9 " "
                    LCurly@9..10 "{"
                    RCurly@10..11 "}"
                    Semicolon@11..12 ";"
                  Whitespace@12..13 " "
                  DtNode@13..30
                    DtLabel@13..22
                      MacroInvocation@13..21
                        Ident@13..16 "FOO"
                        LParen@16..17 "("
                        MacroArgument@17..20
                          Ident@17..20 "bar"
                        RParen@20..21 ")"
                      Colon@21..22 ":"
                    Whitespace@22..23 " "
                    Name@23..26 "bar"
                    Whitespace@26..27 " "
                    LCurly@27..28 "{"
                    RCurly@28..29 "}"
                    Semicolon@29..30 ";"
            "#]],
        );
    }

    /// As value/cell
    #[test]
    fn macro_positions_as_value_cell() {
        check_ep(
            Entrypoint::PropValues,
            "<FOO FOO(bar)>, FOO, FOO(bar)",
            expect![[r#"
                Errors: []

                Tree:
                EntryPropValues@0..29
                  DtCellList@0..14
                    LAngle@0..1 "<"
                    MacroInvocation@1..4
                      Ident@1..4 "FOO"
                    Whitespace@4..5 " "
                    MacroInvocation@5..13
                      Ident@5..8 "FOO"
                      LParen@8..9 "("
                      MacroArgument@9..12
                        Ident@9..12 "bar"
                      RParen@12..13 ")"
                    RAngle@13..14 ">"
                  Comma@14..15 ","
                  Whitespace@15..16 " "
                  MacroInvocation@16..19
                    Ident@16..19 "FOO"
                  Comma@19..20 ","
                  Whitespace@20..21 " "
                  MacroInvocation@21..29
                    Ident@21..24 "FOO"
                    LParen@24..25 "("
                    MacroArgument@25..28
                      Ident@25..28 "bar"
                    RParen@28..29 ")"
            "#]],
        );
    }

    /// As reference
    #[test]
    fn macro_positions_as_reference() {
        check_ep(
            Entrypoint::PropValues,
            "<&FOO &FOO(bar)>, &FOO, &FOO(bar)",
            expect![[r#"
                Errors: []

                Tree:
                EntryPropValues@0..33
                  DtCellList@0..16
                    LAngle@0..1 "<"
                    DtPhandle@1..5
                      Ampersand@1..2 "&"
                      Name@2..5 "FOO"
                    Whitespace@5..6 " "
                    DtPhandle@6..15
                      Ampersand@6..7 "&"
                      MacroInvocation@7..15
                        Ident@7..10 "FOO"
                        LParen@10..11 "("
                        MacroArgument@11..14
                          Ident@11..14 "bar"
                        RParen@14..15 ")"
                    RAngle@15..16 ">"
                  Comma@16..17 ","
                  Whitespace@17..18 " "
                  DtPhandle@18..22
                    Ampersand@18..19 "&"
                    Name@19..22 "FOO"
                  Comma@22..23 ","
                  Whitespace@23..24 " "
                  DtPhandle@24..33
                    Ampersand@24..25 "&"
                    MacroInvocation@25..33
                      Ident@25..28 "FOO"
                      LParen@28..29 "("
                      MacroArgument@29..32
                        Ident@29..32 "bar"
                      RParen@32..33 ")"
            "#]],
        );
    }

    #[test]
    fn parse_directive() {
        check(
            "
/dts-v1/;
/plugin/;
/delete-node/ node-name;
/delete-node/ &label;
/memreserve/ 0x10000000 0x4000;
",
            expect![[r#"
                Errors: []

                Tree:
                SourceFile@0..100
                  Whitespace@0..1 "\n"
                  Directive@1..10
                    V1Directive@1..9 "/dts-v1/"
                    Semicolon@9..10 ";"
                  Whitespace@10..11 "\n"
                  Directive@11..20
                    PluginDirective@11..19 "/plugin/"
                    Semicolon@19..20 ";"
                  Whitespace@20..21 "\n"
                  Directive@21..45
                    DeleteNodeDirective@21..34 "/delete-node/"
                    Whitespace@34..35 " "
                    DirectiveArguments@35..44
                      Name@35..44 "node-name"
                    Semicolon@44..45 ";"
                  Whitespace@45..46 "\n"
                  Directive@46..67
                    DeleteNodeDirective@46..59 "/delete-node/"
                    Whitespace@59..60 " "
                    DirectiveArguments@60..66
                      DtPhandle@60..66
                        Ampersand@60..61 "&"
                        Name@61..66 "label"
                    Semicolon@66..67 ";"
                  Whitespace@67..68 "\n"
                  Directive@68..99
                    MemreserveDirective@68..80 "/memreserve/"
                    Whitespace@80..81 " "
                    DirectiveArguments@81..98
                      Number@81..91 "0x10000000"
                      Whitespace@91..92 " "
                      Number@92..98 "0x4000"
                    Semicolon@98..99 ";"
                  Whitespace@99..100 "\n"
            "#]],
        );
    }

    #[test]
    fn parse_node() {
        check(
            "/ {};",
            expect![[r#"
            Errors: []

            Tree:
            SourceFile@0..5
              DtNode@0..5
                Slash@0..1 "/"
                Whitespace@1..2 " "
                LCurly@2..3 "{"
                RCurly@3..4 "}"
                Semicolon@4..5 ";"
        "#]],
        );

        check(
            "/ { a = <>; };",
            expect![[r#"
            Errors: []

            Tree:
            SourceFile@0..14
              DtNode@0..14
                Slash@0..1 "/"
                Whitespace@1..2 " "
                LCurly@2..3 "{"
                Whitespace@3..4 " "
                DtProperty@4..11
                  Name@4..5 "a"
                  Whitespace@5..6 " "
                  Equals@6..7 "="
                  Whitespace@7..8 " "
                  PropValueList@8..10
                    DtCellList@8..10
                      LAngle@8..9 "<"
                      RAngle@9..10 ">"
                  Semicolon@10..11 ";"
                Whitespace@11..12 " "
                RCurly@12..13 "}"
                Semicolon@13..14 ";"
        "#]],
        );
    }

    #[test]
    fn parse_property() {
        // Odd syntax supported by dtc:
        check(
            "123 = \"foo\";",
            expect![[r#"
                Errors: []

                Tree:
                SourceFile@0..12
                  DtProperty@0..12
                    Name@0..3 "123"
                    Whitespace@3..4 " "
                    Equals@4..5 "="
                    Whitespace@5..6 " "
                    PropValueList@6..11
                      String@6..11 "\"foo\""
                    Semicolon@11..12 ";"
            "#]],
        );

        check(
            "123, = \"foo\";",
            expect![[r#"
                Errors: []

                Tree:
                SourceFile@0..13
                  DtProperty@0..13
                    Name@0..4 "123,"
                    Whitespace@4..5 " "
                    Equals@5..6 "="
                    Whitespace@6..7 " "
                    PropValueList@7..12
                      String@7..12 "\"foo\""
                    Semicolon@12..13 ";"
            "#]],
        );

        check(
            ",,, = \"foo\";",
            expect![[r#"
                Errors: []

                Tree:
                SourceFile@0..12
                  DtProperty@0..12
                    Name@0..3 ",,,"
                    Whitespace@3..4 " "
                    Equals@4..5 "="
                    Whitespace@5..6 " "
                    PropValueList@6..11
                      String@6..11 "\"foo\""
                    Semicolon@11..12 ";"
            "#]],
        );
    }

    #[test]
    fn parse_trivia() {
        check(
            "  ",
            expect![[r#"
            Errors: []

            Tree:
            SourceFile@0..2
              Whitespace@0..2 "  "
        "#]],
        );
        check(
            "/* test */ // test",
            expect![[r#"
                Errors: []

                Tree:
                SourceFile@0..18
                  BlockComment@0..10 "/* test */"
                  Whitespace@10..11 " "
                  LineComment@11..18 "// test"
            "#]],
        );
    }

    #[test]
    fn parse_macro_invocation() {
        check(
            "a = <FOO(bar, 1234)>, FOO((()), ()), FOO(), FOO;",
            expect![[r#"
                Errors: []

                Tree:
                SourceFile@0..48
                  DtProperty@0..48
                    Name@0..1 "a"
                    Whitespace@1..2 " "
                    Equals@2..3 "="
                    Whitespace@3..4 " "
                    PropValueList@4..47
                      DtCellList@4..20
                        LAngle@4..5 "<"
                        MacroInvocation@5..19
                          Ident@5..8 "FOO"
                          LParen@8..9 "("
                          MacroArgument@9..12
                            Ident@9..12 "bar"
                          Comma@12..13 ","
                          Whitespace@13..14 " "
                          MacroArgument@14..18
                            Number@14..18 "1234"
                          RParen@18..19 ")"
                        RAngle@19..20 ">"
                      Comma@20..21 ","
                      Whitespace@21..22 " "
                      MacroInvocation@22..35
                        Ident@22..25 "FOO"
                        LParen@25..26 "("
                        MacroArgument@26..30
                          LParen@26..27 "("
                          LParen@27..28 "("
                          RParen@28..29 ")"
                          RParen@29..30 ")"
                        Comma@30..31 ","
                        Whitespace@31..32 " "
                        MacroArgument@32..34
                          LParen@32..33 "("
                          RParen@33..34 ")"
                        RParen@34..35 ")"
                      Comma@35..36 ","
                      Whitespace@36..37 " "
                      MacroInvocation@37..42
                        Ident@37..40 "FOO"
                        LParen@40..41 "("
                        RParen@41..42 ")"
                      Comma@42..43 ","
                      Whitespace@43..44 " "
                      MacroInvocation@44..47
                        Ident@44..47 "FOO"
                    Semicolon@47..48 ";"
            "#]],
        );
    }

    #[test]
    fn parse_char() {
        check_ep(
            Entrypoint::Cells,
            r"'\0' '\x01' 2",
            expect![[r#"
                Errors: []

                Tree:
                EntryCells@0..13
                  Char@0..4 "'\\0'"
                  Whitespace@4..5 " "
                  Char@5..11 "'\\x01'"
                  Whitespace@11..12 " "
                  Number@12..13 "2"
            "#]],
        );

        check_ep(
            Entrypoint::Cells,
            r"('\0') ('\x01') (2)",
            expect![[r#"
                Errors: []

                Tree:
                EntryCells@0..19
                  DtExpr@0..6
                    LParen@0..1 "("
                    LiteralExpr@1..5
                      Char@1..5 "'\\0'"
                    RParen@5..6 ")"
                  Whitespace@6..7 " "
                  DtExpr@7..15
                    LParen@7..8 "("
                    LiteralExpr@8..14
                      Char@8..14 "'\\x01'"
                    RParen@14..15 ")"
                  Whitespace@15..16 " "
                  DtExpr@16..19
                    LParen@16..17 "("
                    LiteralExpr@17..18
                      Number@17..18 "2"
                    RParen@18..19 ")"
            "#]],
        );
    }
}
