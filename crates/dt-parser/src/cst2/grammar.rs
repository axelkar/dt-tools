use std::borrow::Cow;

#[cfg(feature = "grammar-tracing")]
use tracing::debug;

macro_rules! vis {
    (begin) => {
        #[cfg(feature = "visualize")]
        crate::cst2::parser::visualizer::Event::GramBegin(vis!(@function_name)).visualize();
    };
    (end) => {
        #[cfg(feature = "visualize")]
        crate::cst2::parser::visualizer::Event::GramEnd(vis!(@function_name)).visualize();
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

use crate::cst2::parser::Expected;

use super::{
    lexer::TokenKind,
    parser::{CompletedMarker, Marker, Parser},
    NodeKind,
};

/// Parses a macro invocation.
///
/// May or may not have arguments.
///
/// As cells and values, we know that any idents must be macros, even without arguments.
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
                        } else {
                            level -= 1
                        }
                    }
                    Some(TokenKind::Comma) if level == 0 => {
                        param_m.complete(p, NodeKind::MacroParameter);
                        p.bump();
                        param_m = p.start();
                        continue;
                    }
                    None => break,
                    _ => {}
                }
                p.bump();
            }
            param_m.complete(p, NodeKind::MacroParameter);
        }
        p.expect(TokenKind::RParen);
    }
    m.complete(p, NodeKind::MacroInvocation)
}

/// Parses an int expression.
///
/// - Form: `(1 + 2 + PREPROCESSOR_CONST)`.
// TODO: ternary expressions in macros, see `linux/arch/arm64/boot/dts/marvell/cn9130.dtsi` `CP11X_PCIEx_MEM_BASE`
// TODO: label names from macros, see `linux/arch/arm64/boot/dts/marvell/armada-cp11x.dtsi` line 28
fn dt_expr(p: &mut Parser) {
    vis!(begin);
    let m = p.start();

    // TODO: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
    const OPERATOR_SET: &[TokenKind] = &[
        TokenKind::Plus,
        TokenKind::Asterisk,
        TokenKind::Minus,
        TokenKind::Slash,
        TokenKind::Modulo, // TODO: not in spec?
        TokenKind::BitwiseOr,
    ];

    const EXPR_RECOVERY_SET: &[TokenKind] =
        &[TokenKind::Number, TokenKind::Ident, TokenKind::LParen];

    assert!(p.eat(TokenKind::LParen));

    while !(p.at(TokenKind::RParen) || p.at_end()) {
        if p.silent_at_set(OPERATOR_SET) {
            // Recover if an argument is missing and only got a delimiter,
            // e.g. `a + + b`.
            // TODO: add "Expecter number or preprocessor macro"
            p.error2();
            continue;
        }

        if p.at(TokenKind::Number) {
            p.bump();
        } else if p.at(TokenKind::Ident) {
            macro_invocation(p.start(), p);
        } else if p.at(TokenKind::LParen) {
            // Start a parantesized expression
            dt_expr(p);
        } else {
            p.error2();
            break;
        }

        if p.at_set(OPERATOR_SET) {
            p.bump();
        } else if p.silent_at_set(ITEM_RECOVERY_SET) {
            break;
        } else if p.silent_at_set(EXPR_RECOVERY_SET) {
            p.emit_expect_error();
        } else {
            break;
        }
    }
    p.expect(TokenKind::RParen);

    m.complete(p, NodeKind::DtExpr);
    vis!(end);
}

/// Parses a Devicetree phandle.
///
/// - Form: `&foo` | `&{/path}`.
#[cfg_attr(feature = "grammar-tracing", tracing::instrument(skip_all))]
fn dt_phandle(p: &mut Parser) {
    vis!(begin);
    #[cfg(feature = "grammar-tracing")]
    debug!("dt_phandle start");

    let m = p.start();

    assert!(p.eat(TokenKind::Ampersand));

    if p.at(TokenKind::LCurly) {
        p.bump();
        while !p.at(TokenKind::RCurly) && !p.at_end() {
            // TODO: better recovery
            // TODO: don't allow spaces in path references
            p.expect(TokenKind::Slash);
            if !p.eat_name() {
                p.emit_expect_error();
            }
        }
        p.expect(TokenKind::RCurly);
    } else if !p.eat_name() {
        // According to the DT spec v0.4, labels can only match [0-9a-zA-Z_], but for the IDE
        // use-case I'll just match a name
        p.emit_expect_error();
    }

    m.complete(p, NodeKind::DtPhandle);

    #[cfg(feature = "grammar-tracing")]
    debug!("dt_phandle end");
    vis!(end);
}

/// Parses a Devicetree cell list.
///
/// - Form: `<1>`.
fn dt_cell_list(p: &mut Parser) -> Result<(), ()> {
    vis!(begin);
    let m = p.start();

    assert!(p.eat(TokenKind::LAngle));

    loop {
        p.add_expected(Expected::Cell);
        if p.silent_at_set(&[TokenKind::Number, TokenKind::Char]) {
            p.bump();
        } else if p.silent_at(TokenKind::Ident) {
            macro_invocation(p.start(), p);
        } else if p.silent_at(TokenKind::Ampersand) {
            dt_phandle(p);
        } else if p.silent_at(TokenKind::LParen) {
            // Start a parantesized expression
            dt_expr(p);
        } else if p.at(TokenKind::RAngle) {
            break;
        } else if p.silent_at_set(&[TokenKind::Semicolon, TokenKind::LCurly, TokenKind::RCurly])
            || p.at_end()
        {
            p.emit_expect_error();
            m.complete(p, NodeKind::ParseError);
            return Err(());
        } else {
            p.error2();
        }
    }
    p.expect(TokenKind::RAngle);

    m.complete(p, NodeKind::DtCellList);
    vis!(end);
    Ok(())
}

const ITEM_RECOVERY_SET: &[TokenKind] = &[
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
];

/// The caller is expected to handle the label and name.
///
/// - Form: `= "foo", <1>;` | `;`.
fn dt_property(p: &mut Parser, m: Marker) -> CompletedMarker {
    vis!(begin);
    if p.at(TokenKind::Semicolon) {
        p.bump();
        return m.complete(p, NodeKind::DtProperty);
    }

    assert!(p.eat(TokenKind::Equals));

    const PROPERTY_VALUE_RECOVERY_SET: &[TokenKind] = &[
        TokenKind::String,
        TokenKind::LAngle,
        TokenKind::DtBytestring,
        TokenKind::Ampersand,
    ];

    if p.eat(TokenKind::BitsDirective) {
        p.expect(TokenKind::Number);
    }

    let list_m = p.start();
    while !p.at(TokenKind::Semicolon) && !p.at_end() {
        p.add_expected(Expected::Value);
        if p.silent_at(TokenKind::String) {
            p.bump();
        } else if p.silent_at(TokenKind::LAngle) {
            if dt_cell_list(p).is_err() {
                list_m.complete(p, NodeKind::PropValueList);
                return m.complete(p, NodeKind::ParseError);
            }
        } else if p.silent_at(TokenKind::Ampersand) {
            dt_phandle(p);
        } else if p.silent_at(TokenKind::Ident) {
            macro_invocation(p.start(), p);
        } else if p.silent_at(TokenKind::DtBytestring) {
            p.bump();
        } else {
            p.error2();
            break;
        }

        if p.at(TokenKind::Comma) {
            p.bump();
        } else if p.silent_at_set(PROPERTY_VALUE_RECOVERY_SET) {
            // Missing comma but can be recovered
            p.emit_expect_error();
        } else {
            break;
        }
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

    let lcurly_span = p.range().expect("should not be at end-of-file");

    // TODO: convert other grammars to assert eat
    assert!(p.eat(TokenKind::LCurly));

    while !p.at(TokenKind::RCurly) && !p.at_end() {
        item(p);
    }

    if p.at_end() {
        p.fancy_error(
            Cow::Borrowed("Expected `}`, but found end-of-file"),
            vec![dt_diagnostic::SpanLabel {
                span: lcurly_span,
                msg: Cow::Borrowed("Unclosed delimiter"),
            }],
        );
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

/// TODO: mod_contents: while !(p.at(EOF) || (p.at(T!['}']) && stop_on_r_curly)) {
#[cfg_attr(feature = "grammar-tracing", tracing::instrument(skip_all))]
fn item(p: &mut Parser) {
    vis!(begin);
    #[cfg(feature = "grammar-tracing")]
    debug!("item start");

    let m = p.start();
    if p.at(TokenKind::Slash) {
        p.bump();
        if p.at(TokenKind::LCurly) {
            dt_node_body(p, m);
            // node
        } else {
            p.emit_expect_error();
            m.complete(p, NodeKind::ParseError);
        }
    } else if p.at_name() {
        let mut m = m;
        // parse a node or a property

        if p.silent_at2(TokenKind::Ident, TokenKind::LParen) {
            m = macro_invocation(m, p).precede(p);
        } else {
            p.bump_name();
        }

        if p.at(TokenKind::Colon) {
            // label
            // TODO: include this in the DtNode or DtProperty
            p.bump();
            m = m.complete(p, NodeKind::DtLabel).precede(p);

            while p.at_name() {
                if p.silent_at2(TokenKind::Ident, TokenKind::LParen) {
                    m = macro_invocation(m, p).precede(p);
                } else {
                    p.bump_name();
                }

                if p.at(TokenKind::Colon) {
                    p.bump();

                    m = m.complete(p, NodeKind::DtLabel).precede(p);
                } else if p.at(TokenKind::Ampersand) {
                    // label + extension e.g. `bar: &foo {};`
                    dt_phandle(p);
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
                p.emit_expect_error()
            }
            m.complete(p, NodeKind::UnitAddress);
        }

        if p.at(TokenKind::Equals) || p.at(TokenKind::Semicolon) {
            dt_property(p, m);
        } else if p.silent_at(TokenKind::RCurly) {
            // TODO: what if inside a node?

            p.emit_expect_error();
            p.bump();
            m.complete(p, NodeKind::ParseError);
        } else if p.at(TokenKind::LCurly) {
            dt_node_body(p, m);
        } else {
            p.emit_expect_error();

            if !p.silent_at_set(ITEM_RECOVERY_SET) && !p.at_end() {
                p.bump();
            }

            m.complete(p, NodeKind::ParseError);
        }
    } else if p.at(TokenKind::Ampersand) {
        // parse a node

        dt_phandle(p);

        if p.at(TokenKind::AtSign) {
            // unit address
            // FIXME: move to dt_phandle
            p.bump();
            p.expect(TokenKind::Ident);
        }

        if p.at(TokenKind::Equals) || p.at(TokenKind::Semicolon) {
            dt_property(p, m);
        } else if p.at(TokenKind::LCurly) {
            dt_node_body(p, m);
        } else {
            p.emit_expect_error();
            m.complete(p, NodeKind::ParseError);
        }
    } else if p.silent_at(TokenKind::Equals) {
        p.emit_expect_error();
        p.add_hint(Cow::Borrowed("Recovered as unnamed property"));

        let m_prop = p.start();
        dt_property(p, m_prop);
        m.complete(p, NodeKind::ParseError);
    } else if p.silent_at(TokenKind::LCurly) {
        // TODO: lint & analyze unnamed nodes, remove ParseError wrap
        p.emit_expect_error();
        p.add_hint(Cow::Borrowed("Recovered as unnamed node"));

        let m_node = p.start();
        dt_node_body(p, m_node);
        m.complete(p, NodeKind::ParseError);
    } else if p.silent_at(TokenKind::Semicolon) {
        p.simple_error(Cow::Borrowed("Unmatched `;`"));

        p.bump();
        m.complete(p, NodeKind::ParseError);
    } else if p.at_set(&[TokenKind::V1Directive, TokenKind::PluginDirective]) {
        p.bump();
        p.expect_recoverable(TokenKind::Semicolon, ITEM_RECOVERY_SET);
        m.complete(p, NodeKind::Directive);
    } else if p.at(TokenKind::DtIncludeDirective) {
        // TODO: only match this at root
        // When an error is emitted, hint that include directives aren't supported outside the top
        // level
        p.bump();

        p.expect(TokenKind::String);
        m.complete(p, NodeKind::Directive);
    } else if p.at(TokenKind::MemreserveDirective) {
        p.bump();

        let m_params = p.start();
        p.expect(TokenKind::Number);
        p.expect(TokenKind::Number);
        m_params.complete(p, NodeKind::DirectiveParams);

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
            dt_phandle(p);
        } else if !p.eat_name() {
            p.emit_expect_error()
        }
        m_params.complete(p, NodeKind::DirectiveParams);

        p.expect_recoverable(TokenKind::Semicolon, ITEM_RECOVERY_SET);
        m.complete(p, NodeKind::Directive);
    } else {
        p.error2();
        m.complete(p, NodeKind::ParseError);
    }

    #[cfg(feature = "grammar-tracing")]
    debug!("item end");
    vis!(end);
}

pub(super) fn root(p: &mut Parser) {
    while !p.at_end() {
        if p.at_preprocessor_directive() {
            p.bump();
        } else if p.silent_at(TokenKind::RCurly) {
            p.simple_error(Cow::Borrowed("Unmatched `}`"));

            let e = p.start();
            p.bump();
            e.complete(p, NodeKind::ParseError);
        } else {
            item(p);
        }
    }
}

#[cfg(test)]
pub(super) mod tests {
    use std::sync::Arc;

    use crate::cst2::{
        lexer::TokenKind,
        parser::{parse, ParseError},
        GreenItem, GreenNode, GreenToken, TokenText,
    };

    use super::*;
    use pretty_assertions::assert_eq;

    pub fn node(kind: NodeKind, children: Vec<GreenItem>) -> GreenItem {
        GreenItem::Node(Arc::new(GreenNode {
            kind,
            width: children.iter().map(|item| item.length()).sum(),
            children,
        }))
    }
    pub fn dynamic_token(kind: TokenKind, text: &'static str) -> GreenItem {
        GreenItem::Token(Arc::new(GreenToken {
            kind,
            text: TokenText::Dynamic(text.to_owned()),
        }))
    }
    pub fn static_token(kind: TokenKind) -> GreenItem {
        GreenItem::Token(Arc::new(GreenToken {
            kind,
            text: TokenText::Static(kind.static_text().unwrap()),
        }))
    }
    pub fn ws(text: &'static str) -> GreenItem {
        dynamic_token(TokenKind::Whitespace, text)
    }

    #[track_caller]
    fn check(input: &str, expected_children: Vec<GreenItem>, expected_errors: Vec<ParseError>) {
        let parse_output = parse(input);
        assert_eq!(parse_output.errors, expected_errors);
        assert_eq!(parse_output.green_node.children, expected_children);
    }

    #[test]
    fn parse_directive() {
        check(
            "/dts-v1/;",
            vec![node(
                NodeKind::Directive,
                vec![
                    static_token(TokenKind::V1Directive),
                    static_token(TokenKind::Semicolon),
                ],
            )],
            Vec::new(),
        );

        check(
            "/plugin/;",
            vec![node(
                NodeKind::Directive,
                vec![
                    static_token(TokenKind::PluginDirective),
                    static_token(TokenKind::Semicolon),
                ],
            )],
            Vec::new(),
        );

        check(
            "/delete-node/ node-name;",
            vec![node(
                NodeKind::Directive,
                vec![
                    static_token(TokenKind::DeleteNodeDirective),
                    ws(" "),
                    node(
                        NodeKind::DirectiveParams,
                        vec![dynamic_token(TokenKind::Name, "node-name")],
                    ),
                    static_token(TokenKind::Semicolon),
                ],
            )],
            Vec::new(),
        );

        check(
            "/delete-node/ &label;",
            vec![node(
                NodeKind::Directive,
                vec![
                    static_token(TokenKind::DeleteNodeDirective),
                    ws(" "),
                    node(
                        NodeKind::DirectiveParams,
                        vec![node(
                            NodeKind::DtPhandle,
                            vec![
                                static_token(TokenKind::Ampersand),
                                dynamic_token(TokenKind::Name, "label"),
                            ],
                        )],
                    ),
                    static_token(TokenKind::Semicolon),
                ],
            )],
            Vec::new(),
        );

        check(
            "/memreserve/ 0x10000000 0x4000;",
            vec![node(
                NodeKind::Directive,
                vec![
                    static_token(TokenKind::MemreserveDirective),
                    ws(" "),
                    node(
                        NodeKind::DirectiveParams,
                        vec![
                            dynamic_token(TokenKind::Number, "0x10000000"),
                            ws(" "),
                            dynamic_token(TokenKind::Number, "0x4000"),
                        ],
                    ),
                    static_token(TokenKind::Semicolon),
                ],
            )],
            Vec::new(),
        );
    }

    #[test]
    fn parse_from_test_data_1() {
        let src = include_str!("../../test_data/1.dts");

        let parse_output = parse(src);
        assert_eq!(parse_output.lex_errors, Vec::new());
        assert_eq!(parse_output.errors, Vec::new());

        assert_eq!(
            parse_output.green_node.print_tree(),
            include_str!("../../test_data/1.dts.expect")
        );
    }

    #[test]
    fn parse_from_test_data_2() {
        let src = include_str!("../../test_data/2-macro-def.dts");

        let parse_output = parse(src);
        assert_eq!(parse_output.lex_errors, Vec::new());
        assert_eq!(parse_output.errors, Vec::new());

        assert_eq!(
            parse_output.green_node.print_tree(),
            include_str!("../../test_data/2-macro-def.dts.expect")
        );
    }

    #[test]
    fn parse_node() {
        check(
            "/ {};",
            vec![node(
                NodeKind::DtNode,
                vec![
                    static_token(TokenKind::Slash),
                    ws(" "),
                    static_token(TokenKind::LCurly),
                    static_token(TokenKind::RCurly),
                    static_token(TokenKind::Semicolon),
                ],
            )],
            Vec::new(),
        );

        check(
            "/ { a = <>; };",
            vec![node(
                NodeKind::DtNode,
                vec![
                    static_token(TokenKind::Slash),
                    ws(" "),
                    static_token(TokenKind::LCurly),
                    ws(" "),
                    node(
                        NodeKind::DtProperty,
                        vec![
                            dynamic_token(TokenKind::Name, "a"),
                            ws(" "),
                            static_token(TokenKind::Equals),
                            ws(" "),
                            node(
                                NodeKind::PropValueList,
                                vec![node(
                                    NodeKind::DtCellList,
                                    vec![
                                        static_token(TokenKind::LAngle),
                                        static_token(TokenKind::RAngle),
                                    ],
                                )],
                            ),
                            static_token(TokenKind::Semicolon),
                        ],
                    ),
                    ws(" "),
                    static_token(TokenKind::RCurly),
                    static_token(TokenKind::Semicolon),
                ],
            )],
            Vec::new(),
        );
    }

    #[test]
    fn parse_property() {
        // Odd syntax supported by dtc:
        check(
            "123 = \"foo\";",
            vec![node(
                NodeKind::DtProperty,
                vec![
                    dynamic_token(TokenKind::Name, "123"),
                    ws(" "),
                    static_token(TokenKind::Equals),
                    ws(" "),
                    node(
                        NodeKind::PropValueList,
                        vec![dynamic_token(TokenKind::String, "\"foo\"")],
                    ),
                    static_token(TokenKind::Semicolon),
                ],
            )],
            Vec::new(),
        );

        check(
            "123, = \"foo\";",
            vec![node(
                NodeKind::DtProperty,
                vec![
                    dynamic_token(TokenKind::Name, "123,"),
                    ws(" "),
                    static_token(TokenKind::Equals),
                    ws(" "),
                    node(
                        NodeKind::PropValueList,
                        vec![dynamic_token(TokenKind::String, "\"foo\"")],
                    ),
                    static_token(TokenKind::Semicolon),
                ],
            )],
            Vec::new(),
        );

        check(
            ",,, = \"foo\";",
            vec![node(
                NodeKind::DtProperty,
                vec![
                    dynamic_token(TokenKind::Name, ",,,"),
                    ws(" "),
                    static_token(TokenKind::Equals),
                    ws(" "),
                    node(
                        NodeKind::PropValueList,
                        vec![dynamic_token(TokenKind::String, "\"foo\"")],
                    ),
                    static_token(TokenKind::Semicolon),
                ],
            )],
            Vec::new(),
        );
    }

    #[test]
    fn parse_trivia() {
        check("  ", vec![ws("  ")], Vec::new());
        check(
            "/* test */ // test",
            vec![
                dynamic_token(TokenKind::BlockComment, "/* test */"),
                ws(" "),
                dynamic_token(TokenKind::LineComment, "// test"),
            ],
            Vec::new(),
        );
    }

    #[test]
    fn parse_macro_invocation() {
        check(
            "a = <FOO(bar, 1234)>, FOO((()), ()), FOO(), FOO;",
            vec![node(
                NodeKind::DtProperty,
                vec![
                    dynamic_token(TokenKind::Name, "a"),
                    ws(" "),
                    static_token(TokenKind::Equals),
                    ws(" "),
                    node(
                        NodeKind::PropValueList,
                        vec![
                            node(
                                NodeKind::DtCellList,
                                vec![
                                    static_token(TokenKind::LAngle),
                                    node(
                                        NodeKind::MacroInvocation,
                                        vec![
                                            dynamic_token(TokenKind::Ident, "FOO"),
                                            static_token(TokenKind::LParen),
                                            node(
                                                NodeKind::MacroParameter,
                                                vec![dynamic_token(TokenKind::Ident, "bar")],
                                            ),
                                            static_token(TokenKind::Comma),
                                            ws(" "),
                                            node(
                                                NodeKind::MacroParameter,
                                                vec![dynamic_token(TokenKind::Number, "1234")],
                                            ),
                                            static_token(TokenKind::RParen),
                                        ],
                                    ),
                                    static_token(TokenKind::RAngle),
                                ],
                            ),
                            static_token(TokenKind::Comma),
                            ws(" "),
                            node(
                                NodeKind::MacroInvocation,
                                vec![
                                    dynamic_token(TokenKind::Ident, "FOO"),
                                    static_token(TokenKind::LParen),
                                    node(
                                        NodeKind::MacroParameter,
                                        vec![
                                            static_token(TokenKind::LParen),
                                            static_token(TokenKind::LParen),
                                            static_token(TokenKind::RParen),
                                            static_token(TokenKind::RParen),
                                        ],
                                    ),
                                    static_token(TokenKind::Comma),
                                    ws(" "),
                                    node(
                                        NodeKind::MacroParameter,
                                        vec![
                                            static_token(TokenKind::LParen),
                                            static_token(TokenKind::RParen),
                                        ],
                                    ),
                                    static_token(TokenKind::RParen),
                                ],
                            ),
                            static_token(TokenKind::Comma),
                            ws(" "),
                            node(
                                NodeKind::MacroInvocation,
                                vec![
                                    dynamic_token(TokenKind::Ident, "FOO"),
                                    static_token(TokenKind::LParen),
                                    static_token(TokenKind::RParen),
                                ],
                            ),
                            static_token(TokenKind::Comma),
                            ws(" "),
                            node(
                                NodeKind::MacroInvocation,
                                vec![dynamic_token(TokenKind::Ident, "FOO")],
                            ),
                        ],
                    ),
                    static_token(TokenKind::Semicolon),
                ],
            )],
            Vec::new(),
        );
    }
}
