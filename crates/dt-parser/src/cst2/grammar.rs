use std::borrow::Cow;

#[cfg(feature = "grammar-tracing")]
use tracing::debug;

use super::{
    lexer::TokenKind,
    parser::{CompletedMarker, Marker, Parser, SpanLabel},
    NodeKind,
};

/// Parses an int expression.
///
/// - Form: `(1 + 2 + PREPROCESSOR_CONST)`.
/// - When IN_MACRO is true, form: `1 + CONST)` | `1 + CONST`, ending at comma
fn dt_expr<const IN_MACRO: bool>(p: &mut Parser) {
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

    if !IN_MACRO {
        assert!(p.eat(TokenKind::LParen));
    }

    while !(p.at(TokenKind::RParen) || p.at_end() || (IN_MACRO && p.at(TokenKind::Comma))) {
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
            p.bump();
            if p.silent_at_immediate(TokenKind::LParen) {
                p.bump();

                dt_expr::<true>(p);

                while p.at(TokenKind::Comma) {
                    dt_expr::<true>(p);
                }
            }
        } else if p.at(TokenKind::LParen) {
            dt_expr::<false>(p);
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
}

/// Parses a Devicetree phandle.
///
/// - Form: `&foo` | `&{/path}`.
fn dt_phandle(p: &mut Parser) {
    let m = p.start();

    assert!(p.eat(TokenKind::Ampersand));

    if p.at(TokenKind::LCurly) {
        p.bump();
        while !p.at(TokenKind::RCurly) && !p.at_end() {
            // TODO: better recovery
            p.expect(TokenKind::Slash);
            p.expect(TokenKind::Ident);
        }
        p.expect(TokenKind::RCurly);
    } else {
        // According to the DT spec v0.4, labels can only match [0-9a-zA-Z_], but for the IDE
        // use-case I'll just match a name
        p.expect_name()
    }

    m.complete(p, NodeKind::DtPhandle);
}

/// Parses a Devicetree cell list.
///
/// - Form: `<1>`.
fn dt_cell_list(p: &mut Parser) {
    let m = p.start();

    assert!(p.eat(TokenKind::LAngle));

    loop {
        if p.at_set(&[TokenKind::Number, TokenKind::Ident, TokenKind::Char]) {
            p.bump();
        } else if p.at(TokenKind::Ampersand) {
            dt_phandle(p);
        } else if p.at(TokenKind::LParen) {
            dt_expr::<false>(p);
        } else if p.at(TokenKind::RAngle) {
            break;
        } else {
            p.error2();
            break;
        }
    }
    p.expect(TokenKind::RAngle);

    m.complete(p, NodeKind::DtCellList);
}

const ITEM_RECOVERY_SET: &[TokenKind] = &[
    TokenKind::Slash,
    // Name
    TokenKind::Ident,
    TokenKind::Number,
    TokenKind::Comma,
    TokenKind::Minus,
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
    // ?
    TokenKind::RCurly,
];

/// The caller is expected to handle the label and name.
///
/// - Form: `= "foo", <1>;` | `;`.
fn dt_property(p: &mut Parser, m: Marker) -> CompletedMarker {
    if p.at(TokenKind::Semicolon) {
        p.bump();
        return m.complete(p, NodeKind::DtProperty);
    }

    assert!(p.eat(TokenKind::Equals));

    const PROPERTY_VALUE_FIRST_SET: &[TokenKind] = &[
        TokenKind::String,
        TokenKind::LAngle,
        TokenKind::DtBytestring,
        // Should be in LAngle
        TokenKind::Number,
        TokenKind::Ident,
        TokenKind::Ampersand,
        TokenKind::LParen,
    ];

    if p.eat(TokenKind::BitsDirective) {
        p.expect(TokenKind::Number);
    }

    // TODO: a = <1 2 3> <4>; to only produce one error!

    let list_m = p.start();
    while !p.at(TokenKind::Semicolon) && !p.at_end() {
        if p.at(TokenKind::String) {
            p.bump();
        } else if p.at(TokenKind::LAngle) {
            dt_cell_list(p);
        } else if p.at(TokenKind::Ampersand) {
            dt_phandle(p);
        } else if p.at(TokenKind::DtBytestring) {
            p.bump();
        } else {
            p.error2();
            break;
        }

        if p.at(TokenKind::Comma) {
            p.bump();
        } else if p.silent_at_set(ITEM_RECOVERY_SET) {
            break;
        } else if p.at_set(PROPERTY_VALUE_FIRST_SET) {
            // Missing comma but can be recovered
            p.simple_error("Expected comma".into());
        } else {
            break;
        }
    }
    list_m.complete(p, NodeKind::PropValueList);

    p.expect_recoverable(TokenKind::Semicolon, ITEM_RECOVERY_SET);

    m.complete(p, NodeKind::DtProperty)
}

/// The caller is expected to handle the label, ampersand, name and unit address.
///
/// - Form: `{ foo = "bar"; baz {}; };`.
fn dt_node_body(p: &mut Parser, m: Marker) {
    #[cfg(feature = "grammar-tracing")]
    debug!("dt_node_body start");

    let lcurly_span = p.range().unwrap();

    // TODO: convert other grammars to assert eat
    assert!(p.eat(TokenKind::LCurly));

    while !p.at(TokenKind::RCurly) && !p.at_end() {
        item(p);
    }

    if p.at_end() {
        p.expect(TokenKind::RCurly);
        p.fancy_error(
            Cow::Borrowed("Expected `}`, but found end-of-file"),
            vec![SpanLabel {
                span: lcurly_span,
                message: Cow::Borrowed("Unclosed delimiter"),
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
}

/// TODO: mod_contents: while !(p.at(EOF) || (p.at(T!['}']) && stop_on_r_curly)) {
fn item(p: &mut Parser) {
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
        p.bump_name();
        // parse a node or a property

        if p.at(TokenKind::Colon) {
            // label
            p.bump();
            m.complete(p, NodeKind::DtLabel);
            m = p.start();

            while p.at_name() {
                p.bump_name();
                if p.at(TokenKind::Colon) {
                    p.bump();

                    m.complete(p, NodeKind::DtLabel);
                    m = p.start();
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
            p.expect_name();
            m.complete(p, NodeKind::UnitAddress);
        }

        if p.at(TokenKind::Equals) || p.at(TokenKind::Semicolon) {
            dt_property(p, m);
        } else if p.silent_at(TokenKind::RCurly) {
            p.emit_expect_error();
            p.bump();
            m.complete(p, NodeKind::ParseError);
        } else if p.at(TokenKind::LCurly) {
            dt_node_body(p, m);
        } else {
            p.emit_expect_error();

            if !p.silent_at_set(ITEM_RECOVERY_SET) {
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
        } else {
            // FIXME: require LCurly
            dt_node_body(p, m);
        }
    } else if p.silent_at(TokenKind::Equals) {
        p.emit_expect_error();
        p.add_hint(Cow::Borrowed("Recovered as unnamed property"));

        let m_prop = p.start();
        dt_property(p, m_prop);
        m.complete(p, NodeKind::ParseError);
    } else if p.silent_at(TokenKind::LCurly) {
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

        p.expect_recoverable(TokenKind::Semicolon, ITEM_RECOVERY_SET);
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
        } else {
            p.expect_name();
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
                    node(
                        NodeKind::DirectiveParams,
                        vec![
                            dynamic_token(TokenKind::Whitespace, " "),
                            node(
                                NodeKind::Name,
                                vec![
                                    dynamic_token(TokenKind::Ident, "node"),
                                    static_token(TokenKind::Minus),
                                    dynamic_token(TokenKind::Ident, "name"),
                                ],
                            ),
                        ],
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
                    node(
                        NodeKind::DirectiveParams,
                        vec![
                            dynamic_token(TokenKind::Whitespace, " "),
                            node(
                                NodeKind::DtPhandle,
                                vec![
                                    static_token(TokenKind::Ampersand),
                                    node(
                                        NodeKind::Name,
                                        vec![dynamic_token(TokenKind::Ident, "label")],
                                    ),
                                ],
                            ),
                        ],
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
                    node(
                        NodeKind::DirectiveParams,
                        vec![
                            dynamic_token(TokenKind::Whitespace, " "),
                            dynamic_token(TokenKind::Number, "0x10000000"),
                            dynamic_token(TokenKind::Whitespace, " "),
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
    fn parse_node() {
        check(
            "/ {};",
            vec![node(
                NodeKind::DtNode,
                vec![
                    static_token(TokenKind::Slash),
                    dynamic_token(TokenKind::Whitespace, " "),
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
                    dynamic_token(TokenKind::Whitespace, " "),
                    static_token(TokenKind::LCurly),
                    node(
                        NodeKind::DtProperty,
                        vec![
                            dynamic_token(TokenKind::Whitespace, " "),
                            node(NodeKind::Name, vec![dynamic_token(TokenKind::Ident, "a")]),
                            dynamic_token(TokenKind::Whitespace, " "),
                            static_token(TokenKind::Equals),
                            node(
                                NodeKind::PropValueList,
                                vec![
                                    dynamic_token(TokenKind::Whitespace, " "),
                                    node(
                                        NodeKind::DtCellList,
                                        vec![
                                            static_token(TokenKind::LAngle),
                                            static_token(TokenKind::RAngle),
                                        ],
                                    ),
                                ],
                            ),
                            static_token(TokenKind::Semicolon),
                        ],
                    ),
                    dynamic_token(TokenKind::Whitespace, " "),
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
                    node(
                        NodeKind::Name,
                        vec![dynamic_token(TokenKind::Number, "123")],
                    ),
                    dynamic_token(TokenKind::Whitespace, " "),
                    static_token(TokenKind::Equals),
                    node(
                        NodeKind::PropValueList,
                        vec![
                            dynamic_token(TokenKind::Whitespace, " "),
                            dynamic_token(TokenKind::String, "\"foo\""),
                        ],
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
                    node(
                        NodeKind::Name,
                        vec![
                            dynamic_token(TokenKind::Number, "123"),
                            static_token(TokenKind::Comma),
                        ],
                    ),
                    dynamic_token(TokenKind::Whitespace, " "),
                    static_token(TokenKind::Equals),
                    node(
                        NodeKind::PropValueList,
                        vec![
                            dynamic_token(TokenKind::Whitespace, " "),
                            dynamic_token(TokenKind::String, "\"foo\""),
                        ],
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
                    node(
                        NodeKind::Name,
                        vec![
                            static_token(TokenKind::Comma),
                            static_token(TokenKind::Comma),
                            static_token(TokenKind::Comma),
                        ],
                    ),
                    dynamic_token(TokenKind::Whitespace, " "),
                    static_token(TokenKind::Equals),
                    node(
                        NodeKind::PropValueList,
                        vec![
                            dynamic_token(TokenKind::Whitespace, " "),
                            dynamic_token(TokenKind::String, "\"foo\""),
                        ],
                    ),
                    static_token(TokenKind::Semicolon),
                ],
            )],
            Vec::new(),
        );
    }

    #[test]
    fn parse_trivia() {
        check(
            "  ",
            vec![dynamic_token(TokenKind::Whitespace, "  ")],
            Vec::new(),
        );
        check(
            "/* test */ // test",
            vec![
                dynamic_token(TokenKind::BlockComment, "/* test */"),
                dynamic_token(TokenKind::Whitespace, " "),
                dynamic_token(TokenKind::LineComment, "// test"),
            ],
            Vec::new(),
        );
    }
}
