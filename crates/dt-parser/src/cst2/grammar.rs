use super::{
    lexer::TokenKind,
    parser::{Marker, Parser},
    NodeKind,
};

/// Parses a Devicetree cell.
///
/// - Form: `<1>;`.
fn dt_cell(p: &mut Parser) {
    let m = p.start();

    p.expect(TokenKind::LAngle);
    loop {
        if p.at_set(&[TokenKind::Number, TokenKind::Ident]) {
            // If an ident: Note that this may be a number suffixed with a comma (e.g. `1,`), but
            // is usually a preprocessor constant
            p.bump();
        } else if p.at(TokenKind::Ampersand) {
            p.bump();
            p.expect(TokenKind::Ident);
            // TODO: path-based ident
        } else if p.at(TokenKind::RAngle) {
            break;
        } else {
            p.error();
            break;
        }
    }
    p.expect(TokenKind::RAngle);

    m.complete(p, NodeKind::DtCell);
}

/// The caller is expected to handle the label and name.
///
/// - Form: `= "foo", <1>;` | `;`.
fn dt_property(p: &mut Parser, m: Marker) {
    if p.at(TokenKind::Semicolon) {
        p.bump();
        m.complete(p, NodeKind::DtProperty);
        return;
    }

    p.expect(TokenKind::Equals);

    loop {
        if p.at(TokenKind::String) {
            p.bump();
        } else if p.at(TokenKind::LAngle) {
            dt_cell(p);
        } else if p.at(TokenKind::Semicolon) {
            break;
        } else {
            p.error();
            break;
        }

        if p.at(TokenKind::Semicolon) {
            break;
        } else if p.at(TokenKind::Comma) {
            p.bump();
        } else if p.expect_no_ending(TokenKind::RCurly) || p.expect_no_ending(TokenKind::Ident) {
            // Missing semicolon
            // TODO: hint where it might go
            m.complete(p, NodeKind::DtProperty);
            return;
        } else {
            p.error();
            break;
        }
    }

    println!("a");

    p.expect(TokenKind::Semicolon);

    m.complete(p, NodeKind::DtProperty);
}

/// The caller is expected to handle the label, ampersand, name and unit address.
///
/// - Form: `{ foo = "bar"; baz {}; };`.
fn dt_node_body(p: &mut Parser, m: Marker) {
    p.expect(TokenKind::LCurly);

    loop {
        if p.at(TokenKind::RCurly) || p.at_end() {
            eprintln!("broke!");
            break;
        }
        item(p);
    }

    p.expect(TokenKind::RCurly);
    // TODO: add hint like "this left curly brace was not closed"

    if p.at(TokenKind::Semicolon) {
        p.bump();
    } else if p.expect_no_ending(TokenKind::RCurly) || p.expect_no_ending(TokenKind::Ident) {
    } else {
        p.error();
        // Missing semicolon
        // TODO: hint where it might go
    }

    m.complete(p, NodeKind::DtNode);
}

/// TODO: mod_contents: while !(p.at(EOF) || (p.at(T!['}']) && stop_on_r_curly)) {
/// I can also try to parse properties at this level.
fn item(p: &mut Parser) {
    if p.at(TokenKind::Slash) {
        let m = p.start();
        p.bump();
        // TODO: unit address?
        if p.at(TokenKind::LCurly) {
            dt_node_body(p, m);
            // node
        } else if p.at(TokenKind::Ident) {
            // directive
            p.bump();

            p.expect(TokenKind::Slash);

            // TODO: take until semicolon?
            if p.at(TokenKind::Ampersand) {
                let m = p.start();
                p.bump();
                p.expect(TokenKind::Ident);
                m.complete(p, NodeKind::DirectiveParams);
            } else if p.at(TokenKind::Ident) {
                let m = p.start();
                p.bump();
                m.complete(p, NodeKind::DirectiveParams);
            } else if p.at(TokenKind::Number) {
                let m = p.start();
                p.bump();
                while p.at(TokenKind::Number) {
                    p.bump();
                }
                m.complete(p, NodeKind::DirectiveParams);
            }

            p.expect(TokenKind::Semicolon);
            m.complete(p, NodeKind::Directive);
        } else {
            p.error();
            m.complete(p, NodeKind::ParseError);
        }
    } else if p.at_set(&[TokenKind::Ident, TokenKind::Number]) {
        // parse a node or a property
        let m = p.start();
        p.bump();

        if p.at(TokenKind::Colon) {
            // label
            p.bump();

            if p.at(TokenKind::Ampersand) {
                // label + extension e.g. `bar: &foo {};`
                p.bump();
            }

            p.expect_set(&[TokenKind::Ident, TokenKind::Number]);
        }
        if p.at(TokenKind::AtSign) {
            // unit address
            p.bump();
            if p.at(TokenKind::Ident) || p.at(TokenKind::Number) {
                p.bump();
            }
        }

        if p.at(TokenKind::Equals) || p.at(TokenKind::Semicolon) {
            dt_property(p, m);
        } else {
            dt_node_body(p, m);
        }
    } else if p.at(TokenKind::Ampersand) {
        // parse a node
        let m = p.start();
        p.bump();

        p.expect(TokenKind::Ident);

        if p.at(TokenKind::AtSign) {
            // unit address
            p.bump();
            p.expect(TokenKind::Ident);
        }

        if p.at(TokenKind::Equals) || p.at(TokenKind::Semicolon) {
            dt_property(p, m);
        } else {
            dt_node_body(p, m);
        }
    } else if p.silent_at(TokenKind::Equals) {
        // TODO: add hint that it was expected to be a node
        p.error_no_bump();

        let m = p.start();
        let m_prop = p.start();
        dt_property(p, m_prop);
        m.complete(p, NodeKind::ParseError);
    } else if p.silent_at(TokenKind::LCurly) {
        // TODO: add hint that it was expected to be a node
        p.error_no_bump();

        let m = p.start();
        let m_node = p.start();
        dt_node_body(p, m_node);
        m.complete(p, NodeKind::ParseError);
    } else if p.silent_at(TokenKind::Semicolon) {
        p.error_no_bump();

        let m = p.start();
        p.bump();
        m.complete(p, NodeKind::ParseError);
    } else {
        p.error();
    }
}

pub(super) fn root(p: &mut Parser) {
    while !p.at_end() {
        item(p);
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
        GreenItem::Node(Arc::new(GreenNode { kind, children }))
    }
    pub fn dynamic_token(kind: TokenKind, text: &'static str) -> GreenItem {
        GreenItem::Token(Arc::new(GreenToken { kind, text: TokenText::Dynamic(text.to_owned()) }))
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
                    static_token(TokenKind::Slash),
                    dynamic_token(TokenKind::Ident, "dts-v1"),
                    static_token(TokenKind::Slash),
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
                    static_token(TokenKind::Slash),
                    dynamic_token(TokenKind::Ident, "plugin"),
                    static_token(TokenKind::Slash),
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
                    static_token(TokenKind::Slash),
                    dynamic_token(
                        TokenKind::Ident,
                        "delete-node",
                    ),
                    static_token(TokenKind::Slash),
                    dynamic_token(TokenKind::Whitespace, " "),
                    node(
                        NodeKind::DirectiveParams,
                        vec![dynamic_token(
                            TokenKind::Ident,
                            "node-name",
                        )],
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
                    static_token(TokenKind::Slash),
                    dynamic_token(
                        TokenKind::Ident,
                        "delete-node",
                    ),
                    static_token(TokenKind::Slash),
                    dynamic_token(TokenKind::Whitespace, " "),
                    node(
                        NodeKind::DirectiveParams,
                        vec![
                            static_token(TokenKind::Ampersand),
                            dynamic_token(TokenKind::Ident, "label"),
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
                    static_token(TokenKind::Slash),
                    dynamic_token(
                        TokenKind::Ident,
                        "memreserve",
                    ),
                    static_token(TokenKind::Slash),
                    dynamic_token(TokenKind::Whitespace, " "),
                    node(
                        NodeKind::DirectiveParams,
                        vec![
                            dynamic_token(
                                TokenKind::Number,
                                "0x10000000",
                            ),
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
        assert_eq!(parse_output.errors, Vec::new());
        //assert_eq!(parse_output.green_node.children, expected_children);
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
                    dynamic_token(TokenKind::Whitespace, " "),
                    node(
                        NodeKind::DtProperty,
                        vec![
                            dynamic_token(TokenKind::Ident, "a"),
                            dynamic_token(TokenKind::Whitespace, " "),
                            static_token(TokenKind::Equals),
                            dynamic_token(TokenKind::Whitespace, " "),
                            node(
                                NodeKind::DtCell,
                                vec![
                                    static_token(TokenKind::LAngle),
                                    static_token(TokenKind::RAngle),
                                ],
                            ),
                            static_token(TokenKind::Semicolon),
                            dynamic_token(TokenKind::Whitespace, " "),
                        ],
                    ),
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
                    // TODO: need an Ident node for this
                    dynamic_token(TokenKind::Number, "123"),
                    dynamic_token(TokenKind::Whitespace, " "),
                    static_token(TokenKind::Equals),
                    dynamic_token(TokenKind::Whitespace, " "),
                    dynamic_token(TokenKind::String, "\"foo\""),
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
                    dynamic_token(TokenKind::Ident, "123,"),
                    dynamic_token(TokenKind::Whitespace, " "),
                    static_token(TokenKind::Equals),
                    dynamic_token(TokenKind::Whitespace, " "),
                    dynamic_token(TokenKind::String, "\"foo\""),
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
            vec![dynamic_token(
                TokenKind::Whitespace,
                "  ",
            )],
            Vec::new(),
        );
        check(
            "/* test */ // test",
            vec![
                dynamic_token(
                    TokenKind::BlockComment,
                    "/* test */",
                ),
                dynamic_token(TokenKind::Whitespace, " "),
                dynamic_token(
                    TokenKind::LineComment,
                    "// test",
                ),
            ],
            Vec::new(),
        );
    }
}
