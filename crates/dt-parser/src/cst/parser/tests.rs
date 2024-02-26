use super::*;
use pretty_assertions::assert_eq;

fn node(kind: NodeKind, start: usize, end: usize, children: Vec<GreenItem>) -> GreenItem {
    GreenItem::Node(Arc::new(GreenNode {
        kind,
        span: Span { start, end },
        children,
    }))
}

fn token(kind: TokenKind, start: usize, end: usize) -> GreenItem {
    GreenItem::Token(Arc::new(GreenToken {
        kind,
        span: Span { start, end },
    }))
}
fn id(start: usize, end: usize) -> GreenItem {
    node(
        NodeKind::Ident,
        start,
        end,
        vec![token(TokenKind::Ident, start, end)],
    )
}
fn ws(start: usize, end: usize) -> GreenItem {
    token(TokenKind::Whitespace, start, end)
}

#[test]
fn red_test() {
    use crate::cst::{MySyntaxError, RedNode};
    let text = "/dts-v1/; / {}\n";
    let green_tree = full(text).unwrap().into_node().unwrap();
    let red = RedNode::new(green_tree);
    let vec = red.find_syntax_errors(text).collect::<Vec<_>>();
    assert_eq!(
        vec,
        vec![(
            Span { start: 14, end: 15 },
            MySyntaxError::MissingPunct(';')
        )]
    );
}

#[test]
fn test_green_separated() {
    assert_eq!(
        generic_parse(
            "",
            "src".into(),
            green_separated(true, false, T!['='], T![','])
        ),
        (
            Some(vec![token(TokenKind::SeparatedMissingFirst, 0, 0)]),
            Vec::new()
        )
    );
}

#[test]
fn test_ident() {
    assert_eq!(
        generic_parse("#interrupt-cells", "src".into(), nullable_ident),
        (Some(id(0, 16)), Vec::new())
    );
    assert_eq!(
        generic_parse("a_label", "src".into(), nullable_ident),
        (Some(id(0, 7)), Vec::new())
    );
    assert_eq!(
        generic_parse("dts-v1", "src".into(), nullable_ident),
        (Some(id(0, 6)), Vec::new())
    );
}

#[test]
fn test_property() {
    assert_eq!(
        generic_parse("<1, 2>", "src".into(), super::property::dt_cell),
        (
            Some(node(
                NodeKind::DtCell,
                0,
                6,
                vec![
                    token(T![@'<'], 0, 1),
                    token(TokenKind::DtNumber, 1, 2),
                    node(
                        NodeKind::Separator,
                        2,
                        4,
                        vec![
                            node(NodeKind::InvalidPunct, 2, 3, vec![token(T![@','], 2, 3),]),
                            ws(3, 4)
                        ]
                    ),
                    token(TokenKind::DtNumber, 4, 5),
                    token(T![@'>'], 5, 6),
                ]
            )),
            Vec::new()
        )
    );

    assert_eq!(
        generic_parse(
            "<1 &FOO &{/soc/pic}, &{ /soc/pic}>",
            "src".into(),
            super::property::dt_cell
        ),
        (
            Some(node(
                NodeKind::DtCell,
                0,
                34,
                vec![
                    token(T![@'<'], 0, 1),
                    token(TokenKind::DtNumber, 1, 2),
                    node(NodeKind::Separator, 2, 3, vec![ws(2, 3)]),
                    node(
                        NodeKind::DtPhandle,
                        3,
                        7,
                        vec![token(T![@'&'], 3, 4), id(4, 7),]
                    ),
                    node(NodeKind::Separator, 7, 8, vec![ws(7, 8)]),
                    node(
                        NodeKind::DtPhandle,
                        8,
                        19,
                        vec![
                            token(T![@'&'], 8, 9),
                            token(T![@'{'], 9, 10),
                            id(10, 18),
                            token(T![@'}'], 18, 19),
                            token(TokenKind::DtPathPhandle, 19, 19),
                        ]
                    ),
                    node(
                        NodeKind::Separator,
                        19,
                        21,
                        vec![
                            node(
                                NodeKind::InvalidPunct,
                                19,
                                20,
                                vec![token(T![@','], 19, 20),]
                            ),
                            ws(20, 21)
                        ]
                    ),
                    node(
                        NodeKind::DtPhandle,
                        21,
                        33,
                        vec![
                            token(T![@'&'], 21, 22),
                            token(T![@'{'], 22, 23),
                            token(TokenKind::UnexpectedWhitespace, 23, 24),
                            id(24, 32),
                            token(T![@'}'], 32, 33),
                            token(TokenKind::DtPathPhandle, 33, 33),
                        ]
                    ),
                    token(T![@'>'], 33, 34),
                ]
            )),
            Vec::new()
        )
    );

    assert_eq!(
        generic_parse("a = <1>;", "src".into(), dt_prop),
        (
            Some(node(
                NodeKind::DtProperty,
                0,
                8,
                vec![
                    id(0, 1),
                    ws(1, 2),
                    token(T![@'='], 2, 3),
                    ws(3, 4),
                    node(
                        NodeKind::DtCell,
                        4,
                        7,
                        vec![
                            token(T![@'<'], 4, 5),
                            token(TokenKind::DtNumber, 5, 6),
                            token(T![@'>'], 6, 7),
                        ]
                    ),
                    token(T![@';'], 7, 8),
                ]
            )),
            Vec::new()
        )
    );
}

#[test]
fn test_directive() {
    assert_eq!(
        generic_parse("/foo/;", "src".into(), directive),
        (
            Some(node(
                NodeKind::Directive,
                0,
                6,
                vec![
                    token(T![@'/'], 0, 1),
                    id(1, 4),
                    token(T![@'/'], 4, 5),
                    token(T![@';'], 5, 6),
                ]
            )),
            Vec::new()
        )
    );
    assert_eq!(
        generic_parse("/foo/\n", "src".into(), directive),
        (
            Some(node(
                NodeKind::Directive,
                0,
                6,
                vec![
                    token(T![@'/'], 0, 1),
                    id(1, 4),
                    token(T![@'/'], 4, 5),
                    token(TokenKind::MissingPunct(';'), 5, 6),
                ]
            )),
            Vec::new()
        )
    );
}

fn full(input: &str) -> Option<GreenItem> {
    let (o, e) = super::parse(input, "src".into());
    Some(GreenItem::Node(o.filter(|_| e.is_empty())?))
}

#[test]
fn test_full() {
    assert_eq!(full(""), Some(node(NodeKind::Document, 0, 0, vec![])));
    assert_eq!(
        full("/dts-v1/;"),
        Some(node(
            NodeKind::Document,
            0,
            9,
            vec![node(
                NodeKind::Directive,
                0,
                9,
                vec![
                    token(T![@'/'], 0, 1),
                    id(1, 7),
                    token(T![@'/'], 7, 8),
                    token(T![@';'], 8, 9),
                ]
            ),]
        ))
    );

    assert_eq!(
        full("// test\n/dts-v1/;"),
        Some(node(
            NodeKind::Document,
            0,
            17,
            vec![
                token(TokenKind::Comment, 0, 7),
                ws(7, 8),
                node(
                    NodeKind::Directive,
                    8,
                    17,
                    vec![
                        token(T![@'/'], 8, 9),
                        id(9, 15),
                        token(T![@'/'], 15, 16),
                        token(T![@';'], 16, 17),
                    ]
                ),
            ]
        ))
    );

    assert_eq!(
        // TODO: error message like: did you forget to type the node name?
        full(" {};"),
        Some(node(
            NodeKind::Document,
            0,
            4,
            vec![
                ws(0, 1),
                node(
                    NodeKind::DtNode,
                    1,
                    4,
                    vec![
                        node(NodeKind::Ident, 1, 1, vec![token(TokenKind::Error, 1, 1)]),
                        token(T![@'{'], 1, 2),
                        token(T![@'}'], 2, 3),
                        token(T![@';'], 3, 4),
                    ]
                )
            ]
        ))
    );

    assert_eq!(
        full("test"),
        Some(node(
            NodeKind::Document,
            0,
            4,
            vec![token(TokenKind::UnexpectedItem, 0, 4)]
        ))
    );

    assert_eq!(
        full(r#"/dts-v1/; / { a = "b"; };"#),
        Some(node(
            NodeKind::Document,
            0,
            25,
            vec![
                node(
                    NodeKind::Directive,
                    0,
                    9,
                    vec![
                        token(T![@'/'], 0, 1),
                        id(1, 7),
                        token(T![@'/'], 7, 8),
                        token(T![@';'], 8, 9),
                    ]
                ),
                ws(9, 10),
                node(
                    NodeKind::DtNode,
                    10,
                    25,
                    vec![
                        id(10, 11),
                        ws(11, 12),
                        token(T![@'{'], 12, 13),
                        ws(13, 14),
                        node(
                            NodeKind::DtProperty,
                            14,
                            22,
                            vec![
                                id(14, 15),
                                ws(15, 16),
                                token(T![@'='], 16, 17),
                                ws(17, 18),
                                node(
                                    NodeKind::DtString,
                                    18,
                                    21,
                                    vec![
                                        token(T![@'"'], 18, 19),
                                        token(TokenKind::DtStringContents, 19, 20),
                                        token(T![@'"'], 20, 21),
                                    ]
                                ),
                                token(T![@';'], 21, 22),
                            ]
                        ),
                        ws(22, 23),
                        token(T![@'}'], 23, 24),
                        token(T![@';'], 24, 25),
                    ]
                ),
            ]
        ))
    );

    assert_eq!(
        full(r#"/dts-v1/; / { a = "b";"#),
        Some(node(
            NodeKind::Document,
            0,
            22,
            vec![
                node(
                    NodeKind::Directive,
                    0,
                    9,
                    vec![
                        token(T![@'/'], 0, 1),
                        id(1, 7),
                        token(T![@'/'], 7, 8),
                        token(T![@';'], 8, 9),
                    ]
                ),
                ws(9, 10),
                node(
                    NodeKind::DtNode,
                    10,
                    22,
                    vec![
                        id(10, 11),
                        ws(11, 12),
                        token(T![@'{'], 12, 13),
                        ws(13, 14),
                        node(
                            NodeKind::DtProperty,
                            14,
                            22,
                            vec![
                                id(14, 15),
                                ws(15, 16),
                                token(T![@'='], 16, 17),
                                ws(17, 18),
                                node(
                                    NodeKind::DtString,
                                    18,
                                    21,
                                    vec![
                                        token(T![@'"'], 18, 19),
                                        token(TokenKind::DtStringContents, 19, 20),
                                        token(T![@'"'], 20, 21),
                                    ]
                                ),
                                token(T![@';'], 21, 22),
                            ]
                        ),
                        token(TokenKind::MissingPunct('}'), 22, 22),
                    ]
                ),
            ]
        ))
    );
}

#[test]
fn test_a_dts() {
    let src = include_str!("../../../a.dts");
    assert_eq!(
        full(src).unwrap().into_node().unwrap().print_tree(src),
        include_str!("../../../a.dts.expect")
    );
}

#[test]
fn test_whitespace() {
    fn parse<'i, O>(input: &'i str, parser: impl Parser<Stream<'i>, O, ContextError>) -> Option<O> {
        let (o, e) = generic_parse(input, "src".into(), parser);
        o.filter(|_| e.is_empty())
    }

    assert_eq!(
        parse("// test\n// foo", wst).map(|o| o.collect()),
        Some(vec![
            token(TokenKind::Comment, 0, 7),
            ws(7, 8),
            token(TokenKind::Comment, 8, 14),
        ])
    );
    assert_eq!(
        parse("/* /* test\n foo */", wst).map(|o| o.collect()),
        Some(vec![token(TokenKind::Comment, 0, 18),])
    );
    assert_eq!(parse("/* /* test */ */", wst).map(|o| o.collect()), None);
    assert_eq!(parse("test", wst).map(|o| o.collect()), None);

    assert_eq!(parse(" \t ", wsnt), Some(ws(0, 3)));
    assert_eq!(parse("\n", wsnt), None);
    assert_eq!(parse("", wsnt), None);
    assert_eq!(parse("test", wsnt), None);
}
