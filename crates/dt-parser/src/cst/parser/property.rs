use super::{
    green_separated, node, nullable_ident, opt_err_wsnt, semi_or_ws, token, wst, PushGreenItem,
    Stream, T,
};
use crate::cst::{GreenItem, GreenNode, GreenToken, NodeKind, TokenKind};
use crate::TextRange;
use either::Either;
use std::sync::Arc;
use winnow::{
    ascii::{dec_uint, hex_uint},
    combinator::{alt, empty, opt, preceded, repeat, trace},
    error::StrContext,
    prelude::*,
    token::{any, take_till},
};

pub(crate) fn dt_cell(input: &mut Stream) -> PResult<GreenItem> {
    node!(
        NodeKind::DtCell,
        (
            T!['<'],
            opt(wst),
            green_separated(
                true,
                false,
                alt((
                    token!(
                        TokenKind::DtNumber,
                        preceded(alt(("0x", "0X")), hex_uint::<_, u32, _>)
                    ),
                    token!(TokenKind::Error, alt(("0x", "0X"))),
                    token!(TokenKind::DtNumber, dec_uint::<_, u32, _>),
                    node!(
                        NodeKind::DtPhandle,
                        (
                            T!['&'],
                            alt((
                                (
                                    (T!['{'], opt_err_wsnt, nullable_ident, opt_err_wsnt, T!['}']),
                                    token!(TokenKind::DtPathPhandle, empty),
                                )
                                    .map(Either::Right),
                                (opt_err_wsnt, nullable_ident).map(Either::Left)
                            ))
                        )
                    ),
                    token!(TokenKind::DtNumberArithmetic, {
                        fn take_paren(input: &mut Stream) -> PResult<()> {
                            (
                                T!['('],
                                // TODO: smarter parsing
                                alt((
                                    (take_till(0.., '('), take_paren).void(),
                                    (take_till(0.., ')')).void(),
                                )),
                                T![')'],
                            )
                                .void()
                                .parse_next(input)
                        }
                        (
                            take_paren,
                            // TODO: make parent a node to make this work:
                            repeat(0.., node!(NodeKind::InvalidPunct, T![')']))
                                .map(|vec: Vec<_>| vec),
                        )
                    }),
                    token!(
                        TokenKind::UnexpectedItem,
                        take_till(1.., [' ', '>', '\n', ';'])
                    ),
                )),
                alt((
                    (opt(wst), node!(NodeKind::InvalidPunct, T![',']), opt(wst)).map(Either::Left),
                    wst.map(Either::Right),
                )),
            ),
            opt((opt(wst), node!(NodeKind::InvalidPunct, T![',']))),
            alt((
                (opt(wst), T!['>']).map(Either::Left),
                token!(TokenKind::MissingPunct('>'), empty).map(Either::Right),
            ))
        )
    )
    .context(StrContext::Label("cell"))
    .parse_next(input)
}

fn dt_string(input: &mut Stream) -> PResult<GreenItem> {
    node!(
        NodeKind::DtString,
        (
            T!['"'],
            token!(
                TokenKind::DtStringContents,
                repeat(
                    0..,
                    alt((
                        preceded('\\', any).void(),
                        take_till(1.., ['"', '\\'])
                            .verify(|s: &[u8]| !s.is_empty())
                            .void(),
                    ))
                )
                .map(|_: ()| ())
            ),
            T!['"']
        )
    )
    .context(StrContext::Label("string"))
    .parse_next(input)
}

pub(super) fn dt_prop(input: &mut Stream) -> PResult<GreenItem> {
    node!(
        NodeKind::DtProperty,
        (
            nullable_ident,
            opt(wst),
            opt(alt((
                (
                    T!['='],
                    opt(wst),
                    green_separated(
                        true,
                        false,
                        // TODO: string ref phandle i.e. without <>?
                        alt((dt_cell, dt_string,)).context(StrContext::Label("property value")),
                        alt((
                            (T![','], opt(wst)).map(Either::Left),
                            (token!(TokenKind::MissingPunct(','), empty), wst).map(Either::Right)
                        )),
                    )
                ),
                (
                    token!(TokenKind::MissingPunct('='), empty),
                    opt(wst),
                    green_separated(
                        true,
                        true,
                        // TODO: string ref phandle i.e. without <>?
                        alt((dt_cell, dt_string,)).context(StrContext::Label("property value")),
                        alt((
                            (T![','], opt(wst)).map(Either::Left),
                            (token!(TokenKind::MissingPunct(','), empty), wst).map(Either::Right)
                        )),
                    )
                )
            ))),
            semi_or_ws,
        )
    )
    .context(StrContext::Label("property"))
    .parse_next(input)
}
