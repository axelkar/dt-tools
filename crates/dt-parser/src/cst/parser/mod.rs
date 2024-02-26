use crate::{SourceId, Span};

use super::{GreenItem, GreenNode, GreenToken, NodeKind, TokenKind};
use either::Either;
use std::sync::Arc;
use winnow::{
    combinator::{alt, cut_err, empty, eof, opt, repeat, terminated, trace},
    error::{ContextError, ErrMode, ParserError as _, StrContext},
    prelude::*,
    stream::{Location as _, Recoverable, Stream as _},
    token::{take_till, take_until, take_while},
    Located, Stateful,
};

pub(crate) type Stream<'i> = Recoverable<crate::Printer<'i>, ContextError>;

mod property;
#[cfg(test)]
mod tests;
use property::dt_prop;

mod push_green;
use push_green::PushGreenItem;

// TODO: we don't need .context(_) since we have our own error solution

macro_rules! node {
    (@count_tts) => (0usize);
    (@count_tts $_head:tt $($tail:tt)* ) => (1usize + node!(@count_tts $($tail)*));
    ($kind: expr, ($($item:expr),+ $(,)?)) => {
        node!(@parser $kind, move |input: &mut Stream| {
            let mut children: Vec<GreenItem> = Vec::with_capacity(node!(@count_tts $($item)+));
            $($item.parse_next(input)?.push_onto(&mut children);)+
            Ok(children)
        })
    };
    ($kind: expr, $item: expr) => {
        node!($kind, ($item))
    };
    (@parser $kind: expr, $parser: expr) => {{
        let kind: NodeKind = $kind;
        trace(format!("node {kind:?}"), move |input: &mut Stream| {
            let (children, span) = $parser.with_span().parse_next(input)?;
            Ok(GreenItem::Node(Arc::new(GreenNode {
                kind,
                span: Span {
                    start: span.start,
                    end: span.end,
                },
                children,
            })))
        })
    }};
}

macro_rules! token {
    ($kind: expr, $parser: expr) => {{
        let kind: TokenKind = $kind;
        trace(format!("token {kind:?}"), move |input: &mut Stream| {
            let span = $parser.span().parse_next(input)?;
            Ok(GreenItem::Token(Arc::new(GreenToken {
                kind,
                span: Span {
                    start: span.start,
                    end: span.end,
                },
            })))
        })
    }};
}

macro_rules! T {
    ($item:tt) => (token!(T![@$item], $item));
    (@'{') => (TokenKind::LCurly);
    (@'}') => (TokenKind::RCurly);
    (@'(') => (TokenKind::LParen);
    (@')') => (TokenKind::RParen);
    (@'[') => (TokenKind::LBrack);
    (@']') => (TokenKind::RBrack);
    (@'<') => (TokenKind::LAngle);
    (@'>') => (TokenKind::RAngle);

    (@'"') => (TokenKind::DoubleQuote);
    (@'=') => (TokenKind::Equals);
    (@':') => (TokenKind::Colon);
    (@';') => (TokenKind::Semicolon);
    (@'/') => (TokenKind::Slash);
    (@',') => (TokenKind::Comma);
    (@'\n') => (TokenKind::Newline);
    (@'&') => (TokenKind::Ampersand);
    (@'#') => (TokenKind::Pound);
    (@'@') => (TokenKind::AtSign);
}

use {node, token, T};

pub fn green_separated<'i, O: PushGreenItem, O2: PushGreenItem, P, S>(
    req_first: bool,
    backtrack_on_missing_first: bool,
    mut parser: P,
    mut separator: S,
) -> impl Parser<Stream<'i>, Vec<GreenItem>, ContextError>
where
    P: Parser<Stream<'i>, O, ContextError>,
    S: Parser<Stream<'i>, O2, ContextError>,
{
    trace("green_separated", move |input: &mut Stream<'i>| {
        let mut acc = Vec::new();

        if req_first {
            // First element
            match parser.parse_next(input) {
                Ok(o) => o.push_onto(&mut acc),
                Err(ErrMode::Backtrack(_)) if !backtrack_on_missing_first => {
                    return Ok(vec![
                        token!(TokenKind::SeparatedMissingFirst, empty).parse_next(input)?
                    ])
                }
                Err(e) => return Err(e),
            }
        } else {
            let start = input.checkpoint();
            match parser.parse_next(input) {
                Ok(o) => o.push_onto(&mut acc),
                Err(ErrMode::Backtrack(_)) => {
                    input.reset(&start);
                    return Ok(Vec::new());
                }
                Err(e) => return Err(e),
            }
        }

        loop {
            let start = input.checkpoint();
            let len = input.eof_offset();
            let span_start = input.location();
            match separator.parse_next(input) {
                Err(ErrMode::Backtrack(_)) => {
                    input.reset(&start);
                    return Ok(acc);
                }
                Err(e) => return Err(e),
                Ok(o) => {
                    // infinite loop check
                    if input.eof_offset() == len {
                        return Err(ErrMode::assert(
                            input,
                            "`green_separated` separator parser must always consume",
                        ));
                    }
                    let span_end = input.location();
                    acc.push(GreenItem::Node(Arc::new(GreenNode {
                        kind: NodeKind::Separator,
                        span: Span {
                            start: span_start,
                            end: span_end,
                        },
                        children: o.collect(),
                    })));

                    match parser.parse_next(input) {
                        Err(ErrMode::Backtrack(_)) => {
                            input.reset(&start);
                            return Ok(acc);
                        }
                        Err(e) => return Err(e),
                        Ok(o) => {
                            o.push_onto(&mut acc);
                        }
                    }
                }
            }
        }
    })
}

/// Whitespace Token
fn wst(input: &mut Stream) -> PResult<impl PushGreenItem> {
    fn raw(input: &mut Stream) -> PResult<GreenItem> {
        token!(
            TokenKind::Whitespace,
            take_while(1.., (' ', '\t', '\r', '\n'))
        )
        .parse_next(input)
    }
    repeat(
        1..,
        alt((
            raw.map(|v| (None, v, None)),
            (
                opt(raw),
                token!(TokenKind::Comment, ("/*", take_until(0.., "*/"), "*/")),
                opt(raw),
            ),
            (
                opt(raw),
                token!(TokenKind::Comment, ("//", take_till(0.., ['\n', '\r']))),
                empty.value(None),
            ),
        )),
    )
    .map(|vec: Vec<_>| vec)
    .parse_next(input)
}
/// Whitespace Token without newline
fn wsnt(input: &mut Stream) -> PResult<GreenItem> {
    token!(TokenKind::Whitespace, take_while(1.., (' ', '\t'))).parse_next(input)
}
/// Outputs TokenKind::UnexpectedWhitespace, not TokenKind::Whitespace
///
/// See [`wsnt`]
fn opt_err_wsnt(input: &mut Stream) -> PResult<Option<GreenItem>> {
    opt(token!(
        TokenKind::UnexpectedWhitespace,
        take_while(1.., (' ', '\t'))
    ))
    .parse_next(input)
}

fn inner_ident(input: &mut Stream) -> PResult<()> {
    //none_of([' ', '\t', '\n', '\r', '>'])
    take_while(
        1..,
        (
            'a'..='z',
            ',',
            '-',
            '#',
            '@',
            '/',
            '0'..='9',
            '_',
            'A'..='Z',
        ),
    )
    .void()
    .parse_next(input)
}
/// name validation should be done after the AST, this accepts many kinds of names
fn nullable_ident(input: &mut Stream) -> PResult<GreenItem> {
    node!(
        NodeKind::Ident,
        alt((
            token!(TokenKind::Ident, inner_ident),
            token!(TokenKind::Error, empty)
        ))
    )
    .parse_next(input)
}

fn nullable_node_ident(input: &mut Stream) -> PResult<Either<(GreenItem, Option<GreenItem>), GreenItem>> {
    alt((
        (
            node!(
                NodeKind::Ident,
                token!(
                    TokenKind::Ident,
                    take_while(
                        1..,
                        ('a'..='z', ',', '-', '#', '/', '0'..='9', '_', 'A'..='Z',),
                    )
                )
            ),
            opt(node!(NodeKind::DtNodeUnitAddress, (T!['@'], nullable_ident))),
        )
            .map(Either::Left),
        node!(NodeKind::Ident, token!(TokenKind::Error, empty)).map(Either::Right),
    ))
    .parse_next(input)
}

/// FIXME: Doesn't need to be nullable since `//;` doesn't work and how do you even accidentally type `/ /;`
fn nullable_directive_ident(input: &mut Stream) -> PResult<GreenItem> {
    node!(
        NodeKind::Ident,
        alt((
            token!(
                TokenKind::Ident,
                take_while(
                    1..,
                    ('a'..='z', ',', '-', '#', '@', '0'..='9', '_', 'A'..='Z')
                )
            ),
            token!(TokenKind::Error, empty)
        ))
    )
    .parse_next(input)
}

fn label(input: &mut Stream) -> PResult<GreenItem> {
    node!(
        NodeKind::DtLabel,
        (
            node!(NodeKind::Ident, token!(TokenKind::Ident, inner_ident)),
            T![':'],
            opt(wst)
        )
    )
    .parse_next(input)
}

fn dt_node(input: &mut Stream) -> PResult<GreenItem> {
    node!(
        NodeKind::DtNode,
        (
            // TODO: error if label and extension are both used
            // TODO: error if extension is not at the rootlevel
            opt(label),
            opt(node!(NodeKind::DtNodeExtension, T!['&'])),
            nullable_node_ident,
            opt(wst),
            T!['{'],
            opt(wst),
            repeat(
                0..,
                (
                    alt((
                        dt_node.map(Either::Left),
                        dt_prop.map(Either::Left),
                        (
                            token!(
                                TokenKind::UnexpectedItem,
                                take_till(1.., [' ', '}', '\t', '\n', '\r'])
                            ),
                            wst
                        )
                            .map(Either::Right),
                    )),
                    opt(wst)
                )
            )
            .map(|vec: Vec<_>| vec),
            alt((
                (T!['}'], semi_or_ws).map(Either::Left),
                // TODO: if this happens, add unmatched error to the beginning T!['{']. easiest to do in lint
                token!(TokenKind::MissingPunct('}'), empty).map(Either::Right),
            )),
        )
    )
    .parse_next(input)
}

fn semi_or_ws(input: &mut Stream) -> PResult<Either<(impl PushGreenItem, GreenItem), GreenItem>> {
    alt((
        (opt(wst), T![';']).map(Either::Left),
        token!(TokenKind::MissingPunct(';'), wst).map(Either::Right),
    ))
    .parse_next(input)
}

fn directive(input: &mut Stream) -> PResult<GreenItem> {
    node!(
        NodeKind::Directive,
        (
            // TODO: directive args
            T!['/'],
            opt_err_wsnt,
            cut_err(nullable_directive_ident),
            opt_err_wsnt,
            T!['/'],
            opt((
                opt(wsnt),
                // TODO: smarter parsing
                token!(TokenKind::DirectiveArg, take_till(1.., [';', '\n', '\r'])),
            )),
            semi_or_ws
        )
    )
    .context(StrContext::Label("directive"))
    .parse_next(input)
}
fn preprocessor_directive(input: &mut Stream) -> PResult<GreenItem> {
    alt((
        node!(
            NodeKind::PreprocessorInclude,
            (
                T!['#'],
                opt_err_wsnt,
                token!(TokenKind::KwInclude, "include"),
                alt((
                    // TODO: smarter parsing
                    (
                        wsnt,
                        token!(TokenKind::PreprocessorArg, take_till(1.., ['\n', '\r']))
                    )
                        .map(Either::Left),
                    token!(TokenKind::Error, take_till(0.., ['\n', '\r'])).map(Either::Right)
                )),
                T!['\n']
            )
        ),
        node!(
            NodeKind::PreprocessorDefine,
            (
                T!['#'],
                opt_err_wsnt,
                token!(TokenKind::KwDefine, "define"),
                alt((
                    // TODO: smarter parsing, multiline?
                    (
                        wsnt,
                        token!(TokenKind::PreprocessorArg, take_till(1.., ['\n', '\r']))
                    )
                        .map(Either::Left),
                    token!(TokenKind::Error, take_till(0.., ['\n', '\r'])).map(Either::Right)
                )),
                T!['\n']
            )
        ),
    ))
    .parse_next(input)
}

fn root_level_nodes(input: &mut Stream) -> PResult<GreenItem> {
    alt((
        // root node needs to be before directive because it can begin with /
        dt_node,
        directive,
        preprocessor_directive,
        token!(
            TokenKind::UnexpectedItem,
            take_till(1.., [' ', '\t', '\n', '\r'])
        ),
    ))
    .context(StrContext::Label("top-level"))
    .parse_next(input)
}

pub(crate) fn generic_parse<'i, O>(
    input: &'i str,
    src: SourceId,
    parser: impl Parser<Stream<'i>, O, ContextError>,
) -> (Option<O>, Vec<ContextError>) {
    let (_, o, e) = terminated(parser, eof)
        .recoverable_parse(crate::Printer(Located::new(Stateful { input, state: src })));
    (o, e)
}
pub fn parse(input: &str, src: SourceId) -> (Option<Arc<GreenNode>>, Vec<ContextError>) {
    let (o, e) = generic_parse(
        input,
        src,
        node!(
            NodeKind::Document,
            (
                opt(wst),
                repeat(0.., (root_level_nodes, opt(wst))).map(|vec: Vec<_>| vec),
            )
        ),
    );
    (o.map(|o| o.into_node().expect("just did node! above")), e)
}
