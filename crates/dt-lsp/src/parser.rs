use crate::ast::{Cell, Document, Node, NodeLabel, NodeName, Property, PropertyValue, Spanned};
use smallvec::SmallVec;
use winnow::{
    ascii::{dec_uint, hex_uint, multispace0 as ws, multispace1 as rws},
    combinator::{
        alt, cut_err, delimited, empty, opt, preceded, repeat, separated, terminated, trace,
        Context,
    },
    dispatch,
    error::{AddContext, ContextError, ParseError, StrContext, StrContextValue},
    prelude::*,
    seq,
    token::{any, take_till, take_until, take_while},
    Located,
};

pub trait ItemParser: Sized {
    fn parse(input: &mut Stream) -> PResult<Spanned<Self>>;
}

/// TODO: make sure that `/*/* comment */*/` doesn't parse
pub fn wsc<'i>(input: &mut Stream<'i>) -> PResult<&'i str> {
    trace(
        "wsc",
        (
            ws,
            repeat::<_, _, (), _, _>(
                0..,
                alt((
                    ("//", take_till(1.., ['\n', '\r']), ws).void(),
                    ("/*", take_until(0.., "*/"), "*/", ws).void(),
                )),
            ),
        )
            .recognize(),
    )
    .parse_next(input)
}

// https://buildmedia.readthedocs.org/media/pdf/devicetree-specification/latest/devicetree-specification.pdf#chapter.6

// TODO: /delete-node/
// TODO: /delete-property/
// TODO: /include/ and #include
// TODO: property labels
// TODO: arithmetic, bitwise, logical, relational and ternary operators in paranthesis

pub type Stream<'i> = Located<&'i str>;

// TODO: use u8 and &[u8] instead of char and &str

pub fn label_ident(input: &mut Stream) -> PResult<NodeLabel> {
    trace(
        "label",
        take_while(1.., ('a'..='z', 'A'..='Z', '_', '0'..='9')),
    )
    .parse_next(input)
    .map(Into::into)
}

/// TODO: verify chars according to spec
pub fn prop_ident(input: &mut Stream) -> PResult<String> {
    trace(
        "ident",
        take_while(1.., ('a'..='z', ',', '-', '#', '0'..='9', '_', 'A'..='Z')),
    )
    .parse_next(input)
    .map(ToOwned::to_owned)
}

pub fn prop(input: &mut Stream) -> PResult<Property> {
    seq!{Property {
        name: prop_ident.context(StrContext::Expected(StrContextValue::Description("the property name"))),
        values: alt((
            delimited((ws, '='.expected(), ws), cut_err(separated(1.., property_value, (',', ws)).map(|v: Vec<_>| v.into())).context(StrContext::Label("property value")), (ws, ';'.expected())),
            (ws, ';'.expected()).value(SmallVec::new())
        )),
    }}.parse_next(input)
}

pub fn number(input: &mut Stream) -> PResult<u32> {
    trace(
        "number",
        alt((
            preceded(alt(("0x", "0X")), cut_err(hex_uint)).context(StrContext::Expected(
                StrContextValue::Description("a hexadecimal number"),
            )),
            dec_uint.context(StrContext::Expected(StrContextValue::Description(
                "a decimal number",
            ))),
        )),
    )
    .parse_next(input)
}

enum SingleOrSlice<'s> {
    Single(char),
    Slice(&'s str),
}

pub fn string(input: &mut Stream) -> PResult<String> {
    fn escape_seq_char(input: &mut Stream) -> PResult<char> {
        dispatch! {any;
            'n' => empty.value('\n'),
            'r' => empty.value('\r'),
            't' => empty.value('\t'),
            'x' => hex_byte,
            other => empty.value(other),
        }
        .parse_next(input)
    }
    trace(
        "string",
        delimited(
            '"',
            repeat(
                0..,
                alt((
                    preceded('\\', escape_seq_char).map(SingleOrSlice::Single),
                    take_till(1.., ['"', '\\'])
                        .verify(|s: &str| !s.is_empty())
                        .map(SingleOrSlice::Slice),
                )),
            )
            .fold(String::new, |mut sb, f| {
                match f {
                    SingleOrSlice::Single(c) => sb.push(c),
                    SingleOrSlice::Slice(s) => sb.push_str(s),
                }
                sb
            }),
            '"',
        ),
    )
    .parse_next(input)
}

/// requirement of 2 chars
/// TODO: make this better
pub fn hex_byte(input: &mut Stream) -> PResult<char> {
    trace(
        "hex_byte",
        (
            any.verify_map(|c: char| c.to_digit(16)),
            any.verify_map(|c: char| c.to_digit(16)),
        )
            .map(|(left, right)| ((left << 4) + right) as u8 as char),
    )
    .parse_next(input)
}

/// TODO: < &{/soc/interrupt-controller@40000} >;
pub fn phandle(input: &mut Stream) -> PResult<NodeLabel> {
    preceded('&', label_ident).parse_next(input)
}

/// TODO: only validate this now and when converting from CST to AST
pub fn property_value(input: &mut Stream) -> PResult<Spanned<PropertyValue>> {
    trace(
        "property value",
        alt((
            trace(
                "cells",
                delimited(
                    ('<', ws),
                    separated(
                        1..,
                        alt((number.map(Cell::Number), phandle.map(Cell::Phandle))),
                        rws,
                    ),
                    (ws, '>'),
                )
                .map(|v: Vec<_>| PropertyValue::Cells(v.into())),
            ),
            string.map(|v: String| PropertyValue::String(v.into())),
        )),
    )
    .with_span()
    .parse_next(input)
}

pub fn node_ident(input: &mut Stream) -> PResult<NodeName> {
    trace(
        "ident",
        take_while(
            1..,
            ('a'..='z', ',', '-', '0'..='9', '.', '+', '_', 'A'..='Z'),
        ),
    )
    .parse_next(input)
    .map(Into::into)
}

impl ItemParser for Node {
    fn parse(input: &mut Stream) -> PResult<Spanned<Self>> {
        seq! {Node {
            label: opt((label_ident, ':', ws.map(Into::into)).with_span()),
            name: node_ident,
            unit_address: opt(preceded('@', node_ident)),
            _: (ws, '{', wsc),
            props: trace("props", separated(0.., trace("prop", prop).with_span(), wsc)),
            _: wsc,
            nodes: trace("nodes", separated(0.., trace("node", Self::parse), wsc)),
            _: (wsc, '}', ws, ';'),
        }}
        .with_span()
        .parse_next(input)
    }
}

trait LiteralExt {
    //fn expected<I, O, E, C>(self) -> Context<Self, I, O, E, C> where Self: Sized, I: winnow::stream::Stream, E: AddContext<I, C>, C: Clone + Debug;
    //type E = ContextError;
    fn expected<I, E>(self) -> Context<Self, I, Self, E, StrContext>
    where
        Self: Sized + winnow::Parser<I, Self, E>,
        I: winnow::stream::Stream,
        E: AddContext<I, StrContext>;
}
impl LiteralExt for char {
    fn expected<I, E>(self) -> Context<Self, I, Self, E, StrContext>
    where
        Self: Sized + winnow::Parser<I, Self, E>,
        I: winnow::stream::Stream,
        E: AddContext<I, StrContext>,
    {
        self.context(StrContext::Expected(self.into()))
    }
}
impl LiteralExt for &'static str {
    fn expected<I, E>(self) -> Context<Self, I, Self, E, StrContext>
    where
        Self: Sized + winnow::Parser<I, Self, E>,
        I: winnow::stream::Stream,
        E: AddContext<I, StrContext>,
    {
        self.context(StrContext::Expected(self.into()))
    }
}

pub fn parse(input: &str) -> Result<Document, ParseError<Stream, ContextError>> {
    seq! {Document {
        _: ("/dts-v1/".expected(), wsc, ';'.expected(), wsc, '/'.expected(), ws, '{'.expected(), ws),
        props: trace("props", separated(0.., trace("prop", prop).with_span(), wsc)),
        _: wsc,
        nodes: trace("nodes", separated(0.., trace("node", Node::parse), wsc)),
        _: (wsc, '}', ws, ';', wsc),
    }}
    .parse(Located::new(input))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hex_byte() {
        assert_eq!(hex_byte(&mut Located::new("12")), Ok('\x12'));
        assert!(hex_byte(&mut Located::new("gh")).is_err());
        assert!(preceded("0x", hex_byte)
            .parse_next(&mut Located::new("0xgha"))
            .is_err());
    }

    #[test]
    fn test_string() {
        assert_eq!(
            string(&mut Located::new(r#""\"\r\n\t\x12""#)),
            Ok("\"\r\n\t\x12".to_owned())
        );
        assert_eq!(string(&mut Located::new("\"\n\"")), Ok("\n".to_owned()));
        assert_eq!(
            string(&mut Located::new(r#""text\nwith\nnewlines""#)),
            Ok("text\nwith\nnewlines".to_owned())
        );
    }
}
