use std::borrow::Cow;

use constcat::concat_slices;

use crate::{
    cst::NodeKind,
    grammar::preprocessor_directive::{BEGIN_COND_SET, CONTINUE_COND_SET},
    lexer::TokenKind,
    parser::{CompletedMarker, Expected, LabelName, Marker, NAME_SET, Name, Parser, TokenMatcher},
};

pub(crate) mod expr;
mod preprocessor_directive;

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
pub(super) fn macro_invocation(p: &mut Parser) {
    profiling::scope!("grammar::macro_invocation");

    let m = p.start();
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
    m.complete(p, NodeKind::MacroInvocation);
}

/// Matches a macro invocation with args, or a plain name.
#[derive(Clone, Copy)]
pub(super) struct MacroSubstitutableName;

impl TokenMatcher for MacroSubstitutableName {
    fn matches(self, p: &mut Parser) -> bool {
        // Name matches both cases
        Name.matches(p)
    }
    fn push_expected(self, p: &mut Parser) {
        Name.push_expected(p);
    }
    fn consume(self, p: &mut Parser) {
        if p.silent_at_macro_invocation_with_args() {
            macro_invocation(p);
        } else {
            Name.consume(p);
        }
    }
}

/// Parses a reference without the leading ampersand and a node.
pub(super) fn reference_noamp(p: &mut Parser) {
    if p.eat(TokenKind::LCurly) {
        // An absolute path starts with `/`, a relative path starts with a label.
        let absolute = p.eat(TokenKind::Slash);

        if absolute && p.eat(TokenKind::RCurly) {
            // Root path reference `&{/}`.
            return;
        }

        if absolute {
            path_segment(p);
        } else {
            // A relative path's first segment is a label.
            if !p.eat(Name) {
                p.error().msg_expected().bump_wrap_err().emit();
            }
        }

        while !p.at(TokenKind::RCurly)
            && !p.at_end()
            && !p.check(CONTINUE_COND_SET).silent().at()
            && !p.check(TokenKind::EndifDirective).silent().at()
        {
            // TODO: better recovery
            p.expect(TokenKind::Slash);
            path_segment(p);
        }
        p.expect(TokenKind::RCurly);
    } else if p.silent_at_macro_invocation_with_args() {
        // macro-substitutable name
        macro_invocation(p);
    } else if p.eat(LabelName) {
    } else {
        p.error().msg_expected().bump_wrap_err().emit();
    }
}

/// Parses a node name and its optional unit address in a path reference.
fn path_segment(p: &mut Parser) {
    if p.eat(Name) {
        p.eat(UnitAddress);
    } else {
        p.error().msg_expected().bump_wrap_err().emit();
    }
}

/// Matches a Devicetree reference.
#[derive(Clone, Copy)]
pub(super) struct Reference;

impl TokenMatcher for Reference {
    fn matches(self, p: &mut Parser) -> bool {
        TokenKind::Ampersand.matches(p)
    }
    fn push_expected(self, p: &mut Parser) {
        TokenKind::Ampersand.push_expected(p);
    }
    fn consume(self, p: &mut Parser) {
        let m = p.start();
        p.bump();

        reference_noamp(p);

        // TODO: rename everything phandle to reference
        m.complete(p, NodeKind::DtPhandle);
    }
}

/// Parses cells.
///
/// `AT_END`: whether a successful end of cells is determined by a `>` or end of input
pub(super) fn cells<const AT_END: bool>(p: &mut Parser) -> Result<(), ()> {
    profiling::scope!("grammar::cells");

    loop {
        if eat_cell::<AT_END>(p) {
        } else if if AT_END {
            p.add_expected(Expected::End);
            p.at_end()
        } else {
            p.at(TokenKind::RAngle)
        } {
            return Ok(());
        } else if p
            .check(&[TokenKind::Semicolon, TokenKind::LCurly, TokenKind::RCurly])
            .silent()
            .at()
            || p.at_end()
            || p.check(CONTINUE_COND_SET).silent().at()
            || p.check(TokenKind::EndifDirective).silent().at()
        {
            p.error().msg_expected().emit();
            return Err(());
        } else {
            p.error()
                .msg_expected()
                .bump_wrap_err()
                .add_hint("in cells()".into())
                .emit();
        }
    }
}

fn eat_cell<const AT_END: bool>(p: &mut Parser) -> bool {
    p.add_expected(Expected::Cell);

    if p.check(&[TokenKind::Number, TokenKind::Char])
        .silent()
        .eat()
    {
        true
    } else if p.check(TokenKind::Ident).silent().at() {
        macro_invocation(p);
        true
    } else if p.check(Reference).silent().eat() {
        true
    } else if p.check(TokenKind::LParen).silent().at() {
        // Start a parantesized expression
        dt_expr(p);
        true
    } else if eat_dts_include(p)
        || preprocessor_directive::eat_preprocessor_directive(p, &eat_cell::<AT_END>)
    {
        true
    } else if p.check(expr::OPERATORS_SET).silent().at() && !p.check(TokenKind::RAngle).at() {
        p.error()
            .msg_expected()
            .add_hint("Wrap expressions in parenthesis".into())
            .bump_wrap_err()
            .emit();
        true
    } else {
        false
    }
}

/// Parses a Devicetree expression within parenthesis.
///
/// [From specification]: "values may be represented as arithmetic, bitwise, or logical expressions within parenthesis"
///
/// - Form: `(1 + 2 + PREPROCESSOR_MACRO)`.
///
/// [spec]: https://devicetree-specification.readthedocs.io/en/latest/chapter6-source-language.html#node-and-property-definitions
fn dt_expr(p: &mut Parser) {
    profiling::scope!("grammar::dt_expr");

    let m = p.start();

    assert!(p.eat(TokenKind::LParen));

    let _ = expr::expr(p, false);

    p.expect(TokenKind::RParen);

    m.complete(p, NodeKind::DtExpr);
}

/// Parses a Devicetree cell list.
///
/// - Form: `<1>`.
fn dt_cell_list(m: Marker, p: &mut Parser) -> Result<CompletedMarker, ()> {
    assert!(p.eat(TokenKind::LAngle));

    if cells::<false>(p).is_err() {
        m.complete(p, NodeKind::ParseError);
        return Err(());
    }
    p.expect(TokenKind::RAngle);

    Ok(m.complete(p, NodeKind::DtCellList))
}

const ITEM_RECOVERY_SET: &[TokenKind] = concat_slices!(
[TokenKind]:
    &[
        TokenKind::Slash,
    ],
    &NAME_SET,
    &[
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

    profiling::scope!("grammar::propvalues");

    while !p.at_end() {
        p.add_expected(Expected::Value);

        if p.check(TokenKind::BitsDirective).silent().at() {
            let m_cell_list = p.start();
            let m = p.start();
            p.bump();
            p.expect(TokenKind::Number);
            m.complete(p, NodeKind::DtsDirective);

            if p.at(TokenKind::LAngle) {
                dt_cell_list(m_cell_list, p)?;
            } else {
                p.error().msg_expected().bump().emit();
                m_cell_list.complete(p, NodeKind::ParseError);
            }
        } else if p.check(TokenKind::String).silent().eat() {
        } else if p.check(TokenKind::LAngle).silent().at() {
            dt_cell_list(p.start(), p)?;
        } else if p.check(Reference).silent().eat() {
        } else if p.check(TokenKind::Ident).silent().at() {
            macro_invocation(p);
        } else if p.check(TokenKind::DtBytestring).silent().eat()
            || eat_dts_include(p)
            || preprocessor_directive::eat_preprocessor_directive(p, &|p| {
                let _ = propvalues(p, ending_kinds);
                true
            })
        {
        } else {
            p.error().msg_expected().bump_wrap_err().emit();
            break;
        }

        if p.eat(TokenKind::Comma) {
        } else if p.at(ending_kinds) {
            // This is here and not in the while loop's head to add them to the `expected` list,
            // for the proper error message.
            break;
        } else if p.check(PROPERTY_VALUE_RECOVERY_SET).silent().at() {
            // Missing comma but can be recovered
            p.error().msg_expected().emit();
        } else {
            return Err(());
        }
    }
    Ok(())
}

/// The caller is expected to handle the label and name.
///
/// - Form: `= "foo", <1>;` | `;`.
fn dt_property(m: Marker, p: &mut Parser) -> CompletedMarker {
    profiling::scope!("grammar::dt_property");

    if p.eat(TokenKind::Semicolon) {
        return m.complete(p, NodeKind::DtProperty);
    }

    assert!(p.eat(TokenKind::Equals));

    let list_m = p.start();
    if propvalues(p, &[TokenKind::Semicolon]).is_err() {
        list_m.complete(p, NodeKind::PropValueList);
        return m.complete(p, NodeKind::ParseError);
    }
    list_m.complete(p, NodeKind::PropValueList);

    p.check(TokenKind::Semicolon)
        .expect_recoverable(ITEM_RECOVERY_SET);

    m.complete(p, NodeKind::DtProperty)
}

/// The caller is expected to handle the label, ampersand, name and unit address.
///
/// - Form: `{ foo = "bar"; baz {}; };`.
fn dt_node_body(p: &mut Parser, m: Marker) {
    profiling::scope!("grammar::dt_node_body");

    let lcurly_span = p
        .range()
        .expect("should not be at end of input by caller guarantee");

    // TODO: convert other grammars to assert eat
    assert!(p.eat(TokenKind::LCurly));

    while !p.at(TokenKind::RCurly) && !p.at_end() {
        item(p);
    }

    if p.at_end() {
        // TODO: Is there a way to combine this with `,` and `;`
        p.error()
            .msg_custom(Cow::Borrowed("Expected `}`, but reached end of input"))
            .add_span_label(lcurly_span, Cow::Borrowed("Unclosed delimiter"))
            .emit();
        m.complete(p, NodeKind::DtNode);
        return;
    }

    p.expect(TokenKind::RCurly);

    p.check(TokenKind::Semicolon)
        .expect_recoverable(ITEM_RECOVERY_SET);

    m.complete(p, NodeKind::DtNode);
}

fn item(p: &mut Parser) {
    profiling::scope!("grammar::item");

    if eat_node_or_prop(p) {
    } else if p.check(TokenKind::Equals).silent().at() {
        let m = p.start();
        p.error()
            .msg_expected()
            .add_hint(Cow::Borrowed("Recovered as unnamed property"))
            .emit();

        let m_prop = p.start();
        dt_property(m_prop, p);
        m.complete(p, NodeKind::ParseError);
    } else if p.check(TokenKind::LCurly).silent().at() {
        let m = p.start();
        // TODO: lint & analyze unnamed nodes, remove ParseError wrap
        p.error()
            .msg_expected()
            .add_hint(Cow::Borrowed("Recovered as unnamed node"))
            .emit();

        let m_node = p.start();
        dt_node_body(p, m_node);
        m.complete(p, NodeKind::ParseError);
    } else if p.check(TokenKind::Semicolon).silent().at() {
        p.error()
            .msg_custom(Cow::Borrowed("Unmatched `;`"))
            .bump_wrap_err()
            .emit();
    } else if eat_dts_directive(p)
        || preprocessor_directive::eat_preprocessor_directive(p, &|p| {
            item(p);
            true
        })
    {
    } else {
        p.error().msg_expected().bump_wrap_err().emit();
    }
}

/// Eats e.g. the labels and name of a node or property.
///
/// The boolean in the return value means `allow_prop`.
fn eat_node_or_prop_prefix(p: &mut Parser) -> Option<bool> {
    let allow_prop = !p.eat(TokenKind::OmitIfNoRefDirective);

    loop {
        if let Some(m_label) = p.eat_starting(MacroSubstitutableName) {
            if p.eat(TokenKind::Colon) {
                // label
                m_label.complete(p, NodeKind::DtLabel);
            } else {
                // normal node or prop name
                if p.eat(UnitAddress) && p.check(TokenKind::Colon).silent().at() {
                    p.error()
                        .msg_custom(Cow::Borrowed("Label names can't contain an `@`"))
                        .bump()
                        .complete(m_label)
                        .emit();
                } else {
                    m_label.ignore(p);
                    return Some(allow_prop);
                }
            }
        } else if p.eat(Reference) {
            // extension e.g. `bar: &foo {};`
            return Some(false);
        } else if p.eat(TokenKind::Slash) {
            // root node
            return Some(false);
        } else {
            return None;
        }
    }
}

fn eat_node_or_prop(p: &mut Parser) -> bool {
    let m = p.start();

    let Some(allow_prop) = eat_node_or_prop_prefix(p) else {
        m.ignore(p);
        return false;
    };

    if allow_prop && p.at(&[TokenKind::Equals, TokenKind::Semicolon]) {
        dt_property(m, p);
    } else if p.at(TokenKind::LCurly) {
        dt_node_body(p, m);
    } else if p.check(TokenKind::RCurly).silent().at() {
        // TODO: what if inside a node?
        // axka 2025-04-21: I think this could just be "ignored" and left to the caller of item

        p.error().bump().complete(m).msg_expected().emit();
    } else if !allow_prop
        && p.check(&[TokenKind::Equals, TokenKind::Semicolon])
            .silent()
            .at()
    {
        p.error()
            .msg_expected()
            .add_hint("Property not allowed".into())
            .emit();

        dt_property(p.start(), p);
        m.complete(p, NodeKind::ParseError);
    } else {
        p.error().msg_expected().emit();

        if !p.check(ITEM_RECOVERY_SET).silent().at() && !p.at_end() {
            p.bump();
        }

        m.complete(p, NodeKind::ParseError);
    }

    true
}

/// Note: DTC supports this at the lexer level.
fn eat_dts_include(p: &mut Parser) -> bool {
    if let Some(m) = p.eat_starting(TokenKind::DtIncludeDirective) {
        p.expect(TokenKind::String);
        m.complete(p, NodeKind::DtsDirective);
        true
    } else {
        false
    }
}

/// Note: only directives wrapped by [`NodeKind::DtsDirective`].
fn eat_dts_directive(p: &mut Parser) -> bool {
    if let Some(m) = p.eat_starting(&[TokenKind::V1Directive, TokenKind::PluginDirective]) {
        p.check(TokenKind::Semicolon)
            .expect_recoverable(ITEM_RECOVERY_SET);
        m.complete(p, NodeKind::DtsDirective);
    } else if eat_dts_include(p) {
    } else if let Some(m) = p.eat_starting(TokenKind::MemreserveDirective) {
        let m_params = p.start();
        p.expect(TokenKind::Number);
        p.expect(TokenKind::Number);
        m_params.complete(p, NodeKind::DtsDirectiveArguments);

        p.check(TokenKind::Semicolon)
            .expect_recoverable(ITEM_RECOVERY_SET);
        m.complete(p, NodeKind::DtsDirective);
    } else if let Some(m) = p.eat_starting(&[
        TokenKind::DeleteNodeDirective,
        TokenKind::DeletePropertyDirective,
    ]) {
        // TODO: error for delete-property outside of node

        let m_params = p.start();
        if p.eat(Reference) {
        } else if p.eat(MacroSubstitutableName) {
            p.eat(UnitAddress);
        } else {
            p.error().msg_expected().emit();
        }
        m_params.complete(p, NodeKind::DtsDirectiveArguments);

        p.check(TokenKind::Semicolon)
            .expect_recoverable(ITEM_RECOVERY_SET);
        m.complete(p, NodeKind::DtsDirective);
    } else {
        return false;
    }
    true
}

/// Matches a unit address.
#[derive(Clone, Copy)]
pub(super) struct UnitAddress;

impl TokenMatcher for UnitAddress {
    fn matches(self, p: &mut Parser) -> bool {
        TokenKind::AtSign.matches(p)
    }
    fn push_expected(self, p: &mut Parser) {
        TokenKind::AtSign.push_expected(p);
    }
    fn consume(self, p: &mut Parser) {
        let m = p.start();
        p.bump();

        p.expect(MacroSubstitutableName);

        m.complete(p, NodeKind::UnitAddress);
    }
}

pub(super) fn entry_sourcefile(p: &mut Parser) {
    profiling::scope!("grammar::entry_sourcefile");

    while !p.at_end() {
        if p.check(TokenKind::RCurly).silent().at() {
            p.error().msg_custom(Cow::Borrowed("Unmatched `}`")).emit();

            let e = p.start();
            p.bump();
            e.complete(p, NodeKind::ParseError);
        } else if preprocessor_directive::catch_unmatched_pp_directive(p, &|p| {
            item(p);
            true
        }) {
        } else {
            item(p);
        }
    }
}

pub(super) fn entry_name(p: &mut Parser) -> bool {
    p.eat(MacroSubstitutableName)
}

#[cfg(test)]
mod tests {
    use expect_test::{Expect, expect};

    use crate::parser::{Entrypoint, parse};

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
    #[expect(clippy::too_many_lines, reason = "inline fixture and snapshot")]
    fn parse_1() {
        check(
            r#"/dts-v1/;

/ {
  model = "Qualcomm Technologies, Inc. Lagoon MTP Ebba";
	compatible = "qcom,lagoon-mtp", "qcom,lagoon", "qcom,mtp";
	qcom,msm-id = <434 0x10000>, <459 0x10000>;
	interrupt-parent = <0x01>;
	#address-cells = <0x02>;
	#size-cells = <0x02>;
	qcom,board-id = <8 0>;
	oplus,dtsi_no = <20813>;

	memory {
		ddr_device_type = <0x07>;
		device_type = "memory";
		reg = <0x00 0x80000000 0x00 0x3bb00000 0x01 0x80000000 0x01 0x00 0x00 0xc0000000 0x00 0xc0000000>;
	};

  a: foo {
    setting = <2>;
  };

  bar {
    ref-to-foo = <&a>;
    empty;
  };

  //foo
  /* bar */
};
"#,
            expect![[r##"
                Errors: []

                Tree:
                SourceFile@0..582
                  DtsDirective@0..9
                    V1Directive@0..8 "/dts-v1/"
                    Semicolon@8..9 ";"
                  Whitespace@9..11 "\n\n"
                  DtNode@11..581
                    Slash@11..12 "/"
                    Whitespace@12..13 " "
                    LCurly@13..14 "{"
                    Whitespace@14..17 "\n  "
                    DtProperty@17..71
                      Name@17..22 "model"
                      Whitespace@22..23 " "
                      Equals@23..24 "="
                      Whitespace@24..25 " "
                      PropValueList@25..70
                        String@25..70 "\"Qualcomm Technologies, Inc. Lagoon MTP Ebba\""
                      Semicolon@70..71 ";"
                    Whitespace@71..73 "\n\t"
                    DtProperty@73..131
                      Name@73..83 "compatible"
                      Whitespace@83..84 " "
                      Equals@84..85 "="
                      Whitespace@85..86 " "
                      PropValueList@86..130
                        String@86..103 "\"qcom,lagoon-mtp\""
                        Comma@103..104 ","
                        Whitespace@104..105 " "
                        String@105..118 "\"qcom,lagoon\""
                        Comma@118..119 ","
                        Whitespace@119..120 " "
                        String@120..130 "\"qcom,mtp\""
                      Semicolon@130..131 ";"
                    Whitespace@131..133 "\n\t"
                    DtProperty@133..176
                      Name@133..144 "qcom,msm-id"
                      Whitespace@144..145 " "
                      Equals@145..146 "="
                      Whitespace@146..147 " "
                      PropValueList@147..175
                        DtCellList@147..160
                          LAngle@147..148 "<"
                          Number@148..151 "434"
                          Whitespace@151..152 " "
                          Number@152..159 "0x10000"
                          RAngle@159..160 ">"
                        Comma@160..161 ","
                        Whitespace@161..162 " "
                        DtCellList@162..175
                          LAngle@162..163 "<"
                          Number@163..166 "459"
                          Whitespace@166..167 " "
                          Number@167..174 "0x10000"
                          RAngle@174..175 ">"
                      Semicolon@175..176 ";"
                    Whitespace@176..178 "\n\t"
                    DtProperty@178..204
                      Name@178..194 "interrupt-parent"
                      Whitespace@194..195 " "
                      Equals@195..196 "="
                      Whitespace@196..197 " "
                      PropValueList@197..203
                        DtCellList@197..203
                          LAngle@197..198 "<"
                          Number@198..202 "0x01"
                          RAngle@202..203 ">"
                      Semicolon@203..204 ";"
                    Whitespace@204..206 "\n\t"
                    DtProperty@206..230
                      Name@206..220 "#address-cells"
                      Whitespace@220..221 " "
                      Equals@221..222 "="
                      Whitespace@222..223 " "
                      PropValueList@223..229
                        DtCellList@223..229
                          LAngle@223..224 "<"
                          Number@224..228 "0x02"
                          RAngle@228..229 ">"
                      Semicolon@229..230 ";"
                    Whitespace@230..232 "\n\t"
                    DtProperty@232..253
                      Name@232..243 "#size-cells"
                      Whitespace@243..244 " "
                      Equals@244..245 "="
                      Whitespace@245..246 " "
                      PropValueList@246..252
                        DtCellList@246..252
                          LAngle@246..247 "<"
                          Number@247..251 "0x02"
                          RAngle@251..252 ">"
                      Semicolon@252..253 ";"
                    Whitespace@253..255 "\n\t"
                    DtProperty@255..277
                      Name@255..268 "qcom,board-id"
                      Whitespace@268..269 " "
                      Equals@269..270 "="
                      Whitespace@270..271 " "
                      PropValueList@271..276
                        DtCellList@271..276
                          LAngle@271..272 "<"
                          Number@272..273 "8"
                          Whitespace@273..274 " "
                          Number@274..275 "0"
                          RAngle@275..276 ">"
                      Semicolon@276..277 ";"
                    Whitespace@277..279 "\n\t"
                    DtProperty@279..303
                      Name@279..292 "oplus,dtsi_no"
                      Whitespace@292..293 " "
                      Equals@293..294 "="
                      Whitespace@294..295 " "
                      PropValueList@295..302
                        DtCellList@295..302
                          LAngle@295..296 "<"
                          Number@296..301 "20813"
                          RAngle@301..302 ">"
                      Semicolon@302..303 ";"
                    Whitespace@303..306 "\n\n\t"
                    DtNode@306..473
                      Name@306..312 "memory"
                      Whitespace@312..313 " "
                      LCurly@313..314 "{"
                      Whitespace@314..317 "\n\t\t"
                      DtProperty@317..342
                        Name@317..332 "ddr_device_type"
                        Whitespace@332..333 " "
                        Equals@333..334 "="
                        Whitespace@334..335 " "
                        PropValueList@335..341
                          DtCellList@335..341
                            LAngle@335..336 "<"
                            Number@336..340 "0x07"
                            RAngle@340..341 ">"
                        Semicolon@341..342 ";"
                      Whitespace@342..345 "\n\t\t"
                      DtProperty@345..368
                        Name@345..356 "device_type"
                        Whitespace@356..357 " "
                        Equals@357..358 "="
                        Whitespace@358..359 " "
                        PropValueList@359..367
                          String@359..367 "\"memory\""
                        Semicolon@367..368 ";"
                      Whitespace@368..371 "\n\t\t"
                      DtProperty@371..469
                        Name@371..374 "reg"
                        Whitespace@374..375 " "
                        Equals@375..376 "="
                        Whitespace@376..377 " "
                        PropValueList@377..468
                          DtCellList@377..468
                            LAngle@377..378 "<"
                            Number@378..382 "0x00"
                            Whitespace@382..383 " "
                            Number@383..393 "0x80000000"
                            Whitespace@393..394 " "
                            Number@394..398 "0x00"
                            Whitespace@398..399 " "
                            Number@399..409 "0x3bb00000"
                            Whitespace@409..410 " "
                            Number@410..414 "0x01"
                            Whitespace@414..415 " "
                            Number@415..425 "0x80000000"
                            Whitespace@425..426 " "
                            Number@426..430 "0x01"
                            Whitespace@430..431 " "
                            Number@431..435 "0x00"
                            Whitespace@435..436 " "
                            Number@436..440 "0x00"
                            Whitespace@440..441 " "
                            Number@441..451 "0xc0000000"
                            Whitespace@451..452 " "
                            Number@452..456 "0x00"
                            Whitespace@456..457 " "
                            Number@457..467 "0xc0000000"
                            RAngle@467..468 ">"
                        Semicolon@468..469 ";"
                      Whitespace@469..471 "\n\t"
                      RCurly@471..472 "}"
                      Semicolon@472..473 ";"
                    Whitespace@473..477 "\n\n  "
                    DtNode@477..509
                      DtLabel@477..479
                        Name@477..478 "a"
                        Colon@478..479 ":"
                      Whitespace@479..480 " "
                      Name@480..483 "foo"
                      Whitespace@483..484 " "
                      LCurly@484..485 "{"
                      Whitespace@485..490 "\n    "
                      DtProperty@490..504
                        Name@490..497 "setting"
                        Whitespace@497..498 " "
                        Equals@498..499 "="
                        Whitespace@499..500 " "
                        PropValueList@500..503
                          DtCellList@500..503
                            LAngle@500..501 "<"
                            Number@501..502 "2"
                            RAngle@502..503 ">"
                        Semicolon@503..504 ";"
                      Whitespace@504..507 "\n  "
                      RCurly@507..508 "}"
                      Semicolon@508..509 ";"
                    Whitespace@509..513 "\n\n  "
                    DtNode@513..557
                      Name@513..516 "bar"
                      Whitespace@516..517 " "
                      LCurly@517..518 "{"
                      Whitespace@518..523 "\n    "
                      DtProperty@523..541
                        Name@523..533 "ref-to-foo"
                        Whitespace@533..534 " "
                        Equals@534..535 "="
                        Whitespace@535..536 " "
                        PropValueList@536..540
                          DtCellList@536..540
                            LAngle@536..537 "<"
                            DtPhandle@537..539
                              Ampersand@537..538 "&"
                              Name@538..539 "a"
                            RAngle@539..540 ">"
                        Semicolon@540..541 ";"
                      Whitespace@541..546 "\n    "
                      DtProperty@546..552
                        Name@546..551 "empty"
                        Semicolon@551..552 ";"
                      Whitespace@552..555 "\n  "
                      RCurly@555..556 "}"
                      Semicolon@556..557 ";"
                    Whitespace@557..561 "\n\n  "
                    LineComment@561..566 "//foo"
                    Whitespace@566..569 "\n  "
                    BlockComment@569..578 "/* bar */"
                    Whitespace@578..579 "\n"
                    RCurly@579..580 "}"
                    Semicolon@580..581 ";"
                  Whitespace@581..582 "\n"
            "##]],
        );
    }

    #[test]
    fn parse_2_macros() {
        check(
            r#"/dts-v1/;

// Try line comment
#define SQUARE(x) ((x) * (x)) // This macro squares a number

// Try block comment
#define MULTIPLY(a, b) /* Multiplies two numbers */ \
                       ((a) * (b))

// Real macro from the Linux kernel, try multiline block comment
#define KEY_SWITCHVIDEOMODE	227	/* Cycle between available video
					   outputs (Monitor/LCD/TV-out/etc) */

// This is invalid in GCC
#define FOO "a
b"
// Try multiline strings
#define FOO "a\
b"
#define FOO "a\\
b"
#define FOO "a\"b"
#define FOO "a\\"b"
// Try starting comments in literals
#define FOO "/*"
#define FOO "//"
#define FOO '/*'
#define FOO '//'
"#,
            expect![[r##"
                Errors: []

                Tree:
                SourceFile@0..631
                  DtsDirective@0..9
                    V1Directive@0..8 "/dts-v1/"
                    Semicolon@8..9 ";"
                  Whitespace@9..11 "\n\n"
                  LineComment@11..30 "// Try line comment"
                  Whitespace@30..31 "\n"
                  DefineDirective@31..91 "#define SQUARE(x) ((x) * (x)) // This macro squares a number"
                  Whitespace@91..93 "\n\n"
                  LineComment@93..113 "// Try block comment"
                  Whitespace@113..114 "\n"
                  DefineDirective@114..203 "#define MULTIPLY(a, b) /* Multiplies two numbers */ \\\n                       ((a) * (b))\n"
                  Whitespace@203..204 "\n"
                  LineComment@204..268 "// Real macro from the Linux kernel, try multiline block comment"
                  Whitespace@268..269 "\n"
                  DefineDirective@269..377 "#define KEY_SWITCHVIDEOMODE\t227\t/* Cycle between available video\n\t\t\t\t\t   outputs (Monitor/LCD/TV-out/etc) */"
                  Whitespace@377..379 "\n\n"
                  LineComment@379..404 "// This is invalid in GCC"
                  Whitespace@404..405 "\n"
                  DefineDirective@405..422 "#define FOO \"a\nb\""
                  Whitespace@422..423 "\n"
                  LineComment@423..447 "// Try multiline strings"
                  Whitespace@447..448 "\n"
                  DefineDirective@448..466 "#define FOO \"a\\\nb\""
                  Whitespace@466..467 "\n"
                  DefineDirective@467..486 "#define FOO \"a\\\\\nb\""
                  Whitespace@486..487 "\n"
                  DefineDirective@487..505 "#define FOO \"a\\\"b\""
                  Whitespace@505..506 "\n"
                  DefineDirective@506..525 "#define FOO \"a\\\\\"b\""
                  Whitespace@525..526 "\n"
                  LineComment@526..562 "// Try starting comments in literals"
                  Whitespace@562..563 "\n"
                  DefineDirective@563..579 "#define FOO \"/*\""
                  Whitespace@579..580 "\n"
                  DefineDirective@580..596 "#define FOO \"//\""
                  Whitespace@596..597 "\n"
                  DefineDirective@597..613 "#define FOO '/*'"
                  Whitespace@613..614 "\n"
                  DefineDirective@614..630 "#define FOO '//'"
                  Whitespace@630..631 "\n"
            "##]],
        );
    }

    #[test]
    fn parse_3_preproc() {
        check(
            r#"/dts-v1/;

// Try line comment
#if 1 // This macro squares a number
/ {};
#elif FOO
#else BAR
&do_something {};
#endif

// Try block comment
#if /* Multiplies
two numbers */ \
  ((a) * (b))
/ {};
#endif

#include \
  "filename"
"#,
            expect![[r##"
                Errors: []

                Tree:
                SourceFile@0..228
                  DtsDirective@0..9
                    V1Directive@0..8 "/dts-v1/"
                    Semicolon@8..9 ";"
                  Whitespace@9..11 "\n\n"
                  LineComment@11..30 "// Try line comment"
                  Whitespace@30..31 "\n"
                  PreprocessorConditional@31..118
                    IfDirective@31..67 "#if 1 // This macro squares a number"
                    Whitespace@67..68 "\n"
                    PreprocessorBranch@68..73
                      DtNode@68..73
                        Slash@68..69 "/"
                        Whitespace@69..70 " "
                        LCurly@70..71 "{"
                        RCurly@71..72 "}"
                        Semicolon@72..73 ";"
                    Whitespace@73..74 "\n"
                    ElifDirective@74..83 "#elif FOO"
                    Whitespace@83..84 "\n"
                    PreprocessorBranch@84..84
                    ElseDirective@84..93 "#else BAR"
                    Whitespace@93..94 "\n"
                    PreprocessorBranch@94..111
                      DtNode@94..111
                        DtPhandle@94..107
                          Ampersand@94..95 "&"
                          Name@95..107 "do_something"
                        Whitespace@107..108 " "
                        LCurly@108..109 "{"
                        RCurly@109..110 "}"
                        Semicolon@110..111 ";"
                    Whitespace@111..112 "\n"
                    EndifDirective@112..118 "#endif"
                  Whitespace@118..120 "\n\n"
                  LineComment@120..140 "// Try block comment"
                  Whitespace@140..141 "\n"
                  PreprocessorConditional@141..202
                    IfDirective@141..190 "#if /* Multiplies\ntwo numbers */ \\\n  ((a) * (b))\n"
                    PreprocessorBranch@190..195
                      DtNode@190..195
                        Slash@190..191 "/"
                        Whitespace@191..192 " "
                        LCurly@192..193 "{"
                        RCurly@193..194 "}"
                        Semicolon@194..195 ";"
                    Whitespace@195..196 "\n"
                    EndifDirective@196..202 "#endif"
                  Whitespace@202..204 "\n\n"
                  IncludeDirective@204..228 "#include \\\n  \"filename\"\n"
            "##]],
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
                        message: "Expected ‘,’ or end of input, but found ‘;’",
                        primary_text_range: TextRange {
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
                        message: "Expected cell, ‘/include/‘, preprocessor directive, ‘>’ or end of input, but found ‘>’",
                        primary_text_range: TextRange {
                            start: 3,
                            end: 4,
                        },
                        span_labels: [
                            (
                                TextRange {
                                    start: 3,
                                    end: 4,
                                },
                                "in cells()",
                            ),
                        ],
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
    fn bits_directive() {
        check_ep(
            Entrypoint::PropValues,
            "/bits/ 64 1",
            expect![[r#"
                Errors: [
                    ParseError {
                        message: "Expected ‘<’, but found number literal",
                        primary_text_range: TextRange {
                            start: 10,
                            end: 11,
                        },
                        span_labels: [],
                    },
                ]

                Tree:
                EntryPropValues@0..11
                  ParseError@0..11
                    DtsDirective@0..9
                      BitsDirective@0..6 "/bits/"
                      Whitespace@6..7 " "
                      Number@7..9 "64"
                    Whitespace@9..10 " "
                    Number@10..11 "1"
            "#]],
        );

        check_ep(
            Entrypoint::PropValues,
            "/bits/ 64 <1>",
            expect![[r#"
                Errors: []

                Tree:
                EntryPropValues@0..13
                  DtCellList@0..13
                    DtsDirective@0..9
                      BitsDirective@0..6 "/bits/"
                      Whitespace@6..7 " "
                      Number@7..9 "64"
                    Whitespace@9..10 " "
                    LAngle@10..11 "<"
                    Number@11..12 "1"
                    RAngle@12..13 ">"
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

    #[test]
    fn reference_unit_address() {
        check_ep(
            Entrypoint::ReferenceNoamp,
            "{/bus@0/padctl@3520000/pads/usb2/lanes/usb2-0}",
            expect![[r#"
                Errors: []

                Tree:
                DtPhandle@0..46
                  LCurly@0..1 "{"
                  Slash@1..2 "/"
                  Name@2..5 "bus"
                  UnitAddress@5..7
                    AtSign@5..6 "@"
                    Name@6..7 "0"
                  Slash@7..8 "/"
                  Name@8..14 "padctl"
                  UnitAddress@14..22
                    AtSign@14..15 "@"
                    Name@15..22 "3520000"
                  Slash@22..23 "/"
                  Name@23..27 "pads"
                  Slash@27..28 "/"
                  Name@28..32 "usb2"
                  Slash@32..33 "/"
                  Name@33..38 "lanes"
                  Slash@38..39 "/"
                  Name@39..45 "usb2-0"
                  RCurly@45..46 "}"
            "#]],
        );
    }

    /// A path reference can point at the root.
    #[test]
    fn reference_root_path() {
        check_ep(
            Entrypoint::ReferenceNoamp,
            "{/}",
            expect![[r#"
                Errors: []

                Tree:
                DtPhandle@0..3
                  LCurly@0..1 "{"
                  Slash@1..2 "/"
                  RCurly@2..3 "}"
            "#]],
        );
    }

    /// A path reference can start with a label instead of `/`.
    #[test]
    fn reference_label_path() {
        check_ep(
            Entrypoint::ReferenceNoamp,
            "{foo/bar}",
            expect![[r#"
                Errors: []

                Tree:
                DtPhandle@0..9
                  LCurly@0..1 "{"
                  Name@1..4 "foo"
                  Slash@4..5 "/"
                  Name@5..8 "bar"
                  RCurly@8..9 "}"
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
                  DtsDirective@1..10
                    V1Directive@1..9 "/dts-v1/"
                    Semicolon@9..10 ";"
                  Whitespace@10..11 "\n"
                  DtsDirective@11..20
                    PluginDirective@11..19 "/plugin/"
                    Semicolon@19..20 ";"
                  Whitespace@20..21 "\n"
                  DtsDirective@21..45
                    DeleteNodeDirective@21..34 "/delete-node/"
                    Whitespace@34..35 " "
                    DtsDirectiveArguments@35..44
                      Name@35..44 "node-name"
                    Semicolon@44..45 ";"
                  Whitespace@45..46 "\n"
                  DtsDirective@46..67
                    DeleteNodeDirective@46..59 "/delete-node/"
                    Whitespace@59..60 " "
                    DtsDirectiveArguments@60..66
                      DtPhandle@60..66
                        Ampersand@60..61 "&"
                        Name@61..66 "label"
                    Semicolon@66..67 ";"
                  Whitespace@67..68 "\n"
                  DtsDirective@68..99
                    MemreserveDirective@68..80 "/memreserve/"
                    Whitespace@80..81 " "
                    DtsDirectiveArguments@81..98
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
    fn parse_omit_if_no_ref() {
        check(
            "/ { /omit-if-no-ref/ foo {}; };",
            expect![[r#"
                Errors: []

                Tree:
                SourceFile@0..31
                  DtNode@0..31
                    Slash@0..1 "/"
                    Whitespace@1..2 " "
                    LCurly@2..3 "{"
                    Whitespace@3..4 " "
                    DtNode@4..28
                      OmitIfNoRefDirective@4..20 "/omit-if-no-ref/"
                      Whitespace@20..21 " "
                      Name@21..24 "foo"
                      Whitespace@24..25 " "
                      LCurly@25..26 "{"
                      RCurly@26..27 "}"
                      Semicolon@27..28 ";"
                    Whitespace@28..29 " "
                    RCurly@29..30 "}"
                    Semicolon@30..31 ";"
            "#]],
        );

        check(
            "/ { /omit-if-no-ref/ foo; };",
            expect![[r#"
                Errors: [
                    ParseError {
                        message: "Expected ‘:’, ‘@’ or ‘{’, but found ‘;’",
                        primary_text_range: TextRange {
                            start: 24,
                            end: 25,
                        },
                        span_labels: [
                            (
                                TextRange {
                                    start: 24,
                                    end: 25,
                                },
                                "Property not allowed",
                            ),
                        ],
                    },
                ]

                Tree:
                SourceFile@0..28
                  DtNode@0..28
                    Slash@0..1 "/"
                    Whitespace@1..2 " "
                    LCurly@2..3 "{"
                    Whitespace@3..4 " "
                    ParseError@4..25
                      OmitIfNoRefDirective@4..20 "/omit-if-no-ref/"
                      Whitespace@20..21 " "
                      Name@21..24 "foo"
                      DtProperty@24..25
                        Semicolon@24..25 ";"
                    Whitespace@25..26 " "
                    RCurly@26..27 "}"
                    Semicolon@27..28 ";"
            "#]],
        );
    }

    #[test]
    fn labels() {
        check(
            "foo: &bar {};",
            expect![[r#"
                Errors: []

                Tree:
                SourceFile@0..13
                  DtNode@0..13
                    DtLabel@0..4
                      Name@0..3 "foo"
                      Colon@3..4 ":"
                    Whitespace@4..5 " "
                    DtPhandle@5..9
                      Ampersand@5..6 "&"
                      Name@6..9 "bar"
                    Whitespace@9..10 " "
                    LCurly@10..11 "{"
                    RCurly@11..12 "}"
                    Semicolon@12..13 ";"
            "#]],
        );

        check(
            "foo: bar: baz {};",
            expect![[r#"
                Errors: []

                Tree:
                SourceFile@0..17
                  DtNode@0..17
                    DtLabel@0..4
                      Name@0..3 "foo"
                      Colon@3..4 ":"
                    Whitespace@4..5 " "
                    DtLabel@5..9
                      Name@5..8 "bar"
                      Colon@8..9 ":"
                    Whitespace@9..10 " "
                    Name@10..13 "baz"
                    Whitespace@13..14 " "
                    LCurly@14..15 "{"
                    RCurly@15..16 "}"
                    Semicolon@16..17 ";"
            "#]],
        );

        check(
            "foo: bar(): baz {};",
            expect![[r#"
                Errors: []

                Tree:
                SourceFile@0..19
                  DtNode@0..19
                    DtLabel@0..4
                      Name@0..3 "foo"
                      Colon@3..4 ":"
                    Whitespace@4..5 " "
                    DtLabel@5..11
                      MacroInvocation@5..10
                        Ident@5..8 "bar"
                        LParen@8..9 "("
                        RParen@9..10 ")"
                      Colon@10..11 ":"
                    Whitespace@11..12 " "
                    Name@12..15 "baz"
                    Whitespace@15..16 " "
                    LCurly@16..17 "{"
                    RCurly@17..18 "}"
                    Semicolon@18..19 ";"
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
    fn parse_node_name_macro_invocation() {
        check(
            "FOO()@BAR {}; FOO@BAR() {};",
            expect![[r#"
                Errors: []

                Tree:
                SourceFile@0..27
                  DtNode@0..13
                    MacroInvocation@0..5
                      Ident@0..3 "FOO"
                      LParen@3..4 "("
                      RParen@4..5 ")"
                    UnitAddress@5..9
                      AtSign@5..6 "@"
                      Name@6..9 "BAR"
                    Whitespace@9..10 " "
                    LCurly@10..11 "{"
                    RCurly@11..12 "}"
                    Semicolon@12..13 ";"
                  Whitespace@13..14 " "
                  DtNode@14..27
                    Name@14..17 "FOO"
                    UnitAddress@17..23
                      AtSign@17..18 "@"
                      MacroInvocation@18..23
                        Ident@18..21 "BAR"
                        LParen@21..22 "("
                        RParen@22..23 ")"
                    Whitespace@23..24 " "
                    LCurly@24..25 "{"
                    RCurly@25..26 "}"
                    Semicolon@26..27 ";"
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
