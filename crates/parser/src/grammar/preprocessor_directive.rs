use std::borrow::Cow;

use crate::{
    cst::NodeKind,
    lexer::TokenKind,
    parser::{Expected, Parser},
};

/// Set to begin preprocessor conditional
pub(super) const BEGIN_COND_SET: &[TokenKind] = &[
    TokenKind::IfndefDirective,
    TokenKind::IfdefDirective,
    TokenKind::IfDirective,
];

/// Set to continue preprocessor conditional
pub(super) const CONTINUE_COND_SET: &[TokenKind] = &[
    TokenKind::ElifndefDirective,
    TokenKind::ElifdefDirective,
    TokenKind::ElifDirective,
    TokenKind::ElseDirective,
];

/// Parses a preprocessor directive.
///
/// - `inside`: Parser function for things inside a preprocessor directive. Don't put a loop inside
///   it, don't explicitly run [`preprocessor_directive`] and don't explicitly handle tokens that
///   end a preprocessor conditional branch, as they are all done automatically.
pub(super) fn eat_preprocessor_directive(p: &mut Parser, inside: fn(&mut Parser)) -> bool {
    // TODO: this inside nodes

    fn cond_branches_and_end(p: &mut Parser, inside: fn(&mut Parser)) {
        let mut m_branch = p.start();

        while !p.check(TokenKind::EndifDirective).silent().at() && !p.at_end() {
            if p.check(CONTINUE_COND_SET).silent().at() {
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

    if p.check(BEGIN_COND_SET).silent().at() {
        let m_cond = p.start();
        p.bump();
        cond_branches_and_end(p, inside);
        m_cond.complete(p, NodeKind::PreprocessorConditional);
        true
    } else if p.check(TokenKind::EndifDirective).silent().at() {
        p.error()
            .bump_wrap_err()
            .msg_custom(Cow::Borrowed("Unmatched `#endif`"))
            .emit();
        true
    } else if p.check(CONTINUE_COND_SET).silent().at() {
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
    } else {
        p.check(&[
            TokenKind::UndefDirective,
            TokenKind::PragmaDirective,
            TokenKind::DefineDirective,
            TokenKind::IncludeDirective,
            TokenKind::ErrorDirective,
        ])
        .silent()
        .eat()
    }
}
