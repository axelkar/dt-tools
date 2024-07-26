use std::sync::Arc;

use dt_parser::{
    cst2::{lexer::TokenKind, RedNode},
    TextRange,
};

/// Preprocessor includes
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PPInclude {
    /// The position of the token in the file.
    ///
    /// Usage of constants defined in this file is only valid after this position.
    pub text_range: TextRange,
    /// The raw path pointed to.
    pub path: String,
    /// Whether the current directory should be searched too.
    ///
    /// True if double quotes are used.
    pub relative: bool,
}
impl PPInclude {
    fn parse(text_range: TextRange, s: &str) -> Option<Self> {
        debug_assert!(s.starts_with('#'));
        let s = s
            .get(1..)
            .expect("lexer safe")
            .trim_start_matches(|ch| ch == ' ' || ch == '\t');

        debug_assert!(s.starts_with("include"));
        let s = s
            .get("include".len()..)
            .expect("lexer safe")
            .trim_start_matches(|ch| ch == ' ' || ch == '\t');

        let (relative, (path, _rest)) = match s.as_bytes().first()? {
            b'<' => (false, s.get(1..).expect("safe").split_once('>')?),
            b'"' => (true, s.get(1..).expect("safe").split_once('"')?),
            _ => return None,
        };

        // TODO: add diagnostic when rest is non-empty

        Some(PPInclude {
            text_range,
            path: path.to_owned(),
            relative,
        })
    }

    // TODO: child_tokens that returns just GreenNode and the text_offset, no need for Arcs
    //pub fn gather_includes(doc: &ast::Document) -> impl Iterator<Item = PPInclude> {
    pub fn gather_includes(doc: &Arc<RedNode>) -> impl Iterator<Item = PPInclude> + '_ {
        doc.child_tokens()
            .filter(|tok| tok.green.kind == TokenKind::IncludeDirective)
            .filter_map(|tok| Self::parse(tok.text_range(), &tok.green.text))
    }
}
