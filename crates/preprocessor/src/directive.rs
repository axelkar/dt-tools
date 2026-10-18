//! Preprocessing directive scanning.

use dt_tools_diagnostic::text_range::TextRange;

use crate::{lexer::PpTokenKind, token::PpToken};

/// The kind of a preprocessing directive.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum DirectiveKind {
    Define,
    Undef,
    Error,
    Warning,
    If,
    Ifdef,
    Ifndef,
    Elif,
    Elifdef,
    Elifndef,
    Else,
    Endif,
    /// A known C directive that the preprocessor does not implement yet.
    Unsupported,
}

impl DirectiveKind {
    /// Returns true if the directive belongs to a conditional group.
    pub(crate) fn is_conditional(self) -> bool {
        matches!(
            self,
            Self::If
                | Self::Ifdef
                | Self::Ifndef
                | Self::Elif
                | Self::Elifdef
                | Self::Elifndef
                | Self::Else
                | Self::Endif
        )
    }
}

/// One directive line.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Directive<'a> {
    /// The directive kind like `Define`.
    pub(crate) kind: DirectiveKind,
    /// The directive name like `define`.
    pub(crate) name: &'a str,
    /// Tokens after the directive name, up to the end of the logical line.
    pub(crate) argument: &'a [PpToken],
    /// Range of the whole directive line in the original file.
    pub(crate) text_range: TextRange,
    /// The directive line's length in tokens.
    pub(crate) len_tokens: usize,
}

/// Parses a directive starting at the first token in `tokens`.
///
/// Returns `None` when the tokens do not start a known directive. Unknown `#` lines are ordinary
/// tokens in assembler mode.
pub(crate) fn parse<'a>(tokens: &'a [PpToken], cleaned_text: &'a str) -> Option<Directive<'a>> {
    let hash = tokens.first()?;
    if hash.kind != PpTokenKind::Punct || hash.text(cleaned_text) != "#" || !hash.line_start {
        return None;
    }

    let name_token = tokens.get(1)?;
    if name_token.kind != PpTokenKind::Ident || name_token.line_start {
        return None;
    }
    let name = name_token.text(cleaned_text);

    let kind = match name {
        "define" => DirectiveKind::Define,
        "undef" => DirectiveKind::Undef,
        "error" => DirectiveKind::Error,
        "warning" => DirectiveKind::Warning,
        "if" => DirectiveKind::If,
        "ifdef" => DirectiveKind::Ifdef,
        "ifndef" => DirectiveKind::Ifndef,
        "elif" => DirectiveKind::Elif,
        "elifdef" => DirectiveKind::Elifdef,
        "elifndef" => DirectiveKind::Elifndef,
        "else" => DirectiveKind::Else,
        "endif" => DirectiveKind::Endif,
        "include" | "include_next" | "import" | "embed" | "pragma" | "line" => {
            DirectiveKind::Unsupported
        }
        _ => return None,
    };

    // Until EOL or EOF
    let len_tokens = tokens[2..]
        .iter()
        .position(|token| token.line_start)
        .map_or(tokens.len(), |offset| 2 + offset);

    let last = &tokens[len_tokens - 1];
    let text_range = TextRange::new(hash.orig.start, last.orig.end);

    Some(Directive {
        kind,
        name,
        argument: &tokens[2..len_tokens],
        text_range,
        len_tokens,
    })
}

#[cfg(test)]
mod tests {
    use dt_tools_diagnostic::text_range::TextRange;

    use super::{DirectiveKind, parse};
    use crate::lexer::lex;

    fn kind(input: &str) -> Option<DirectiveKind> {
        let cleaned = lex(input);
        parse(&cleaned.tokens, &cleaned.cleaned_text).map(|directive| directive.kind)
    }

    #[test]
    fn parses_known_directives() {
        assert_eq!(kind("#define A 1\n"), Some(DirectiveKind::Define));
        assert_eq!(kind("# define A 1\n"), Some(DirectiveKind::Define));
        assert_eq!(kind("  #undef A\n"), Some(DirectiveKind::Undef));
        assert_eq!(kind("#error boom\n"), Some(DirectiveKind::Error));
        assert_eq!(kind("#warning careful\n"), Some(DirectiveKind::Warning));
        assert_eq!(kind("#if 1\n"), Some(DirectiveKind::If));
        assert_eq!(kind("#ifdef A\n"), Some(DirectiveKind::Ifdef));
        assert_eq!(kind("#ifndef A\n"), Some(DirectiveKind::Ifndef));
        assert_eq!(kind("#elif 1\n"), Some(DirectiveKind::Elif));
        assert_eq!(kind("#elifdef A\n"), Some(DirectiveKind::Elifdef));
        assert_eq!(kind("#elifndef A\n"), Some(DirectiveKind::Elifndef));
        assert_eq!(kind("#else\n"), Some(DirectiveKind::Else));
        assert_eq!(kind("#endif\n"), Some(DirectiveKind::Endif));
        assert_eq!(kind("#include <a.h>\n"), Some(DirectiveKind::Unsupported));
    }

    #[test]
    fn ignores_unknown_embedded_and_empty_hashes() {
        assert_eq!(kind("#address-cells = <1>;\n"), None);
        assert_eq!(kind("#printf\n"), None);
        assert_eq!(kind("x #define A 1\n"), None);
        assert_eq!(kind("#\n"), None);
    }

    #[test]
    fn directive_ends_at_the_next_line() {
        let lexed = lex("#if A\nB\n");
        let directive = parse(&lexed.tokens, &lexed.cleaned_text).expect("directive");

        assert_eq!(directive.text_range, TextRange { start: 0, end: 5 });
        assert_eq!(directive.len_tokens, 3);
        assert_eq!(
            directive
                .argument
                .iter()
                .map(|arg| arg.text(&lexed.cleaned_text))
                .collect::<Vec<_>>(),
            vec!["A"]
        );
    }
}
