use std::{borrow::Cow, sync::Arc};

use dt_tools_diagnostic::Severity;
use dt_tools_parser::{
    TextRange,
    ast::{self, AstNode, AstToken},
    cst::RedNode,
};

use crate::{
    db::BaseDb,
    diag::{Diag, SourceMap},
    emit_parse_errors, expr_eval,
    macros::env::TrackedMapEnvMut,
};

/// Strips the `#directive` prefix from a preprocessor directive.
pub(crate) fn get_pp_directive_args(
    input: &str,
    directive: &str,
    text_range: TextRange,
) -> (String, TextRange) {
    debug_assert!(input.starts_with('#'));
    let s = input
        .get(1..)
        .expect("lexer safe")
        .trim_start_matches([' ', '\t']);

    debug_assert!(s.starts_with(directive));
    let s = s
        .get(directive.len()..)
        .expect("lexer safe")
        .trim_start_matches([' ', '\t']);

    let start_offset = subslice_offset(input, s).unwrap();

    // Newlines are escaped outside comments. Let's undo that to make the text parseable.
    let s = s.replace('\n', " \n");

    debug_assert_eq!(text_range.start + input.len(), text_range.end);
    (
        s,
        TextRange {
            start: text_range.start + start_offset,
            end: text_range.end,
        },
    )
}

#[cfg(test)]
#[test]
pub(crate) fn test_get_pp_directive_args() {
    let input = "#include a";
    assert_eq!(
        get_pp_directive_args(
            input,
            "include",
            TextRange {
                start: 0,
                end: input.len()
            }
        ),
        (
            "a".to_owned(),
            TextRange {
                start: "#include ".len(),
                end: input.len()
            }
        )
    );
}

/// Returns the byte offset of `inner` within `this` using pointer arithmetic.
fn subslice_offset(this: &str, inner: &str) -> Option<usize> {
    let self_beg = this.as_ptr() as usize;
    let inner = inner.as_ptr() as usize;
    if inner < self_beg || inner > self_beg.wrapping_add(this.len()) {
        None
    } else {
        Some(inner.wrapping_sub(self_beg))
    }
}

#[cfg(test)]
#[test]
pub(crate) fn test_subslice_offset() {
    let s = "foo";
    assert_eq!(subslice_offset(s, s.to_owned().as_str()), None);
    assert_eq!(subslice_offset(s, &s[0..]), Some(0));
    assert_eq!(subslice_offset(s, &s[1..]), Some(1));
}

/// Extracts a string literal argument from a preprocessor directive.
///
/// Returns an include's relativity (`""` quoting or `<>` quoting) and the actual string.
pub(crate) fn get_pp_include_arg(
    input: &str,
    directive: &str,
    text_range: TextRange,
    diag: &mut Diag<'_, '_>,
) -> Option<(bool, String)> {
    // TODO: macro substitution in #include

    let (args, args_text_range) = get_pp_directive_args(input, directive, text_range);

    let (relative, split) = match args.as_bytes().first() {
        Some(b'<') => (false, args.get(1..).expect("safe").split_once('>')),
        Some(b'"') => (true, args.get(1..).expect("safe").split_once('"')),
        None => {
            diag.emit(
                args_text_range,
                Cow::Borrowed("Expected an argument"),
                Severity::Error,
            );
            return None;
        }
        _ => {
            diag.emit(
                args_text_range,
                Cow::Borrowed("Unexpected character in argument, expected `\"` or `<`."),
                Severity::Error,
            );
            return None;
        }
    };

    let Some((s, _rest)) = split else {
        diag.emit(
            args_text_range,
            Cow::Borrowed("Missing string terminator"),
            Severity::Error,
        );
        return None;
    };

    Some((relative, s.to_owned()))
}

/// Evaluates a preprocessor conditional's condition.
pub(crate) fn pp_cond_directive_eval(
    db: &dyn BaseDb,
    env: &mut TrackedMapEnvMut,
    directive: &ast::PreprocessorDirective,
    diag: &mut Diag<'_, '_>,
) -> Result<bool, ()> {
    let input = directive.syntax().text();

    let directive_text_range = directive.syntax().text_range();

    let Some(directive_name) = directive.syntax().green.kind.preprocessor_directive_name() else {
        diag.emit(
            directive_text_range,
            Cow::Borrowed("Internal compiler error: preprocessor directive should have a name"),
            Severity::Warn,
        );
        return Err(());
    };

    let (condition, condition_text_range) =
        get_pp_directive_args(input, directive_name, directive_text_range);

    if directive_name == "ifdef" || directive_name == "elifdef" {
        return is_defined(db, env, diag, &condition, condition_text_range);
    } else if directive_name == "ifndef" || directive_name == "elifndef" {
        return is_defined(db, env, diag, &condition, condition_text_range).map(|defined| !defined);
    } else if directive_name == "else" {
        if !condition.is_empty() {
            diag.emit(
                condition_text_range,
                Cow::Borrowed("Extra arguments to `#else`"),
                Severity::Warn,
            );
        }

        // Else branch is always enabled
        return Ok(true);
    }

    let parse = dt_tools_parser::parser::Entrypoint::PreprocessorConditional.parse(&condition);

    // TODO: also use during eval instead of the AST offset hack?
    {
        let offset_map = SourceMap::Offset {
            parent: diag.map,
            offset: condition_text_range.start,
        };
        emit_parse_errors(&parse, &mut Diag::new(&mut *diag.sink, &offset_map));
    }

    let expr_ast = RedNode::new_offset(Arc::new(parse.green_node), condition_text_range.start)
        .child_nodes()
        .find_map(ast::Expr::cast)
        .ok_or(())?;

    let val = expr_eval::eval(db, env, expr_ast, diag)?;

    Ok(val != 0)
}

fn is_defined(
    db: &dyn BaseDb,
    env: &mut TrackedMapEnvMut<'_>,
    diag: &mut Diag<'_, '_>,
    condition: &str,
    condition_text_range: TextRange,
) -> Result<bool, ()> {
    let macro_name = condition.trim();
    if macro_name.is_empty() || macro_name.contains(char::is_whitespace) {
        diag.emit(
            condition_text_range,
            Cow::Borrowed("Expected a single identifier for this preprocessor directive"),
            Severity::Error,
        );
        Err(())
    } else {
        Ok(env.get_macro(db, macro_name).is_some())
    }
}

// TODO: tests for preprocessor directives
