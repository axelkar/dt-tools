//! Devicetree `/include/` splicing.

use std::borrow::Cow;

use camino::Utf8Path;
use dt_tools_diagnostic::{Diagnostic, Severity, Span, text_range::TextRange};
use dt_tools_parser::lexer::{Token, TokenKind, lex};

use crate::{
    config::PreprocessorConfig,
    files::{IncludeSearch, SourceId, SourceSet, resolve_file},
    origin::{Origin, ProvenanceMap},
    output::IncludeEdge,
};

/// The result of splicing `/include/` directives.
pub(crate) struct Splice {
    pub(crate) text: String,
    pub(crate) map: ProvenanceMap,
    pub(crate) includes: Vec<IncludeEdge>,
    pub(crate) diagnostics: Vec<Diagnostic<SourceId>>,
}

/// Splices every `/include/` directive in `text`.
pub(crate) fn splice<R>(
    text: &str,
    map: &ProvenanceMap,
    sources: &mut SourceSet,
    config: &PreprocessorConfig,
    read_file: &mut R,
    root: SourceId,
) -> Splice
where
    R: FnMut(&Utf8Path) -> Option<String>,
{
    if !text.contains("/include/") {
        return Splice {
            text: text.to_owned(),
            map: map.clone(),
            includes: Vec::new(),
            diagnostics: Vec::new(),
        };
    }

    let mut out = Splice {
        text: String::new(),
        map: ProvenanceMap::new(),
        includes: Vec::new(),
        diagnostics: Vec::new(),
    };
    splice_text(&mut out, text, map, sources, config, read_file, root, 0);
    out
}

/// Copies `range` from `text`, preserving its provenance.
fn copy_plain(out: &mut Splice, text: &str, map: &ProvenanceMap, range: TextRange) {
    if range.length() == 0 {
        return;
    }

    let out_start = out.text.len();
    out.text.push_str(&text[range.byte_range()]);

    for segment in map.segments() {
        let Some(intersection) = segment.range.intersect(range) else {
            continue;
        };
        let offset = intersection.start - segment.range.start;
        let origin = match &segment.origin {
            Origin::File {
                file,
                range: origin,
            } => {
                let start = (origin.start + offset).min(origin.end);
                let end = (start + intersection.length()).min(origin.end);

                Origin::File {
                    file: *file,
                    range: TextRange::new(start, end),
                }
            }
            Origin::Concat { .. } | Origin::Stringify { .. } => segment.origin.clone(),
            Origin::Generated => Origin::Generated,
        };
        let start = out_start + (intersection.start - range.start);
        out.map.push(
            TextRange::new(start, start + intersection.length()),
            origin,
            segment.expansion.clone(),
            segment.arguments.clone(),
        );
    }
}

/// Appends a file's spliced text, its provenance, and its diagnostics.
fn append_nested(out: &mut Splice, mut nested: Splice) {
    out.includes.append(&mut nested.includes);
    out.diagnostics.append(&mut nested.diagnostics);

    if nested.text.is_empty() {
        return;
    }

    if !out.text.is_empty() && !out.text.ends_with('\n') {
        push_generated(out, "\n");
    }

    let base = out.text.len();
    out.text.push_str(&nested.text);
    for segment in nested.map.segments() {
        out.map.push(
            segment.range.offset(base),
            segment.origin.clone(),
            segment.expansion.clone(),
            segment.arguments.clone(),
        );
    }

    if !out.text.ends_with('\n') {
        push_generated(out, "\n");
    }
}

/// Appends preprocessor-generated text, such as a separating newline.
fn push_generated(out: &mut Splice, text: &str) {
    let start = out.text.len();
    out.text.push_str(text);
    out.map.push(
        TextRange::new(start, out.text.len()),
        Origin::Generated,
        None,
        Vec::new(),
    );
}

/// Returns the name and end offset of an `/include/` directive's string.
fn include_name<'a>(tokens: &'a [Token<'a>], index: usize) -> Option<(&'a str, usize, usize)> {
    let string_index = tokens[index + 1..]
        .iter()
        .position(|token| !token.kind.is_ok_and(TokenKind::is_trivia))?;
    let string_index = index + 1 + string_index;
    let string = &tokens[string_index];
    if string.kind != Ok(TokenKind::String) {
        return None;
    }

    let name = string
        .text
        .strip_prefix('"')
        .and_then(|text| text.strip_suffix('"'))?;
    Some((name, string.text_range.end, string_index))
}

/// Reports a missing or malformed include name.
fn report_expected_string(out: &mut Splice, span: Span<SourceId>) {
    out.diagnostics.push(Diagnostic::new(
        span,
        Cow::Borrowed("expected a string after `/include/`"),
        Severity::Error,
    ));
}

/// Returns `range`'s first source span.
fn source_span(map: &ProvenanceMap, range: TextRange, fallback: SourceId) -> Span<SourceId> {
    map.resolve(range)
        .primary_spans
        .first()
        .copied()
        .unwrap_or(Span {
            file: fallback,
            text_range: range,
        })
}

/// Splices `/include/` directives in one file's text.
#[expect(clippy::too_many_arguments, reason = "recursive splice step")]
fn splice_text<R>(
    out: &mut Splice,
    text: &str,
    map: &ProvenanceMap,
    sources: &mut SourceSet,
    config: &PreprocessorConfig,
    read_file: &mut R,
    fallback: SourceId,
    depth: usize,
) where
    R: FnMut(&Utf8Path) -> Option<String>,
{
    let tokens = lex(text);
    let mut copied = 0;
    let mut index = 0;

    while index < tokens.len() {
        if tokens[index].kind != Ok(TokenKind::DtIncludeDirective) {
            index += 1;
            continue;
        }

        let directive = &tokens[index];
        let span = source_span(map, directive.text_range, fallback);
        let Some((name, end, string_index)) = include_name(&tokens, index) else {
            report_expected_string(out, span);
            index += 1;
            continue;
        };

        let replaced = TextRange::new(directive.text_range.start, end);
        copy_plain(out, text, map, TextRange::new(copied, replaced.start));
        copied = replaced.end;
        index = string_index + 1;

        if depth >= config.max_include_depth {
            out.diagnostics.push(Diagnostic::new(
                span,
                Cow::Borrowed("`/include/` nested too deeply"),
                Severity::Error,
            ));
            continue;
        }

        let includer = sources.get(span.file).path.clone();
        let Some((path, contents)) = resolve_file(
            &config.include_dirs,
            &includer,
            name,
            IncludeSearch::IncluderFirst,
            read_file,
        ) else {
            out.diagnostics.push(Diagnostic::new(
                span,
                Cow::Owned(format!("`{name}`: No such file or directory")),
                Severity::Error,
            ));
            continue;
        };

        let id = sources.add(&path);
        out.includes.push(IncludeEdge { file: id, span });

        let mut raw_map = ProvenanceMap::new();
        raw_map.push(
            TextRange::new(0, contents.len()),
            Origin::File {
                file: id,
                range: TextRange::new(0, contents.len()),
            },
            None,
            Vec::new(),
        );
        let mut nested = Splice {
            text: String::new(),
            map: ProvenanceMap::new(),
            includes: Vec::new(),
            diagnostics: Vec::new(),
        };
        splice_text(
            &mut nested,
            &contents,
            &raw_map,
            sources,
            config,
            read_file,
            id,
            depth + 1,
        );
        append_nested(out, nested);
    }

    copy_plain(out, text, map, TextRange::new(copied, text.len()));
}
