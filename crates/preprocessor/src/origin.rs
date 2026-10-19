//! Expanded text provenance.

use std::sync::Arc;

use dt_tools_diagnostic::{MultiSpan, Span, SpanLabel, text_range::TextRange};

use crate::files::SourceId;

/// A macro expansion that produced a run of text.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MacroExpansion {
    /// The macro name.
    pub name: Arc<str>,
    /// Where the macro was invoked.
    pub invocation: Span<SourceId>,
    /// Where the macro was defined.
    pub definition: Span<SourceId>,
    /// The expansion that produced the invocation, for nested expansions.
    pub parent: Option<Arc<MacroExpansion>>,
}

/// The macro argument a run of text came from.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Argument {
    /// The parameter name.
    pub parameter: Arc<str>,
    /// The macro that declares the parameter.
    pub macro_name: Arc<str>,
    /// The argument's span in the enclosing text.
    pub span: Span<SourceId>,
}

/// Where a run of expanded text came from.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Origin {
    /// Verbatim text from a source file.
    File { file: SourceId, range: TextRange },
    /// A token pasted with `##`.
    Concat {
        /// The left operand.
        left: Span<SourceId>,
        /// The right operand.
        right: Span<SourceId>,
        /// The `##` operator.
        operator: Span<SourceId>,
    },
    /// A token produced by `#`.
    Stringify {
        /// The `#` operator.
        operator: Span<SourceId>,
        /// The stringified argument.
        argument: Span<SourceId>,
    },
    /// Text synthesized by the preprocessor, e.g. a separator preventing an accidental token paste.
    Generated,
}

/// A contiguous run of expanded text with one origin.
///
/// The origin's range maps onto the run's range by relative offset. If the lengths differ, the
/// mapping is approximate. For example, a token containing a removed line continuation is shorter
/// than its origin.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Segment {
    /// Range in the expanded text.
    pub range: TextRange,
    /// The origin of the run.
    pub origin: Origin,
    /// The macro expansion that produced the run, if any.
    pub expansion: Option<Arc<MacroExpansion>>,
    /// The macro arguments the run came from.
    pub arguments: Vec<Arc<Argument>>,
}

/// The segments covering the expanded text.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ProvenanceMap {
    segments: Vec<Segment>,
}

impl ProvenanceMap {
    /// Returns an empty map.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Returns the segments, in expanded-text order.
    #[must_use]
    pub fn segments(&self) -> &[Segment] {
        &self.segments
    }

    /// Appends a run of expanded text.
    pub fn push(
        &mut self,
        range: TextRange,
        origin: Origin,
        expansion: Option<Arc<MacroExpansion>>,
        arguments: Vec<Arc<Argument>>,
    ) {
        if range.length() == 0 {
            return;
        }

        // Merge contiguous runs.
        if let Some(last) = self.segments.last_mut()
            && last.range.end == range.start
            && last.expansion == expansion
            && last.arguments == arguments
        {
            match (&origin, &last.origin) {
                (
                    Origin::File {
                        file,
                        range: new_orig,
                    },
                    Origin::File {
                        file: last_file,
                        range: last_orig,
                    },
                ) if last_file == file
                    && last_orig.end == new_orig.start
                    && last.range.length() == last_orig.length()
                    && range.length() == new_orig.length() =>
                {
                    last.range.end = range.end;
                    last.origin = Origin::File {
                        file: *file,
                        range: TextRange::new(last_orig.start, new_orig.end),
                    };
                    return;
                }
                (Origin::Generated, Origin::Generated) => {
                    last.range.end = range.end;
                    return;
                }
                _ => {}
            }
        }

        self.segments.push(Segment {
            range,
            origin,
            expansion,
            arguments,
        });
    }

    /// Resolves a range in the expanded text to a multi-span of source files.
    #[must_use]
    pub fn resolve(&self, range: TextRange) -> MultiSpan<SourceId> {
        let mut primary_spans: Vec<Span<SourceId>> = Vec::new();
        let mut groups: Vec<Option<PrimaryGroup<'_>>> = Vec::new();
        let mut span_labels: Vec<SpanLabel<SourceId>> = Vec::new();

        for segment in &self.segments {
            let Some(subrange) = segment.range.intersect(range) else {
                continue;
            };

            let (segment_spans, mergeable) = match &segment.origin {
                Origin::File { file, range: orig } => (
                    vec![clipped_span(*file, *orig, segment.range, subrange)],
                    true,
                ),
                Origin::Concat {
                    left,
                    right,
                    operator,
                } => {
                    push_label(&mut span_labels, *operator, "pasted here with `##`");
                    // Keep both operands as distinct locations.
                    (vec![*left, *right], false)
                }
                Origin::Stringify { operator, argument } => {
                    push_label(&mut span_labels, *operator, "stringified here");
                    (vec![*argument], true)
                }
                Origin::Generated => (Vec::new(), true),
            };

            for span in &segment_spans {
                push_span(
                    &mut primary_spans,
                    &mut groups,
                    *span,
                    mergeable,
                    segment.expansion.as_deref(),
                    &segment.arguments,
                );
            }

            for argument in &segment.arguments {
                let message = format!(
                    "argument `{}` of `{}`",
                    argument.parameter, argument.macro_name
                );
                push_label(&mut span_labels, argument.span, &message);
            }

            let mut expansion = segment.expansion.as_deref();
            while let Some(current) = expansion {
                push_label(
                    &mut span_labels,
                    current.invocation,
                    &format!("in this expansion of `{}`", current.name),
                );
                push_label(
                    &mut span_labels,
                    current.definition,
                    &format!("`{}` defined here", current.name),
                );
                expansion = current.parent.as_deref();
            }
        }

        if primary_spans.is_empty()
            && let Some(span) = self.nearest_span(range.start)
        {
            primary_spans.push(span);
        }

        MultiSpan {
            primary_spans,
            span_labels,
        }
    }

    /// Returns a file span close to `offset`, for ranges that only cover generated text.
    fn nearest_span(&self, offset: usize) -> Option<Span<SourceId>> {
        let before_idx = self
            .segments
            .partition_point(|segment| segment.range.start <= offset);
        self.segments[..before_idx]
            .iter()
            .rev()
            .find_map(|segment| file_span_at(segment, offset))
            .or_else(|| {
                self.segments
                    .iter()
                    .find_map(|segment| file_span_at(segment, offset))
            })
    }
}

/// Returns the file span of `segment` closest to `offset`.
fn file_span_at(segment: &Segment, offset: usize) -> Option<Span<SourceId>> {
    let (file, range) = match &segment.origin {
        Origin::File { file, range } => (*file, *range),
        Origin::Concat { left, .. } => (left.file, left.text_range),
        Origin::Stringify { argument, .. } => (argument.file, argument.text_range),
        Origin::Generated => return None,
    };
    let clamped = offset.clamp(segment.range.start, segment.range.end);
    let start = range.start + (clamped - segment.range.start).min(range.length());
    let end = (start + 1).min(range.end).max(start);
    Some(Span {
        file,
        text_range: TextRange::new(start, end),
    })
}

/// Returns `segment`'s origin clipped to `subrange`.
fn clipped_span(
    file: SourceId,
    origin: TextRange,
    segment: TextRange,
    subrange: TextRange,
) -> Span<SourceId> {
    let range = if segment.length() == origin.length() {
        segment.project_subrange(subrange, origin)
    } else {
        // A removed line continuation makes the origin shorter than the segment.
        let start = (subrange.start - segment.start).min(origin.length());
        let end = (segment.end - subrange.end).min(origin.length() - start);

        origin.trim_start(start).trim_end(end)
    };
    Span {
        file,
        text_range: range,
    }
}

/// Primary spans sharing a source file, macro expansion, and argument set.
#[derive(Debug, PartialEq)]
struct PrimaryGroup<'a> {
    file: SourceId,
    expansion: Option<&'a MacroExpansion>,
    arguments: &'a [Arc<Argument>],
}

/// Pushes `span`, merging it into the covering span of its origin group if requested.
fn push_span<'a>(
    spans: &mut Vec<Span<SourceId>>,
    groups: &mut Vec<Option<PrimaryGroup<'a>>>,
    span: Span<SourceId>,
    mergeable: bool,
    expansion: Option<&'a MacroExpansion>,
    arguments: &'a [Arc<Argument>],
) {
    if spans.contains(&span) {
        return;
    }

    let group = mergeable.then_some(PrimaryGroup {
        file: span.file,
        expansion,
        arguments,
    });

    if let Some(group) = &group
        && let Some(index) = groups
            .iter()
            .position(|existing| existing.as_ref() == Some(group))
    {
        let existing = &mut spans[index];
        existing.text_range = TextRange::new(
            existing.text_range.start.min(span.text_range.start),
            existing.text_range.end.max(span.text_range.end),
        );
        return;
    }

    spans.push(span);
    groups.push(group);
}

/// Pushes a label unless it is already present.
fn push_label(labels: &mut Vec<SpanLabel<SourceId>>, span: Span<SourceId>, message: &str) {
    let label = SpanLabel {
        span,
        msg: message.to_owned().into(),
    };
    if !labels.contains(&label) {
        labels.push(label);
    }
}

#[cfg(test)]
mod tests {
    use std::{ops::Range, sync::Arc};

    use dt_tools_diagnostic::text_range::TextRange;

    use super::{MacroExpansion, Origin, ProvenanceMap};
    use crate::files::SourceId;

    #[test]
    fn merges_contiguous_file_runs() {
        let mut map = ProvenanceMap::new();
        let file = SourceId(0);
        map.push(
            TextRange::new(0, 3),
            Origin::File {
                file,
                range: TextRange::new(0, 3),
            },
            None,
            Vec::new(),
        );
        map.push(
            TextRange::new(3, 5),
            Origin::File {
                file,
                range: TextRange::new(3, 5),
            },
            None,
            Vec::new(),
        );

        assert_eq!(map.segments().len(), 1);
        assert_eq!(map.segments()[0].range, TextRange::new(0, 5));
    }

    #[test]
    fn resolves_whole_segments_exactly() {
        let mut map = ProvenanceMap::new();
        let file = SourceId(0);
        // A token containing a removed line continuation: cleaned 0..6, original 0..8.
        map.push(
            TextRange::new(0, 6),
            Origin::File {
                file,
                range: TextRange::new(0, 8),
            },
            None,
            Vec::new(),
        );

        let span = map.resolve(TextRange::new(0, 6));
        assert_eq!(span.primary_spans.len(), 1);
        assert_eq!(span.primary_spans[0].text_range, TextRange::new(0, 8));
    }

    #[test]
    fn merges_segments_of_the_origin_group() {
        let mut map = ProvenanceMap::new();
        let file = SourceId(0);
        // Two runs split by a removed comment: 0..3 -> 0..3, 4..7 -> 8..11.
        let push = |map: &mut ProvenanceMap, cleaned: Range<usize>, orig: Range<usize>| {
            map.push(
                TextRange::from(cleaned),
                Origin::File {
                    file,
                    range: TextRange::from(orig),
                },
                None,
                Vec::new(),
            );
        };
        push(&mut map, 0..3, 0..3);
        push(&mut map, 4..7, 8..11);

        let span = map.resolve(TextRange::new(0, 7));
        assert_eq!(span.primary_spans.len(), 1);
        assert_eq!(span.primary_spans[0].text_range, TextRange::new(0, 11));
    }

    #[test]
    fn keeps_different_origin_groups_separate() {
        let mut map = ProvenanceMap::new();
        let file = SourceId(0);
        let expansion = Arc::new(MacroExpansion {
            name: "X".into(),
            invocation: TextRange::new(10, 11).within_file(file),
            definition: TextRange::new(12, 13).within_file(file),
            parent: None,
        });

        let push = |map: &mut ProvenanceMap, cleaned: Range<usize>| {
            map.push(
                TextRange::from(cleaned.clone()),
                Origin::File {
                    file,
                    range: TextRange::from(cleaned),
                },
                Some(Arc::clone(&expansion)),
                Vec::new(),
            );
        };
        push(&mut map, 0..3);
        push(&mut map, 4..7);

        assert_eq!(map.resolve(TextRange::new(0, 7)).primary_spans.len(), 1);

        let expansion = Arc::new(MacroExpansion {
            name: "Y".into(),
            invocation: TextRange::new(20, 21).within_file(file),
            definition: TextRange::new(22, 23).within_file(file),
            parent: None,
        });
        map.push(
            TextRange::new(10, 13),
            Origin::File {
                file,
                range: TextRange::new(10, 13),
            },
            Some(expansion),
            Vec::new(),
        );

        assert_eq!(map.resolve(TextRange::new(0, 13)).primary_spans.len(), 2);
    }

    #[test]
    fn resolves_offsets_inside_a_segment_by_relative_offset() {
        let mut map = ProvenanceMap::new();
        let file = SourceId(0);
        map.push(
            TextRange::new(0, 10),
            Origin::File {
                file,
                range: TextRange::new(100, 110),
            },
            None,
            Vec::new(),
        );

        let span = map.resolve(TextRange::new(2, 5));
        assert_eq!(span.primary_spans.len(), 1);
        assert_eq!(span.primary_spans[0].text_range, TextRange::new(102, 105));
    }

    #[test]
    fn clamps_offsets_past_a_shorter_origin() {
        let mut map = ProvenanceMap::new();
        let file = SourceId(0);
        // The run is longer than its origin, so offsets past the origin clamp to its end.
        map.push(
            TextRange::new(0, 10),
            Origin::File {
                file,
                range: TextRange::new(0, 2),
            },
            None,
            Vec::new(),
        );

        let span = map.resolve(TextRange::new(8, 10));
        assert_eq!(span.primary_spans.len(), 1);
        assert_eq!(span.primary_spans[0].text_range, TextRange::new(2, 2));
    }
}
