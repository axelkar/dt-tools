//! Expanded text provenance.

use dt_tools_diagnostic::{MultiSpan, Span, text_range::TextRange};

use crate::files::SourceId;

/// Where a run of expanded text came from.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Origin {
    /// Verbatim text from a source file.
    File { file: SourceId, range: TextRange },
    /// Text synthesized by the preprocessor, e.g. a separator preventing an accidental token paste.
    Generated,
}

/// A contiguous run of expanded text with one origin.
///
/// The origin's range maps onto the run's range by relative offset. If the lengths differ, the
/// mapping is approximate. For example, a token containing a removed line continuation is shorter
/// than its origin.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Segment {
    /// Range in the expanded text.
    pub range: TextRange,
    /// The origin of the run.
    pub origin: Origin,
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
    pub fn push(&mut self, range: TextRange, origin: Origin) {
        if range.length() == 0 {
            return;
        }

        // Merge contiguous runs.
        if let Some(last) = self.segments.last_mut() {
            match (origin, last.origin) {
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
                    && last.range.end == range.start
                    && last_orig.end == new_orig.start
                    && last.range.length() == last_orig.length()
                    && range.length() == new_orig.length() =>
                {
                    last.range.end = range.end;
                    last.origin = Origin::File {
                        file,
                        range: TextRange::new(last_orig.start, new_orig.end),
                    };
                    return;
                }
                (Origin::Generated, Origin::Generated) if last.range.end == range.start => {
                    last.range.end = range.end;
                    return;
                }
                _ => {}
            }
        }

        self.segments.push(Segment { range, origin });
    }

    /// Resolves a range in the expanded text to a multi-span of source files.
    #[must_use]
    pub fn resolve(&self, range: TextRange) -> MultiSpan<SourceId> {
        let mut primary_spans: Vec<Span<SourceId>> = Vec::new();

        for segment in &self.segments {
            let Some(subrange) = segment.range.intersect(range) else {
                continue;
            };
            let Origin::File { file, range: orig } = segment.origin else {
                continue;
            };
            let (start, end) = if subrange == segment.range {
                (orig.start, orig.end)
            } else {
                let start = (orig.start + (subrange.start - segment.range.start)).min(orig.end);
                let end = (orig.start + (subrange.end - segment.range.start))
                    .min(orig.end)
                    .max(start);
                (start, end)
            };
            let span = Span {
                file,
                text_range: TextRange::new(start, end),
            };
            if primary_spans.last() != Some(&span) {
                primary_spans.push(span);
            }
        }

        if primary_spans.is_empty()
            && let Some(span) = self.nearest_span(range.start)
        {
            primary_spans.push(span);
        }

        MultiSpan {
            primary_spans,
            span_labels: Vec::new(),
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
    let Origin::File { file, range } = segment.origin else {
        return None;
    };
    let clamped = offset.clamp(segment.range.start, segment.range.end);
    let start = range.start + (clamped - segment.range.start).min(range.length());
    let end = (start + 1).min(range.end).max(start);
    Some(Span {
        file,
        text_range: TextRange::new(start, end),
    })
}

#[cfg(test)]
mod tests {
    use std::ops::Range;

    use dt_tools_diagnostic::text_range::TextRange;

    use super::{Origin, ProvenanceMap};
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
        );
        map.push(
            TextRange::new(3, 5),
            Origin::File {
                file,
                range: TextRange::new(3, 5),
            },
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
        );

        let span = map.resolve(TextRange::new(0, 6));
        assert_eq!(span.primary_spans.len(), 1);
        assert_eq!(span.primary_spans[0].text_range, TextRange::new(0, 8));
    }

    #[test]
    fn resolves_ranges_across_segments() {
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
            );
        };
        push(&mut map, 0..3, 0..3);
        push(&mut map, 4..7, 8..11);

        let span = map.resolve(TextRange::new(0, 7));
        assert_eq!(span.primary_spans.len(), 2);
        assert_eq!(span.primary_spans[0].text_range, TextRange::new(0, 3));
        assert_eq!(span.primary_spans[1].text_range, TextRange::new(8, 11));
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
        );

        let span = map.resolve(TextRange::new(8, 10));
        assert_eq!(span.primary_spans.len(), 1);
        assert_eq!(span.primary_spans[0].text_range, TextRange::new(2, 2));
    }
}
