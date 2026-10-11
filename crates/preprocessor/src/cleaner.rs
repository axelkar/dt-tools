//! Line continuation removal.

use dt_tools_diagnostic::text_range::TextRange;

/// A mapping between a byte offset in one text and the corresponding byte offset in another.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Checkpoint {
    /// The byte offset in the mapped-from text.
    pub(crate) from: usize,
    /// The byte offset in the mapped-to text.
    pub(crate) to: usize,
}
impl Checkpoint {
    /// Maps an offset in the mapped-from text back to the mapped-to text.
    ///
    /// [`Checkpoint`]s are sorted, so the shift comes from the last checkpoint at or before `from`.
    /// Before the first checkpoint nothing has been changed yet, and the offset is unchanged.
    pub(crate) fn map_offset(checkpoints: &[Checkpoint], from: usize) -> usize {
        debug_assert!(checkpoints.is_sorted_by_key(|checkpoint| checkpoint.from));

        let idx = checkpoints.partition_point(|checkpoint| checkpoint.from <= from);
        if idx == 0 {
            return from;
        }
        let checkpoint = checkpoints[idx - 1];
        from + checkpoint.to - checkpoint.from
    }

    /// Maps the end of a range in the mapped-from text to the mapped-to text.
    ///
    /// A checkpoint at `offset` is skipped because the range ends before the text changed there.
    /// This keeps removed text out of ranges that end right before it.
    fn map_offset_end(checkpoints: &[Checkpoint], offset: usize) -> usize {
        let idx = checkpoints.partition_point(|checkpoint| checkpoint.from < offset);
        if idx == 0 {
            return offset;
        }
        let checkpoint = checkpoints[idx - 1];
        offset + checkpoint.to - checkpoint.from
    }

    /// Maps a range in the mapped-from text back to the mapped-to text.
    pub(crate) fn map_range(checkpoints: &[Checkpoint], range: TextRange) -> TextRange {
        TextRange::new(
            Self::map_offset(checkpoints, range.start),
            Self::map_offset_end(checkpoints, range.end),
        )
    }
}

/// Removes the BOM and line continuations, and normalizes line endings to `\n`.
///
/// Corresponds to the [C translation phases](https://en.cppreference.com/c/language/translation_phases) 1 and 2.
pub(crate) fn splice_lines(src: &str) -> (String, Vec<Checkpoint>) {
    let mut out = String::with_capacity(src.len());
    let mut checkpoints = Vec::new();
    let mut pos = 0;

    // Remove a leading BOM.
    if src.starts_with('\u{feff}') {
        pos = '\u{feff}'.len_utf8();
        checkpoints.push(Checkpoint { from: 0, to: pos });
    }

    while pos < src.len() {
        // Remove a line continuation, joining the two physical lines.
        if let Some(len) = continuation_len(&src[pos..]) {
            pos += len;
            checkpoints.push(Checkpoint {
                from: out.len(),
                to: pos,
            });
            continue;
        }

        // Check the current character.
        let ch = src[pos..].chars().next().expect("position is in bounds");
        if ch == '\r' {
            if src.as_bytes().get(pos + 1) == Some(&b'\n') {
                // Normalize a CRLF to a single newline.
                pos += 2;
                out.push('\n');
                checkpoints.push(Checkpoint {
                    from: out.len(),
                    to: pos,
                });
            } else {
                // Normalize a lone CR to a single newline.
                pos += 1;
                out.push('\n');
            }
        } else {
            out.push(ch);
            pos += ch.len_utf8();
        }
    }

    (out, checkpoints)
}

/// Returns the length of a line continuation at the start of `text`, if there is one.
///
/// A continuation is a backslash, then zero or more non-newline whitespace characters, then a
/// line ending.
fn continuation_len(text: &str) -> Option<usize> {
    let bytes = text.as_bytes();
    if bytes.first() != Some(&b'\\') {
        return None;
    }

    // Whitespace other than a newline may sit between the backslash and the line ending.
    let mut pos = 1;
    while matches!(bytes.get(pos), Some(b' ' | b'\t' | 0x0b | 0x0c)) {
        pos += 1;
    }

    match bytes.get(pos) {
        Some(b'\r') if bytes.get(pos + 1) == Some(&b'\n') => Some(pos + 2),
        Some(b'\n' | b'\r') => Some(pos + 1),
        _ => None,
    }
}
