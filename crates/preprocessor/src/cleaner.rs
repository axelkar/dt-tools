//! Line continuation removal.

use std::borrow::Cow;

use dt_tools_diagnostic::text_range::TextRange;
use memchr::memchr2;

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
/// Borrows `src` unchanged when there is nothing to remove.
pub(crate) fn splice_lines(src: &str) -> (Cow<'_, str>, Vec<Checkpoint>) {
    profiling::scope!("preprocessor::splice_lines");

    let has_bom = src.starts_with('\u{feff}');
    let bytes = src.as_bytes();

    if !has_bom && memchr2(b'\\', b'\r', bytes).is_none() {
        return (Cow::Borrowed(src), Vec::new());
    }

    let mut out = String::with_capacity(src.len());
    let mut checkpoints = Vec::new();
    let mut pos = 0;

    // Remove a leading BOM.
    if has_bom {
        pos = '\u{feff}'.len_utf8();
        checkpoints.push(Checkpoint { from: 0, to: pos });
    }

    loop {
        let Some(offset) = memchr2(b'\\', b'\r', &bytes[pos..]) else {
            out.push_str(&src[pos..]);
            break;
        };
        let special = pos + offset;
        out.push_str(&src[pos..special]);

        if bytes[special] == b'\r' {
            if bytes.get(special + 1) == Some(&b'\n') {
                // Normalize a CRLF to a single newline.
                pos = special + 2;
                out.push('\n');
                checkpoints.push(Checkpoint {
                    from: out.len(),
                    to: pos,
                });
            } else {
                // Normalize a lone CR to a single newline.
                pos = special + 1;
                out.push('\n');
            }
            continue;
        }

        // Remove a line continuation, joining the two physical lines.
        if let Some(len) = continuation_len(&bytes[special..]) {
            pos = special + len;
            checkpoints.push(Checkpoint {
                from: out.len(),
                to: pos,
            });
            continue;
        }

        out.push('\\');
        pos = special + 1;
    }

    (Cow::Owned(out), checkpoints)
}

/// Returns the length of a line continuation at the start of `text`, if there is one.
///
/// A continuation is a backslash, then zero or more non-newline whitespace characters, then a
/// line ending.
fn continuation_len(bytes: &[u8]) -> Option<usize> {
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
