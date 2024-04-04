use std::{
    fmt::{Debug, Display},
    iter::{Cloned, Enumerate},
    slice::Iter,
};
use winnow::{
    stream::{Checkpoint, Compare, CompareResult, FindSlice, Offset, Stream},
    Located,
};

type Inner<'i> = Located<&'i [u8]>;
/// Make Located print the inner str on Debug
#[derive(Clone, PartialEq, Eq)]
pub struct Printer<'i>(pub Inner<'i>);

impl Debug for Printer<'_> {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !f.alternate() {
            write!(f, "\"")?;
        }
        for byte in *self.0 {
            let c = *byte as char;
            write!(f, "{}", c.escape_debug())?;
        }
        if !f.alternate() {
            write!(f, "\"")?;
        }
        Ok(())
    }
}
impl Display for Printer<'_> {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&std::string::String::from_utf8_lossy(*self.0), f)
    }
}

impl<'i> Offset<<Inner<'i> as Stream>::Checkpoint> for Printer<'i> {
    #[inline(always)]
    fn offset_from(&self, other: &Checkpoint<Checkpoint<&'i [u8], &'i [u8]>, Inner<'i>>) -> usize {
        self.checkpoint().offset_from(other)
    }
}

impl winnow::stream::Location for Printer<'_> {
    fn location(&self) -> usize {
        self.0.location()
    }
}

impl<'i> winnow::stream::StreamIsPartial for Printer<'i> {
    type PartialState = ();
    fn complete(&mut self) -> Self::PartialState {}
    fn restore_partial(&mut self, _state: Self::PartialState) {}

    #[inline(always)]
    fn is_partial_supported() -> bool {
        false
    }
}

impl<'i> winnow::stream::Stream for Printer<'i> {
    type Token = u8;
    type Slice = &'i [u8];
    type IterOffsets = Enumerate<Cloned<Iter<'i, u8>>>;
    type Checkpoint = <Inner<'i> as winnow::stream::Stream>::Checkpoint;
    #[inline(always)]
    fn iter_offsets(&self) -> Self::IterOffsets {
        self.0.iter_offsets()
    }
    #[inline(always)]
    fn eof_offset(&self) -> usize {
        self.0.eof_offset()
    }

    #[inline(always)]
    fn next_token(&mut self) -> Option<Self::Token> {
        self.0.next_token()
    }

    #[inline(always)]
    fn offset_for<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Token) -> bool,
    {
        self.0.offset_for(predicate)
    }
    #[inline(always)]
    fn offset_at(&self, tokens: usize) -> Result<usize, winnow::error::Needed> {
        self.0.offset_at(tokens)
    }
    #[inline(always)]
    fn next_slice(&mut self, offset: usize) -> Self::Slice {
        self.0.next_slice(offset)
    }

    #[inline(always)]
    fn checkpoint(&self) -> Self::Checkpoint {
        self.0.checkpoint()
    }
    #[inline(always)]
    fn reset(&mut self, checkpoint: &Self::Checkpoint) {
        self.0.reset(checkpoint);
    }

    #[inline(always)]
    fn raw(&self) -> &dyn std::fmt::Debug {
        &self.0
    }
}

impl<'i> AsRef<Inner<'i>> for Printer<'i> {
    #[inline(always)]
    fn as_ref(&self) -> &Inner<'i> {
        &self.0
    }
}

impl<'i> std::ops::Deref for Printer<'i> {
    type Target = Inner<'i>;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'i, U> Compare<U> for Printer<'i>
where
    for<'a> &'a [u8]: Compare<U>,
{
    #[inline(always)]
    fn compare(&self, other: U) -> CompareResult {
        self.0.compare(other)
    }
}

impl<'i, S> FindSlice<S> for Printer<'i>
where
    &'i [u8]: FindSlice<S>,
{
    #[inline(always)]
    fn find_slice(&self, substr: S) -> Option<std::ops::Range<usize>> {
        self.0.find_slice(substr)
    }
}
