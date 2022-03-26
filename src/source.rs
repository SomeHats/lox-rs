use std::ops::{Range, RangeInclusive};

#[derive(Debug, Clone, Copy)]
pub struct SourceOffset(usize);

impl SourceOffset {
    pub fn byte_offset(&self) -> usize {
        self.0
    }
}

impl From<usize> for SourceOffset {
    fn from(offset: usize) -> Self {
        Self(offset)
    }
}

impl From<&SourceOffset> for miette::SourceOffset {
    fn from(offset: &SourceOffset) -> Self {
        offset.0.into()
    }
}
impl From<SourceOffset> for miette::SourceOffset {
    fn from(offset: SourceOffset) -> Self {
        Self::from(&offset)
    }
}
impl From<&SourceOffset> for miette::SourceSpan {
    fn from(offset: &SourceOffset) -> Self {
        Self::from(&SourceSpan::from(offset))
    }
}
impl From<SourceOffset> for miette::SourceSpan {
    fn from(offset: SourceOffset) -> Self {
        Self::from(&offset)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SourceSpan {
    offset: SourceOffset,
    length: SourceOffset,
}

impl SourceSpan {
    pub fn new(offset: SourceOffset, length: SourceOffset) -> Self {
        Self { offset, length }
    }
    pub fn len(&self) -> SourceOffset {
        self.length
    }
    pub fn range(start: SourceOffset, end: SourceOffset) -> Self {
        Self {
            offset: start,
            length: (end.0 - start.0).into(),
        }
    }
    pub fn range_inclusive(start: SourceOffset, end: SourceOffset) -> Self {
        Self {
            offset: start,
            length: (end.0 - start.0 + 1).into(),
        }
    }
    pub fn start(&self) -> SourceOffset {
        self.offset
    }
    pub fn end(&self) -> SourceOffset {
        (self.offset.0 + self.length.0).into()
    }
}

impl From<&SourceOffset> for SourceSpan {
    fn from(offset: &SourceOffset) -> Self {
        Self::new(*offset, 1.into())
    }
}
impl From<SourceOffset> for SourceSpan {
    fn from(offset: SourceOffset) -> Self {
        Self::from(&offset)
    }
}

impl From<&SourceSpan> for miette::SourceSpan {
    fn from(span: &SourceSpan) -> Self {
        Self::new((&span.offset).into(), (&span.length).into())
    }
}
impl From<SourceSpan> for miette::SourceSpan {
    fn from(span: SourceSpan) -> Self {
        Self::from(&span)
    }
}

impl From<Range<usize>> for SourceSpan {
    fn from(range: Range<usize>) -> Self {
        Self::range(range.start.into(), range.end.into())
    }
}
impl From<RangeInclusive<usize>> for SourceSpan {
    fn from(range: RangeInclusive<usize>) -> Self {
        Self::range_inclusive((*range.start()).into(), (*range.end()).into())
    }
}

impl From<usize> for SourceSpan {
    fn from(offset: usize) -> Self {
        Self::new(offset.into(), 1.into())
    }
}
