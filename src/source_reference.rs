use std::sync::Arc;

use miette::{NamedSource, SourceCode};

#[derive(Debug)]
struct SourceReferenceInner {
    named_source: NamedSource,
}

#[derive(Clone, Debug)]
pub struct SourceReference(Arc<SourceReferenceInner>);

impl SourceReference {
    pub fn new(name: String, source: String) -> Self {
        SourceReference(Arc::new(SourceReferenceInner {
            named_source: NamedSource::new(name, source),
        }))
    }
    fn inner(&self) -> &SourceReferenceInner {
        self.0.as_ref()
    }
}

impl SourceCode for SourceReference {
    fn read_span<'a>(
        &'a self,
        span: &miette::SourceSpan,
        context_lines_before: usize,
        context_lines_after: usize,
    ) -> Result<Box<dyn miette::SpanContents<'a> + 'a>, miette::MietteError> {
        self.inner()
            .named_source
            .read_span(span, context_lines_before, context_lines_after)
    }
}
