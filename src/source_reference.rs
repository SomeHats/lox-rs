use std::sync::Arc;

use miette::{MietteSpanContents, SourceCode};

#[derive(Debug)]
struct SourceReferenceInner {
    name: String,
    source: String,
}

#[derive(Clone, Debug)]
pub struct SourceReference(Arc<SourceReferenceInner>);

impl SourceReference {
    pub fn new(name: String, source: String) -> Self {
        SourceReference(Arc::new(SourceReferenceInner {
            // named_source: NamedSource::new(name, source),
            name,
            source,
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
        let contents =
            self.inner()
                .source
                .read_span(span, context_lines_before, context_lines_after)?;
        Ok(Box::new(MietteSpanContents::new_named(
            self.inner().name.clone(),
            contents.data(),
            contents.span().clone(),
            contents.line(),
            contents.column(),
            contents.line_count(),
        )))
    }
}
