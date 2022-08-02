use super::{
    gc::{Gc, GcString, Trace},
    Chunk,
};
use crate::{custom_trace_impl, SourceReference};

#[derive(Debug)]
enum FunctionKind {
    Script,
    Function { arity: usize, name: GcString },
}
impl Trace for FunctionKind {
    custom_trace_impl!(|this| match this {
        FunctionKind::Script => {}
        FunctionKind::Function { arity, name } => {
            mark(name);
            mark(arity);
        }
    });
}
impl FunctionKind {
    fn name(&self) -> &str {
        match self {
            FunctionKind::Script => "<script>",
            FunctionKind::Function { arity: _, name } => name,
        }
    }
    fn arity(&self) -> usize {
        match self {
            FunctionKind::Script => 0,
            FunctionKind::Function { arity, name: _ } => *arity,
        }
    }
}

pub struct FunctionBuider {
    chunk: Chunk,
    kind: FunctionKind,
}
impl FunctionBuider {
    pub fn new_script(source: SourceReference) -> Self {
        Self {
            chunk: Chunk::new(source),
            kind: FunctionKind::Script,
        }
    }
    pub fn new_function(name: GcString, arity: usize, source: SourceReference) -> Self {
        Self {
            chunk: Chunk::new(source),
            kind: FunctionKind::Function { arity, name },
        }
    }
    pub fn chunk_mut(&mut self) -> &mut Chunk {
        &mut self.chunk
    }
    pub fn build(self) -> FunctionObj {
        FunctionObj(Gc::new(FunctionObjImpl {
            chunk: self.chunk,
            kind: self.kind,
        }))
    }
}

#[derive(Debug)]
struct FunctionObjImpl {
    chunk: Chunk,
    kind: FunctionKind,
}
impl Trace for FunctionObjImpl {
    custom_trace_impl!(|fun| {
        mark(&fun.kind);
        mark(&fun.chunk);
    });
}

#[derive(Debug, Clone)]
pub struct FunctionObj(Gc<FunctionObjImpl>);
impl FunctionObj {
    pub fn name(&self) -> &str {
        &self.0.kind.name()
    }
    pub fn chunk(&self) -> &Chunk {
        &self.0.chunk
    }
}
impl Trace for FunctionObj {
    custom_trace_impl!(|fun| { mark(&fun.0) });
}
impl PartialEq for FunctionObj {
    fn eq(&self, other: &Self) -> bool {
        self.0.ref_eq(&other.0)
    }
}
