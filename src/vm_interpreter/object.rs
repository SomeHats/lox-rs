use super::{
    gc::{Gc, Trace},
    string_interner::InternedString,
    Chunk,
};
use crate::custom_trace;
use std::fmt::{Debug, Display};

#[derive(Debug, Clone)]
pub enum Object {
    String(InternedString),
}
impl Trace for Object {
    custom_trace!(|obj| {
        match obj {
            Object::String(value) => mark(value),
        }
    });
}

pub struct FunctionObj(Gc<FunctionObjImpl>);
#[derive(Debug)]
struct FunctionObjImpl {
    arity: u8,
    name: InternedString,
    chunk: Chunk,
}
impl Display for FunctionObj {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fun {}() {{ ... }}", self.0.name)
    }
}
impl Trace for FunctionObjImpl {
    custom_trace!(|this| {
        mark(&this.name);
    });
}
