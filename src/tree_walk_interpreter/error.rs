use super::{ValueDescriptor, ValueType};
use crate::{SourceReference, SourceSpan};
use miette::Diagnostic;
use thiserror::Error;

#[derive(Error, Diagnostic, Debug)]
pub enum RuntimeError {
    #[error("Operand must be {}, but found {}", .expected_type.fmt_a(), .actual_type.fmt_a())]
    OperandTypeError {
        expected_type: ValueDescriptor,
        actual_type: ValueType,
        #[label("{} was found here", .actual_type.fmt_a())]
        operand_loc: SourceSpan,
        operator: String,
        #[label("the '{operator}' operator expected {}", .expected_type.fmt_a())]
        operator_loc: SourceSpan,
        #[source_code]
        source_code: SourceReference,
    },
    #[error("Undefined variable {name}")]
    UndefinedVariable {
        name: String,
        #[label("found here")]
        found_at: SourceSpan,
        #[source_code]
        source_code: SourceReference,
    },
    #[error("Already a variable named {name} in this scope")]
    AlreadyDefinedVariable {
        name: String,
        #[label("'{name}' here is already a variable")]
        found_at: SourceSpan,
        #[source_code]
        source_code: SourceReference,
    },
    #[error("Expected {expected_arity} arguments but got {actual_arity}")]
    UnexpectedCallArity {
        expected_arity: usize,
        actual_arity: usize,
        #[label("On this function call")]
        found_at: SourceSpan,
        #[source_code]
        source_code: SourceReference,
    },
    #[error("Can only call functions and classes")]
    UncallableValue {
        actual_type: ValueType,
        #[label("Attempted to call {} here", .actual_type.fmt_a())]
        found_at: SourceSpan,
        #[source_code]
        source_code: SourceReference,
    },
    #[error("Only objects have properties")]
    PropertyAccessOnNonObject {
        actual_type: ValueType,
        property_name: String,
        #[label("Attempted to access {property_name} on {} here", .actual_type.fmt_a())]
        found_at: SourceSpan,
        #[source_code]
        source_code: SourceReference,
    },
    #[error("Unknown property {name}")]
    UnknownProperty {
        name: String,
        #[label("This property is unknown")]
        found_at: SourceSpan,
        #[source_code]
        source_code: SourceReference,
    },
    #[error("Class {class_name} cannot extend non-class {super_class_name}")]
    NonClassExtend {
        class_name: String,
        super_class_name: String,
        actual_type: ValueType,
        #[label("{} is {}, not a class", .super_class_name, .actual_type.fmt_a())]
        found_at: SourceSpan,
        #[source_code]
        source_code: SourceReference,
    },
}
