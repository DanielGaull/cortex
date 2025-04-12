use thiserror::Error;

#[derive(Error, Debug, PartialEq)]
pub enum PreprocessingError {
    #[error("Could not infer type bindings for {0} (consider manually providing bindings)")]
    CouldNotInferTypeBinding(String),
    #[error("Cannot have type arguments on a generic type: {0}")]
    CannotHaveTypeArgsOnGeneric(String),
    #[error("Expected type {0} for {2} but expression of type {1} was found (context: {3})")]
    MismatchedType(String, String, String, String),
    #[error("Could not determine type for list literal: expected {0} but found {1}")]
    CannotDetermineListLiteralType(String, String),
    #[error("Value not found: {0} (module constants are currently not supported)")]
    ValueNotFound(String),
    #[error("Invalid unary operator values: only the type(s) {0} are allowed")]
    InvalidOperatorUnary(&'static str),
    #[error("Invalid binary operator values: only the type(s) {0} and {1} are allowed")]
    InvalidOperator(&'static str, &'static str),
    #[error("Cannot assign multiple times to struct field {0} in construction")]
    MultipleFieldAssignment(String),
    #[error("Fields not assigned on struct {0}: {1}")]
    NotAllFieldsAssigned(String, String),
    #[error("If arm types do not match: expected {0} but found {1}")]
    IfArmsDoNotMatch(String, String),
    #[error("If an if arm returns a value, then there must be an else block")]
    IfRequiresElseBlock,
    #[error("Loop body cannot have a return value")]
    LoopCannotHaveReturnValue,
    #[error("Type {0} requires {1} type arguments but only {2} was/were provided")]
    MismatchedTypeArgCount(String, usize, usize),
    #[error("Invalid type: {0} is not valid in this context")]
    TypeInvalidInThisContext(String),
    #[error("Cannot modify value \"{0}\" if it comes from a module")]
    CannotModifyModuleEnvironment(String),
    #[error("Mismatched argument count: Function {0} expects {1} arguments but received {2}")]
    MismatchedArgumentCount(String, usize, usize),
    #[error("Parent environment not found")]
    NoParentEnv,
    #[error("Cannot modify constant variable {0}")]
    CannotModifyConst(String),
    #[error("Function declared to return {0} but actually returns {1}")]
    ReturnTypeMismatch(String, String),
    #[error("Field \"{0}\" does not exist on type {1}")]
    FieldDoesNotExist(String, String),
    #[error("You cannot access fields on a non-composite value")]
    CannotAccessMemberOfNonComposite,
    #[error("Cannot modify field on immutable reference for type {0}")]
    CannotModifyFieldOnImmutableReference(String),
    #[error("Unknown type found!")]
    UnknownTypeFound,
    #[error("`break` used in a non-loop context! Can only use within loops!")]
    BreakUsedInNonLoopContext,
    #[error("`continue` used in a non-loop context! Can only use within loops!")]
    ContinueUsedInNonLoopContext,
    #[error("Tuple elements must be accessed in this form: .t{{index}}. {0} is an invalid tuple member access")]
    TupleMemberSyntaxInvalid(String),
    #[error("Tuple has size of {0} but index {1} was attempted to index into it!")]
    TupleIndexValueInvalid(usize, usize),
    #[error("Ambiguous function call: Multiple extension functions named {0} defined on type {1}")]
    AmbiguousExtensionCall(String, String),
    #[error("Cannot access members on optional value: {0} (consider using `!` to assert it is not `none`")]
    CannotAccessMemberOfOptional(String),
}
