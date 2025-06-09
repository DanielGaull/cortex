use std::error::Error;

use thiserror::Error;

use super::value::CortexValue;

pub type CortexError = Box<dyn Error>;

#[derive(Error, Debug, PartialEq)]
pub enum InterpreterError {
    #[error("Program threw an error: {0}")]
    ProgramThrow(CortexValue),
    #[error("Parent environment does not exist")]
    NoParentEnv,
    #[error("Expected an integer value in this context; {0} is not an integer")]
    ExpectedInteger(f64),
    #[error("Bang operator called on a none value")]
    BangCalledOnNoneValue,
    #[error("Value not found: {0} (module constants are currently not supported)")]
    ValueNotFound(String),
    #[error("Mismatched type found: please run preprocessing on code before executing it")]
    MismatchedTypeNoPreprocess,
    #[error("Invalid {0} found: please run preprocessing on code before executing it")]
    InvalidObject(&'static str),
    #[error("Expected a fat pointer value but received {0}")]
    ExpectedFatPointer(String),
    #[error("Function not found (dynamic dispatch)")]
    FunctionNotFoundDynamicDispatch,
}
