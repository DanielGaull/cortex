use thiserror::Error;

#[derive(Error, Debug)]
pub enum RuntimeError {
    #[error("Expected arg {0} to be of type {1}")]
    InvalidArg(&'static str, &'static str),
    #[error("Invalid index {0} for list of length {1}")]
    InvalidIndex(usize, usize),
    #[error("Expected arg {0} to be a non-negative integer")]
    ExpectedInteger(f64),
}

impl PartialEq for RuntimeError {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::InvalidArg(l0, l1), Self::InvalidArg(r0, r1)) => *l0 == *r0 && *l1 == *r1,
            (Self::InvalidIndex(l0, l1), Self::InvalidIndex(r0, r1)) => *l0 == *r0 &&  *l1 == *r1,
            (Self::ExpectedInteger(l0), Self::ExpectedInteger(r0)) => (*l0 - *r0).abs() < f64::EPSILON,
            _ => false,
        }
    }
}
