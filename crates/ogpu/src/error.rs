use std::fmt::{self};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ErrorKind {
    InvalidArgument,
    InvalidState,
    Unsupported,
    NoAdapter,
    Capacity,
    DeviceLost,
    StaleHandle,
    CrossDevice,
    WrongDomain,
    Destroyed,
    Decode,
    Io,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Error {
    pub operation: String,
    pub kind: ErrorKind,
    pub message: String,
}

pub fn make(operation: &str, kind: ErrorKind, message: &str) -> Error {
    Error {
        operation: operation.to_owned(),
        kind,
        message: message.to_owned(),
    }
}

pub fn pp(formatter: &mut dyn fmt::Write, error: &Error) {
    let _ = write!(formatter, "{}: {}", error.operation, error.message);
}

pub fn to_string(error: &Error) -> String {
    format!("{}: {}", error.operation, error.message)
}
