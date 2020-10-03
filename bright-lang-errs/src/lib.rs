use bright_lang_types::Type;
use crate::source_location::SourceLocation;
use std::fmt::{Display, Formatter, Result};

pub mod source_location;

#[derive(Debug, Clone)]
pub enum Error {
    ClosureNotAllowed(SourceLocation),
    ImportFailed(SourceLocation, String),
    NotYetImplemented(SourceLocation, String),
    TypeGuardReapply(SourceLocation, Type),
    UnexpectedToken(SourceLocation, String),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Error::ClosureNotAllowed(ref loc) => write!(f, "ERROR {}: closure is not allowed here", loc),
            Error::ImportFailed(ref loc, ref mes) => write!(f, "ERROR {}: import failed because {}", loc, mes),
            Error::NotYetImplemented(ref loc, ref msg) => write!(f, "ERROR {}: not yet implemented: {}", loc, msg),
            Error::TypeGuardReapply(ref loc, ref t) => write!(f, "WARNING {}: typeguard {} is already in place", loc, t),
            Error::UnexpectedToken(ref loc, ref msg) => write!(f, "ERROR {}: unexpected token {}", loc, msg),
        }
    }
}