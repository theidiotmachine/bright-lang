use bright_lang_types::QualifiedType;
use bright_lang_types::Type;
use crate::source_location::SourceLocation;
use std::fmt::{Display, Formatter, Result};

pub mod source_location;

#[derive(Debug, Clone)]
pub enum Error {
    CastFailure(SourceLocation, QualifiedType, QualifiedType),
    CastNotNeeded(SourceLocation, Type),
    ClosureNotAllowed(SourceLocation),
    ConstFailure(SourceLocation, QualifiedType, QualifiedType),
    ExportedFunctionFatArrow(SourceLocation),
    FailedTypeArgConstraint(SourceLocation, Type, String),
    ImportFailed(SourceLocation, String),
    InvalidType(SourceLocation, String),
    MissingTypeArgs(SourceLocation),
    NoValueReturned(SourceLocation),
    NotYetImplemented(SourceLocation, String),
    TypeFailure(SourceLocation, QualifiedType, QualifiedType),
    TypeGuardReapply(SourceLocation, Type),
    UnexpectedToken(SourceLocation, String),
    UnnecessaryMut(SourceLocation, Type),
    UnrecognizedTypeArg(SourceLocation, String),
    UnrecognizedTypeArgConstraint(SourceLocation, String),
    UnsafeCodeNotAllowed(SourceLocation),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Error::CastFailure(ref loc, ref wanted, ref got) => write!(f, "ERROR {}: can't cast to type {}, from {}", loc, wanted, got),
            Error::CastNotNeeded(ref loc, ref wanted) => write!(f, "WARNING {}: no need to cast to type {}", loc, wanted),
            Error::ClosureNotAllowed(ref loc) => write!(f, "ERROR {}: closure is not allowed here", loc),
            Error::ConstFailure(ref loc, ref wanted, ref got) => write!(f, "ERROR {}: can't cast to type {}, from {}", loc, wanted, got),
            Error::ExportedFunctionFatArrow(ref loc) => write!(f, "ERROR {}: exported functions must have explicit return type", loc),
            Error::FailedTypeArgConstraint(ref loc, ref t, ref msg) => write!(f, "ERROR {}: failed type arg constraint on arg {} with type {}", loc, msg, t),
            Error::ImportFailed(ref loc, ref mes) => write!(f, "ERROR {}: import failed because {}", loc, mes),
            Error::InvalidType(ref loc, ref t) => write!(f, "ERROR {}: Invalid type {}", loc, t),
            Error::MissingTypeArgs(ref loc) => write!(f, "ERROR {}: not all type arguments supplied", loc),
            Error::NoValueReturned(ref loc) => write!(f, "ERROR {}: must return a value", loc),
            Error::NotYetImplemented(ref loc, ref msg) => write!(f, "ERROR {}: not yet implemented: {}", loc, msg),
            Error::TypeFailure(ref loc, ref wanted, ref got) => write!(f, "ERROR {}: expecting expression of type {}, found {}", loc, wanted, got),
            Error::TypeGuardReapply(ref loc, ref t) => write!(f, "WARNING {}: typeguard {} is already in place", loc, t),
            Error::UnexpectedToken(ref loc, ref msg) => write!(f, "ERROR {}: unexpected token {}", loc, msg),
            Error::UnnecessaryMut(ref loc, ref t) => write!(f, "WARNING{}: type {} can't be mut, ignoring. Did you mean to mark the variable var?", loc, t),
            Error::UnrecognizedTypeArg(ref loc, ref t) => write!(f, "ERROR {}: can't parse type arg {}", loc, t),
            Error::UnrecognizedTypeArgConstraint(ref loc, ref t) => write!(f, "ERROR {}: unrecognized type arg constraint {}", loc, t),
            Error::UnsafeCodeNotAllowed(ref loc) => write!(f, "ERROR {}: unsafe code not allowed to be called without --unsafe", loc),
        }
    }
}