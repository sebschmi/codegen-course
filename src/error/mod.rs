use crate::scanner::{ScanInterval, Token};

/// A shorthand to a `Result` type with the error type of the compiler.
pub type Result<T> = std::result::Result<T, Error>;

/// The set of all errors that can occur in the scanner.
#[derive(Debug)]
pub struct ScannerError {
    /// The error kind.
    kind: ScannerErrorKind,

    /// The coordinates of this error in the source code.
    interval: ScanInterval,
}

/// The set of all error kinds that can occur in the scanner.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum ScannerErrorKind {
    /// The end of input was reached while being inside a comment environment.
    UnclosedComment,

    /// A real literal was discovered that does not match \[0-9\]+.\[0-9\]+(e\[+-\]?\[0-9\]+)?.
    MalformedRealLiteral,

    /// A token was expected, but the next character found is not the start of any token.
    NotTheStartOfAToken,

    /// A string literal was never closed.
    UnclosedStringLiteral,
}

/// A convenience function for creating scanner errors.
pub fn scanner_error(interval: ScanInterval, kind: ScannerErrorKind) -> Error {
    ScannerError { kind, interval }.into()
}

/// The set of all errors that can occur in the parser.
#[derive(Debug)]
pub struct ParserError {
    /// The error kind.
    kind: ParserErrorKind,

    /// The coordinates of this error in the source code.
    interval: ScanInterval,
}

/// The set of all error kinds that can occur in the parser.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum ParserErrorKind {
    /// A token was found that was not expected at this point.
    UnexpectedToken { expected: Vec<Token>, found: Token },

    /// Expected an identifier, but found something else.
    ExpectedIdentifier { found: Token },

    /// Expected a type identifier, but found a different identifier.
    ExpectedTypeName { found: Token },
}

/// A convenience function for creating parser errors.
pub fn parser_error(interval: ScanInterval, kind: ParserErrorKind) -> Error {
    ParserError { kind, interval }.into()
}

/// The set of all errors that can occur in the compiler.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum Error {
    ScannerError(ScannerError),
    ParserError(ParserError),

    /// An error when reading the input file.
    ReadError(std::io::Error),

    /// An invalid utf8 character was found in the input.
    InvalidUtf8Character {
        invalid: u32,
    },

    /// More input was expected, but none was found.
    UnexpectedEndOfInput,
}

impl From<ScannerError> for Error {
    fn from(error: ScannerError) -> Self {
        Self::ScannerError(error)
    }
}

impl From<ParserError> for Error {
    fn from(error: ParserError) -> Self {
        Self::ParserError(error)
    }
}
