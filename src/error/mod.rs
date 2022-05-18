use crate::parser::{AstNode, AstNodeKind};
use crate::scanner::{ScanInterval, Token};
use crate::symbol_table::SymbolType;

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

    /// Expected an identifier or predefined identifier, but found something else.
    ExpectedIdentifier { found: Token },

    /// Expected the predefined identifier `size`, but found something else.
    ExpectedSize { found: AstNode },

    /// Expected a type identifier, but found a different identifier.
    ExpectedTypeName { found: Token },

    /// Expected a factor, i.e. an identifier, predefined identifier, literal, parenthesis expression or not operator.
    ExpectedFactor { found: Token },

    /// Found a block that contains no statements, which is forbidden.
    EmptyBlock,

    /// Found a trailing comma in a parameter or argument list, which is forbidden.
    TrailingComma,
}

/// A convenience function for creating parser errors.
pub fn parser_error(interval: ScanInterval, kind: ParserErrorKind) -> Error {
    ParserError { kind, interval }.into()
}

/// The set of all errors that can occur during static analysis.
#[derive(Debug)]
pub struct StaticError {
    /// The error kind.
    kind: StaticErrorKind,

    /// The coordinates of this error in the source code.
    interval: ScanInterval,
}

/// The set of all error kinds that can occur during static analysis.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum StaticErrorKind {
    /// A symbol was used that was not declared before.
    UndeclaredSymbol {
        lower_case: String,
        original: String,
    },

    /// A type was found that was not expected.
    TypeMismatch {
        expected: SymbolType,
        actual: SymbolType,
    },

    /// The empty type was found, but a different type was expected.
    UnexpectedEmptyType,

    /// Expected a function type, but found something else.
    ExpectedFunction { actual: SymbolType },

    /// Expected a variable type, but found something else.
    ExpectedVariable { actual: SymbolType },

    /// Expected a primitive variable type, but found something else.
    ExpectedPrimitiveType { actual: SymbolType },

    /// Expected a primitive numeric variable type, but found something else.
    ExpectedNumericType { actual: SymbolType },

    /// Expected a primitive numeric or string variable type, but found something else.
    ExpectedNumericOrStringType { actual: SymbolType },

    /// Expected an array type, but found something else.
    ExpectedArray { actual: SymbolType },

    /// Expected an integer type, but found something else.
    ExpectedInteger { actual: SymbolType },

    /// A function or procedure is called with a wrong number of arguments.
    WrongArgumentCount { expected: usize, actual: usize },

    /// Expected an identifier or predefined identifier, but found something else.
    ExpectedIdentifier { actual: AstNodeKind },
}

/// A convenience function for creating static errors.
pub fn static_error(interval: ScanInterval, kind: StaticErrorKind) -> Error {
    StaticError { kind, interval }.into()
}

/// The set of all errors that can occur in the compiler.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum Error {
    ScannerError(ScannerError),
    ParserError(ParserError),
    StaticError(StaticError),

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

impl From<StaticError> for Error {
    fn from(error: StaticError) -> Self {
        Self::StaticError(error)
    }
}
