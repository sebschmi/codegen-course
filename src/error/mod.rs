/// A shorthand to a `Result` type with the error type of the compiler.
pub type Result<T> = std::result::Result<T, Error>;

/// The set of all errors that can occur in the compiler.
pub enum Error {
    /// The end of input was reached while being inside a comment environment.
    UnclosedComment,

    /// A real literal was discovered that does not match \[0-9\]+.\[0-9\]+(e\[+-\]?\[0-9\]+)?.
    MalformedRealLiteral,

    /// A token was expected, but the next character found is not the start of any token.
    NotTheStartOfAToken,

    /// A string literal was never closed.
    UnclosedStringLiteral,
}
