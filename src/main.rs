//! A (cross-)compiler for the Mini-PL language.
#![warn(missing_docs)]

/// The error types used by the compiler.
pub mod error;
/// Converts the tokens into an AST.
pub mod parser;
/// Converts the input text into tokens.
pub mod scanner;

fn main() {
    println!("Hello, world!");
}
