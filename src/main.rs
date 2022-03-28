//! A (cross-)compiler for the Mini-PL language.
#![warn(missing_docs)]

use crate::error::{Error, Result};
use crate::io::ReadIterator;
use crate::parser::build_ast;
use crate::scanner::Scanner;
use clap::Parser;
use std::fs::File;
use std::io::BufReader;
use std::path::PathBuf;

/// The error types used by the compiler.
pub mod error;
/// Helpers for reading and writing files.
pub mod io;
/// Converts the tokens into an AST.
pub mod parser;
/// Converts the input text into tokens.
pub mod scanner;

/// The command line arguments used for configuring the compiler.
#[derive(Parser)]
struct Configuration {
    /// The input file to compile.
    #[clap(index = 1)]
    input: PathBuf,
}

fn main() -> Result<()> {
    let configuration = Configuration::parse();

    let input = BufReader::new(File::open(&configuration.input).map_err(Error::ReadError)?);
    let scanner = Scanner::new(ReadIterator::new(input))?;
    let ast = build_ast(scanner)?;

    println!("{ast:#?}");

    Ok(())
}
