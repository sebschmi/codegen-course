//! A (cross-)compiler for the Mini-PL language.
#![warn(missing_docs)]
// This makes the code less uniform, as the first push to a vector is then written differently than all others
#![allow(clippy::vec_init_then_push)]

use crate::codegen::generate_code;
use crate::error::{Error, Result};
use crate::io::ReadIterator;
use crate::parser::build_ast;
use crate::scanner::Scanner;
use crate::symbol_table::build_symbol_table;
use crate::type_checker::type_check;
use clap::Parser;
use log::{info, LevelFilter};
use simplelog::{ColorChoice, CombinedLogger, Config, TermLogger, TerminalMode};
use std::fs::File;
use std::io::{BufReader, Read, Write};
use std::path::PathBuf;
use std::sync::atomic::{AtomicU8, Ordering};

/// Code generation.
pub mod codegen;
/// The error types used by the compiler.
pub mod error;
/// Helpers for reading and writing files.
pub mod io;
/// Converts the tokens into an AST.
pub mod parser;
/// Converts the input text into tokens.
pub mod scanner;
/// The symbol table extracted from the AST.
pub mod symbol_table;
#[cfg(test)]
mod tests;
/// Type checks the AST.
pub mod type_checker;

/// The command line arguments used for configuring the compiler.
#[derive(Parser)]
struct Configuration {
    /// The input file to compile.
    #[clap(index = 1)]
    input: PathBuf,

    /// The output file for the intermediate representation (restricted C).
    #[clap(index = 2)]
    output: PathBuf,
}

// Make sure that logging gets initialised only once in tests
static LOGGING_INITIALISED: AtomicU8 = AtomicU8::new(0);

fn initialise_logging() {
    if LOGGING_INITIALISED
        .compare_exchange(0, 1, Ordering::SeqCst, Ordering::SeqCst)
        .is_ok()
    {
        let log_level = if cfg!(debug_assertions) {
            LevelFilter::Trace
        } else {
            LevelFilter::Info
        };
        CombinedLogger::init(vec![TermLogger::new(
            log_level,
            Config::default(),
            TerminalMode::Mixed,
            ColorChoice::Auto,
        )])
        .unwrap();

        info!("Logging initialised successfully to {log_level}");
        LOGGING_INITIALISED
            .compare_exchange(1, 2, Ordering::SeqCst, Ordering::SeqCst)
            .unwrap();
    }

    while LOGGING_INITIALISED.load(Ordering::SeqCst) != 2 {
        // busy wait for logging to be initialised
    }
}

/// Compile the given input.
pub fn compile(input: impl Read, output: impl Write) -> Result<()> {
    let input = BufReader::new(input);
    let scanner = Scanner::new(ReadIterator::new(input))?;
    let mut ast = build_ast(scanner)?;
    let symbol_table = build_symbol_table(&mut ast)?;
    type_check(&ast, &symbol_table)?;
    generate_code(&ast, &symbol_table, output)?;

    Ok(())
}

fn main() -> Result<()> {
    initialise_logging();
    let configuration = Configuration::parse();

    compile(
        File::open(&configuration.input).map_err(Error::ReadError)?,
        File::create(&configuration.output).map_err(Error::WriteError)?,
    )?;

    Ok(())
}
