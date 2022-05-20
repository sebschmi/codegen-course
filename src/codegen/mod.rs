use crate::error::Result;
use crate::parser::{AstNode, AstNodeKind, PrimitiveTypeName};
use crate::symbol_table::{SymbolTable, SymbolType};
use std::fmt::Write;

/// The "runtime" of our language, which includes the read and write predefined procedures.
const RUNTIME: &str = r#"
#include <stdio.h>
#include <stdlib.h>

void read_integer(int* output) {
    fscanf(stdin, "%d", output);
}

void read_float(float* output) {
    fscanf(stdin, "%f", output);
}

void read_boolean(char* output) {
    fscanf(stdin, "%d", output);
}

void read_string(char** output) {
    *output = malloc(1024);
    fscanf(stdin, "%s", *output);
}

void write_integer(int output) {
    fprintf(stdout, "%d\n", output);
}

void write_float(float output) {
    fprintf(stdout, "%f\n", output);
}

void write_boolean(char output) {
    fprintf(stdout, "%d\n", output);
}

void write_string(char* output) {
    fprintf(stdout, "%s\n", output);
}
"#;

struct FmtIoWriteWrapper<T>(T);

impl<T: std::io::Write> std::io::Write for FmtIoWriteWrapper<T> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.0.flush()
    }
}

impl<T: std::io::Write> std::fmt::Write for FmtIoWriteWrapper<T> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.0.write_all(s.as_bytes()).map_err(|_| std::fmt::Error)
    }
}

/// Generate code in the "restricted C" intermediate representation.
pub fn generate_code(
    ast: &AstNode,
    symbol_table: &SymbolTable,
    output: impl std::io::Write,
) -> Result<()> {
    let mut output = FmtIoWriteWrapper(output);
    output.write_str(RUNTIME)?;

    // write forward declarations
    for child in ast.children().iter().skip(1).rev().skip(1).rev() {
        if child.kind() == &AstNodeKind::Function {
            generate_code_recursively(
                child.children().iter().rev().nth(1).unwrap(),
                symbol_table,
                &mut output,
            )?;
        } else {
            write!(output, "void ")?;
        }
        generate_code_recursively(&child.children()[0], symbol_table, &mut output)?;
        write!(output, "(")?;
        for (index, child) in child
            .children()
            .iter()
            .skip(1)
            .rev()
            .skip(if child.kind() == &AstNodeKind::Function {
                2
            } else {
                1
            })
            .rev()
            .enumerate()
        {
            if index > 0 {
                write!(output, ", ")?;
            }
            generate_code_recursively(child, symbol_table, &mut output)?;
        }
        writeln!(output, ");")?;
    }

    generate_code_recursively(ast, symbol_table, &mut output)
}

fn generate_code_recursively<Writer: std::io::Write>(
    ast: &AstNode,
    symbol_table: &SymbolTable,
    output: &mut FmtIoWriteWrapper<Writer>,
) -> Result<()> {
    match ast.kind() {
        AstNodeKind::Program => {
            for child in ast.children().iter().skip(1).rev().skip(1).rev() {
                generate_code_recursively(child, symbol_table, output)?;
            }
            // the main function is just a block in the AST, so we generate the function header manually
            write!(output, "void main()")?;
            generate_code_recursively(ast.children().last().unwrap(), symbol_table, output)?;
        }
        AstNodeKind::Procedure | AstNodeKind::Function => {
            if ast.kind() == &AstNodeKind::Function {
                generate_code_recursively(
                    ast.children().iter().rev().nth(1).unwrap(),
                    symbol_table,
                    output,
                )?;
            } else {
                write!(output, "void ")?;
            }
            generate_code_recursively(&ast.children()[0], symbol_table, output)?;
            write!(output, "(")?;
            for (index, child) in ast
                .children()
                .iter()
                .skip(1)
                .rev()
                .skip(if ast.kind() == &AstNodeKind::Function {
                    2
                } else {
                    1
                })
                .rev()
                .enumerate()
            {
                if index > 0 {
                    write!(output, ", ")?;
                }
                generate_code_recursively(child, symbol_table, output)?;
            }
            writeln!(output, ")")?;
            generate_code_recursively(ast.children().last().unwrap(), symbol_table, output)?;
        }
        AstNodeKind::VarParameter | AstNodeKind::ValueParameter => {
            // write the type
            generate_code_recursively(&ast.children()[1], symbol_table, output)?;
            // add an additional layer of indirection for var parameters
            if ast.kind() == &AstNodeKind::VarParameter {
                write!(output, "*")?;
            }
            write!(output, " ")?;
            // write the identifier
            generate_code_recursively(&ast.children()[0], symbol_table, output)?;
        }
        AstNodeKind::Block => {
            writeln!(output, "{{")?;
            for child in ast.children() {
                generate_code_recursively(child, symbol_table, output)?;
                writeln!(output, ";")?;
            }
            writeln!(output, "}}")?;
        }
        AstNodeKind::VariableDeclaration => {
            for child in ast.children().iter().rev().skip(1).rev() {
                generate_code_recursively(ast.children().last().unwrap(), symbol_table, output)?;
                write!(output, " ")?;
                generate_code_recursively(child, symbol_table, output)?;
            }
        }
        AstNodeKind::AssignmentStatement => { /* TODO */ }
        AstNodeKind::CallStatement => { /* TODO */ }
        AstNodeKind::ReturnStatement => { /* TODO */ }
        AstNodeKind::ReadStatement => { /* TODO */ }
        AstNodeKind::WriteStatement => { /* TODO */ }
        AstNodeKind::AssertStatement => { /* TODO */ }
        AstNodeKind::IfStatement => { /* TODO */ }
        AstNodeKind::WhileStatement => { /* TODO */ }
        AstNodeKind::EqOperator => { /* TODO */ }
        AstNodeKind::NeqOperator => { /* TODO */ }
        AstNodeKind::LtOperator => { /* TODO */ }
        AstNodeKind::LeqOperator => { /* TODO */ }
        AstNodeKind::GeqOperator => { /* TODO */ }
        AstNodeKind::GtOperator => { /* TODO */ }
        AstNodeKind::NegOperator => { /* TODO */ }
        AstNodeKind::AddOperator => { /* TODO */ }
        AstNodeKind::SubOperator => { /* TODO */ }
        AstNodeKind::OrOperator => { /* TODO */ }
        AstNodeKind::NotOperator => { /* TODO */ }
        AstNodeKind::MulOperator => { /* TODO */ }
        AstNodeKind::DivOperator => { /* TODO */ }
        AstNodeKind::ModOperator => { /* TODO */ }
        AstNodeKind::AndOperator => { /* TODO */ }
        AstNodeKind::DotOperator => { /* TODO */ }
        AstNodeKind::IndexOperator => { /* TODO */ }
        AstNodeKind::Literal {
            literal_type,
            value,
        } => {
            if literal_type == &PrimitiveTypeName::String {
                write!(output, "\"{value}\"")?;
            } else if literal_type == &PrimitiveTypeName::Boolean {
                unreachable!("true and false are parsed into predefined identifiers");
            } else {
                write!(output, "{value}")?;
            }
        }
        AstNodeKind::Identifier { symbol_index, .. }
        | AstNodeKind::PredefinedIdentifier { symbol_index, .. } => {
            let symbol = symbol_table.get(*symbol_index).unwrap();
            let identifier = symbol.name();
            match symbol.symbol_type() {
                SymbolType::Function(_) => {
                    write!(output, "f{symbol_index}_{identifier}")?;
                }
                SymbolType::Variable(_) => {
                    // assume infinite registers for now
                    write!(output, "r{symbol_index}_{identifier}")?;
                }
                SymbolType::BuiltinFunction => {
                    unreachable!("builtin functions are parsed on the CallStatement level");
                }
                SymbolType::BuiltinConstant => match symbol.name() {
                    "true" => write!(output, "1")?,
                    "false" => write!(output, "0")?,
                    other => unreachable!("not a builtin constant: {other}"),
                },
                SymbolType::Program => {
                    unreachable!("the program name is never output");
                }
                SymbolType::Empty => {
                    unreachable!("symbols cannot have the empty type");
                }
            }
        }
        AstNodeKind::Type { type_name } => write!(output, "{}*", type_name.c_type_name())?,
    }

    Ok(())
}
