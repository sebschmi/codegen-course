use crate::error::Result;
use crate::parser::{AstNode, AstNodeKind, PrimitiveTypeName, TypeName};
use crate::symbol_table::{SymbolTable, SymbolType};
use std::fmt::Write;
use std::mem;

/// The "runtime" of our language, which includes the read and write predefined procedures.
const RUNTIME: &str = r#"
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

void read_integer(int64_t* output) {
    fscanf(stdin, "%ld", output);
}

void read_float(double* output) {
    fscanf(stdin, "%lf", output);
}

void read_boolean(int64_t* output) {
    fscanf(stdin, "%ld", output);
}

void read_string(char** output) {
    *output = malloc(1024);
    fscanf(stdin, "%s", *output);
}

void write_integer(int64_t output) {
    fprintf(stdout, "%ld\n", output);
}

void write_float(double output) {
    fprintf(stdout, "%lf\n", output);
}

void write_boolean(int64_t output) {
    fprintf(stdout, "%ld\n", output);
}

void write_string(char* output) {
    fprintf(stdout, "%s\n", output);
}

void assert(int64_t condition) {
    if (condition) return;
    write_string("assertion error");
    exit(1);
}

int64_t string_eq(char* a, char* b) {
    for (uint64_t i = 0; ; i++) {
        if (a[i] != b[i]) return 0;
        if (a[i] == 0) return 1;
    }
    assert(0);
}

int64_t string_neq(char* a, char* b) {
    for (uint64_t i = 0; ; i++) {
        if (a[i] != b[i]) return 1;
        if (a[i] == 0) return 0;
    }
    assert(0);
}

// lexicographic ordering of strings, prefixes are shorter than the original
int64_t string_leq(char* a, char* b) {
    for (uint64_t i = 0; ; i++) {
        if (a[i] > b[i]) return 0;
        if (a[i] < b[i]) return 1;
        if (a[i] == 0) return 1;
    }
    assert(0);
}

int64_t string_geq(char* a, char* b) {
    for (uint64_t i = 0; ; i++) {
        if (a[i] < b[i]) return 0;
        if (a[i] > b[i]) return 1;
        if (a[i] == 0) return 1;
    }
    assert(0);
}

int64_t string_lt(char* a, char* b) {
    for (uint64_t i = 0; ; i++) {
        if (a[i] > b[i]) return 0;
        if (a[i] < b[i]) return 1;
        if (a[i] == 0) return 0;
    }
    assert(0);
}

int64_t string_gt(char* a, char* b) {
    for (uint64_t i = 0; ; i++) {
        if (a[i] < b[i]) return 0;
        if (a[i] > b[i]) return 1;
        if (a[i] == 0) return 0;
    }
    assert(0);
}

char* string_concat(char* a, char* b) {
    uint64_t len_a;
    for (len_a = 0; a[len_a] != 0; len_a++);
    uint64_t len_b;
    for (len_b = 0; b[len_b] != 0; len_b++);
    char* result = malloc(len_a + len_b + 1);
    assert(result != 0);
    for (uint64_t i = 0; i < len_a; i++) result[i] = a[i];
    for (uint64_t i = 0; i < len_b; i++) result[len_a + i] = b[i];
    return result;
}

void main() {
void* stack = malloc(1024 * 1024); // 1 Mib of stack space
assert(stack != 0);
// these pointers are assumed to be registers, so we can manipulate them directly without loading or storing
void* stack_pointer = stack; // points to the address after the top of the stack
void* frame_pointer = stack; // points to the base of the current stack frame

// integer registers
int64_t r0, r1, r2, r3;
// float registers
double f0, f1, f2, f3;

goto l_main; // we use goto to keep everything in the same order as in the input, makes it hopefully easier to debug later
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

/// Generate code for pushing an integer register value onto the stack.
fn push_int(output: &mut impl Write, source: &str) -> Result<()> {
    writeln!(output, "// push_int({source})")?;
    writeln!(output, "*((int64_t*) stack_pointer) = {source};")?;
    writeln!(
        output,
        "stack_pointer = stack_pointer + {};",
        mem::size_of::<usize>()
    )?;
    Ok(())
}

/// Generate code for popping from the stack into an integer register.
fn pop_int(output: &mut impl Write, target: &str) -> Result<()> {
    writeln!(output, "// pop_int({target})")?;
    writeln!(
        output,
        "stack_pointer = stack_pointer - {};",
        mem::size_of::<usize>()
    )?;
    writeln!(output, "{target} = *((int64_t*) stack_pointer);")?;
    Ok(())
}

/// Generate code for pushing an integer register value onto the stack.
fn push_float(output: &mut impl Write, source: &str) -> Result<()> {
    writeln!(output, "// push_float({source})")?;
    writeln!(output, "*((double*) stack_pointer) = {source};")?;
    writeln!(
        output,
        "stack_pointer = stack_pointer + {};",
        mem::size_of::<usize>()
    )?;
    Ok(())
}

/// Generate code for popping from the stack into an integer register.
fn pop_float(output: &mut impl Write, target: &str) -> Result<()> {
    writeln!(output, "// pop_float({target})")?;
    writeln!(
        output,
        "stack_pointer = stack_pointer - {};",
        mem::size_of::<usize>()
    )?;
    writeln!(output, "{target} = *((double*) stack_pointer);")?;
    Ok(())
}

struct CodeGenerationContext {
    /// Valid return labels are between 1 and this value.
    last_return_id: usize,
    /// The label of the current function or procedure.
    current_function_label: String,
    /// A counter to generate ids for labels to ensure uniqueness.
    current_label_id: usize,
}

/// Generate code in the "restricted C" intermediate representation.
pub fn generate_code(
    ast: &AstNode,
    symbol_table: &SymbolTable,
    output: impl std::io::Write,
) -> Result<()> {
    let mut output = FmtIoWriteWrapper(output);
    output.write_str(RUNTIME)?;
    generate_code_recursively(
        ast,
        symbol_table,
        &mut output,
        &mut CodeGenerationContext {
            last_return_id: 0,
            current_function_label: "".to_string(),
            current_label_id: 0,
        },
        false,
    )
    .map(|_| ())
}

// calling convention: first return address then frame pointer between frames.
// caller sets up the stack before call, callee cleans up.
// return value is passed through the stack location of the frame pointer.

// we assume a stack-based machine, i.e. registers are only used for special computations, but generally temporaries are put onto the stack.

/// This function returns the type of the last value on the stack, if it pushed something.
///
/// `indirection` is true if the value returned by an expression or statement should be returned as a pointer to the value, as opposed to the value itself.
/// This has nothing to do with arrays or var parameters, but is used the left hand side of an assignment or read statement.
fn generate_code_recursively(
    ast: &AstNode,
    symbol_table: &SymbolTable,
    output: &mut impl Write,
    context: &mut CodeGenerationContext,
    indirection: bool,
) -> Result<Option<TypeName>> {
    // assert that indirection is only true in the supported cases
    if indirection {
        match ast.kind() {
            AstNodeKind::IndexOperator
            | AstNodeKind::Identifier { .. }
            | AstNodeKind::PredefinedIdentifier { .. } => { /* ok */ }
            other => debug_assert!(false, "{other:?}"), // only check for programming errors in debug mode. there is no debug_panic! macro unfortunately.
        }
    }

    match ast.kind() {
        AstNodeKind::Program => {
            for child in ast.children().iter().skip(1).rev().skip(1).rev() {
                generate_code_recursively(child, symbol_table, output, context, false)?;
            }
            writeln!(output, "l_main:")?;
            // set up stack for main
            writeln!(
                output,
                "stack_pointer = stack_pointer + {};",
                symbol_table.main_frame_size()
            )?;
            // generate code for main
            generate_code_recursively(
                ast.children().last().unwrap(),
                symbol_table,
                output,
                context,
                false,
            )?;
            // cleanup
            writeln!(output, "free(stack);")?;
            writeln!(output, "return;")?;

            // write return jump table
            writeln!(output, "return_table:")?;
            writeln!(output, "switch (r1) {{")?;
            for index in 1..=context.last_return_id {
                writeln!(output, "case {index}: goto l_return_{index}; break;")?;
            }
            writeln!(output, "}}")?;

            writeln!(output, "}}")?;
        }
        AstNodeKind::Procedure | AstNodeKind::Function => {
            // update context
            let mut label = String::new();
            generate_code_recursively(
                &ast.children()[0],
                symbol_table,
                &mut label,
                context,
                false,
            )?;
            context.current_function_label = label;

            // our calling convention is that the caller sets up the stack for the callee
            // write label for entry gotos
            writeln!(output, "{}:", &context.current_function_label)?;

            // write body
            generate_code_recursively(
                ast.children().last().unwrap(),
                symbol_table,
                output,
                context,
                false,
            )?;

            // write label for return gotos
            write!(output, "{}_return", &context.current_function_label)?;
            writeln!(output, ":")?;

            // write return mechanism
            writeln!(
                output,
                "r0 = (int64_t) (frame_pointer - {});",
                mem::size_of::<usize>()
            )?;
            writeln!(output, "r2 = *((int64_t*) r0);")?; // caller frame pointer
            writeln!(
                output,
                "r0 = (int64_t) (frame_pointer - {});",
                2 * mem::size_of::<usize>()
            )?;
            writeln!(output, "r1 = *((int64_t*) r0);")?; // return address
            writeln!(output, "frame_pointer = (void*) r2;")?;
            writeln!(output, "stack_pointer = (void*) r0;")?; // return address is the first element between frames
            writeln!(output, "r0 = r0 + {};", mem::size_of::<usize>())?; // return value is passed through former location of frame pointer
            writeln!(output, "*((int64_t*) r0) = r3;")?; // return value is stored in r3 before return
            writeln!(output, "goto return_table;")?; // jump to jump table for returns
        }
        AstNodeKind::VarParameter | AstNodeKind::ValueParameter => {
            // nothing to do
        }
        AstNodeKind::Block => {
            // we do not alter the stack within function execution, so nothing special about blocks
            for child in ast.children() {
                generate_code_recursively(child, symbol_table, output, context, false)?;
            }
        }
        AstNodeKind::VariableDeclaration => {
            // space for variables was created when setting up the stack
            // unsized arrays cannot be allocated since the size is not known
            let variable_type = ast.get_variable_type();
            if let TypeName::SizedArray { .. } = variable_type {
                // evaluate array length
                generate_code_recursively(
                    &ast.children().last().unwrap().children()[0],
                    symbol_table,
                    output,
                    context,
                    false,
                )?;
                pop_int(output, "r0")?;
                // compute required memory
                writeln!(output, "r2 = r0 + 1;")?;
                writeln!(output, "r2 = r2 * {};", mem::size_of::<usize>())?;

                for child in ast.children().iter().rev().skip(1).rev() {
                    // allocate
                    writeln!(output, "r1 = (int64_t) malloc(r2);")?;
                    // store length
                    writeln!(output, "*((int64_t*) r1) = (int64_t) r0;")?;
                    // store pointer
                    let symbol_index = child.get_symbol_index().unwrap();
                    let symbol = symbol_table.get(symbol_index).unwrap();
                    let variable_type = symbol.symbol_type().unwrap_variable();
                    writeln!(
                        output,
                        "r2 = (int64_t) (frame_pointer + {});",
                        variable_type.frame_offset
                    )?;
                    writeln!(output, "*((int64_t*) r2) = r1;")?;
                }
            }
        }
        AstNodeKind::AssignmentStatement => {
            // evaluate rhs, push result onto stack
            generate_code_recursively(&ast.children()[1], symbol_table, output, context, false)?;

            let lhs = &ast.children()[0];
            generate_code_recursively(lhs, symbol_table, output, context, true)?;
            pop_int(output, "r0")?;

            // perform assignment
            // since we don't do arithmetic, we can perform the assignment through the integer registers in all cases
            pop_int(output, "r1")?;
            writeln!(output, "*((int64_t*) r0) = r1;")?;
        }
        AstNodeKind::CallStatement => {
            // push return address (creating a new one)
            context.last_return_id += 1;
            push_int(output, &format!("{}", context.last_return_id))?;
            // push frame pointer
            push_int(output, "(int64_t) frame_pointer")?;

            // compute parameters (they will be pushed onto the stack one by one, implicitly setting up the stack for the callee)
            for child in ast.children().iter().skip(1) {
                generate_code_recursively(child, symbol_table, output, context, false)?;
            }
            // set up callee frame pointer
            writeln!(
                output,
                "frame_pointer = stack_pointer - {};",
                (ast.children().len() - 1) * 8
            )?;
            // set up callee stack pointer
            let callee_symbol = symbol_table
                .get(ast.children()[0].get_symbol_index().unwrap())
                .unwrap();
            let callee_type = callee_symbol.symbol_type().unwrap_function();
            writeln!(
                output,
                "stack_pointer = frame_pointer + {};",
                callee_type.frame_size
            )?;
            // jump to callee
            write!(output, "goto ")?;
            generate_code_recursively(&ast.children()[0], symbol_table, output, context, false)?;
            writeln!(output, ";")?;

            // return label for this location
            writeln!(output, "l_return_{}:", context.last_return_id)?;

            if callee_type.return_type.is_some() {
                // push return value onto stack (ignore given return value on stack, we can just use the one from r3)
                push_int(output, "r3")?;
                return Ok(callee_type.return_type.clone());
            }
        }
        AstNodeKind::ReturnStatement => {
            generate_code_recursively(&ast.children()[0], symbol_table, output, context, false)?;
            // return value is stored in r3 before return
            pop_int(output, "r3")?;
            // goto return code
            writeln!(output, "goto {}_return;", context.current_function_label)?;
        }
        AstNodeKind::ReadStatement => {
            for child in ast.children().iter().skip(1) {
                // generate target address
                let type_name =
                    generate_code_recursively(child, symbol_table, output, context, true)?.unwrap();
                pop_int(output, "r0")?;

                // read value with correct type
                writeln!(
                    output,
                    "read_{}(({}*)r0);",
                    match type_name.primitive_type_name() {
                        PrimitiveTypeName::Boolean => "boolean",
                        PrimitiveTypeName::Integer => "integer",
                        PrimitiveTypeName::Real => "float",
                        PrimitiveTypeName::String => "string",
                    },
                    type_name.primitive_type_name().c_type_name(),
                )?;
            }
        }
        AstNodeKind::WriteStatement => {
            for child in ast.children().iter().skip(1) {
                // generate target address
                let type_name =
                    generate_code_recursively(child, symbol_table, output, context, false)?
                        .unwrap();
                pop_int(output, "r0")?;

                // write value with correct type
                writeln!(
                    output,
                    "write_{}((({})r0));",
                    match type_name.primitive_type_name() {
                        PrimitiveTypeName::Boolean => "boolean",
                        PrimitiveTypeName::Integer => "integer",
                        PrimitiveTypeName::Real => "float",
                        PrimitiveTypeName::String => "string",
                    },
                    type_name.primitive_type_name().c_type_name(),
                )?;
            }
        }
        AstNodeKind::AssertStatement => {
            // evaluate assertion
            generate_code_recursively(&ast.children()[0], symbol_table, output, context, false)?
                .unwrap();
            pop_int(output, "r0")?;
            // test assertion
            writeln!(output, "assert(r0)")?;
        }
        AstNodeKind::IfStatement => {
            let else_label = format!("l_else_{}", context.current_label_id);
            let after_label = format!("l_after_if_{}", context.current_label_id);
            context.current_label_id += 1;

            // evaluate condition
            generate_code_recursively(&ast.children()[0], symbol_table, output, context, false)?;
            pop_int(output, "r0")?;
            // test condition
            writeln!(output, "if (!r0) goto {else_label};")?;

            // evaluate then branch
            generate_code_recursively(&ast.children()[1], symbol_table, output, context, false)?;
            // skip over else
            writeln!(output, "goto {after_label};")?;

            // write else label
            writeln!(output, "{else_label}:")?;

            if ast.children().len() == 3 {
                // evaluate else branch if exists
                generate_code_recursively(
                    &ast.children()[2],
                    symbol_table,
                    output,
                    context,
                    false,
                )?;
            }
            // write after label
            writeln!(output, "{after_label}:")?;
        }
        AstNodeKind::WhileStatement => {
            let while_label = format!("l_while_{}", context.current_label_id);
            let after_label = format!("l_after_while_{}", context.current_label_id);
            context.current_label_id += 1;

            // write while label
            writeln!(output, "{while_label}:")?;
            // evaluate condition
            generate_code_recursively(&ast.children()[0], symbol_table, output, context, false)?;
            pop_int(output, "r0")?;
            // test condition
            writeln!(output, "if (!r0) goto {after_label};")?;

            // evaluate loop body
            generate_code_recursively(&ast.children()[1], symbol_table, output, context, false)?;
            // jump back to condition
            writeln!(output, "goto {while_label};")?;

            // write after label
            writeln!(output, "{after_label}:")?;
        }
        binary_operator @ (AstNodeKind::EqOperator
        | AstNodeKind::NeqOperator
        | AstNodeKind::LtOperator
        | AstNodeKind::LeqOperator
        | AstNodeKind::GeqOperator
        | AstNodeKind::GtOperator
        | AstNodeKind::AddOperator
        | AstNodeKind::SubOperator
        | AstNodeKind::OrOperator
        | AstNodeKind::MulOperator
        | AstNodeKind::DivOperator
        | AstNodeKind::ModOperator
        | AstNodeKind::AndOperator) => {
            // evaluate left side
            generate_code_recursively(&ast.children()[0], symbol_table, output, context, false)?;
            // evaluate right side (since we did type checking, the input type is uniquely determined by one of the arguments)
            let input_type_name = generate_code_recursively(
                &ast.children()[1],
                symbol_table,
                output,
                context,
                false,
            )?
            .unwrap();
            let input_type_name = input_type_name.primitive_type_name();
            // float special case
            let registers = if input_type_name == &PrimitiveTypeName::Real {
                pop_float(output, "f1")?;
                pop_float(output, "f0")?;
                "f"
            } else {
                pop_int(output, "r1")?;
                pop_int(output, "r0")?;
                "r"
            };

            if input_type_name == &PrimitiveTypeName::String {
                let runtime_function = match binary_operator {
                    AstNodeKind::EqOperator => "eq",
                    AstNodeKind::NeqOperator => "neq",
                    AstNodeKind::LtOperator => "lt",
                    AstNodeKind::LeqOperator => "leq",
                    AstNodeKind::GeqOperator => "geq",
                    AstNodeKind::GtOperator => "gt",
                    AstNodeKind::AddOperator => "concat",
                    other => unreachable!("{other:?}"),
                };
                writeln!(output, "r0 = string_{runtime_function}(r0, r1);")?;
            } else {
                // get c operator
                let operator = match binary_operator {
                    AstNodeKind::EqOperator => "==",
                    AstNodeKind::NeqOperator => "!=",
                    AstNodeKind::LtOperator => "<",
                    AstNodeKind::LeqOperator => "<=",
                    AstNodeKind::GeqOperator => ">=",
                    AstNodeKind::GtOperator => ">",
                    AstNodeKind::AddOperator => "+",
                    AstNodeKind::SubOperator => "-",
                    AstNodeKind::OrOperator => "|",
                    AstNodeKind::MulOperator => "*",
                    AstNodeKind::DivOperator => "/",
                    AstNodeKind::ModOperator => "%",
                    AstNodeKind::AndOperator => "&",
                    other => unreachable!("{other:?}"),
                };

                // perform operation
                writeln!(
                    output,
                    "{registers}0 = {registers}0 {operator} {registers}1;"
                )?;
            }
            // store result
            if input_type_name == &PrimitiveTypeName::Real {
                push_float(output, "f0")?;
            } else {
                push_int(output, "r0")?;
            }

            let return_type = match binary_operator {
                AstNodeKind::EqOperator
                | AstNodeKind::NeqOperator
                | AstNodeKind::LtOperator
                | AstNodeKind::LeqOperator
                | AstNodeKind::GeqOperator
                | AstNodeKind::GtOperator => PrimitiveTypeName::Boolean,
                AstNodeKind::AddOperator
                | AstNodeKind::SubOperator
                | AstNodeKind::OrOperator
                | AstNodeKind::MulOperator
                | AstNodeKind::DivOperator
                | AstNodeKind::ModOperator
                | AstNodeKind::AndOperator => input_type_name.clone(),
                other => unreachable!("{other:?}"),
            };
            return Ok(Some(TypeName::Primitive {
                primitive_type: return_type,
            }));
        }
        unary_operator @ (AstNodeKind::NegOperator | AstNodeKind::NotOperator) => {
            // evaluate inner
            let input_type_name = generate_code_recursively(
                &ast.children()[0],
                symbol_table,
                output,
                context,
                false,
            )?
            .unwrap();
            let input_type_name = input_type_name.primitive_type_name();
            // float special case
            let registers = if input_type_name == &PrimitiveTypeName::Real {
                pop_float(output, "f0")?;
                "f"
            } else {
                pop_int(output, "r0")?;
                "r"
            };

            // get c operator
            let operator = match unary_operator {
                AstNodeKind::NotOperator => "~",
                AstNodeKind::NegOperator => "-",
                other => unreachable!("{other:?}"),
            };

            // perform operation
            writeln!(output, "{registers}0 = {operator} {registers}0;")?;
            // store result
            if input_type_name == &PrimitiveTypeName::Real {
                push_float(output, "f0")?;
            } else {
                push_int(output, "r0")?;
            }

            return Ok(Some(TypeName::Primitive {
                primitive_type: input_type_name.clone(),
            }));
        }
        AstNodeKind::DotOperator => {
            // calculate pointer into array
            let array_symbol = symbol_table
                .get(ast.children()[0].get_symbol_index().unwrap())
                .unwrap();
            let array_symbol_type = array_symbol.symbol_type().unwrap_variable();
            // get array symbol position on stack
            writeln!(
                output,
                "r0 = ((int64_t) (frame_pointer + {}));",
                array_symbol_type.frame_offset
            )?;
            // vars have one more step of indirection
            if array_symbol_type.var {
                writeln!(output, "r0 = *((int64_t*) r0);")?;
            }
            // index into array at position zero where the length is stored
            writeln!(output, "r0 = *((int64_t*) r0);")?;
            push_int(output, "r0")?;
            return Ok(Some(TypeName::Primitive {
                primitive_type: PrimitiveTypeName::Integer,
            }));
        }
        AstNodeKind::IndexOperator => {
            // evaluate index
            generate_code_recursively(&ast.children()[1], symbol_table, output, context, false)?;
            pop_int(output, "r1")?;

            // calculate pointer into array
            let array_symbol = symbol_table
                .get(ast.children()[0].get_symbol_index().unwrap())
                .unwrap();
            let array_symbol_type = array_symbol.symbol_type().unwrap_variable();
            // get array symbol position on stack
            writeln!(
                output,
                "r0 = ((int64_t) (frame_pointer + {}));",
                array_symbol_type.frame_offset
            )?;
            // vars have one more step of indirection
            if array_symbol_type.var {
                writeln!(output, "r0 = *((int64_t*) r0);")?;
            }
            // skip zero element of array
            writeln!(output, "r0 = r0 + 1;")?;
            // index into array
            writeln!(
                output,
                "r0 = (int64_t) ((({}) r0) + r1);",
                array_symbol_type.variable_type.c_type_name()
            )?;
            // remove indirection if required
            if !indirection {
                writeln!(output, "r0 = *((int64_t*) r0);")?;
            }
            push_int(output, "r0")?;
            return Ok(Some(array_symbol_type.variable_type.clone()));
        }
        AstNodeKind::Literal {
            literal_type,
            value,
        } => {
            if literal_type == &PrimitiveTypeName::String {
                writeln!(output, "r0 = (int64_t) \"{value}\"")?;
                push_int(output, "r0")?;
            } else if literal_type == &PrimitiveTypeName::Boolean {
                unreachable!("true and false are parsed into predefined identifiers");
            } else {
                push_int(output, value)?;
            }
            return Ok(Some(TypeName::Primitive {
                primitive_type: literal_type.clone(),
            }));
        }
        AstNodeKind::Identifier {
            symbol_index,
            lower_case,
            ..
        }
        | AstNodeKind::PredefinedIdentifier {
            symbol_index,
            lower_case,
            ..
        } => {
            let symbol = symbol_table.get(*symbol_index).unwrap();
            let identifier = symbol.name();
            match symbol.symbol_type() {
                SymbolType::Function(_) => {
                    // generate the function's label name
                    write!(output, "l_f{symbol_index}_{identifier}")?;
                }
                SymbolType::Variable(variable_symbol_type) => {
                    // variable access, return a pointer to the variable if indirection is true, and the value otherwise
                    // for e.g. arrays or strings this still means that a pointer is returned
                    writeln!(output, "// variable access '{lower_case}'")?;
                    writeln!(
                        output,
                        "r0 = ((int64_t) (frame_pointer + {}));",
                        variable_symbol_type.frame_offset
                    )?;
                    if variable_symbol_type.var {
                        writeln!(output, "r0 = *((int64_t*) r0);")?;
                    }
                    if !indirection {
                        writeln!(output, "r0 = *((int64_t*) r0);")?;
                    }
                    push_int(output, "r0")?;
                    return Ok(Some(variable_symbol_type.variable_type.clone()));
                }
                SymbolType::BuiltinFunction => {
                    unreachable!("builtin functions are generated on the CallStatement level");
                }
                SymbolType::BuiltinConstant => match symbol.name() {
                    "true" => {
                        write!(output, "1")?;
                        return Ok(Some(TypeName::Primitive {
                            primitive_type: PrimitiveTypeName::Boolean,
                        }));
                    }
                    "false" => {
                        write!(output, "0")?;
                        return Ok(Some(TypeName::Primitive {
                            primitive_type: PrimitiveTypeName::Boolean,
                        }));
                    }
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
        AstNodeKind::Type { .. } => unreachable!("types are used on higher levels only"),
    }

    Ok(None)
}
