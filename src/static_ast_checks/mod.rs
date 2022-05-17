use crate::error::{static_error, Result, StaticErrorKind};
use crate::parser::{AstNode, AstNodeKind};
use crate::symbol_table::{SymbolTable, SymbolType, VariableSymbolType};

pub fn type_check(ast: &AstNode, symbol_table: &SymbolTable) -> Result<()> {
    type_check_recursively(ast, symbol_table).map(|_| ())
}

/// Type check the AST recursively, using the type information for symbols from the symbol table.
/// Operators return their result types as expected.
/// A special case are blocks, which return the type of any return statement they contain, or the `Empty` type otherwise.
/// This way, the return types with functions can be propagated up to the function level, where they will be compared against the function's return type.
/// (The same holds for procedures, expecting the `Empty` return type.)
/// This recursively means that statements return no type, except for return statements.
fn type_check_recursively(ast: &AstNode, symbol_table: &SymbolTable) -> Result<Option<SymbolType>> {
    use AstNodeKind::*;

    Ok(match ast.kind() {
        Program => {
            for child in ast.children() {
                type_check_recursively(child, symbol_table)?;
            }
            None
        }
        Function | Procedure => {
            // only type check the body, as the identifier and head don't need type checking
            let return_type =
                type_check_recursively(ast.children().last().unwrap(), symbol_table)?.unwrap();
            let expected_return_type = if ast.kind() == &Function {
                symbol_table
                    .get(ast.children()[0].get_symbol_index().unwrap())
                    .unwrap()
                    .symbol_type()
                    .clone()
            } else {
                SymbolType::Empty
            };
            if return_type != expected_return_type {
                return Err(static_error(
                    ast.interval().clone(),
                    StaticErrorKind::TypeMismatch {
                        expected: expected_return_type.clone(),
                        actual: return_type,
                    },
                ));
            }
            None
        }
        VarParameter | ValueParameter | VariableDeclaration | Type { .. } => {
            /* does not need type checking */
            None
        }
        Block => {
            let mut return_type: Option<SymbolType> = None;
            for child in ast.children() {
                if let Some(alternate_return_type) = type_check_recursively(child, symbol_table)? {
                    if let Some(return_type) = return_type.as_ref() {
                        if return_type != &alternate_return_type {
                            return Err(static_error(
                                ast.interval().clone(),
                                StaticErrorKind::TypeMismatch {
                                    expected: return_type.clone(),
                                    actual: alternate_return_type,
                                },
                            ));
                        }
                    } else {
                        return_type = Some(alternate_return_type);
                    }
                }
            }
            // a block without return statement implicitly returns the empty type
            if return_type == None {
                return_type = Some(SymbolType::Empty);
            }
            return_type
        }
        AssignmentStatement => {
            todo!()
        }
        CallStatement => {
            todo!()
        }
        ReturnStatement => {
            todo!()
        }
        ReadStatement => {
            todo!()
        }
        WriteStatement => {
            todo!()
        }
        AssertStatement => {
            todo!()
        }
        IfStatement => {
            todo!()
        }
        WhileStatement => {
            todo!()
        }
        EqOperator => {
            todo!()
        }
        NeqOperator => {
            todo!()
        }
        LtOperator => {
            todo!()
        }
        LeqOperator => {
            todo!()
        }
        GeqOperator => {
            todo!()
        }
        GtOperator => {
            todo!()
        }
        NegOperator => {
            todo!()
        }
        AddOperator => {
            todo!()
        }
        SubOperator => {
            todo!()
        }
        OrOperator => {
            todo!()
        }
        NotOperator => {
            todo!()
        }
        MulOperator => {
            todo!()
        }
        DivOperator => {
            todo!()
        }
        ModOperator => {
            todo!()
        }
        AndOperator => {
            todo!()
        }
        DotOperator => {
            todo!()
        }
        IndexOperator => {
            todo!()
        }
        Literal { literal_type, .. } => Some(SymbolType::Variable(VariableSymbolType {
            var: false,
            variable_type: literal_type.clone(),
        })),
        Identifier { symbol_index, .. } | PredefinedIdentifier { symbol_index, .. } => Some(
            symbol_table
                .get(*symbol_index)
                .unwrap()
                .symbol_type()
                .clone(),
        ),
    })
}
