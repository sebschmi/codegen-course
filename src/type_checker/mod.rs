use crate::error::{static_error, Result, StaticErrorKind};
use crate::parser::{AstNode, AstNodeKind, PrimitiveTypeName, TypeName};
use crate::symbol_table::{FunctionType, Symbol, SymbolTable, SymbolType, VariableSymbolType};
use log::trace;

/// Check for all possible type errors, i.e. that there are no type-conflicts between AST nodes.
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
    trace!(
        "type_check_recursively {:?} {:?}",
        ast.kind(),
        ast.interval()
    );
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
            let return_type = type_check_recursively(ast.children().last().unwrap(), symbol_table)?
                // a block without return statements implicitly returns the empty type at the end
                .unwrap_or(SymbolType::Empty);
            let expected_return_type = if ast.kind() == &Function {
                match symbol_table
                    .get(ast.children()[0].get_symbol_index().unwrap())
                    .unwrap()
                    .symbol_type()
                {
                    SymbolType::Function(FunctionType {
                        return_type: Some(return_type),
                        ..
                    }) => SymbolType::Variable(VariableSymbolType {
                        frame_offset: 0,
                        var: false,
                        variable_type: return_type.clone(),
                    }),
                    other => unreachable!("{other:#?}"),
                }
            } else {
                SymbolType::Empty
            };
            trace!("return_type: {return_type:?}; expected_return_type: {expected_return_type:?}");
            let error =
                if return_type == SymbolType::Empty || expected_return_type == SymbolType::Empty {
                    return_type != expected_return_type
                } else {
                    return_type
                        .get_variable_or_value_type_name()
                        .to_unsized_array()
                        != expected_return_type
                            .get_variable_or_value_type_name()
                            .to_unsized_array()
                };
            if error {
                return Err(static_error(
                    ast.interval().clone(),
                    StaticErrorKind::TypeMismatch {
                        expected: expected_return_type,
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
                    trace!("statement_return_type: {alternate_return_type:?}");
                    if let Some(return_type) = return_type.as_ref() {
                        if return_type
                            .get_variable_or_value_type_name()
                            .to_unsized_array()
                            != alternate_return_type
                                .get_variable_or_value_type_name()
                                .to_unsized_array()
                        {
                            return Err(static_error(
                                ast.interval().clone(),
                                StaticErrorKind::TypeMismatch {
                                    expected: return_type.clone(),
                                    actual: alternate_return_type,
                                },
                            ));
                        }
                    } else {
                        trace!("statement_return_type: None");
                        return_type = Some(alternate_return_type);
                    }
                }
            }
            trace!("block_return_type: {return_type:?}");
            return_type.map(|symbol_type| {
                if symbol_type == SymbolType::Empty {
                    symbol_type
                } else {
                    symbol_type.variable_into_value()
                }
            })
        }
        AssignmentStatement => {
            let assignee_type = type_check_recursively(&ast.children()[0], symbol_table)?;
            let value_type = type_check_recursively(&ast.children()[1], symbol_table)?;
            if let (Some(assignee_type), Some(value_type)) = (assignee_type, value_type) {
                if assignee_type
                    .get_variable_or_value_type_name()
                    .to_unsized_array()
                    != value_type
                        .get_variable_or_value_type_name()
                        .to_unsized_array()
                {
                    return Err(static_error(
                        ast.interval().clone(),
                        StaticErrorKind::TypeMismatch {
                            expected: assignee_type,
                            actual: value_type,
                        },
                    ));
                }
                if !assignee_type.is_variable() {
                    return Err(static_error(
                        ast.interval().clone(),
                        StaticErrorKind::ExpectedVariable {
                            actual: assignee_type,
                        },
                    ));
                }
            } else {
                return Err(static_error(
                    ast.interval().clone(),
                    StaticErrorKind::UnexpectedEmptyType,
                ));
            }
            None
        }
        CallStatement | AssertStatement => {
            trace!("ast: {ast:?}");
            let symbol_index = if ast.kind() == &AssertStatement {
                debug_assert_eq!(symbol_table.get(3).map(Symbol::name), Some("assert"));
                3
            } else {
                ast.children()[0].get_symbol_index().unwrap()
            };
            let symbol = symbol_table.get(symbol_index).unwrap();
            trace!("function_name_symbol: {symbol:?}");
            if let SymbolType::Function(FunctionType {
                parameter_types,
                return_type,
                ..
            }) = symbol.symbol_type()
            {
                let argument_children =
                    &ast.children()[if ast.kind() == &AssertStatement { 0 } else { 1 }..];
                let argument_count = argument_children.len();
                if argument_count != parameter_types.len() {
                    return Err(static_error(
                        ast.interval().clone(),
                        StaticErrorKind::WrongArgumentCount {
                            expected: parameter_types.len(),
                            actual: argument_count,
                        },
                    ));
                }

                for (child, expected_type) in argument_children.iter().zip(parameter_types.iter()) {
                    let expected_type = SymbolType::Variable(expected_type.clone());
                    let child_type = type_check_recursively(child, symbol_table)?;
                    if let Some(child_type) = child_type {
                        if expected_type
                            .get_variable_or_value_type_name()
                            .to_unsized_array()
                            != child_type
                                .get_variable_or_value_type_name()
                                .to_unsized_array()
                        {
                            return Err(static_error(
                                ast.interval().clone(),
                                StaticErrorKind::TypeMismatch {
                                    expected: expected_type,
                                    actual: child_type,
                                },
                            ));
                        }
                        if expected_type.unwrap_variable().var && child_type.is_value() {
                            return Err(static_error(
                                ast.interval().clone(),
                                StaticErrorKind::ExpectedVariable { actual: child_type },
                            ));
                        }
                    } else {
                        return Err(static_error(
                            ast.interval().clone(),
                            StaticErrorKind::UnexpectedEmptyType,
                        ));
                    }
                }

                // returns are always values
                return_type.clone().map(SymbolType::Value)
            } else {
                return Err(static_error(
                    ast.interval().clone(),
                    StaticErrorKind::ExpectedFunction {
                        actual: symbol.symbol_type().clone(),
                    },
                ));
            }
        }
        ReturnStatement => {
            let result = type_check_recursively(&ast.children()[0], symbol_table)?;
            trace!("return_type: {result:?}");
            if result.is_none() {
                // return statements return a special marker type if they return nothing,
                // such that in the block type checking logic an error can be raised if
                // a different type than nothing was expected.
                Some(SymbolType::Empty)
            } else {
                // returns are always values
                result.map(|symbol_type| symbol_type.variable_into_value())
            }
        }
        ReadStatement => {
            for child in ast.children().iter().skip(1) {
                match child.kind() {
                    Identifier { symbol_index, .. } | PredefinedIdentifier { symbol_index, .. } => {
                        let identifier_type =
                            symbol_table.get(*symbol_index).unwrap().symbol_type();
                        if let SymbolType::Variable(VariableSymbolType {
                            variable_type: TypeName::Primitive { .. },
                            ..
                        }) = identifier_type
                        {
                            /* ok */
                        } else {
                            return Err(static_error(
                                child.interval().clone(),
                                StaticErrorKind::ExpectedPrimitiveType {
                                    actual: identifier_type.clone(),
                                },
                            ));
                        }
                    }
                    IndexOperator => {
                        let symbol_index = child.children()[0].get_symbol_index().unwrap();
                        let array_type = symbol_table.get(symbol_index).unwrap().symbol_type();
                        if let SymbolType::Variable(_) = array_type {
                            /* ok */
                        } else {
                            return Err(static_error(
                                child.interval().clone(),
                                StaticErrorKind::ExpectedVariable {
                                    actual: array_type.clone(),
                                },
                            ));
                        }
                    }
                    other => {
                        return Err(static_error(
                            child.interval().clone(),
                            StaticErrorKind::ExpectedIdentifier {
                                actual: other.clone(),
                            },
                        ))
                    }
                }
            }
            None
        }
        WriteStatement => {
            for child in ast.children().iter().skip(1) {
                let child_type = type_check_recursively(child, symbol_table)?;
                if let Some(SymbolType::Variable(_) | SymbolType::Value(_)) = child_type {
                    /* ok */
                } else {
                    return Err(static_error(
                        ast.interval().clone(),
                        StaticErrorKind::UnexpectedEmptyType,
                    ));
                }
            }
            None
        }
        IfStatement | WhileStatement => {
            let condition_type = type_check_recursively(&ast.children()[0], symbol_table)?
                .unwrap_or(SymbolType::Empty);
            if let SymbolType::Variable(VariableSymbolType {
                variable_type:
                    TypeName::Primitive {
                        primitive_type: PrimitiveTypeName::Boolean,
                    },
                ..
            })
            | SymbolType::Value(TypeName::Primitive {
                primitive_type: PrimitiveTypeName::Boolean,
            }) = condition_type
            {
                let mut if_while_type = type_check_recursively(&ast.children()[1], symbol_table)?;
                if ast.children().len() == 3 {
                    debug_assert_eq!(ast.kind(), &IfStatement);
                    let else_type = type_check_recursively(&ast.children()[2], symbol_table)?;
                    if let Some(if_while_type) = &if_while_type {
                        if let Some(else_type) = else_type {
                            if if_while_type
                                .get_variable_or_value_type_name()
                                .to_unsized_array()
                                != else_type
                                    .get_variable_or_value_type_name()
                                    .to_unsized_array()
                            {
                                return Err(static_error(
                                    ast.interval().clone(),
                                    StaticErrorKind::TypeMismatch {
                                        expected: if_while_type.clone(),
                                        actual: else_type,
                                    },
                                ));
                            }
                        }
                    } else {
                        if_while_type = else_type;
                    }
                }
                if_while_type
            } else {
                // expecting a value here, but it could also be a variable
                return Err(static_error(
                    ast.children()[0].interval().clone(),
                    StaticErrorKind::TypeMismatch {
                        expected: SymbolType::Value(TypeName::Primitive {
                            primitive_type: PrimitiveTypeName::Boolean,
                        }),
                        actual: condition_type,
                    },
                ));
            }
        }
        EqOperator | NeqOperator | LtOperator | LeqOperator | GeqOperator | GtOperator => {
            let first_type = type_check_recursively(&ast.children()[0], symbol_table)?
                .unwrap_or(SymbolType::Empty);
            let second_type = type_check_recursively(&ast.children()[1], symbol_table)?
                .unwrap_or(SymbolType::Empty);
            if let SymbolType::Variable(VariableSymbolType {
                variable_type: TypeName::Primitive { .. },
                ..
            })
            | SymbolType::Value(TypeName::Primitive { .. }) = &first_type
            {
                /* ok */
            } else {
                return Err(static_error(
                    ast.children()[0].interval().clone(),
                    StaticErrorKind::ExpectedPrimitiveType { actual: first_type },
                ));
            }
            if first_type
                .get_variable_or_value_type_name()
                .to_unsized_array()
                != second_type
                    .get_variable_or_value_type_name()
                    .to_unsized_array()
            {
                return Err(static_error(
                    ast.interval().clone(),
                    StaticErrorKind::TypeMismatch {
                        expected: first_type,
                        actual: second_type,
                    },
                ));
            }
            Some(SymbolType::Value(TypeName::Primitive {
                primitive_type: PrimitiveTypeName::Boolean,
            }))
        }
        NegOperator => {
            let child_type = type_check_recursively(&ast.children()[0], symbol_table)?
                .unwrap_or(SymbolType::Empty);
            if let SymbolType::Variable(VariableSymbolType {
                variable_type: TypeName::Primitive { primitive_type },
                ..
            })
            | SymbolType::Value(TypeName::Primitive { primitive_type }) = &child_type
            {
                if primitive_type == &PrimitiveTypeName::Integer
                    || primitive_type == &PrimitiveTypeName::Real
                {
                    /* ok */
                } else {
                    return Err(static_error(
                        ast.children()[0].interval().clone(),
                        StaticErrorKind::ExpectedNumericType { actual: child_type },
                    ));
                }
            } else {
                return Err(static_error(
                    ast.children()[0].interval().clone(),
                    StaticErrorKind::ExpectedNumericType { actual: child_type },
                ));
            }
            Some(child_type)
        }
        AddOperator | SubOperator | MulOperator | DivOperator => {
            let first_type = type_check_recursively(&ast.children()[0], symbol_table)?
                .unwrap_or(SymbolType::Empty);
            let second_type = type_check_recursively(&ast.children()[1], symbol_table)?
                .unwrap_or(SymbolType::Empty);
            if let SymbolType::Variable(VariableSymbolType {
                variable_type: TypeName::Primitive { primitive_type },
                ..
            })
            | SymbolType::Value(TypeName::Primitive { primitive_type }) = &first_type
            {
                if primitive_type == &PrimitiveTypeName::Integer
                    || primitive_type == &PrimitiveTypeName::Real
                {
                    /* ok */
                } else if ast.kind() == &AddOperator {
                    if primitive_type == &PrimitiveTypeName::String {
                        /* ok */
                    } else {
                        return Err(static_error(
                            ast.children()[0].interval().clone(),
                            StaticErrorKind::ExpectedNumericOrStringType { actual: first_type },
                        ));
                    }
                } else {
                    return Err(static_error(
                        ast.children()[0].interval().clone(),
                        StaticErrorKind::ExpectedNumericType { actual: first_type },
                    ));
                }
            } else {
                return Err(static_error(
                    ast.children()[0].interval().clone(),
                    StaticErrorKind::ExpectedPrimitiveType { actual: first_type },
                ));
            }
            if first_type
                .get_variable_or_value_type_name()
                .to_unsized_array()
                != second_type
                    .get_variable_or_value_type_name()
                    .to_unsized_array()
            {
                return Err(static_error(
                    ast.interval().clone(),
                    StaticErrorKind::TypeMismatch {
                        expected: first_type,
                        actual: second_type,
                    },
                ));
            }
            Some(first_type.variable_into_value())
        }
        OrOperator | AndOperator => {
            let first_type = type_check_recursively(&ast.children()[0], symbol_table)?
                .unwrap_or(SymbolType::Empty);
            let second_type = type_check_recursively(&ast.children()[1], symbol_table)?
                .unwrap_or(SymbolType::Empty);
            if let SymbolType::Variable(VariableSymbolType {
                variable_type:
                    TypeName::Primitive {
                        primitive_type: PrimitiveTypeName::Boolean,
                    },
                ..
            })
            | SymbolType::Value(TypeName::Primitive {
                primitive_type: PrimitiveTypeName::Boolean,
            }) = &first_type
            {
                /* ok */
            } else {
                // expecting a value here, but it could also be a variable
                return Err(static_error(
                    ast.children()[0].interval().clone(),
                    StaticErrorKind::TypeMismatch {
                        expected: SymbolType::Value(TypeName::Primitive {
                            primitive_type: PrimitiveTypeName::Boolean,
                        }),
                        actual: first_type,
                    },
                ));
            }
            if first_type
                .get_variable_or_value_type_name()
                .to_unsized_array()
                != second_type
                    .get_variable_or_value_type_name()
                    .to_unsized_array()
            {
                return Err(static_error(
                    ast.interval().clone(),
                    StaticErrorKind::TypeMismatch {
                        expected: first_type,
                        actual: second_type,
                    },
                ));
            }
            Some(first_type.variable_into_value())
        }
        NotOperator => {
            let child_type = type_check_recursively(&ast.children()[0], symbol_table)?
                .unwrap_or(SymbolType::Empty);
            if let SymbolType::Variable(VariableSymbolType {
                variable_type:
                    TypeName::Primitive {
                        primitive_type: PrimitiveTypeName::Boolean,
                    },
                ..
            })
            | SymbolType::Value(TypeName::Primitive {
                primitive_type: PrimitiveTypeName::Boolean,
            }) = &child_type
            {
                /* ok */
            } else {
                // expecting a value here, but it could also be a variable
                return Err(static_error(
                    ast.children()[0].interval().clone(),
                    StaticErrorKind::TypeMismatch {
                        expected: SymbolType::Value(TypeName::Primitive {
                            primitive_type: PrimitiveTypeName::Boolean,
                        }),
                        actual: child_type,
                    },
                ));
            }
            Some(child_type.variable_into_value())
        }
        ModOperator => {
            let first_type = type_check_recursively(&ast.children()[0], symbol_table)?
                .unwrap_or(SymbolType::Empty);
            let second_type = type_check_recursively(&ast.children()[1], symbol_table)?
                .unwrap_or(SymbolType::Empty);
            if let SymbolType::Variable(VariableSymbolType {
                variable_type:
                    TypeName::Primitive {
                        primitive_type: PrimitiveTypeName::Integer,
                    },
                ..
            })
            | SymbolType::Value(TypeName::Primitive {
                primitive_type: PrimitiveTypeName::Integer,
            }) = &first_type
            {
                /* ok */
            } else {
                // expecting a value here, but it could also be a variable
                return Err(static_error(
                    ast.children()[0].interval().clone(),
                    StaticErrorKind::TypeMismatch {
                        expected: SymbolType::Value(TypeName::Primitive {
                            primitive_type: PrimitiveTypeName::Integer,
                        }),
                        actual: first_type,
                    },
                ));
            }
            if first_type
                .get_variable_or_value_type_name()
                .to_unsized_array()
                != second_type
                    .get_variable_or_value_type_name()
                    .to_unsized_array()
            {
                return Err(static_error(
                    ast.interval().clone(),
                    StaticErrorKind::TypeMismatch {
                        expected: first_type,
                        actual: second_type,
                    },
                ));
            }
            Some(first_type.variable_into_value())
        }
        DotOperator => {
            let child_type = type_check_recursively(&ast.children()[0], symbol_table)?
                .unwrap_or(SymbolType::Empty);
            if let SymbolType::Variable(VariableSymbolType { variable_type, .. })
            | SymbolType::Value(variable_type) = &child_type
            {
                match variable_type {
                    TypeName::UnsizedArray { .. } | TypeName::SizedArray { .. } => { /* ok */ }
                    other => {
                        // expecting a value here, but it could also be a variable
                        return Err(static_error(
                            ast.children()[0].interval().clone(),
                            StaticErrorKind::ExpectedArray {
                                actual: SymbolType::Value(other.clone()),
                            },
                        ));
                    }
                }
                /* ok */
            } else {
                // expecting a non-var type here, but we actually do not care if it is var or not
                return Err(static_error(
                    ast.children()[0].interval().clone(),
                    StaticErrorKind::ExpectedArray { actual: child_type },
                ));
            }
            Some(SymbolType::Value(TypeName::Primitive {
                primitive_type: PrimitiveTypeName::Integer,
            }))
        }
        IndexOperator => {
            let array_type = type_check_recursively(&ast.children()[0], symbol_table)?
                .unwrap_or(SymbolType::Empty);
            let index_type = type_check_recursively(&ast.children()[1], symbol_table)?
                .unwrap_or(SymbolType::Empty);
            let return_type =
                if let SymbolType::Variable(VariableSymbolType { variable_type, .. })
                | SymbolType::Value(variable_type) = array_type
                {
                    match variable_type {
                        TypeName::UnsizedArray { primitive_type }
                        | TypeName::SizedArray { primitive_type } => {
                            /* ok */
                            SymbolType::Value(TypeName::Primitive { primitive_type })
                        }
                        other => {
                            // expecting a value here, but it could also be a variable
                            return Err(static_error(
                                ast.children()[0].interval().clone(),
                                StaticErrorKind::ExpectedArray {
                                    actual: SymbolType::Value(other),
                                },
                            ));
                        }
                    }
                } else {
                    return Err(static_error(
                        ast.children()[0].interval().clone(),
                        StaticErrorKind::ExpectedArray { actual: array_type },
                    ));
                };
            if let SymbolType::Variable(VariableSymbolType {
                variable_type:
                    TypeName::Primitive {
                        primitive_type: PrimitiveTypeName::Integer,
                    },
                ..
            })
            | SymbolType::Value(TypeName::Primitive {
                primitive_type: PrimitiveTypeName::Integer,
            }) = &index_type
            {
                /* ok */
            } else {
                return Err(static_error(
                    ast.children()[0].interval().clone(),
                    StaticErrorKind::ExpectedInteger { actual: index_type },
                ));
            }
            // when indexing into an array, we can always get a pointer to the result
            Some(return_type.value_to_variable())
        }
        Literal { literal_type, .. } => Some(SymbolType::Value(TypeName::Primitive {
            primitive_type: literal_type.clone(),
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
