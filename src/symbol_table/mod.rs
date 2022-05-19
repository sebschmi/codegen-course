use crate::error::{static_error, Result, StaticErrorKind};
use crate::parser::{AstNode, AstNodeKind, PrimitiveTypeName, TypeName};
use log::trace;
use std::collections::HashMap;
use std::hash::Hash;

#[cfg(test)]
mod tests;

/// The symbol table used by the compiler.
/// Stores the name and type of each symbol.
/// Different symbols can have the same name if they are declared in different scopes for example.
/// Each identifier declaration and usage points to the symbol table after it is built, such that the names of the identifiers can be ignored.
/// Each symbol has an index, but the index 0 is reserved to mean "uninitialised".
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SymbolTable {
    symbols: Vec<Symbol>,
}

/// An entry in the symbol table.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Symbol {
    /// The index of the symbol in the symbol table.
    index: usize,
    /// The name of the symbol as string (lower case).
    name: String,
    /// The type of the symbol.
    symbol_type: SymbolType,
}

/// The type of a symbol.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum SymbolType {
    /// A function or procedure, i.e. function without return type.
    Function(FunctionType),
    /// A variable.
    Variable(VariableSymbolType),
    /// A built-in function, which is a special case of a function as it can have a variable amount of parameters.
    BuiltinFunction,
    /// A built-in constant like `true` or `false`, which can be overridden by variable declarations.
    BuiltinConstant,
    /// The name of the program.
    Program,
    /// The empty type, returned by procedures.
    Empty,
}

/// The type of a function.
/// It is the list of its parameter types plus the (optional) return type.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FunctionType {
    /// The list of its parameter types.
    pub parameter_types: Vec<TypeName>,
    /// The optional return type.
    /// This is `None` for a procedure, and `Some(_)` for a function.
    pub return_type: Option<TypeName>,
}

/// The type of a variable.
/// This is the type of the value plus the optional property of being a reference.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct VariableSymbolType {
    /// `true` if the variable is a var-parameter.
    pub var: bool,
    /// The type of the value of the variable.
    pub variable_type: TypeName,
    /// This variable's offset from the frame pointer.
    pub frame_offset: Option<usize>,
}

impl SymbolTable {
    /// Create a new symbol table initialised with the builtin functions `read` and `writeln`.
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        let mut result = Self {
            symbols: vec![Symbol {
                index: 0,
                name: "".to_string(),
                symbol_type: SymbolType::BuiltinFunction,
            }],
        };
        result.add_symbol("read".to_string(), SymbolType::BuiltinFunction);
        result.add_symbol("writeln".to_string(), SymbolType::BuiltinFunction);
        result.add_symbol(
            "assert".to_string(),
            SymbolType::Function(FunctionType {
                parameter_types: vec![TypeName::Primitive {
                    primitive_type: PrimitiveTypeName::Boolean,
                }],
                return_type: None,
            }),
        );
        result.add_symbol("true".to_string(), SymbolType::BuiltinConstant);
        result.add_symbol("false".to_string(), SymbolType::BuiltinConstant);
        result
    }

    /// Insert a symbol into the symbol table.
    /// The symbols index will be set to its index in the table.
    pub fn add_symbol(&mut self, name: String, symbol_type: SymbolType) -> usize {
        let symbol = Symbol {
            index: self.symbols.len(),
            name,
            symbol_type,
        };
        debug_assert!(symbol.index > 0);
        self.symbols.push(symbol);
        self.symbols.len() - 1
    }

    /// Iterate over all symbols in the table.
    /// This skips the "null"-symbol at position zero, and starts from position one instead.
    pub fn iter(&self) -> impl Iterator<Item = &Symbol> {
        self.symbols.iter().skip(1)
    }

    pub fn get(&self, index: usize) -> Option<&Symbol> {
        if index == 0 {
            return None;
        }
        self.symbols.get(index)
    }
}

impl Symbol {
    pub fn symbol_type(&self) -> &SymbolType {
        &self.symbol_type
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}

impl SymbolType {
    /// Compare two `SymbolType`s, treating differing `var` properties of variables as equal.
    /// This is useful in type checking, since there it does not matter if anything is a reference or value.
    pub fn equals_ignore_var(&self, other: &Self) -> bool {
        self.clone_without_var() == other.clone_without_var()
    }

    /// Clone the type, setting `var` to false.
    /// This is used in comparisons where the `var` property does not matter.
    // this hopefully gets optimised properly, since the cloning is not actually necessary, but makes it simpler to write
    pub fn clone_without_var(&self) -> Self {
        match self.clone() {
            SymbolType::Variable(VariableSymbolType { variable_type, .. }) => {
                SymbolType::Variable(VariableSymbolType {
                    var: false,
                    variable_type,
                    frame_offset: None,
                })
            }
            other => other,
        }
    }
}

#[derive(Default, Debug, Clone)]
struct MapStack<K, V> {
    stack: Vec<HashMap<K, V>>,
}

impl<K, V> MapStack<K, V> {
    fn push(&mut self) {
        self.stack.push(HashMap::new());
    }

    fn pop(&mut self) {
        self.stack.pop();
    }
}

impl<K: Eq + Hash, V> MapStack<K, V> {
    fn insert(&mut self, key: K, value: V) {
        self.stack.last_mut().unwrap().insert(key, value);
    }

    fn get(&mut self, key: &K) -> Option<&V> {
        for map in self.stack.iter().rev() {
            if let Some(value) = map.get(key) {
                return Some(value);
            }
        }
        None
    }
}

/// Build a single global symbol table and resolve all usages of symbols.
pub fn build_symbol_table(ast: &mut AstNode) -> Result<SymbolTable> {
    let mut symbol_table = SymbolTable::new();
    let mut map_stack = MapStack::default();
    map_stack.push();
    for symbol in symbol_table.iter() {
        map_stack.insert(symbol.name.to_string(), symbol.index);
    }

    build_symbol_table_recursively(ast, &mut symbol_table, &mut map_stack).map(|()| symbol_table)
}

fn build_symbol_table_recursively(
    ast: &mut AstNode,
    symbol_table: &mut SymbolTable,
    map_stack: &mut MapStack<String, usize>,
) -> Result<()> {
    use AstNodeKind::*;

    match ast.kind() {
        Program => {
            map_stack.push();
            let identifier = ast.children_mut()[0].get_identifier_lower_case().unwrap();
            let index = symbol_table.add_symbol(identifier.to_string(), SymbolType::Program);
            map_stack.insert(identifier.to_string(), index);
            *ast.children_mut()[0].get_symbol_index_mut().unwrap() = index;
            for child in ast.children_mut().iter_mut().skip(1) {
                build_symbol_table_recursively(child, symbol_table, map_stack)?;
            }
            map_stack.pop();
        }
        Procedure => {
            let parameter_types = ast.get_parameter_types();
            let identifier = ast.children_mut()[0].get_identifier_lower_case().unwrap();
            let index = symbol_table.add_symbol(
                identifier.to_string(),
                SymbolType::Function(FunctionType {
                    parameter_types,
                    return_type: None,
                }),
            );
            map_stack.insert(identifier.to_string(), index);
            *ast.children_mut()[0].get_symbol_index_mut().unwrap() = index;
            map_stack.push();
            // skip to not the build symbol table for the identifier twice
            for child in ast.children_mut().iter_mut().skip(1) {
                build_symbol_table_recursively(child, symbol_table, map_stack)?;
            }
            map_stack.pop();
        }
        Function => {
            let (parameter_types, return_type) = ast.get_parameter_types_and_return_type();
            let identifier = ast.children_mut()[0].get_identifier_lower_case().unwrap();
            let index = symbol_table.add_symbol(
                identifier.to_string(),
                SymbolType::Function(FunctionType {
                    parameter_types,
                    return_type: Some(return_type),
                }),
            );
            map_stack.insert(identifier.to_string(), index);
            *ast.children_mut()[0].get_symbol_index_mut().unwrap() = index;
            map_stack.push();
            // skip to not build the symbol table for the identifier twice
            for child in ast.children_mut().iter_mut().skip(1) {
                build_symbol_table_recursively(child, symbol_table, map_stack)?;
            }
            map_stack.pop();
        }
        VarParameter => {
            let identifier = ast.children_mut()[0]
                .get_identifier_lower_case()
                .unwrap()
                .to_string();
            let index = symbol_table.add_symbol(
                identifier.clone(),
                SymbolType::Variable(VariableSymbolType {
                    var: true,
                    variable_type: ast.get_variable_type(),
                    frame_offset: None,
                }),
            );
            map_stack.insert(identifier, index);
            *ast.children_mut()[0].get_symbol_index_mut().unwrap() = index;
        }
        ValueParameter => {
            let identifier = ast.children_mut()[0]
                .get_identifier_lower_case()
                .unwrap()
                .to_string();
            let index = symbol_table.add_symbol(
                identifier.clone(),
                SymbolType::Variable(VariableSymbolType {
                    var: false,
                    variable_type: ast.get_variable_type(),
                    frame_offset: None,
                }),
            );
            map_stack.insert(identifier, index);
            *ast.children_mut()[0].get_symbol_index_mut().unwrap() = index;
        }
        Block => {
            map_stack.push();
            for child in ast.children_mut() {
                build_symbol_table_recursively(child, symbol_table, map_stack)?;
            }
            map_stack.pop();
        }
        VariableDeclaration => {
            let variable_type = ast.get_variable_type();
            for child in ast.children_mut().iter_mut().rev().skip(1).rev() {
                let identifier = child.get_identifier_lower_case().unwrap().to_string();
                let index = symbol_table.add_symbol(
                    identifier.clone(),
                    SymbolType::Variable(VariableSymbolType {
                        var: false,
                        variable_type: variable_type.clone(),
                        frame_offset: None,
                    }),
                );
                map_stack.insert(identifier, index);
                *child.get_symbol_index_mut().unwrap() = index;
            }
        }
        AssignmentStatement | CallStatement | ReturnStatement | ReadStatement | WriteStatement
        | AssertStatement | IfStatement | WhileStatement | EqOperator | NeqOperator
        | LtOperator | LeqOperator | GeqOperator | GtOperator | NegOperator | AddOperator
        | SubOperator | OrOperator | NotOperator | MulOperator | DivOperator | ModOperator
        | AndOperator | DotOperator | IndexOperator => {
            for child in ast.children_mut().iter_mut() {
                build_symbol_table_recursively(child, symbol_table, map_stack)?;
            }
        }
        Literal { .. } => { /* literals cannot use symbols */ }
        Identifier {
            lower_case,
            original,
            ..
        }
        | PredefinedIdentifier {
            lower_case,
            original,
            ..
        } => {
            if let Some(&index) = map_stack.get(lower_case) {
                *ast.get_symbol_index_mut().unwrap() = index;
            } else {
                trace!("Map stack does not contain {lower_case}: {map_stack:#?}");
                return Err(static_error(
                    ast.interval().clone(),
                    StaticErrorKind::UndeclaredSymbol {
                        lower_case: lower_case.clone(),
                        original: original.clone(),
                    },
                ));
            }
        }
        Type { .. } => { /* types cannot use symbols */ }
    }

    Ok(())
}
