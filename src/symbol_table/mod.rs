use crate::error::{static_error, Result, StaticErrorKind};
use crate::parser::{AstNode, AstNodeKind, TypeName};
use log::trace;
use std::collections::HashMap;
use std::hash::Hash;
use std::num::NonZeroUsize;

#[cfg(test)]
mod tests;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SymbolTable {
    symbols: Vec<Symbol>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Symbol {
    /// The index of the symbol in the symbol table.
    index: usize,
    /// The name of the symbol as string (lower case).
    name: String,
    /// The type of the symbol.
    symbol_type: SymbolType,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum SymbolType {
    Function(FunctionType),
    Variable(VariableSymbolType),
    BuiltinFunction,
    Program,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FunctionType {
    parameter_types: Vec<TypeName>,
    return_type: Option<TypeName>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct VariableSymbolType {
    /// `true` if the variable is a var-parameter.
    var: bool,
    variable_type: TypeName,
}

impl SymbolTable {
    /// Create a new symbol table initialised with the builtin functions `read` and `writeln`.
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        let mut result = Self {
            symbols: vec![Symbol::new("".to_string(), SymbolType::BuiltinFunction)],
        };
        result.add_symbol(Symbol::new("read".to_string(), SymbolType::BuiltinFunction));
        result.add_symbol(Symbol::new(
            "writeln".to_string(),
            SymbolType::BuiltinFunction,
        ));
        result
    }

    pub fn add_symbol(&mut self, mut symbol: Symbol) -> NonZeroUsize {
        symbol.index = self.symbols.len();
        debug_assert!(symbol.index > 0);
        self.symbols.push(symbol);
        NonZeroUsize::new(self.symbols.len()).unwrap()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Symbol> {
        self.symbols.iter().skip(1)
    }
}

impl Symbol {
    pub fn new(name: String, symbol_type: SymbolType) -> Self {
        Self {
            index: 0,
            name,
            symbol_type,
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
            let index = symbol_table
                .add_symbol(Symbol::new(identifier.to_string(), SymbolType::Program))
                .get();
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
            let index = symbol_table
                .add_symbol(Symbol::new(
                    identifier.to_string(),
                    SymbolType::Function(FunctionType {
                        parameter_types,
                        return_type: None,
                    }),
                ))
                .get();
            map_stack.insert(identifier.to_string(), index);
            *ast.children_mut()[0].get_symbol_index_mut().unwrap() = index;
            map_stack.push();
            for child in ast.children_mut().iter_mut().skip(1) {
                build_symbol_table_recursively(child, symbol_table, map_stack)?;
            }
            map_stack.pop();
        }
        Function => {
            let (parameter_types, return_type) = ast.get_parameter_types_and_return_type();
            let identifier = ast.children_mut()[0].get_identifier_lower_case().unwrap();
            let index = symbol_table
                .add_symbol(Symbol::new(
                    identifier.to_string(),
                    SymbolType::Function(FunctionType {
                        parameter_types,
                        return_type: Some(return_type),
                    }),
                ))
                .get();
            map_stack.insert(identifier.to_string(), index);
            *ast.children_mut()[0].get_symbol_index_mut().unwrap() = index;
            map_stack.push();
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
            let index = symbol_table
                .add_symbol(Symbol::new(
                    identifier.clone(),
                    SymbolType::Variable(VariableSymbolType {
                        var: true,
                        variable_type: ast.get_variable_type(),
                    }),
                ))
                .get();
            map_stack.insert(identifier, index);
            *ast.children_mut()[0].get_symbol_index_mut().unwrap() = index;
        }
        ValueParameter => {
            let identifier = ast.children_mut()[0]
                .get_identifier_lower_case()
                .unwrap()
                .to_string();
            let index = symbol_table
                .add_symbol(Symbol::new(
                    identifier.clone(),
                    SymbolType::Variable(VariableSymbolType {
                        var: false,
                        variable_type: ast.get_variable_type(),
                    }),
                ))
                .get();
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
                let index = symbol_table
                    .add_symbol(Symbol::new(
                        identifier.clone(),
                        SymbolType::Variable(VariableSymbolType {
                            var: false,
                            variable_type: variable_type.clone(),
                        }),
                    ))
                    .get();
                map_stack.insert(identifier, index);
                *child.get_symbol_index_mut().unwrap() = index;
            }
        }
        AssignmentStatement | CallStatement | ReturnStatement | ReadStatement | WriteStatement
        | AssertStatement | IfStatement | WhileStatement | EqOperator | NeqOperator
        | LtOperator | LeqOperator | GeqOperator | GtOperator | NegOperator | AddOperator
        | SubOperator | OrOperator | NotOperator | MulOperator | DivOperator | ModOperator
        | AndOperator | DotOperator => {
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
