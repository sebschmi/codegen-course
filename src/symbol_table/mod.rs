use crate::error::{static_error, Result, StaticErrorKind};
use crate::parser::{AstNode, AstNodeKind, PrimitiveTypeName, TypeName};
use log::trace;
use std::collections::HashMap;
use std::hash::Hash;
use std::mem;

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
    main_frame_size: usize,
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
    /// A value.
    Value(TypeName),
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
    pub parameter_types: Vec<VariableSymbolType>,
    /// The optional return type.
    /// This is `None` for a procedure, and `Some(_)` for a function.
    pub return_type: Option<TypeName>,
    /// The size of the frame of this function, including its arguments.
    pub frame_size: usize,
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
    pub frame_offset: usize,
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
            main_frame_size: 0,
        };
        result.add_symbol("read".to_string(), SymbolType::BuiltinFunction);
        result.add_symbol("writeln".to_string(), SymbolType::BuiltinFunction);
        result.add_symbol(
            "assert".to_string(),
            SymbolType::Function(FunctionType {
                parameter_types: vec![VariableSymbolType {
                    var: false,
                    variable_type: TypeName::Primitive {
                        primitive_type: PrimitiveTypeName::Boolean,
                    },
                    frame_offset: 0,
                }],
                return_type: None,
                frame_size: 0,
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

    /// Return the symbol with the given index.
    pub fn get(&self, index: usize) -> Option<&Symbol> {
        if index == 0 {
            return None;
        }
        self.symbols.get(index)
    }

    /// Iterate over all symbols part of the main function (the "main-block").
    pub fn iter_symbols_in_main(&self) -> impl Iterator<Item = &Symbol> {
        // the main block is always last in the symbol table
        let offset = self
            .symbols
            .iter()
            .rev()
            .find(|symbol| symbol.index == 0)
            .unwrap()
            .index;
        self.symbols.iter().skip(offset)
    }

    /// Returns the size of the frame of the implicit main function.
    pub fn main_frame_size(&self) -> usize {
        self.main_frame_size
    }
}

impl Symbol {
    /// Return the type of the symbol.
    pub fn symbol_type(&self) -> &SymbolType {
        &self.symbol_type
    }

    /// Return the name (identifier) of the symbol in lower case.
    /// Multiple symbols can have the same name, if they are e.g. in different scopes.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Returns the index of this symbol in the symbol table.
    pub fn index(&self) -> usize {
        self.index
    }
}

impl SymbolType {
    /// Assume that this [SymbolType] is a variable, and return the corresponding [VariableSymbolType].
    /// Panics if the [SymbolType] is anything else.
    pub fn unwrap_variable(&self) -> &VariableSymbolType {
        if let SymbolType::Variable(result) = self {
            result
        } else {
            panic!("Not a variable: {self:?}");
        }
    }

    /// Assume that this [SymbolType] is a value, and return the corresponding [TypeName].
    /// Panics if the [SymbolType] is anything else.
    pub fn unwrap_value(&self) -> &TypeName {
        if let SymbolType::Value(result) = self {
            result
        } else {
            panic!("Not a value: {self:?}");
        }
    }

    /// Assume that this [SymbolType] is a function, and return the corresponding [FunctionType].
    /// Panics if the [SymbolType] is anything else.
    pub fn unwrap_function(&self) -> &FunctionType {
        if let SymbolType::Function(result) = self {
            result
        } else {
            panic!("Not a function: {self:?}");
        }
    }

    /// Returns true if this [SymbolType] is a variable.
    pub fn is_variable(&self) -> bool {
        matches!(self, SymbolType::Variable(_))
    }

    /// Returns true if this [SymbolType] is a value.
    pub fn is_value(&self) -> bool {
        matches!(self, SymbolType::Value(_))
    }

    /// Returns the type name of the variable or value represented by this [SymbolType].
    /// Panics if the [SymbolType] is not a variable or value.
    pub fn get_variable_or_value_type_name(&self) -> &TypeName {
        if let SymbolType::Variable(VariableSymbolType { variable_type, .. }) = self {
            variable_type
        } else if let SymbolType::Value(type_name) = self {
            type_name
        } else {
            panic!("Not a variable or value: {self:?}");
        }
    }

    /// Clones this [SymbolType] into a value if it is a variable, or just as a value if it is a value already.
    /// Panics if the [SymbolType] is not a variable or value.
    pub fn variable_into_value(&self) -> Self {
        if let SymbolType::Variable(VariableSymbolType { variable_type, .. }) = self {
            Self::Value(variable_type.clone())
        } else if let SymbolType::Value(_) = self {
            self.clone()
        } else {
            panic!("Not a variable: {self:?}");
        }
    }

    /// Clones this [SymbolType] into a variable if it is a value, or just as a variable if it is a variable already.
    /// The resulting variable will have `var == false` and `frame_offset == 0`.
    /// Panics if the [SymbolType] is not a variable or value.
    pub fn value_to_variable(&self) -> Self {
        if let SymbolType::Variable(_) = self {
            self.clone()
        } else if let SymbolType::Value(type_name) = self {
            SymbolType::Variable(VariableSymbolType {
                var: false,
                variable_type: type_name.clone(),
                frame_offset: 0,
            })
        } else {
            panic!("Not a variable: {self:?}");
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

/// Round up to the next multiple of step.
fn ceil_int(value: usize, step: usize) -> usize {
    // this can probably done without branch
    if value % step != 0 {
        value + step - (value % step)
    } else {
        value
    }
}

/// Build a single global symbol table and resolve all usages of symbols.
/// Symbols will also be assigned to an offset in the stack frame.
pub fn build_symbol_table(ast: &mut AstNode) -> Result<SymbolTable> {
    let mut symbol_table = SymbolTable::new();
    let mut map_stack = MapStack::default();
    map_stack.push();
    for symbol in symbol_table.iter() {
        map_stack.insert(symbol.name.to_string(), symbol.index);
    }

    build_symbol_table_recursively(ast, &mut symbol_table, &mut map_stack, &mut 0)
        .map(|()| symbol_table)
}

fn build_symbol_table_recursively(
    ast: &mut AstNode,
    symbol_table: &mut SymbolTable,
    map_stack: &mut MapStack<String, usize>,
    frame_offset: &mut usize,
) -> Result<()> {
    use AstNodeKind::*;

    match ast.kind() {
        Program => {
            map_stack.push();
            let identifier = ast.children_mut()[0].get_identifier_lower_case().unwrap();
            let index = symbol_table.add_symbol(identifier.to_string(), SymbolType::Program);
            map_stack.insert(identifier.to_string(), index);
            *ast.children_mut()[0].get_symbol_index_mut().unwrap() = index;
            for child in ast.children_mut().iter_mut().skip(1).rev().skip(1).rev() {
                // use a new frame offset for each procedure and function
                build_symbol_table_recursively(child, symbol_table, map_stack, &mut 0)?;
            }
            // only set main frame size if it contains any symbols
            let symbol_count = symbol_table.symbols.len();
            build_symbol_table_recursively(
                ast.children_mut().last_mut().unwrap(),
                symbol_table,
                map_stack,
                &mut 0,
            )?;
            if symbol_table.symbols.len() > symbol_count {
                let last_symbol = symbol_table.symbols.last().unwrap();
                let last_symbol_type = last_symbol.symbol_type.unwrap_variable();
                symbol_table.main_frame_size = ceil_int(
                    last_symbol_type.frame_offset + last_symbol_type.variable_type.type_size(),
                    mem::size_of::<usize>(),
                );
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
                    frame_size: 0,
                }),
            );
            map_stack.insert(identifier.to_string(), index);
            *ast.children_mut()[0].get_symbol_index_mut().unwrap() = index;
            map_stack.push();
            // only set frame size if it contains any symbols
            let symbol_count = symbol_table.symbols.len();
            // skip to not build the symbol table for the identifier twice
            for child in ast.children_mut().iter_mut().skip(1) {
                build_symbol_table_recursively(child, symbol_table, map_stack, frame_offset)?;
            }
            if symbol_table.symbols.len() > symbol_count {
                let last_symbol = symbol_table.symbols.last().unwrap();
                let last_symbol_type = last_symbol.symbol_type.unwrap_variable();
                // couldn't figure out a better way to satisfy the borrow checker than computing this outside of the branch
                let new_frame_size = ceil_int(
                    last_symbol_type.frame_offset + last_symbol_type.variable_type.type_size(),
                    mem::size_of::<usize>(),
                );
                if let SymbolType::Function(FunctionType { frame_size, .. }) =
                    &mut symbol_table.symbols[index].symbol_type
                {
                    *frame_size = new_frame_size;
                } else {
                    unreachable!()
                }
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
                    frame_size: 0,
                }),
            );
            map_stack.insert(identifier.to_string(), index);
            *ast.children_mut()[0].get_symbol_index_mut().unwrap() = index;
            map_stack.push();
            // only set frame size if it contains any symbols
            let symbol_count = symbol_table.symbols.len();
            // skip first to not build the symbol table for the identifier twice
            for child in ast.children_mut().iter_mut().skip(1) {
                build_symbol_table_recursively(child, symbol_table, map_stack, frame_offset)?;
            }
            if symbol_table.symbols.len() > symbol_count {
                let last_symbol = symbol_table.symbols.last().unwrap();
                let last_symbol_type = last_symbol.symbol_type.unwrap_variable();
                // couldn't figure out a better way to satisfy the borrow checker than computing this outside of the branch
                let new_frame_size = ceil_int(
                    last_symbol_type.frame_offset + last_symbol_type.variable_type.type_size(),
                    mem::size_of::<usize>(),
                );
                if let SymbolType::Function(FunctionType { frame_size, .. }) =
                    &mut symbol_table.symbols[index].symbol_type
                {
                    *frame_size = new_frame_size;
                } else {
                    unreachable!()
                }
            }
            map_stack.pop();
        }
        VarParameter => {
            let identifier = ast.children_mut()[0]
                .get_identifier_lower_case()
                .unwrap()
                .to_string();
            let variable_type = ast.get_variable_type();
            let type_size = mem::size_of::<usize>(); // var parameters are pointers in C
            *frame_offset = ceil_int(*frame_offset, type_size);
            let index = symbol_table.add_symbol(
                identifier.clone(),
                SymbolType::Variable(VariableSymbolType {
                    var: true,
                    variable_type,
                    frame_offset: *frame_offset,
                }),
            );
            *frame_offset += type_size;
            map_stack.insert(identifier, index);
            *ast.children_mut()[0].get_symbol_index_mut().unwrap() = index;
        }
        ValueParameter => {
            let identifier = ast.children_mut()[0]
                .get_identifier_lower_case()
                .unwrap()
                .to_string();
            let variable_type = ast.get_variable_type();
            let type_size = variable_type.type_size();
            *frame_offset = ceil_int(*frame_offset, type_size);
            let index = symbol_table.add_symbol(
                identifier.clone(),
                SymbolType::Variable(VariableSymbolType {
                    var: false,
                    variable_type,
                    frame_offset: *frame_offset,
                }),
            );
            *frame_offset += type_size;
            map_stack.insert(identifier, index);
            *ast.children_mut()[0].get_symbol_index_mut().unwrap() = index;
        }
        Block => {
            map_stack.push();
            for child in ast.children_mut() {
                build_symbol_table_recursively(child, symbol_table, map_stack, frame_offset)?;
            }
            map_stack.pop();
        }
        VariableDeclaration => {
            let variable_type = ast.get_variable_type();
            let type_size = variable_type.type_size();
            for child in ast.children_mut().iter_mut().rev().skip(1).rev() {
                let identifier = child.get_identifier_lower_case().unwrap().to_string();
                *frame_offset = ceil_int(*frame_offset, type_size);
                let index = symbol_table.add_symbol(
                    identifier.clone(),
                    SymbolType::Variable(VariableSymbolType {
                        var: false,
                        variable_type: variable_type.clone(),
                        frame_offset: *frame_offset,
                    }),
                );
                *frame_offset += type_size;
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
                build_symbol_table_recursively(child, symbol_table, map_stack, frame_offset)?;
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
