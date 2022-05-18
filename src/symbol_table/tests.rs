use crate::parser::{PrimitiveTypeName, TypeName};
use crate::symbol_table::{FunctionType, Symbol, SymbolTable, SymbolType, VariableSymbolType};
use crate::{build_ast, build_symbol_table, initialise_logging, ReadIterator, Scanner};

#[test]
fn test_gcd_1() {
    let program = "program gcd;
begin
var n1, n2: integer;
read(n1);
read(n2);
if n2 > n1 then
begin
var tmp: integer;
tmp := n1;
n1 := n2;
n2 := tmp;
end;
while n2 > 0 do
begin
var new: integer;
new := n1 % n2;
n1 := n2;
n2 := new;
end;
writeln(n1);
end.
";

    initialise_logging();
    let scanner = Scanner::new(ReadIterator::new(program.as_bytes())).unwrap();
    let mut ast = build_ast(scanner).unwrap();
    let symbol_table = build_symbol_table(&mut ast).unwrap();
    assert_eq!(
        symbol_table,
        SymbolTable {
            symbols: vec![
                Symbol {
                    index: 0,
                    name: "".to_string(),
                    symbol_type: SymbolType::BuiltinFunction,
                },
                Symbol {
                    index: 1,
                    name: "read".to_string(),
                    symbol_type: SymbolType::BuiltinFunction,
                },
                Symbol {
                    index: 2,
                    name: "writeln".to_string(),
                    symbol_type: SymbolType::BuiltinFunction,
                },
                Symbol {
                    index: 3,
                    name: "assert".to_string(),
                    symbol_type: SymbolType::Function(FunctionType {
                        parameter_types: vec![TypeName::Primitive {
                            primitive_type: PrimitiveTypeName::Boolean
                        }],
                        return_type: None
                    }),
                },
                Symbol {
                    index: 4,
                    name: "gcd".to_string(),
                    symbol_type: SymbolType::Program,
                },
                Symbol {
                    index: 5,
                    name: "n1".to_string(),
                    symbol_type: SymbolType::Variable(VariableSymbolType {
                        var: false,
                        variable_type: TypeName::Primitive {
                            primitive_type: PrimitiveTypeName::Integer,
                        },
                    },),
                },
                Symbol {
                    index: 6,
                    name: "n2".to_string(),
                    symbol_type: SymbolType::Variable(VariableSymbolType {
                        var: false,
                        variable_type: TypeName::Primitive {
                            primitive_type: PrimitiveTypeName::Integer,
                        },
                    },),
                },
                Symbol {
                    index: 7,
                    name: "tmp".to_string(),
                    symbol_type: SymbolType::Variable(VariableSymbolType {
                        var: false,
                        variable_type: TypeName::Primitive {
                            primitive_type: PrimitiveTypeName::Integer,
                        },
                    },),
                },
                Symbol {
                    index: 8,
                    name: "new".to_string(),
                    symbol_type: SymbolType::Variable(VariableSymbolType {
                        var: false,
                        variable_type: TypeName::Primitive {
                            primitive_type: PrimitiveTypeName::Integer,
                        },
                    },),
                },
            ],
        },
    );
}

#[test]
fn test_gcd_2() {
    let program = "program gcd2;

function gcd(n1: integer, n2: integer): integer;
begin
    if n2 > n1 then
    begin
        var new: integer;
        new := n2 % n1;
        n2 := new;
    end
    else
    begin
        var new: integer;
        new := n1 % n2;
        n1 := n2;
        n2 := new;
    end;

    if n2 > 0 then
    begin
        return gcd(n1, n2);
    end
    else
    begin
        return n1;
    end;
end;

begin
    var n1, n2: integer;
    read(n1);
    read(n2);
    writeln(gcd(n1, n2));
end.";

    initialise_logging();
    let scanner = Scanner::new(ReadIterator::new(program.as_bytes())).unwrap();
    let mut ast = build_ast(scanner).unwrap();
    let symbol_table = build_symbol_table(&mut ast).unwrap();
    assert_eq!(
        symbol_table,
        SymbolTable {
            symbols: vec![
                Symbol {
                    index: 0,
                    name: "".to_string(),
                    symbol_type: SymbolType::BuiltinFunction,
                },
                Symbol {
                    index: 1,
                    name: "read".to_string(),
                    symbol_type: SymbolType::BuiltinFunction,
                },
                Symbol {
                    index: 2,
                    name: "writeln".to_string(),
                    symbol_type: SymbolType::BuiltinFunction,
                },
                Symbol {
                    index: 3,
                    name: "assert".to_string(),
                    symbol_type: SymbolType::Function(FunctionType {
                        parameter_types: vec![TypeName::Primitive {
                            primitive_type: PrimitiveTypeName::Boolean
                        }],
                        return_type: None
                    }),
                },
                Symbol {
                    index: 4,
                    name: "gcd2".to_string(),
                    symbol_type: SymbolType::Program,
                },
                Symbol {
                    index: 5,
                    name: "gcd".to_string(),
                    symbol_type: SymbolType::Function(FunctionType {
                        parameter_types: vec![
                            TypeName::Primitive {
                                primitive_type: PrimitiveTypeName::Integer,
                            },
                            TypeName::Primitive {
                                primitive_type: PrimitiveTypeName::Integer,
                            },
                        ],
                        return_type: Some(TypeName::Primitive {
                            primitive_type: PrimitiveTypeName::Integer,
                        },),
                    },),
                },
                Symbol {
                    index: 6,
                    name: "n1".to_string(),
                    symbol_type: SymbolType::Variable(VariableSymbolType {
                        var: false,
                        variable_type: TypeName::Primitive {
                            primitive_type: PrimitiveTypeName::Integer,
                        },
                    },),
                },
                Symbol {
                    index: 7,
                    name: "n2".to_string(),
                    symbol_type: SymbolType::Variable(VariableSymbolType {
                        var: false,
                        variable_type: TypeName::Primitive {
                            primitive_type: PrimitiveTypeName::Integer,
                        },
                    },),
                },
                Symbol {
                    index: 8,
                    name: "new".to_string(),
                    symbol_type: SymbolType::Variable(VariableSymbolType {
                        var: false,
                        variable_type: TypeName::Primitive {
                            primitive_type: PrimitiveTypeName::Integer,
                        },
                    },),
                },
                Symbol {
                    index: 9,
                    name: "new".to_string(),
                    symbol_type: SymbolType::Variable(VariableSymbolType {
                        var: false,
                        variable_type: TypeName::Primitive {
                            primitive_type: PrimitiveTypeName::Integer,
                        },
                    },),
                },
                Symbol {
                    index: 10,
                    name: "n1".to_string(),
                    symbol_type: SymbolType::Variable(VariableSymbolType {
                        var: false,
                        variable_type: TypeName::Primitive {
                            primitive_type: PrimitiveTypeName::Integer,
                        },
                    },),
                },
                Symbol {
                    index: 11,
                    name: "n2".to_string(),
                    symbol_type: SymbolType::Variable(VariableSymbolType {
                        var: false,
                        variable_type: TypeName::Primitive {
                            primitive_type: PrimitiveTypeName::Integer,
                        },
                    },),
                },
            ],
        }
    );
}

#[test]
fn test_gcd_3() {
    let program = "program gcd3;
procedure sort_pair_descending(pair: array[2] of integer);
begin
    if pair[0] < pair[1] then
    begin
        var tmp: integer;
        tmp := pair[0];
        pair[0] := pair[1];
        pair[1] := pair[0];
    end;
end;

procedure gcd(pair: array[2] of integer, var result: integer);
begin
    sort_pair_descending(pair);

    var new: integer;
    new := pair[0] % pair[1];
    pair[0] := pair[1];
    pair[1] := new;

    if pair[1] > 0 then
        return gcd(pair, result) {* semicolons are only used between statements in a block *}
    else
        result := pair[0]
    ;
end;

begin
    var pair: array[2] of integer;
    read(pair[0]);
    read(pair[1]);

    var result: integer;
    gcd(pair, result);
    writeln(result);
end.";

    initialise_logging();
    let scanner = Scanner::new(ReadIterator::new(program.as_bytes())).unwrap();
    let mut ast = build_ast(scanner).unwrap();
    let symbol_table = build_symbol_table(&mut ast).unwrap();
    assert_eq!(
        symbol_table,
        SymbolTable {
            symbols: vec![
                Symbol {
                    index: 0,
                    name: "".to_string(),
                    symbol_type: SymbolType::BuiltinFunction,
                },
                Symbol {
                    index: 1,
                    name: "read".to_string(),
                    symbol_type: SymbolType::BuiltinFunction,
                },
                Symbol {
                    index: 2,
                    name: "writeln".to_string(),
                    symbol_type: SymbolType::BuiltinFunction,
                },
                Symbol {
                    index: 3,
                    name: "assert".to_string(),
                    symbol_type: SymbolType::Function(FunctionType {
                        parameter_types: vec![TypeName::Primitive {
                            primitive_type: PrimitiveTypeName::Boolean
                        }],
                        return_type: None
                    }),
                },
                Symbol {
                    index: 4,
                    name: "gcd3".to_string(),
                    symbol_type: SymbolType::Program,
                },
                Symbol {
                    index: 5,
                    name: "sort_pair_descending".to_string(),
                    symbol_type: SymbolType::Function(FunctionType {
                        parameter_types: vec![TypeName::SizedArray {
                            primitive_type: PrimitiveTypeName::Integer,
                        },],
                        return_type: None,
                    },),
                },
                Symbol {
                    index: 6,
                    name: "pair".to_string(),
                    symbol_type: SymbolType::Variable(VariableSymbolType {
                        var: false,
                        variable_type: TypeName::SizedArray {
                            primitive_type: PrimitiveTypeName::Integer,
                        },
                    },),
                },
                Symbol {
                    index: 7,
                    name: "tmp".to_string(),
                    symbol_type: SymbolType::Variable(VariableSymbolType {
                        var: false,
                        variable_type: TypeName::Primitive {
                            primitive_type: PrimitiveTypeName::Integer,
                        },
                    },),
                },
                Symbol {
                    index: 8,
                    name: "gcd".to_string(),
                    symbol_type: SymbolType::Function(FunctionType {
                        parameter_types: vec![
                            TypeName::SizedArray {
                                primitive_type: PrimitiveTypeName::Integer,
                            },
                            TypeName::Primitive {
                                primitive_type: PrimitiveTypeName::Integer,
                            },
                        ],
                        return_type: None,
                    },),
                },
                Symbol {
                    index: 9,
                    name: "pair".to_string(),
                    symbol_type: SymbolType::Variable(VariableSymbolType {
                        var: false,
                        variable_type: TypeName::SizedArray {
                            primitive_type: PrimitiveTypeName::Integer,
                        },
                    },),
                },
                Symbol {
                    index: 10,
                    name: "result".to_string(),
                    symbol_type: SymbolType::Variable(VariableSymbolType {
                        var: true,
                        variable_type: TypeName::Primitive {
                            primitive_type: PrimitiveTypeName::Integer,
                        },
                    },),
                },
                Symbol {
                    index: 11,
                    name: "new".to_string(),
                    symbol_type: SymbolType::Variable(VariableSymbolType {
                        var: false,
                        variable_type: TypeName::Primitive {
                            primitive_type: PrimitiveTypeName::Integer,
                        },
                    },),
                },
                Symbol {
                    index: 12,
                    name: "pair".to_string(),
                    symbol_type: SymbolType::Variable(VariableSymbolType {
                        var: false,
                        variable_type: TypeName::SizedArray {
                            primitive_type: PrimitiveTypeName::Integer,
                        },
                    },),
                },
                Symbol {
                    index: 13,
                    name: "result".to_string(),
                    symbol_type: SymbolType::Variable(VariableSymbolType {
                        var: false,
                        variable_type: TypeName::Primitive {
                            primitive_type: PrimitiveTypeName::Integer,
                        },
                    },),
                },
            ],
        }
    );
}
