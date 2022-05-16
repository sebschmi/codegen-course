use crate::parser::{AstNode, AstNodeKind, PrimitiveTypeName, TypeName};
use crate::scanner::ScanInterval;
use crate::{build_ast, initialise_logging, ReadIterator, Scanner};

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
    let ast = build_ast(scanner).unwrap();
    assert_eq!(
        ast,
        AstNode {
            children: vec![
                AstNode {
                    children: vec![],
                    kind: AstNodeKind::Identifier {
                        original: "gcd".to_string(),
                        lower_case: "gcd".to_string(),
                    },
                    interval: ScanInterval {
                        start_line: 1,
                        end_line: 1,
                        start_column: 9,
                        end_column: 12,
                    },
                },
                AstNode {
                    children: vec![
                        AstNode {
                            children: vec![
                                AstNode {
                                    children: vec![],
                                    kind: AstNodeKind::Identifier {
                                        original: "n1".to_string(),
                                        lower_case: "n1".to_string(),
                                    },
                                    interval: ScanInterval {
                                        start_line: 3,
                                        end_line: 3,
                                        start_column: 5,
                                        end_column: 7,
                                    },
                                },
                                AstNode {
                                    children: vec![],
                                    kind: AstNodeKind::Identifier {
                                        original: "n2".to_string(),
                                        lower_case: "n2".to_string(),
                                    },
                                    interval: ScanInterval {
                                        start_line: 3,
                                        end_line: 3,
                                        start_column: 9,
                                        end_column: 11,
                                    },
                                },
                                AstNode {
                                    children: vec![],
                                    kind: AstNodeKind::Type {
                                        type_name: TypeName::Primitive {
                                            primitive_type: PrimitiveTypeName::Integer,
                                        },
                                    },
                                    interval: ScanInterval {
                                        start_line: 3,
                                        end_line: 3,
                                        start_column: 13,
                                        end_column: 20,
                                    },
                                },
                            ],
                            kind: AstNodeKind::VariableDeclaration,
                            interval: ScanInterval {
                                start_line: 3,
                                end_line: 3,
                                start_column: 1,
                                end_column: 20,
                            },
                        },
                        AstNode {
                            children: vec![
                                AstNode {
                                    children: vec![],
                                    kind: AstNodeKind::PredefinedIdentifier {
                                        original: "read".to_string(),
                                        lower_case: "read".to_string(),
                                    },
                                    interval: ScanInterval {
                                        start_line: 4,
                                        end_line: 4,
                                        start_column: 1,
                                        end_column: 5,
                                    },
                                },
                                AstNode {
                                    children: vec![],
                                    kind: AstNodeKind::Identifier {
                                        original: "n1".to_string(),
                                        lower_case: "n1".to_string(),
                                    },
                                    interval: ScanInterval {
                                        start_line: 4,
                                        end_line: 4,
                                        start_column: 6,
                                        end_column: 8,
                                    },
                                },
                            ],
                            kind: AstNodeKind::CallStatement,
                            interval: ScanInterval {
                                start_line: 4,
                                end_line: 4,
                                start_column: 1,
                                end_column: 8,
                            },
                        },
                        AstNode {
                            children: vec![
                                AstNode {
                                    children: vec![],
                                    kind: AstNodeKind::PredefinedIdentifier {
                                        original: "read".to_string(),
                                        lower_case: "read".to_string(),
                                    },
                                    interval: ScanInterval {
                                        start_line: 5,
                                        end_line: 5,
                                        start_column: 1,
                                        end_column: 5,
                                    },
                                },
                                AstNode {
                                    children: vec![],
                                    kind: AstNodeKind::Identifier {
                                        original: "n2".to_string(),
                                        lower_case: "n2".to_string(),
                                    },
                                    interval: ScanInterval {
                                        start_line: 5,
                                        end_line: 5,
                                        start_column: 6,
                                        end_column: 8,
                                    },
                                },
                            ],
                            kind: AstNodeKind::CallStatement,
                            interval: ScanInterval {
                                start_line: 5,
                                end_line: 5,
                                start_column: 1,
                                end_column: 8,
                            },
                        },
                        AstNode {
                            children: vec![
                                AstNode {
                                    children: vec![
                                        AstNode {
                                            children: vec![],
                                            kind: AstNodeKind::Identifier {
                                                original: "n2".to_string(),
                                                lower_case: "n2".to_string(),
                                            },
                                            interval: ScanInterval {
                                                start_line: 6,
                                                end_line: 6,
                                                start_column: 4,
                                                end_column: 6,
                                            },
                                        },
                                        AstNode {
                                            children: vec![],
                                            kind: AstNodeKind::Identifier {
                                                original: "n1".to_string(),
                                                lower_case: "n1".to_string(),
                                            },
                                            interval: ScanInterval {
                                                start_line: 6,
                                                end_line: 6,
                                                start_column: 9,
                                                end_column: 11,
                                            },
                                        },
                                    ],
                                    kind: AstNodeKind::GtOperator,
                                    interval: ScanInterval {
                                        start_line: 6,
                                        end_line: 6,
                                        start_column: 4,
                                        end_column: 11,
                                    },
                                },
                                AstNode {
                                    children: vec![
                                        AstNode {
                                            children: vec![
                                                AstNode {
                                                    children: vec![],
                                                    kind: AstNodeKind::Identifier {
                                                        original: "tmp".to_string(),
                                                        lower_case: "tmp".to_string(),
                                                    },
                                                    interval: ScanInterval {
                                                        start_line: 8,
                                                        end_line: 8,
                                                        start_column: 5,
                                                        end_column: 8,
                                                    },
                                                },
                                                AstNode {
                                                    children: vec![],
                                                    kind: AstNodeKind::Type {
                                                        type_name: TypeName::Primitive {
                                                            primitive_type:
                                                                PrimitiveTypeName::Integer,
                                                        },
                                                    },
                                                    interval: ScanInterval {
                                                        start_line: 8,
                                                        end_line: 8,
                                                        start_column: 10,
                                                        end_column: 17,
                                                    },
                                                },
                                            ],
                                            kind: AstNodeKind::VariableDeclaration,
                                            interval: ScanInterval {
                                                start_line: 8,
                                                end_line: 8,
                                                start_column: 1,
                                                end_column: 17,
                                            },
                                        },
                                        AstNode {
                                            children: vec![
                                                AstNode {
                                                    children: vec![],
                                                    kind: AstNodeKind::Identifier {
                                                        original: "tmp".to_string(),
                                                        lower_case: "tmp".to_string(),
                                                    },
                                                    interval: ScanInterval {
                                                        start_line: 9,
                                                        end_line: 9,
                                                        start_column: 1,
                                                        end_column: 4,
                                                    },
                                                },
                                                AstNode {
                                                    children: vec![],
                                                    kind: AstNodeKind::Identifier {
                                                        original: "n1".to_string(),
                                                        lower_case: "n1".to_string(),
                                                    },
                                                    interval: ScanInterval {
                                                        start_line: 9,
                                                        end_line: 9,
                                                        start_column: 8,
                                                        end_column: 10,
                                                    },
                                                },
                                            ],
                                            kind: AstNodeKind::AssignmentStatement,
                                            interval: ScanInterval {
                                                start_line: 9,
                                                end_line: 9,
                                                start_column: 1,
                                                end_column: 10,
                                            },
                                        },
                                        AstNode {
                                            children: vec![
                                                AstNode {
                                                    children: vec![],
                                                    kind: AstNodeKind::Identifier {
                                                        original: "n1".to_string(),
                                                        lower_case: "n1".to_string(),
                                                    },
                                                    interval: ScanInterval {
                                                        start_line: 10,
                                                        end_line: 10,
                                                        start_column: 1,
                                                        end_column: 3,
                                                    },
                                                },
                                                AstNode {
                                                    children: vec![],
                                                    kind: AstNodeKind::Identifier {
                                                        original: "n2".to_string(),
                                                        lower_case: "n2".to_string(),
                                                    },
                                                    interval: ScanInterval {
                                                        start_line: 10,
                                                        end_line: 10,
                                                        start_column: 7,
                                                        end_column: 9,
                                                    },
                                                },
                                            ],
                                            kind: AstNodeKind::AssignmentStatement,
                                            interval: ScanInterval {
                                                start_line: 10,
                                                end_line: 10,
                                                start_column: 1,
                                                end_column: 9,
                                            },
                                        },
                                        AstNode {
                                            children: vec![
                                                AstNode {
                                                    children: vec![],
                                                    kind: AstNodeKind::Identifier {
                                                        original: "n2".to_string(),
                                                        lower_case: "n2".to_string(),
                                                    },
                                                    interval: ScanInterval {
                                                        start_line: 11,
                                                        end_line: 11,
                                                        start_column: 1,
                                                        end_column: 3,
                                                    },
                                                },
                                                AstNode {
                                                    children: vec![],
                                                    kind: AstNodeKind::Identifier {
                                                        original: "tmp".to_string(),
                                                        lower_case: "tmp".to_string(),
                                                    },
                                                    interval: ScanInterval {
                                                        start_line: 11,
                                                        end_line: 11,
                                                        start_column: 7,
                                                        end_column: 10,
                                                    },
                                                },
                                            ],
                                            kind: AstNodeKind::AssignmentStatement,
                                            interval: ScanInterval {
                                                start_line: 11,
                                                end_line: 11,
                                                start_column: 1,
                                                end_column: 10,
                                            },
                                        },
                                    ],
                                    kind: AstNodeKind::Block,
                                    interval: ScanInterval {
                                        start_line: 7,
                                        end_line: 12,
                                        start_column: 1,
                                        end_column: 4,
                                    },
                                },
                            ],
                            kind: AstNodeKind::IfStatement,
                            interval: ScanInterval {
                                start_line: 6,
                                end_line: 12,
                                start_column: 1,
                                end_column: 4,
                            },
                        },
                        AstNode {
                            children: vec![
                                AstNode {
                                    children: vec![
                                        AstNode {
                                            children: vec![],
                                            kind: AstNodeKind::Identifier {
                                                original: "n2".to_string(),
                                                lower_case: "n2".to_string(),
                                            },
                                            interval: ScanInterval {
                                                start_line: 13,
                                                end_line: 13,
                                                start_column: 7,
                                                end_column: 9,
                                            },
                                        },
                                        AstNode {
                                            children: vec![],
                                            kind: AstNodeKind::Literal {
                                                literal_type: TypeName::Primitive {
                                                    primitive_type: PrimitiveTypeName::Integer,
                                                },
                                                value: "0".to_string(),
                                            },
                                            interval: ScanInterval {
                                                start_line: 13,
                                                end_line: 13,
                                                start_column: 12,
                                                end_column: 13,
                                            },
                                        },
                                    ],
                                    kind: AstNodeKind::GtOperator,
                                    interval: ScanInterval {
                                        start_line: 13,
                                        end_line: 13,
                                        start_column: 7,
                                        end_column: 13,
                                    },
                                },
                                AstNode {
                                    children: vec![
                                        AstNode {
                                            children: vec![
                                                AstNode {
                                                    children: vec![],
                                                    kind: AstNodeKind::Identifier {
                                                        original: "new".to_string(),
                                                        lower_case: "new".to_string(),
                                                    },
                                                    interval: ScanInterval {
                                                        start_line: 15,
                                                        end_line: 15,
                                                        start_column: 5,
                                                        end_column: 8,
                                                    },
                                                },
                                                AstNode {
                                                    children: vec![],
                                                    kind: AstNodeKind::Type {
                                                        type_name: TypeName::Primitive {
                                                            primitive_type:
                                                                PrimitiveTypeName::Integer,
                                                        },
                                                    },
                                                    interval: ScanInterval {
                                                        start_line: 15,
                                                        end_line: 15,
                                                        start_column: 10,
                                                        end_column: 17,
                                                    },
                                                },
                                            ],
                                            kind: AstNodeKind::VariableDeclaration,
                                            interval: ScanInterval {
                                                start_line: 15,
                                                end_line: 15,
                                                start_column: 1,
                                                end_column: 17,
                                            },
                                        },
                                        AstNode {
                                            children: vec![
                                                AstNode {
                                                    children: vec![],
                                                    kind: AstNodeKind::Identifier {
                                                        original: "new".to_string(),
                                                        lower_case: "new".to_string(),
                                                    },
                                                    interval: ScanInterval {
                                                        start_line: 16,
                                                        end_line: 16,
                                                        start_column: 1,
                                                        end_column: 4,
                                                    },
                                                },
                                                AstNode {
                                                    children: vec![
                                                        AstNode {
                                                            children: vec![],
                                                            kind: AstNodeKind::Identifier {
                                                                original: "n1".to_string(),
                                                                lower_case: "n1".to_string(),
                                                            },
                                                            interval: ScanInterval {
                                                                start_line: 16,
                                                                end_line: 16,
                                                                start_column: 8,
                                                                end_column: 10,
                                                            },
                                                        },
                                                        AstNode {
                                                            children: vec![],
                                                            kind: AstNodeKind::Identifier {
                                                                original: "n2".to_string(),
                                                                lower_case: "n2".to_string(),
                                                            },
                                                            interval: ScanInterval {
                                                                start_line: 16,
                                                                end_line: 16,
                                                                start_column: 13,
                                                                end_column: 15,
                                                            },
                                                        },
                                                    ],
                                                    kind: AstNodeKind::ModOperator,
                                                    interval: ScanInterval {
                                                        start_line: 16,
                                                        end_line: 16,
                                                        start_column: 11,
                                                        end_column: 15,
                                                    },
                                                },
                                            ],
                                            kind: AstNodeKind::AssignmentStatement,
                                            interval: ScanInterval {
                                                start_line: 16,
                                                end_line: 16,
                                                start_column: 1,
                                                end_column: 15,
                                            },
                                        },
                                        AstNode {
                                            children: vec![
                                                AstNode {
                                                    children: vec![],
                                                    kind: AstNodeKind::Identifier {
                                                        original: "n1".to_string(),
                                                        lower_case: "n1".to_string(),
                                                    },
                                                    interval: ScanInterval {
                                                        start_line: 17,
                                                        end_line: 17,
                                                        start_column: 1,
                                                        end_column: 3,
                                                    },
                                                },
                                                AstNode {
                                                    children: vec![],
                                                    kind: AstNodeKind::Identifier {
                                                        original: "n2".to_string(),
                                                        lower_case: "n2".to_string(),
                                                    },
                                                    interval: ScanInterval {
                                                        start_line: 17,
                                                        end_line: 17,
                                                        start_column: 7,
                                                        end_column: 9,
                                                    },
                                                },
                                            ],
                                            kind: AstNodeKind::AssignmentStatement,
                                            interval: ScanInterval {
                                                start_line: 17,
                                                end_line: 17,
                                                start_column: 1,
                                                end_column: 9,
                                            },
                                        },
                                        AstNode {
                                            children: vec![
                                                AstNode {
                                                    children: vec![],
                                                    kind: AstNodeKind::Identifier {
                                                        original: "n2".to_string(),
                                                        lower_case: "n2".to_string(),
                                                    },
                                                    interval: ScanInterval {
                                                        start_line: 18,
                                                        end_line: 18,
                                                        start_column: 1,
                                                        end_column: 3,
                                                    },
                                                },
                                                AstNode {
                                                    children: vec![],
                                                    kind: AstNodeKind::Identifier {
                                                        original: "new".to_string(),
                                                        lower_case: "new".to_string(),
                                                    },
                                                    interval: ScanInterval {
                                                        start_line: 18,
                                                        end_line: 18,
                                                        start_column: 7,
                                                        end_column: 10,
                                                    },
                                                },
                                            ],
                                            kind: AstNodeKind::AssignmentStatement,
                                            interval: ScanInterval {
                                                start_line: 18,
                                                end_line: 18,
                                                start_column: 1,
                                                end_column: 10,
                                            },
                                        },
                                    ],
                                    kind: AstNodeKind::Block,
                                    interval: ScanInterval {
                                        start_line: 14,
                                        end_line: 19,
                                        start_column: 1,
                                        end_column: 4,
                                    },
                                },
                            ],
                            kind: AstNodeKind::WhileStatement,
                            interval: ScanInterval {
                                start_line: 13,
                                end_line: 19,
                                start_column: 1,
                                end_column: 4,
                            },
                        },
                        AstNode {
                            children: vec![
                                AstNode {
                                    children: vec![],
                                    kind: AstNodeKind::PredefinedIdentifier {
                                        original: "writeln".to_string(),
                                        lower_case: "writeln".to_string(),
                                    },
                                    interval: ScanInterval {
                                        start_line: 20,
                                        end_line: 20,
                                        start_column: 1,
                                        end_column: 8,
                                    },
                                },
                                AstNode {
                                    children: vec![],
                                    kind: AstNodeKind::Identifier {
                                        original: "n1".to_string(),
                                        lower_case: "n1".to_string(),
                                    },
                                    interval: ScanInterval {
                                        start_line: 20,
                                        end_line: 20,
                                        start_column: 9,
                                        end_column: 11,
                                    },
                                },
                            ],
                            kind: AstNodeKind::CallStatement,
                            interval: ScanInterval {
                                start_line: 20,
                                end_line: 20,
                                start_column: 1,
                                end_column: 11,
                            },
                        },
                    ],
                    kind: AstNodeKind::Block,
                    interval: ScanInterval {
                        start_line: 2,
                        end_line: 21,
                        start_column: 1,
                        end_column: 4,
                    },
                },
            ],
            kind: AstNodeKind::Program,
            interval: ScanInterval {
                start_line: 1,
                end_line: 21,
                start_column: 1,
                end_column: 5,
            },
        }
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
    let ast = build_ast(scanner).unwrap();
    assert_eq!(
        ast,
        AstNode {
            children: vec![
                AstNode {
                    children: vec![],
                    kind: AstNodeKind::Identifier {
                        original: "gcd2".to_string(),
                        lower_case: "gcd2".to_string(),
                    },
                    interval: ScanInterval {
                        start_line: 1,
                        end_line: 1,
                        start_column: 9,
                        end_column: 13,
                    },
                },
                AstNode {
                    children: vec![
                        AstNode {
                            children: vec![],
                            kind: AstNodeKind::Identifier {
                                original: "gcd".to_string(),
                                lower_case: "gcd".to_string(),
                            },
                            interval: ScanInterval {
                                start_line: 3,
                                end_line: 3,
                                start_column: 10,
                                end_column: 13,
                            },
                        },
                        AstNode {
                            children: vec![
                                AstNode {
                                    children: vec![],
                                    kind: AstNodeKind::Identifier {
                                        original: "n1".to_string(),
                                        lower_case: "n1".to_string(),
                                    },
                                    interval: ScanInterval {
                                        start_line: 3,
                                        end_line: 3,
                                        start_column: 14,
                                        end_column: 16,
                                    },
                                },
                                AstNode {
                                    children: vec![],
                                    kind: AstNodeKind::Type {
                                        type_name: TypeName::Primitive {
                                            primitive_type: PrimitiveTypeName::Integer,
                                        },
                                    },
                                    interval: ScanInterval {
                                        start_line: 3,
                                        end_line: 3,
                                        start_column: 18,
                                        end_column: 25,
                                    },
                                },
                            ],
                            kind: AstNodeKind::ValueParameter,
                            interval: ScanInterval {
                                start_line: 3,
                                end_line: 3,
                                start_column: 14,
                                end_column: 25,
                            },
                        },
                        AstNode {
                            children: vec![
                                AstNode {
                                    children: vec![],
                                    kind: AstNodeKind::Identifier {
                                        original: "n2".to_string(),
                                        lower_case: "n2".to_string(),
                                    },
                                    interval: ScanInterval {
                                        start_line: 3,
                                        end_line: 3,
                                        start_column: 27,
                                        end_column: 29,
                                    },
                                },
                                AstNode {
                                    children: vec![],
                                    kind: AstNodeKind::Type {
                                        type_name: TypeName::Primitive {
                                            primitive_type: PrimitiveTypeName::Integer,
                                        },
                                    },
                                    interval: ScanInterval {
                                        start_line: 3,
                                        end_line: 3,
                                        start_column: 31,
                                        end_column: 38,
                                    },
                                },
                            ],
                            kind: AstNodeKind::ValueParameter,
                            interval: ScanInterval {
                                start_line: 3,
                                end_line: 3,
                                start_column: 27,
                                end_column: 38,
                            },
                        },
                        AstNode {
                            children: vec![],
                            kind: AstNodeKind::Type {
                                type_name: TypeName::Primitive {
                                    primitive_type: PrimitiveTypeName::Integer,
                                },
                            },
                            interval: ScanInterval {
                                start_line: 3,
                                end_line: 3,
                                start_column: 41,
                                end_column: 48,
                            },
                        },
                        AstNode {
                            children: vec![
                                AstNode {
                                    children: vec![
                                        AstNode {
                                            children: vec![
                                                AstNode {
                                                    children: vec![],
                                                    kind: AstNodeKind::Identifier {
                                                        original: "n2".to_string(),
                                                        lower_case: "n2".to_string(),
                                                    },
                                                    interval: ScanInterval {
                                                        start_line: 5,
                                                        end_line: 5,
                                                        start_column: 8,
                                                        end_column: 10,
                                                    },
                                                },
                                                AstNode {
                                                    children: vec![],
                                                    kind: AstNodeKind::Identifier {
                                                        original: "n1".to_string(),
                                                        lower_case: "n1".to_string(),
                                                    },
                                                    interval: ScanInterval {
                                                        start_line: 5,
                                                        end_line: 5,
                                                        start_column: 13,
                                                        end_column: 15,
                                                    },
                                                },
                                            ],
                                            kind: AstNodeKind::GtOperator,
                                            interval: ScanInterval {
                                                start_line: 5,
                                                end_line: 5,
                                                start_column: 8,
                                                end_column: 15,
                                            },
                                        },
                                        AstNode {
                                            children: vec![
                                                AstNode {
                                                    children: vec![
                                                        AstNode {
                                                            children: vec![],
                                                            kind: AstNodeKind::Identifier {
                                                                original: "new".to_string(),
                                                                lower_case: "new".to_string(),
                                                            },
                                                            interval: ScanInterval {
                                                                start_line: 7,
                                                                end_line: 7,
                                                                start_column: 13,
                                                                end_column: 16,
                                                            },
                                                        },
                                                        AstNode {
                                                            children: vec![],
                                                            kind: AstNodeKind::Type {
                                                                type_name: TypeName::Primitive {
                                                                    primitive_type:
                                                                        PrimitiveTypeName::Integer,
                                                                },
                                                            },
                                                            interval: ScanInterval {
                                                                start_line: 7,
                                                                end_line: 7,
                                                                start_column: 18,
                                                                end_column: 25,
                                                            },
                                                        },
                                                    ],
                                                    kind: AstNodeKind::VariableDeclaration,
                                                    interval: ScanInterval {
                                                        start_line: 7,
                                                        end_line: 7,
                                                        start_column: 9,
                                                        end_column: 25,
                                                    },
                                                },
                                                AstNode {
                                                    children: vec![
                                                        AstNode {
                                                            children: vec![],
                                                            kind: AstNodeKind::Identifier {
                                                                original: "new".to_string(),
                                                                lower_case: "new".to_string(),
                                                            },
                                                            interval: ScanInterval {
                                                                start_line: 8,
                                                                end_line: 8,
                                                                start_column: 9,
                                                                end_column: 12,
                                                            },
                                                        },
                                                        AstNode {
                                                            children: vec![
                                                                AstNode {
                                                                    children: vec![],
                                                                    kind: AstNodeKind::Identifier {
                                                                        original: "n2".to_string(),
                                                                        lower_case: "n2"
                                                                            .to_string(),
                                                                    },
                                                                    interval: ScanInterval {
                                                                        start_line: 8,
                                                                        end_line: 8,
                                                                        start_column: 16,
                                                                        end_column: 18,
                                                                    },
                                                                },
                                                                AstNode {
                                                                    children: vec![],
                                                                    kind: AstNodeKind::Identifier {
                                                                        original: "n1".to_string(),
                                                                        lower_case: "n1"
                                                                            .to_string(),
                                                                    },
                                                                    interval: ScanInterval {
                                                                        start_line: 8,
                                                                        end_line: 8,
                                                                        start_column: 21,
                                                                        end_column: 23,
                                                                    },
                                                                },
                                                            ],
                                                            kind: AstNodeKind::ModOperator,
                                                            interval: ScanInterval {
                                                                start_line: 8,
                                                                end_line: 8,
                                                                start_column: 19,
                                                                end_column: 23,
                                                            },
                                                        },
                                                    ],
                                                    kind: AstNodeKind::AssignmentStatement,
                                                    interval: ScanInterval {
                                                        start_line: 8,
                                                        end_line: 8,
                                                        start_column: 9,
                                                        end_column: 23,
                                                    },
                                                },
                                                AstNode {
                                                    children: vec![
                                                        AstNode {
                                                            children: vec![],
                                                            kind: AstNodeKind::Identifier {
                                                                original: "n2".to_string(),
                                                                lower_case: "n2".to_string(),
                                                            },
                                                            interval: ScanInterval {
                                                                start_line: 9,
                                                                end_line: 9,
                                                                start_column: 9,
                                                                end_column: 11,
                                                            },
                                                        },
                                                        AstNode {
                                                            children: vec![],
                                                            kind: AstNodeKind::Identifier {
                                                                original: "new".to_string(),
                                                                lower_case: "new".to_string(),
                                                            },
                                                            interval: ScanInterval {
                                                                start_line: 9,
                                                                end_line: 9,
                                                                start_column: 15,
                                                                end_column: 18,
                                                            },
                                                        },
                                                    ],
                                                    kind: AstNodeKind::AssignmentStatement,
                                                    interval: ScanInterval {
                                                        start_line: 9,
                                                        end_line: 9,
                                                        start_column: 9,
                                                        end_column: 18,
                                                    },
                                                },
                                            ],
                                            kind: AstNodeKind::Block,
                                            interval: ScanInterval {
                                                start_line: 6,
                                                end_line: 10,
                                                start_column: 5,
                                                end_column: 8,
                                            },
                                        },
                                        AstNode {
                                            children: vec![
                                                AstNode {
                                                    children: vec![
                                                        AstNode {
                                                            children: vec![],
                                                            kind: AstNodeKind::Identifier {
                                                                original: "new".to_string(),
                                                                lower_case: "new".to_string(),
                                                            },
                                                            interval: ScanInterval {
                                                                start_line: 13,
                                                                end_line: 13,
                                                                start_column: 13,
                                                                end_column: 16,
                                                            },
                                                        },
                                                        AstNode {
                                                            children: vec![],
                                                            kind: AstNodeKind::Type {
                                                                type_name: TypeName::Primitive {
                                                                    primitive_type:
                                                                        PrimitiveTypeName::Integer,
                                                                },
                                                            },
                                                            interval: ScanInterval {
                                                                start_line: 13,
                                                                end_line: 13,
                                                                start_column: 18,
                                                                end_column: 25,
                                                            },
                                                        },
                                                    ],
                                                    kind: AstNodeKind::VariableDeclaration,
                                                    interval: ScanInterval {
                                                        start_line: 13,
                                                        end_line: 13,
                                                        start_column: 9,
                                                        end_column: 25,
                                                    },
                                                },
                                                AstNode {
                                                    children: vec![
                                                        AstNode {
                                                            children: vec![],
                                                            kind: AstNodeKind::Identifier {
                                                                original: "new".to_string(),
                                                                lower_case: "new".to_string(),
                                                            },
                                                            interval: ScanInterval {
                                                                start_line: 14,
                                                                end_line: 14,
                                                                start_column: 9,
                                                                end_column: 12,
                                                            },
                                                        },
                                                        AstNode {
                                                            children: vec![
                                                                AstNode {
                                                                    children: vec![],
                                                                    kind: AstNodeKind::Identifier {
                                                                        original: "n1".to_string(),
                                                                        lower_case: "n1"
                                                                            .to_string(),
                                                                    },
                                                                    interval: ScanInterval {
                                                                        start_line: 14,
                                                                        end_line: 14,
                                                                        start_column: 16,
                                                                        end_column: 18,
                                                                    },
                                                                },
                                                                AstNode {
                                                                    children: vec![],
                                                                    kind: AstNodeKind::Identifier {
                                                                        original: "n2".to_string(),
                                                                        lower_case: "n2"
                                                                            .to_string(),
                                                                    },
                                                                    interval: ScanInterval {
                                                                        start_line: 14,
                                                                        end_line: 14,
                                                                        start_column: 21,
                                                                        end_column: 23,
                                                                    },
                                                                },
                                                            ],
                                                            kind: AstNodeKind::ModOperator,
                                                            interval: ScanInterval {
                                                                start_line: 14,
                                                                end_line: 14,
                                                                start_column: 19,
                                                                end_column: 23,
                                                            },
                                                        },
                                                    ],
                                                    kind: AstNodeKind::AssignmentStatement,
                                                    interval: ScanInterval {
                                                        start_line: 14,
                                                        end_line: 14,
                                                        start_column: 9,
                                                        end_column: 23,
                                                    },
                                                },
                                                AstNode {
                                                    children: vec![
                                                        AstNode {
                                                            children: vec![],
                                                            kind: AstNodeKind::Identifier {
                                                                original: "n1".to_string(),
                                                                lower_case: "n1".to_string(),
                                                            },
                                                            interval: ScanInterval {
                                                                start_line: 15,
                                                                end_line: 15,
                                                                start_column: 9,
                                                                end_column: 11,
                                                            },
                                                        },
                                                        AstNode {
                                                            children: vec![],
                                                            kind: AstNodeKind::Identifier {
                                                                original: "n2".to_string(),
                                                                lower_case: "n2".to_string(),
                                                            },
                                                            interval: ScanInterval {
                                                                start_line: 15,
                                                                end_line: 15,
                                                                start_column: 15,
                                                                end_column: 17,
                                                            },
                                                        },
                                                    ],
                                                    kind: AstNodeKind::AssignmentStatement,
                                                    interval: ScanInterval {
                                                        start_line: 15,
                                                        end_line: 15,
                                                        start_column: 9,
                                                        end_column: 17,
                                                    },
                                                },
                                                AstNode {
                                                    children: vec![
                                                        AstNode {
                                                            children: vec![],
                                                            kind: AstNodeKind::Identifier {
                                                                original: "n2".to_string(),
                                                                lower_case: "n2".to_string(),
                                                            },
                                                            interval: ScanInterval {
                                                                start_line: 16,
                                                                end_line: 16,
                                                                start_column: 9,
                                                                end_column: 11,
                                                            },
                                                        },
                                                        AstNode {
                                                            children: vec![],
                                                            kind: AstNodeKind::Identifier {
                                                                original: "new".to_string(),
                                                                lower_case: "new".to_string(),
                                                            },
                                                            interval: ScanInterval {
                                                                start_line: 16,
                                                                end_line: 16,
                                                                start_column: 15,
                                                                end_column: 18,
                                                            },
                                                        },
                                                    ],
                                                    kind: AstNodeKind::AssignmentStatement,
                                                    interval: ScanInterval {
                                                        start_line: 16,
                                                        end_line: 16,
                                                        start_column: 9,
                                                        end_column: 18,
                                                    },
                                                },
                                            ],
                                            kind: AstNodeKind::Block,
                                            interval: ScanInterval {
                                                start_line: 12,
                                                end_line: 17,
                                                start_column: 5,
                                                end_column: 8,
                                            },
                                        },
                                    ],
                                    kind: AstNodeKind::IfStatement,
                                    interval: ScanInterval {
                                        start_line: 5,
                                        end_line: 17,
                                        start_column: 5,
                                        end_column: 8,
                                    },
                                },
                                AstNode {
                                    children: vec![
                                        AstNode {
                                            children: vec![
                                                AstNode {
                                                    children: vec![],
                                                    kind: AstNodeKind::Identifier {
                                                        original: "n2".to_string(),
                                                        lower_case: "n2".to_string(),
                                                    },
                                                    interval: ScanInterval {
                                                        start_line: 19,
                                                        end_line: 19,
                                                        start_column: 8,
                                                        end_column: 10,
                                                    },
                                                },
                                                AstNode {
                                                    children: vec![],
                                                    kind: AstNodeKind::Literal {
                                                        literal_type: TypeName::Primitive {
                                                            primitive_type:
                                                                PrimitiveTypeName::Integer,
                                                        },
                                                        value: "0".to_string(),
                                                    },
                                                    interval: ScanInterval {
                                                        start_line: 19,
                                                        end_line: 19,
                                                        start_column: 13,
                                                        end_column: 14,
                                                    },
                                                },
                                            ],
                                            kind: AstNodeKind::GtOperator,
                                            interval: ScanInterval {
                                                start_line: 19,
                                                end_line: 19,
                                                start_column: 8,
                                                end_column: 14,
                                            },
                                        },
                                        AstNode {
                                            children: vec![AstNode {
                                                children: vec![AstNode {
                                                    children: vec![
                                                        AstNode {
                                                            children: vec![],
                                                            kind: AstNodeKind::Identifier {
                                                                original: "gcd".to_string(),
                                                                lower_case: "gcd".to_string(),
                                                            },
                                                            interval: ScanInterval {
                                                                start_line: 21,
                                                                end_line: 21,
                                                                start_column: 16,
                                                                end_column: 19,
                                                            },
                                                        },
                                                        AstNode {
                                                            children: vec![],
                                                            kind: AstNodeKind::Identifier {
                                                                original: "n1".to_string(),
                                                                lower_case: "n1".to_string(),
                                                            },
                                                            interval: ScanInterval {
                                                                start_line: 21,
                                                                end_line: 21,
                                                                start_column: 20,
                                                                end_column: 22,
                                                            },
                                                        },
                                                        AstNode {
                                                            children: vec![],
                                                            kind: AstNodeKind::Identifier {
                                                                original: "n2".to_string(),
                                                                lower_case: "n2".to_string(),
                                                            },
                                                            interval: ScanInterval {
                                                                start_line: 21,
                                                                end_line: 21,
                                                                start_column: 24,
                                                                end_column: 26,
                                                            },
                                                        },
                                                    ],
                                                    kind: AstNodeKind::CallStatement,
                                                    interval: ScanInterval {
                                                        start_line: 21,
                                                        end_line: 21,
                                                        start_column: 16,
                                                        end_column: 26,
                                                    },
                                                },],
                                                kind: AstNodeKind::ReturnStatement,
                                                interval: ScanInterval {
                                                    start_line: 21,
                                                    end_line: 21,
                                                    start_column: 9,
                                                    end_column: 26,
                                                },
                                            },],
                                            kind: AstNodeKind::Block,
                                            interval: ScanInterval {
                                                start_line: 20,
                                                end_line: 22,
                                                start_column: 5,
                                                end_column: 8,
                                            },
                                        },
                                        AstNode {
                                            children: vec![AstNode {
                                                children: vec![AstNode {
                                                    children: vec![],
                                                    kind: AstNodeKind::Identifier {
                                                        original: "n1".to_string(),
                                                        lower_case: "n1".to_string(),
                                                    },
                                                    interval: ScanInterval {
                                                        start_line: 25,
                                                        end_line: 25,
                                                        start_column: 16,
                                                        end_column: 18,
                                                    },
                                                },],
                                                kind: AstNodeKind::ReturnStatement,
                                                interval: ScanInterval {
                                                    start_line: 25,
                                                    end_line: 25,
                                                    start_column: 9,
                                                    end_column: 18,
                                                },
                                            },],
                                            kind: AstNodeKind::Block,
                                            interval: ScanInterval {
                                                start_line: 24,
                                                end_line: 26,
                                                start_column: 5,
                                                end_column: 8,
                                            },
                                        },
                                    ],
                                    kind: AstNodeKind::IfStatement,
                                    interval: ScanInterval {
                                        start_line: 19,
                                        end_line: 26,
                                        start_column: 5,
                                        end_column: 8,
                                    },
                                },
                            ],
                            kind: AstNodeKind::Block,
                            interval: ScanInterval {
                                start_line: 4,
                                end_line: 27,
                                start_column: 1,
                                end_column: 4,
                            },
                        },
                    ],
                    kind: AstNodeKind::Procedure,
                    interval: ScanInterval {
                        start_line: 3,
                        end_line: 27,
                        start_column: 1,
                        end_column: 5,
                    },
                },
                AstNode {
                    children: vec![
                        AstNode {
                            children: vec![
                                AstNode {
                                    children: vec![],
                                    kind: AstNodeKind::Identifier {
                                        original: "n1".to_string(),
                                        lower_case: "n1".to_string(),
                                    },
                                    interval: ScanInterval {
                                        start_line: 30,
                                        end_line: 30,
                                        start_column: 9,
                                        end_column: 11,
                                    },
                                },
                                AstNode {
                                    children: vec![],
                                    kind: AstNodeKind::Identifier {
                                        original: "n2".to_string(),
                                        lower_case: "n2".to_string(),
                                    },
                                    interval: ScanInterval {
                                        start_line: 30,
                                        end_line: 30,
                                        start_column: 13,
                                        end_column: 15,
                                    },
                                },
                                AstNode {
                                    children: vec![],
                                    kind: AstNodeKind::Type {
                                        type_name: TypeName::Primitive {
                                            primitive_type: PrimitiveTypeName::Integer,
                                        },
                                    },
                                    interval: ScanInterval {
                                        start_line: 30,
                                        end_line: 30,
                                        start_column: 17,
                                        end_column: 24,
                                    },
                                },
                            ],
                            kind: AstNodeKind::VariableDeclaration,
                            interval: ScanInterval {
                                start_line: 30,
                                end_line: 30,
                                start_column: 5,
                                end_column: 24,
                            },
                        },
                        AstNode {
                            children: vec![
                                AstNode {
                                    children: vec![],
                                    kind: AstNodeKind::PredefinedIdentifier {
                                        original: "read".to_string(),
                                        lower_case: "read".to_string(),
                                    },
                                    interval: ScanInterval {
                                        start_line: 31,
                                        end_line: 31,
                                        start_column: 5,
                                        end_column: 9,
                                    },
                                },
                                AstNode {
                                    children: vec![],
                                    kind: AstNodeKind::Identifier {
                                        original: "n1".to_string(),
                                        lower_case: "n1".to_string(),
                                    },
                                    interval: ScanInterval {
                                        start_line: 31,
                                        end_line: 31,
                                        start_column: 10,
                                        end_column: 12,
                                    },
                                },
                            ],
                            kind: AstNodeKind::CallStatement,
                            interval: ScanInterval {
                                start_line: 31,
                                end_line: 31,
                                start_column: 5,
                                end_column: 12,
                            },
                        },
                        AstNode {
                            children: vec![
                                AstNode {
                                    children: vec![],
                                    kind: AstNodeKind::PredefinedIdentifier {
                                        original: "read".to_string(),
                                        lower_case: "read".to_string(),
                                    },
                                    interval: ScanInterval {
                                        start_line: 32,
                                        end_line: 32,
                                        start_column: 5,
                                        end_column: 9,
                                    },
                                },
                                AstNode {
                                    children: vec![],
                                    kind: AstNodeKind::Identifier {
                                        original: "n2".to_string(),
                                        lower_case: "n2".to_string(),
                                    },
                                    interval: ScanInterval {
                                        start_line: 32,
                                        end_line: 32,
                                        start_column: 10,
                                        end_column: 12,
                                    },
                                },
                            ],
                            kind: AstNodeKind::CallStatement,
                            interval: ScanInterval {
                                start_line: 32,
                                end_line: 32,
                                start_column: 5,
                                end_column: 12,
                            },
                        },
                        AstNode {
                            children: vec![
                                AstNode {
                                    children: vec![],
                                    kind: AstNodeKind::PredefinedIdentifier {
                                        original: "writeln".to_string(),
                                        lower_case: "writeln".to_string(),
                                    },
                                    interval: ScanInterval {
                                        start_line: 33,
                                        end_line: 33,
                                        start_column: 5,
                                        end_column: 12,
                                    },
                                },
                                AstNode {
                                    children: vec![
                                        AstNode {
                                            children: vec![],
                                            kind: AstNodeKind::Identifier {
                                                original: "gcd".to_string(),
                                                lower_case: "gcd".to_string(),
                                            },
                                            interval: ScanInterval {
                                                start_line: 33,
                                                end_line: 33,
                                                start_column: 13,
                                                end_column: 16,
                                            },
                                        },
                                        AstNode {
                                            children: vec![],
                                            kind: AstNodeKind::Identifier {
                                                original: "n1".to_string(),
                                                lower_case: "n1".to_string(),
                                            },
                                            interval: ScanInterval {
                                                start_line: 33,
                                                end_line: 33,
                                                start_column: 17,
                                                end_column: 19,
                                            },
                                        },
                                        AstNode {
                                            children: vec![],
                                            kind: AstNodeKind::Identifier {
                                                original: "n2".to_string(),
                                                lower_case: "n2".to_string(),
                                            },
                                            interval: ScanInterval {
                                                start_line: 33,
                                                end_line: 33,
                                                start_column: 21,
                                                end_column: 23,
                                            },
                                        },
                                    ],
                                    kind: AstNodeKind::CallStatement,
                                    interval: ScanInterval {
                                        start_line: 33,
                                        end_line: 33,
                                        start_column: 13,
                                        end_column: 23,
                                    },
                                },
                            ],
                            kind: AstNodeKind::CallStatement,
                            interval: ScanInterval {
                                start_line: 33,
                                end_line: 33,
                                start_column: 5,
                                end_column: 23,
                            },
                        },
                    ],
                    kind: AstNodeKind::Block,
                    interval: ScanInterval {
                        start_line: 29,
                        end_line: 34,
                        start_column: 1,
                        end_column: 4,
                    },
                },
            ],
            kind: AstNodeKind::Program,
            interval: ScanInterval {
                start_line: 1,
                end_line: 34,
                start_column: 1,
                end_column: 5,
            },
        }
    );
}
