use crate::error::Result;
use crate::error::{parser_error, ParserErrorKind};
use crate::scanner::{ScanInterval, Scanner, Token};

/// A node of the abstract syntax tree.
#[derive(Debug)]
pub struct AstNode {
    /// The children of the node.
    /// A valid number of children is decided by the kind of the node.
    children: Vec<Box<AstNode>>,

    /// The kind of the node.
    kind: AstNodeKind,

    /// The coordinates of this node in the source code.
    interval: ScanInterval,
}

/// The kind of an AST node.
/// This can be an operator, function, etc.
#[allow(missing_docs)]
#[derive(Debug)]
pub enum AstNodeKind {
    /// The root node.
    Program,
    /// A function without return value.
    Procedure,
    /// A function with return value.
    Function,
    /// A variable function parameter (passed by reference).
    VarParameter,
    /// A value function parameter (passed by value).
    ValueParameter,
    /// A list of statements.
    Block,

    /// A variable declaration statement.
    VariableDeclaration,
    /// An assignment statement.
    AssignmentStatement,
    /// A function or procedure call statement.
    CallStatement,
    /// A return statement.
    ReturnStatement,
    /// A read statement.
    ReadStatement,
    /// A write statement.
    WriteStatement,
    /// An assert statement.
    AssertStatement,
    /// An if-branch with optional else.
    IfStatement,
    /// A while loop.
    WhileStatement,

    /// An equality check.
    EqOperator,
    /// An inequality check.
    NeqOperator,
    /// A lower-than check.
    LtOperator,
    /// A lower-or-equal check.
    LeqOperator,
    /// A greater-or-equal check.
    GeqOperator,
    /// A greater-than check.
    GtOperator,

    /// A negation operator.
    NegOperator,
    /// An addition operator.
    AddOperator,
    /// A subtraction operator.
    SubOperator,
    /// A logical or operator.
    OrOperator,

    /// A multiplication operator.
    MulOperator,
    /// A division operator.
    DivOperator,
    /// A modulo operator.
    ModOperator,
    /// A logical and operator.
    AndOperator,

    /// A literal representing a constant value.
    Literal {
        literal_type: TypeName,
        value: String,
    },
    /// An identifier of a variable, function, procedure or program.
    Identifier {
        original: String,
        lower_case: String,
    },
    /// An identifier of a variable, function, procedure or program.
    /// This identifier is predefined and thus has special rules on its value.
    PredefinedIdentifier {
        original: String,
        lower_case: String,
    },
    /// The name of a type.
    Type {
        type_name: TypeName,
        /// True if this type is an array.
        /// This variable is required because unsized arrays cannot be distinguished from plain types in the AST.
        is_array: bool,
    },
}

/// A Mini-PL primitive type name.
#[allow(missing_docs)]
#[derive(Debug)]
pub enum TypeName {
    Boolean,
    Integer,
    Real,
    String,
}

/// Build the AST via recursive descent parsing.
/// The build_* methods below all build an AST node according to the LL(1) grammar.
pub fn build_ast(mut scanner: Scanner<impl Iterator<Item = Result<char>>>) -> Result<Box<AstNode>> {
    parse_program(&mut scanner)
}

fn parse_program(
    scanner: &mut Scanner<impl Iterator<Item = Result<char>>>,
) -> Result<Box<AstNode>> {
    let start_interval = scanner.current_interval();
    expect_token(scanner, Token::Program)?;
    let mut children = vec![parse_identifier(scanner)?];
    expect_token(scanner, Token::Semicolon)?;

    let main_interval;
    loop {
        children.push(match scanner.next_with_interval() {
            Some((Ok(Token::Procedure), interval)) => parse_procedure(scanner, interval),
            Some((Ok(Token::Function), interval)) => parse_function(scanner, interval),
            Some((Ok(Token::Begin), interval)) => {
                main_interval = interval;
                break;
            }
            Some((Ok(other), interval)) => Err(parser_error(
                interval,
                ParserErrorKind::UnexpectedToken {
                    expected: vec![Token::Procedure, Token::Function, Token::Begin],
                    found: Some(other),
                },
            )),
            Some((Err(error), _)) => Err(error),
            None => Err(parser_error(
                scanner.current_interval(),
                ParserErrorKind::UnexpectedToken {
                    expected: vec![Token::Procedure, Token::Function, Token::Begin],
                    found: None,
                },
            )),
        }?)
    }

    children.push(parse_block(scanner, main_interval)?);
    expect_token(scanner, Token::Dot)?;
    let total_interval = start_interval.extend_clone(&scanner.current_interval());

    Ok(Box::new(AstNode {
        children,
        kind: AstNodeKind::Program,
        interval: total_interval,
    }))
}

fn parse_procedure(
    scanner: &mut Scanner<impl Iterator<Item = Result<char>>>,
    start_interval: ScanInterval,
) -> Result<Box<AstNode>> {
    let mut children = vec![parse_identifier(scanner)?];
    expect_token(scanner, Token::OpenParenthesis)?;
    children.push(parse_parameters(scanner)?);
    expect_token(scanner, Token::CloseParenthesis)?;
    expect_token(scanner, Token::Semicolon)?;
    let block_start = scanner.current_interval();
    expect_token(scanner, Token::Begin)?;
    children.push(parse_block(scanner, block_start)?);
    expect_token(scanner, Token::Semicolon)?;
    let total_interval = start_interval.extend_clone(&scanner.current_interval());

    Ok(Box::new(AstNode {
        children,
        kind: AstNodeKind::Procedure,
        interval: total_interval,
    }))
}

fn parse_function(
    scanner: &mut Scanner<impl Iterator<Item = Result<char>>>,
    start_interval: ScanInterval,
) -> Result<Box<AstNode>> {
    let mut children = vec![parse_identifier(scanner)?];
    expect_token(scanner, Token::OpenParenthesis)?;
    children.push(parse_parameters(scanner)?);
    expect_token(scanner, Token::CloseParenthesis)?;
    expect_token(scanner, Token::Colon)?;
    children.push(parse_type(scanner, false)?);
    expect_token(scanner, Token::Semicolon)?;
    let block_start = scanner.current_interval();
    expect_token(scanner, Token::Begin)?;
    children.push(parse_block(scanner, block_start)?);
    expect_token(scanner, Token::Semicolon)?;
    let total_interval = start_interval.extend_clone(&scanner.current_interval());

    Ok(Box::new(AstNode {
        children,
        kind: AstNodeKind::Procedure,
        interval: total_interval,
    }))
}

fn parse_parameters(
    scanner: &mut Scanner<impl Iterator<Item = Result<char>>>,
) -> Result<Box<AstNode>> {
    todo!()
}

fn parse_block(
    scanner: &mut Scanner<impl Iterator<Item = Result<char>>>,
    start_interval: ScanInterval,
) -> Result<Box<AstNode>> {
    todo!()
}

fn parse_type(
    scanner: &mut Scanner<impl Iterator<Item = Result<char>>>,
    is_array: bool,
) -> Result<Box<AstNode>> {
    match scanner.next_with_interval() {
        Some((Ok(Token::Array), interval)) => {
            expect_token(scanner, Token::OpenBracket)?;
            let children = match scanner.next_with_interval() {
                Some((Ok(Token::CloseBracket), _)) => vec![],
                Some((Ok(other), interval)) => {
                    let result = vec![parse_expression(scanner, interval, other)?];
                    expect_token(scanner, Token::CloseBracket)?;
                    result
                }
                Some((Err(error), _)) => return Err(error),
                None => {
                    return Err(parser_error(
                        scanner.current_interval(),
                        ParserErrorKind::ExpectedTypeName { found: None },
                    ))
                }
            };
            expect_token(scanner, Token::Of)?;

            // recurse to parse the type of the array
            // this allows to create multidimensional arrays, which need to be filtered later
            let mut result = parse_type(scanner, true)?;
            result.children = children;
            result.interval = result.interval.extend_clone(&interval);
            Ok(result)
        }
        Some((Ok(ref token @ Token::PredefinedIdentifier { ref original, .. }), interval)) => {
            let type_name = match original.as_str() {
                "Boolean" => TypeName::Boolean,
                "integer" => TypeName::Integer,
                "real" => TypeName::Real,
                "string" => TypeName::String,
                // pattern matching and lifetimes don't work as well together yet, so we need to take the pattern by reference and clone here
                _ => {
                    return Err(parser_error(
                        interval,
                        ParserErrorKind::ExpectedTypeName {
                            found: Some(token.clone()),
                        },
                    ))
                }
            };

            Ok(Box::new(AstNode {
                children: vec![],
                kind: AstNodeKind::Type {
                    type_name,
                    is_array,
                },
                interval,
            }))
        }
        Some((Ok(other), interval)) => Err(parser_error(
            interval,
            ParserErrorKind::ExpectedTypeName { found: Some(other) },
        )),
        Some((Err(error), _)) => Err(error),
        None => Err(parser_error(
            scanner.current_interval(),
            ParserErrorKind::ExpectedTypeName { found: None },
        )),
    }
}

fn parse_expression(
    scanner: &mut Scanner<impl Iterator<Item = Result<char>>>,
    start_interval: ScanInterval,
    first_token: Token,
) -> Result<Box<AstNode>> {
    todo!()
}

/// Parses an identifier or predefined identifier into an identifier.
fn parse_identifier(
    scanner: &mut Scanner<impl Iterator<Item = Result<char>>>,
) -> Result<Box<AstNode>> {
    match scanner.next_with_interval() {
        Some((
            Ok(Token::Identifier {
                original,
                lower_case,
            }),
            interval,
        )) => Ok(Box::new(AstNode::leaf(
            AstNodeKind::Identifier {
                original,
                lower_case,
            },
            interval,
        ))),
        Some((
            Ok(Token::PredefinedIdentifier {
                original,
                lower_case,
            }),
            interval,
        )) => Ok(Box::new(AstNode::leaf(
            AstNodeKind::Identifier {
                original,
                lower_case,
            },
            interval,
        ))),
        Some((Ok(other), interval)) => Err(parser_error(
            interval,
            ParserErrorKind::ExpectedIdentifier { found: Some(other) },
        )),
        Some((Err(error), _)) => Err(error),
        None => Err(parser_error(
            scanner.current_interval(),
            ParserErrorKind::ExpectedIdentifier { found: None },
        )),
    }
}

/// Expect the next token to be the given token.
/// Note that this should only be used for tokens without attached data.
fn expect_token(
    scanner: &mut Scanner<impl Iterator<Item = Result<char>>>,
    expected: Token,
) -> Result<()> {
    if let Some((token, interval)) = scanner.next_with_interval() {
        match token {
            Ok(token) => {
                if expected == token {
                    Ok(())
                } else {
                    Err(parser_error(
                        interval,
                        ParserErrorKind::UnexpectedToken {
                            expected: vec![expected],
                            found: Some(token),
                        },
                    ))
                }
            }
            Err(error) => Err(error),
        }
    } else {
        Err(parser_error(
            scanner.current_interval(),
            ParserErrorKind::UnexpectedToken {
                expected: vec![expected],
                found: None,
            },
        ))
    }
}

/// Expect the next token to be in the given vector.
/// Note that this should only be used for tokens without attached data.
fn expect_tokens(
    scanner: &mut Scanner<impl Iterator<Item = Result<char>>>,
    expected: Vec<Token>,
) -> Result<()> {
    if let Some((token, interval)) = scanner.next_with_interval() {
        match token {
            Ok(token) => {
                if expected.contains(&token) {
                    Ok(())
                } else {
                    Err(parser_error(
                        interval,
                        ParserErrorKind::UnexpectedToken {
                            expected,
                            found: Some(token),
                        },
                    ))
                }
            }
            Err(error) => Err(error),
        }
    } else {
        Err(parser_error(
            scanner.current_interval(),
            ParserErrorKind::UnexpectedToken {
                expected,
                found: None,
            },
        ))
    }
}

impl AstNode {
    /// Creates a new leaf AST node.
    pub fn leaf(kind: AstNodeKind, interval: ScanInterval) -> Self {
        Self {
            children: Vec::new(),
            kind,
            interval,
        }
    }
}
