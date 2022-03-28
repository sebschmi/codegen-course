use crate::error::Result;
use crate::error::{parser_error, ParserErrorKind};
use crate::scanner::{ScanInterval, Scanner, Token};
use crate::Error;
use crate::Error::UnexpectedEndOfInput;
use std::iter::Peekable;

/// A node of the abstract syntax tree.
#[derive(Debug)]
pub struct AstNode {
    /// The children of the node.
    /// A valid number of children is decided by the kind of the node.
    children: Vec<AstNode>,

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
    /// Its children are its identifier and each single parameter.
    Procedure,
    /// A function with return value.
    /// Its children are its identifier, each single parameter, and then its return type.
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
    Type { type_name: TypeName },
}

/// A Mini-PL primitive type name.
#[allow(missing_docs)]
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TypeName {
    /// A primitive type.
    Primitive { primitive_type: PrimitiveTypeName },
    /// An unsized array.
    UnsizedArray { primitive_type: PrimitiveTypeName },
    /// A statically sized array of a specific primitive type.
    SizedArray { primitive_type: PrimitiveTypeName },
}

/// A Mini-PL primitive type name.
#[allow(missing_docs)]
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PrimitiveTypeName {
    Boolean,
    Integer,
    Real,
    String,
}

/// Build the AST via recursive descent parsing.
/// The build_* methods below all build an AST node according to the LL(1) grammar.
pub fn build_ast(scanner: Scanner<impl Iterator<Item = Result<char>>>) -> Result<AstNode> {
    let mut peekable_scanner = scanner.peekable();
    parse_program(&mut peekable_scanner)
}

fn parse_program(
    scanner: &mut Peekable<Scanner<impl Iterator<Item = Result<char>>>>,
) -> Result<AstNode> {
    let start_interval = expect_token(scanner, Token::Program)?;
    let mut children = vec![parse_identifier(scanner)?];
    expect_token(scanner, Token::Semicolon)?;

    let main_interval;
    loop {
        children.push(match scanner.next() {
            Some((Ok(Token::Procedure), interval)) => parse_procedure(scanner, interval)?,
            Some((Ok(Token::Function), interval)) => parse_function(scanner, interval)?,
            Some((Ok(Token::Begin), interval)) => {
                main_interval = interval;
                break;
            }
            Some((Ok(other), interval)) => {
                return Err(parser_error(
                    interval,
                    ParserErrorKind::UnexpectedToken {
                        expected: vec![Token::Procedure, Token::Function, Token::Begin],
                        found: other,
                    },
                ))
            }
            Some((Err(error), _)) => return Err(error),
            None => return Err(Error::UnexpectedEndOfInput),
        })
    }

    children.push(parse_block(scanner, main_interval)?);
    let last_interval = expect_token(scanner, Token::Dot)?;
    let total_interval = start_interval.extend_clone(&last_interval);

    Ok(AstNode {
        children,
        kind: AstNodeKind::Program,
        interval: total_interval,
    })
}

fn parse_procedure(
    scanner: &mut Peekable<Scanner<impl Iterator<Item = Result<char>>>>,
    start_interval: ScanInterval,
) -> Result<AstNode> {
    let mut children = vec![parse_identifier(scanner)?];
    expect_token(scanner, Token::OpenParenthesis)?;
    children.extend(parse_parameters(scanner)?);
    expect_token(scanner, Token::CloseParenthesis)?;
    expect_token(scanner, Token::Semicolon)?;
    let block_start = expect_token(scanner, Token::Begin)?;
    children.push(parse_block(scanner, block_start)?);
    let last_interval = expect_token(scanner, Token::Semicolon)?;
    let total_interval = start_interval.extend_clone(&last_interval);

    Ok(AstNode {
        children,
        kind: AstNodeKind::Procedure,
        interval: total_interval,
    })
}

fn parse_function(
    scanner: &mut Peekable<Scanner<impl Iterator<Item = Result<char>>>>,
    start_interval: ScanInterval,
) -> Result<AstNode> {
    let mut children = vec![parse_identifier(scanner)?];
    expect_token(scanner, Token::OpenParenthesis)?;
    children.extend(parse_parameters(scanner)?);
    expect_token(scanner, Token::CloseParenthesis)?;
    expect_token(scanner, Token::Colon)?;
    children.push(parse_type(scanner)?);
    expect_token(scanner, Token::Semicolon)?;
    let block_start = expect_token(scanner, Token::Begin)?;
    children.push(parse_block(scanner, block_start)?);
    let last_interval = expect_token(scanner, Token::Semicolon)?;
    let total_interval = start_interval.extend_clone(&last_interval);

    Ok(AstNode {
        children,
        kind: AstNodeKind::Procedure,
        interval: total_interval,
    })
}

/// Parse the parameters of a function as a vector of [AstNode]s.
/// There is no separate node for a parameter list, that is why we return a plain vector here.
fn parse_parameters(
    scanner: &mut Peekable<Scanner<impl Iterator<Item = Result<char>>>>,
) -> Result<Vec<AstNode>> {
    let mut children = Vec::new();

    loop {
        match scanner.peek() {
            Some((Ok(Token::CloseParenthesis), _)) => break,
            Some(_) => {
                children.push(parse_parameter(scanner)?);
                if let Some((Ok(Token::Comma), _)) = scanner.peek() {
                    // a comma means there is another parameter, so we can continue the loop
                    scanner.next();
                    // we need to make sure though that there is another parameter after the comma,
                    // since trailing commas in parameter lists are not allowed
                    if let Some((Ok(Token::CloseParenthesis), interval)) = scanner.peek() {
                        return Err(parser_error(
                            interval.clone(),
                            ParserErrorKind::ExpectedIdentifier {
                                found: Token::CloseParenthesis,
                            },
                        ));
                    }
                }
            }
            None => return Err(Error::UnexpectedEndOfInput),
        };
    }

    Ok(children)
}

fn parse_parameter(
    scanner: &mut Peekable<Scanner<impl Iterator<Item = Result<char>>>>,
) -> Result<AstNode> {
    let (is_var, start_interval) = match scanner.peek() {
        Some((Ok(Token::Var), interval)) => {
            let interval = interval.clone();
            scanner.next();
            (true, interval)
        }
        Some((_, interval)) => (false, interval.clone()),
        None => return Err(Error::UnexpectedEndOfInput),
    };

    let identifier = parse_identifier(scanner)?;
    let type_name = parse_type(scanner)?;
    let interval = start_interval.extend_clone(&type_name.interval);

    Ok(AstNode {
        children: vec![identifier, type_name],
        kind: if is_var {
            AstNodeKind::VarParameter
        } else {
            AstNodeKind::ValueParameter
        },
        interval,
    })
}

fn parse_block(
    scanner: &mut Peekable<Scanner<impl Iterator<Item = Result<char>>>>,
    start_interval: ScanInterval,
) -> Result<AstNode> {
    todo!()
}

fn parse_type(
    scanner: &mut Peekable<Scanner<impl Iterator<Item = Result<char>>>>,
) -> Result<AstNode> {
    let (type_name, children, interval) = match scanner.peek() {
        Some((Ok(Token::Array), interval)) => {
            let interval = interval.clone();
            scanner.next();
            let current_interval = expect_token(scanner, Token::OpenBracket)?;
            let children = match scanner.peek() {
                Some((Ok(Token::CloseBracket), _)) => vec![],
                Some((Ok(other), interval)) => {
                    let interval = interval.clone();
                    let result = vec![parse_expression(scanner, interval)?];
                    result
                }
                Some((Err(error), _)) => {
                    return Err(scanner
                        .next()
                        .expect("peek() returned Some, so next() must also return Some")
                        .0
                        .expect_err("peek() returned Err, so next() must also return Err"))
                }
                None => return Err(Error::UnexpectedEndOfInput),
            };
            expect_token(scanner, Token::CloseBracket)?;
            let last_interval = expect_token(scanner, Token::Of)?;

            // parse the primitive type of the array
            let type_name = parse_primitive_type(scanner)?;
            let type_name = if children.is_empty() {
                TypeName::UnsizedArray {
                    primitive_type: type_name,
                }
            } else {
                TypeName::SizedArray {
                    primitive_type: type_name,
                }
            };
            Ok((type_name, children, interval.extend_clone(&last_interval)))
        }
        Some((Ok(Token::PredefinedIdentifier { .. }), interval)) => {
            let interval = interval.clone();
            Ok((
                TypeName::Primitive {
                    primitive_type: parse_primitive_type(scanner)?,
                },
                vec![],
                interval,
            ))
        }
        Some((Ok(_), interval)) => Err(parser_error(
            interval.clone(),
            ParserErrorKind::ExpectedTypeName {
                found: scanner.next().unwrap().0.unwrap(),
            },
        )),
        Some((Err(_), _)) => Err(scanner.next().unwrap().0.unwrap_err()),
        None => Err(UnexpectedEndOfInput),
    }?;

    Ok(AstNode {
        children,
        kind: AstNodeKind::Type { type_name },
        interval,
    })
}

fn parse_primitive_type(
    scanner: &mut Peekable<Scanner<impl Iterator<Item = Result<char>>>>,
) -> Result<PrimitiveTypeName> {
    match scanner.next() {
        Some((Ok(ref token @ Token::PredefinedIdentifier { ref original, .. }), interval)) => {
            Ok(match original.as_str() {
                "Boolean" => PrimitiveTypeName::Boolean,
                "integer" => PrimitiveTypeName::Integer,
                "real" => PrimitiveTypeName::Real,
                "string" => PrimitiveTypeName::String,
                // pattern matching and lifetimes don't work as well together yet, so we need to take the pattern by reference and clone here
                _ => {
                    return Err(parser_error(
                        interval,
                        ParserErrorKind::ExpectedTypeName {
                            found: token.clone(),
                        },
                    ))
                }
            })
        }

        Some((Ok(other), interval)) => Err(parser_error(
            interval,
            ParserErrorKind::ExpectedTypeName { found: other },
        )),
        Some((Err(error), _)) => Err(error),
        None => Err(Error::UnexpectedEndOfInput),
    }
}

fn parse_expression(
    scanner: &mut Peekable<Scanner<impl Iterator<Item = Result<char>>>>,
    start_interval: ScanInterval,
) -> Result<AstNode> {
    todo!()
}

/// Parses an identifier or predefined identifier into an identifier.
fn parse_identifier(
    scanner: &mut Peekable<Scanner<impl Iterator<Item = Result<char>>>>,
) -> Result<AstNode> {
    match scanner.next() {
        Some((
            Ok(Token::Identifier {
                original,
                lower_case,
            }),
            interval,
        )) => Ok(AstNode::leaf(
            AstNodeKind::Identifier {
                original,
                lower_case,
            },
            interval,
        )),
        Some((
            Ok(Token::PredefinedIdentifier {
                original,
                lower_case,
            }),
            interval,
        )) => Ok(AstNode::leaf(
            AstNodeKind::Identifier {
                original,
                lower_case,
            },
            interval,
        )),
        Some((Ok(other), interval)) => Err(parser_error(
            interval,
            ParserErrorKind::ExpectedIdentifier { found: other },
        )),
        Some((Err(error), _)) => Err(error),
        None => Err(Error::UnexpectedEndOfInput),
    }
}

/// Expect the next token to be the given token.
/// Note that this should only be used for tokens without attached data.
fn expect_token(
    scanner: &mut Peekable<Scanner<impl Iterator<Item = Result<char>>>>,
    expected: Token,
) -> Result<ScanInterval> {
    if let Some((token, interval)) = scanner.next() {
        match token {
            Ok(token) => {
                if expected == token {
                    Ok(interval)
                } else {
                    Err(parser_error(
                        interval,
                        ParserErrorKind::UnexpectedToken {
                            expected: vec![expected],
                            found: token,
                        },
                    ))
                }
            }
            Err(error) => Err(error),
        }
    } else {
        Err(Error::UnexpectedEndOfInput)
    }
}

/// Expect the next token to be in the given vector.
/// Note that this should only be used for tokens without attached data.
fn expect_tokens(
    scanner: &mut Peekable<Scanner<impl Iterator<Item = Result<char>>>>,
    expected: Vec<Token>,
) -> Result<ScanInterval> {
    if let Some((token, interval)) = scanner.next() {
        match token {
            Ok(token) => {
                if expected.contains(&token) {
                    Ok(interval)
                } else {
                    Err(parser_error(
                        interval,
                        ParserErrorKind::UnexpectedToken {
                            expected,
                            found: token,
                        },
                    ))
                }
            }
            Err(error) => Err(error),
        }
    } else {
        Err(Error::UnexpectedEndOfInput)
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
