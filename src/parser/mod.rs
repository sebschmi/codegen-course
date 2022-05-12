use crate::error::Result;
use crate::error::{parser_error, ParserErrorKind};
use crate::scanner::{ScanInterval, Scanner, Token};
use crate::Error;
use std::collections::VecDeque;
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
    /// A logical not operator.
    NotOperator,

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

/// Parse the parameters of a function or procedure as a vector of [AstNode]s.
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
    let mut children = Vec::new();

    let mut block_interval = start_interval;
    loop {
        children.push(match scanner.peek() {
            Some((Ok(Token::End), interval)) => {
                block_interval = block_interval.extend_clone(interval);
                expect_token(scanner, Token::End)?;
                break;
            }
            Some((Ok(_), _)) => parse_statement(scanner)?,
            Some((Err(_), _)) => return Err(scanner.next().unwrap().0.unwrap_err()),
            None => return Err(Error::UnexpectedEndOfInput),
        });
    }

    Ok(AstNode {
        children,
        kind: AstNodeKind::Block,
        interval: block_interval,
    })
}

fn parse_if(
    scanner: &mut Peekable<Scanner<impl Iterator<Item = Result<char>>>>,
    start_interval: ScanInterval,
) -> Result<AstNode> {
    let mut children = Vec::new();
    children.push(parse_expression(scanner)?);
    expect_token(scanner, Token::Then)?;
    children.push(parse_statement(scanner)?);

    if let Some((Ok(Token::Else), _)) = scanner.peek() {
        expect_token(scanner, Token::Else)?;
        children.push(parse_expression(scanner)?);
    }

    let interval = start_interval.extend_clone(&children.last().unwrap().interval);
    Ok(AstNode {
        children,
        kind: AstNodeKind::IfStatement,
        interval,
    })
}

fn parse_while(
    scanner: &mut Peekable<Scanner<impl Iterator<Item = Result<char>>>>,
    start_interval: ScanInterval,
) -> Result<AstNode> {
    let mut children = Vec::new();
    children.push(parse_expression(scanner)?);
    expect_token(scanner, Token::Then)?;
    children.push(parse_statement(scanner)?);

    let interval = start_interval.extend_clone(&children.last().unwrap().interval);
    Ok(AstNode {
        children,
        kind: AstNodeKind::WhileStatement,
        interval,
    })
}

fn parse_var(
    scanner: &mut Peekable<Scanner<impl Iterator<Item = Result<char>>>>,
    start_interval: ScanInterval,
) -> Result<AstNode> {
    let mut children = Vec::new();
    children.push(parse_identifier(scanner)?);

    while let Some((Ok(Token::Comma), _)) = scanner.peek() {
        expect_token(scanner, Token::Comma)?;
        children.push(parse_identifier(scanner)?);
    }

    expect_token(scanner, Token::Colon)?;
    children.push(parse_type(scanner)?);

    let interval = start_interval.extend_clone(&children.last().unwrap().interval);
    Ok(AstNode {
        children,
        kind: AstNodeKind::VariableDeclaration,
        interval,
    })
}

fn parse_return(
    scanner: &mut Peekable<Scanner<impl Iterator<Item = Result<char>>>>,
    start_interval: ScanInterval,
) -> Result<AstNode> {
    let mut children = Vec::new();

    if let Some((Ok(Token::Semicolon | Token::End), _)) = scanner.peek() {
        // return has no argument
    } else {
        children.push(parse_expression(scanner)?);
    }

    let interval = start_interval.extend_clone(&children.last().unwrap().interval);
    Ok(AstNode {
        children,
        kind: AstNodeKind::ReturnStatement,
        interval,
    })
}

fn parse_assert(
    scanner: &mut Peekable<Scanner<impl Iterator<Item = Result<char>>>>,
    start_interval: ScanInterval,
) -> Result<AstNode> {
    let mut children = Vec::new();
    expect_token(scanner, Token::OpenParenthesis)?;
    children.push(parse_expression(scanner)?);
    expect_token(scanner, Token::CloseParenthesis)?;

    let interval = start_interval.extend_clone(&children.last().unwrap().interval);
    Ok(AstNode {
        children,
        kind: AstNodeKind::AssertStatement,
        interval,
    })
}

/// Panics if called with `identifier != Token::Identifier`.
fn parse_cass(
    identifier: Token,
    scanner: &mut Peekable<Scanner<impl Iterator<Item = Result<char>>>>,
    start_interval: ScanInterval,
) -> Result<AstNode> {
    let mut children = if let Token::Identifier {
        original,
        lower_case,
    } = identifier
    {
        vec![AstNode::leaf(
            AstNodeKind::Identifier {
                original: "".to_string(),
                lower_case: "".to_string(),
            },
            start_interval.clone(),
        )]
    } else {
        unreachable!("This method can only be called with a Token::Identifier");
    };

    let ast_node_kind = if let Some((Ok(Token::OpenParenthesis), _)) = scanner.peek() {
        expect_token(scanner, Token::OpenParenthesis)?;
        children.extend(parse_arguments(scanner)?);
        AstNodeKind::CallStatement
    } else {
        children.push(parse_identifier(scanner)?);
        expect_token(scanner, Token::AssignOperator)?;
        children.push(parse_expression(scanner)?);
        AstNodeKind::AssignmentStatement
    };

    let interval = start_interval.extend_clone(&children.last().unwrap().interval);
    Ok(AstNode {
        children,
        kind: ast_node_kind,
        interval,
    })
}

fn parse_statement(
    scanner: &mut Peekable<Scanner<impl Iterator<Item = Result<char>>>>,
) -> Result<AstNode> {
    Ok(match scanner.next() {
        Some((Ok(Token::Begin), interval)) => parse_block(scanner, interval)?,
        Some((Ok(Token::If), interval)) => parse_if(scanner, interval)?,
        Some((Ok(Token::While), interval)) => parse_while(scanner, interval)?,
        Some((Ok(Token::Var), interval)) => parse_var(scanner, interval)?,
        Some((Ok(Token::Return), interval)) => parse_return(scanner, interval)?,
        Some((Ok(Token::Assert), interval)) => parse_assert(scanner, interval)?,
        Some((
            Ok(Token::PredefinedIdentifier {
                lower_case,
                original,
            }),
            interval,
        )) => parse_cass(
            Token::Identifier {
                lower_case,
                original,
            },
            scanner,
            interval,
        )?,
        Some((Ok(identifier @ Token::Identifier { .. }), interval)) => {
            parse_cass(identifier, scanner, interval)?
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

/// Parse the arguments of a function or procedure call as a vector of [AstNode]s.
/// There is no separate node for a parameter list, that is why we return a plain vector here.
fn parse_arguments(
    scanner: &mut Peekable<Scanner<impl Iterator<Item = Result<char>>>>,
) -> Result<Vec<AstNode>> {
    let mut children = Vec::new();

    loop {
        match scanner.peek() {
            Some((Ok(Token::CloseParenthesis), _)) => {
                expect_token(scanner, Token::CloseParenthesis)?;
                break;
            }
            Some((Ok(_), _)) => {
                children.push(parse_expression(scanner)?);
                if let Some((Ok(Token::Comma), _)) = scanner.peek() {
                    expect_token(scanner, Token::Comma)?;
                }
            }
            Some((Err(error), _)) => return Err(scanner.next().unwrap().0.unwrap_err()),
            None => return Err(Error::UnexpectedEndOfInput),
        }
    }

    Ok(children)
}

fn parse_type(
    scanner: &mut Peekable<Scanner<impl Iterator<Item = Result<char>>>>,
) -> Result<AstNode> {
    let (type_name, children, interval) = match scanner.peek() {
        Some((Ok(Token::Array), interval)) => {
            let interval = interval.clone();
            scanner.next();
            let children = match scanner.peek() {
                Some((Ok(Token::CloseBracket), _)) => vec![],
                Some((Ok(_), _)) => {
                    let result = vec![parse_expression(scanner)?];
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
        None => Err(Error::UnexpectedEndOfInput),
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
) -> Result<AstNode> {
    Ok(match scanner.peek() {
        Some((Ok(Token::NotOperator), interval)) => {
            let interval = interval.clone();
            expect_token(scanner, Token::NotOperator)?;
            let child = parse_expression(scanner)?;
            let interval = interval.extend_clone(&child.interval);

            AstNode {
                children: vec![child],
                kind: AstNodeKind::NotOperator,
                interval,
            }
        }
        Some((Ok(_), interval)) => {
            let interval = interval.clone();

            let first = parse_simple_expression(scanner)?;
            if let Some((
                Ok(
                    token @ (Token::EqOperator
                    | Token::NeqOperator
                    | Token::GtOperator
                    | Token::GeqOperator
                    | Token::LtOperator
                    | Token::LeqOperator),
                ),
                _,
            )) = scanner.peek()
            {
                let kind = match token {
                    Token::EqOperator => AstNodeKind::EqOperator,
                    Token::NeqOperator => AstNodeKind::NeqOperator,
                    Token::LtOperator => AstNodeKind::LtOperator,
                    Token::LeqOperator => AstNodeKind::LeqOperator,
                    Token::GtOperator => AstNodeKind::GtOperator,
                    Token::GeqOperator => AstNodeKind::GeqOperator,
                    token => unreachable!("{token:?}"),
                };
                let children = vec![first, parse_simple_expression(scanner)?];
                let interval = interval.extend_clone(&children.last().unwrap().interval);

                AstNode {
                    children,
                    kind,
                    interval,
                }
            } else {
                first
            }
        }
        Some((Err(_), _)) => return Err(scanner.next().unwrap().0.unwrap_err()),
        None => return Err(Error::UnexpectedEndOfInput),
    })
}

fn parse_simple_expression(
    scanner: &mut Peekable<Scanner<impl Iterator<Item = Result<char>>>>,
) -> Result<AstNode> {
    if let Some((Ok(Token::PlusOperator), _)) = scanner.peek() {
        // ignore identity sign
        expect_token(scanner, Token::PlusOperator)?;
        parse_unsigned_simple_expression(scanner)
    } else if let Some((Ok(Token::MinusOperator), _)) = scanner.peek() {
        let interval = expect_token(scanner, Token::MinusOperator)?;
        let children = vec![parse_unsigned_simple_expression(scanner)?];
        let interval = interval.extend_clone(&children.last().unwrap().interval);
        Ok(AstNode {
            children,
            kind: AstNodeKind::NegOperator,
            interval,
        })
    } else {
        parse_unsigned_simple_expression(scanner)
    }
}

fn parse_unsigned_simple_expression(
    scanner: &mut Peekable<Scanner<impl Iterator<Item = Result<char>>>>,
) -> Result<AstNode> {
    let mut terms = VecDeque::from(vec![parse_term(scanner)?]);

    loop {
        match scanner.peek() {
            Some((
                Ok(token @ (Token::PlusOperator | Token::MinusOperator | Token::OrOperator)),
                interval,
            )) => {
                let interval = interval.clone();
                let kind = match token {
                    Token::PlusOperator => AstNodeKind::AddOperator,
                    Token::MinusOperator => AstNodeKind::SubOperator,
                    Token::OrOperator => AstNodeKind::OrOperator,
                    token => unreachable!("{token:?}"),
                };

                let term = parse_term(scanner)?;
                let interval = interval.extend_clone(&term.interval);
                terms.push_back(AstNode {
                    children: vec![term],
                    kind,
                    interval,
                });
            }
            Some((Ok(_), _)) => break,
            Some((Err(_), _)) => return Err(scanner.next().unwrap().0.unwrap_err()),
            None => return Err(Error::UnexpectedEndOfInput),
        }
    }

    let mut result = terms.pop_front().unwrap();

    while let Some(mut first) = terms.pop_front() {
        first.children.insert(0, result);
        result = first;
    }

    Ok(result)
}

fn parse_term(
    scanner: &mut Peekable<Scanner<impl Iterator<Item = Result<char>>>>,
) -> Result<AstNode> {
    let mut factors = VecDeque::from(vec![parse_factor(scanner)?]);

    loop {
        match scanner.peek() {
            Some((
                Ok(
                    token @ (Token::MulOperator
                    | Token::DivOperator
                    | Token::ModOperator
                    | Token::AndOperator),
                ),
                interval,
            )) => {
                let interval = interval.clone();
                let kind = match token {
                    Token::MulOperator => AstNodeKind::MulOperator,
                    Token::DivOperator => AstNodeKind::DivOperator,
                    Token::ModOperator => AstNodeKind::ModOperator,
                    Token::AndOperator => AstNodeKind::AndOperator,
                    token => unreachable!("{token:?}"),
                };

                let factor = parse_factor(scanner)?;
                let interval = interval.extend_clone(&factor.interval);
                factors.push_back(AstNode {
                    children: vec![factor],
                    kind,
                    interval,
                });
            }
            Some((Ok(_), _)) => break,
            Some((Err(_), _)) => return Err(scanner.next().unwrap().0.unwrap_err()),
            None => return Err(Error::UnexpectedEndOfInput),
        }
    }

    let mut result = factors.pop_front().unwrap();

    while let Some(mut first) = factors.pop_front() {
        first.children.insert(0, result);
        result = first;
    }

    Ok(result)
}

fn parse_factor(
    scanner: &mut Peekable<Scanner<impl Iterator<Item = Result<char>>>>,
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
