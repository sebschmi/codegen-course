use crate::error::Result;
use crate::error::{parser_error, ParserErrorKind};
use crate::scanner::{ScanInterval, Scanner, Token};
use crate::Error;
use log::trace;
use std::collections::VecDeque;
use std::iter::Peekable;

#[cfg(test)]
mod tests;

/// A node of the abstract syntax tree.
#[derive(Debug, PartialEq, Eq)]
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
#[derive(Debug, Eq, PartialEq)]
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

    /// A dot operator.
    DotOperator,
    /// The index operator for array access.
    IndexOperator,

    /// A literal representing a constant value.
    Literal {
        literal_type: TypeName,
        value: String,
    },
    /// An identifier of a variable, function, procedure or program.
    Identifier {
        original: String,
        lower_case: String,
        symbol_index: usize,
    },
    /// An identifier of a variable, function, procedure or program.
    /// This identifier is predefined and thus has special rules on its value.
    PredefinedIdentifier {
        original: String,
        lower_case: String,
        symbol_index: usize,
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

impl AstNode {
    /// Return the children of this node.
    pub fn children(&self) -> &[AstNode] {
        &self.children
    }

    /// Return the children of this node as a mutable reference.
    pub fn children_mut(&mut self) -> &mut [AstNode] {
        &mut self.children
    }

    /// Return the source code interval spanned by this node.
    pub fn interval(&self) -> &ScanInterval {
        &self.interval
    }

    /// Return the kind of this node.
    pub fn kind(&self) -> &AstNodeKind {
        &self.kind
    }

    /// Return the lower-case variant of the identifier, if this node is an identifier or predefined identifier.
    pub fn get_identifier_lower_case(&self) -> Option<&str> {
        if let AstNodeKind::Identifier { lower_case, .. }
        | AstNodeKind::PredefinedIdentifier { lower_case, .. } = &self.kind
        {
            Some(lower_case)
        } else {
            None
        }
    }

    /// Return this node's index in the symbol table, if this node is an identifier or predefined identifier.
    pub fn get_symbol_index(&self) -> Option<usize> {
        if let AstNodeKind::Identifier { symbol_index, .. }
        | AstNodeKind::PredefinedIdentifier { symbol_index, .. } = &self.kind
        {
            Some(*symbol_index)
        } else {
            None
        }
    }

    /// Return a mutable reference to this node's index in the symbol table, if this node is an identifier or predefined identifier.
    pub fn get_symbol_index_mut(&mut self) -> Option<&mut usize> {
        if let AstNodeKind::Identifier { symbol_index, .. }
        | AstNodeKind::PredefinedIdentifier { symbol_index, .. } = &mut self.kind
        {
            Some(symbol_index)
        } else {
            None
        }
    }

    /// Assume that this node is a procedure and get a vector of the types of its parameters.
    /// Panics if not a procedure, or the AST is malformed.
    pub fn get_parameter_types(&self) -> Vec<TypeName> {
        trace!("get_parameter_types {self:?}");
        debug_assert_eq!(self.kind, AstNodeKind::Procedure);
        let mut result = Vec::new();
        // first child is the identifier of the procedure, last is the body (a block)
        for parameter in self.children.iter().skip(1).rev().skip(1).rev() {
            result.push(parameter.get_variable_type());
        }
        result
    }

    /// Assume that this node is a function and get a vector of the types of its parameters as well as its return type.
    /// Panics if not a function, or the AST is malformed.
    pub fn get_parameter_types_and_return_type(&self) -> (Vec<TypeName>, TypeName) {
        trace!("get_parameter_types_and_return_type {self:?}");
        debug_assert_eq!(self.kind, AstNodeKind::Function);
        let mut result = Vec::new();
        // first child is the identifier of the procedure, second to last is the return type and last is the body (a block)
        for parameter in self.children.iter().skip(1).rev().skip(2).rev() {
            result.push(parameter.get_variable_type());
        }
        let return_type = if let AstNodeKind::Type { type_name } = &self
            .children()
            .iter()
            .rev()
            .nth(1)
            .unwrap_or_else(|| unreachable!("Illegal AST shape: {self:#?}"))
            .kind
        {
            type_name.clone()
        } else {
            unreachable!("Illegal AST shape: {self:#?}")
        };
        (result, return_type)
    }

    /// Return the type information that is stored as the last child of this node.
    /// Panics if this node does not have a type node as its last child.
    pub fn get_variable_type(&self) -> TypeName {
        if let AstNodeKind::Type { type_name } = &self
            .children
            .last()
            .unwrap_or_else(|| unreachable!("Illegal AST shape: {self:#?}"))
            .kind
        {
            type_name.clone()
        } else {
            unreachable!("Illegal AST shape: {self:#?}")
        }
    }
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
    trace!("parse_program {:?}", scanner.peek());

    let start_interval = expect_token(scanner, Token::Program)?;
    let mut children = vec![parse_identifier_declaration(scanner)?];
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
    trace!("parse_procedure {:?}", scanner.peek());

    let mut children = vec![parse_identifier_declaration(scanner)?];
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
    trace!("parse_function {:?}", scanner.peek());

    let mut children = vec![parse_identifier_declaration(scanner)?];
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
        kind: AstNodeKind::Function,
        interval: total_interval,
    })
}

/// Parse the parameters of a function or procedure as a vector of [AstNode]s.
/// There is no separate node for a parameter list, that is why we return a plain vector here.
fn parse_parameters(
    scanner: &mut Peekable<Scanner<impl Iterator<Item = Result<char>>>>,
) -> Result<Vec<AstNode>> {
    trace!("parse_parameters {:?}", scanner.peek());

    let mut children = Vec::new();

    loop {
        match scanner.peek() {
            Some((Ok(Token::CloseParenthesis), _)) => {
                break;
            }
            Some(_) => {
                children.push(parse_parameter(scanner)?);
                if let Ok(Some(_)) = optional_token(scanner, Token::Comma) {
                    // we need to make sure though that there is another parameter after the comma,
                    // since trailing commas in parameter lists are not allowed
                    if let Some((Ok(Token::CloseParenthesis), interval)) = scanner.peek() {
                        return Err(parser_error(
                            interval.clone(),
                            ParserErrorKind::TrailingComma,
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
    trace!("parse_parameter {:?}", scanner.peek());

    let (is_var, start_interval) = match scanner.peek() {
        Some((Ok(Token::Var), interval)) => {
            let interval = interval.clone();
            scanner.next();
            (true, interval)
        }
        Some((_, interval)) => (false, interval.clone()),
        None => return Err(Error::UnexpectedEndOfInput),
    };

    let identifier = parse_identifier_declaration(scanner)?;
    expect_token(scanner, Token::Colon)?;
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
    trace!("parse_block {:?}", scanner.peek());

    let mut children = Vec::new();

    let mut block_interval = start_interval;
    loop {
        match scanner.peek() {
            Some((Ok(Token::End), interval)) => {
                block_interval = block_interval.extend_clone(interval);
                expect_token(scanner, Token::End)?;
                break;
            }
            Some((Ok(_), _)) => {
                children.push(parse_statement(scanner)?);
                optional_token(scanner, Token::Semicolon)?;
            }
            Some((Err(_), _)) => return Err(scanner.next().unwrap().0.unwrap_err()),
            None => return Err(Error::UnexpectedEndOfInput),
        };
    }

    if children.is_empty() {
        Err(parser_error(block_interval, ParserErrorKind::EmptyBlock))
    } else {
        Ok(AstNode {
            children,
            kind: AstNodeKind::Block,
            interval: block_interval,
        })
    }
}

fn parse_statement(
    scanner: &mut Peekable<Scanner<impl Iterator<Item = Result<char>>>>,
) -> Result<AstNode> {
    trace!("parse_statement {:?}", scanner.peek());

    Ok(match scanner.peek() {
        Some((
            Ok(
                Token::Begin
                | Token::If
                | Token::While
                | Token::Var
                | Token::Return
                | Token::Assert,
            ),
            _,
        )) => match scanner.next() {
            Some((Ok(Token::Begin), interval)) => parse_block(scanner, interval)?,
            Some((Ok(Token::If), interval)) => parse_if(scanner, interval)?,
            Some((Ok(Token::While), interval)) => parse_while(scanner, interval)?,
            Some((Ok(Token::Var), interval)) => parse_var(scanner, interval)?,
            Some((Ok(Token::Return), interval)) => parse_return(scanner, interval)?,
            Some((Ok(Token::Assert), interval)) => parse_assert(scanner, interval)?,
            _ => unreachable!(),
        },
        Some((Ok(Token::Identifier { .. } | Token::PredefinedIdentifier { .. }), _)) => {
            parse_cass(scanner)?
        }
        Some((Ok(other), interval)) => {
            return Err(parser_error(
                interval.clone(),
                ParserErrorKind::UnexpectedToken {
                    expected: vec![
                        Token::Begin,
                        Token::If,
                        Token::While,
                        Token::Var,
                        Token::Return,
                        Token::Assert,
                        Token::Identifier {
                            original: "".to_string(),
                            lower_case: "".to_string(),
                        },
                        Token::PredefinedIdentifier {
                            original: "".to_string(),
                            lower_case: "".to_string(),
                        },
                    ],
                    found: other.clone(),
                },
            ))
        }
        Some((Err(_), _)) => return Err(scanner.next().unwrap().0.unwrap_err()),
        None => return Err(Error::UnexpectedEndOfInput),
    })
}

fn parse_if(
    scanner: &mut Peekable<Scanner<impl Iterator<Item = Result<char>>>>,
    start_interval: ScanInterval,
) -> Result<AstNode> {
    trace!("parse_if {:?}", scanner.peek());

    let mut children = Vec::new();
    children.push(parse_expression(scanner)?);
    expect_token(scanner, Token::Then)?;
    children.push(parse_statement(scanner)?);

    if let Ok(Some(_)) = optional_token(scanner, Token::Else) {
        children.push(parse_statement(scanner)?);
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
    trace!("parse_while {:?}", scanner.peek());

    let mut children = Vec::new();
    children.push(parse_expression(scanner)?);
    expect_token(scanner, Token::Do)?;
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
    trace!("parse_var {:?}", scanner.peek());

    let mut children = Vec::new();
    children.push(parse_identifier_declaration(scanner)?);

    while let Ok(Some(_)) = optional_token(scanner, Token::Comma) {
        children.push(parse_identifier_declaration(scanner)?);
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
    trace!("parse_return {:?}", scanner.peek());

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
    trace!("parse_assert {:?}", scanner.peek());

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

/// Called with the identifier not consumed yet.
fn parse_cass(
    scanner: &mut Peekable<Scanner<impl Iterator<Item = Result<char>>>>,
) -> Result<AstNode> {
    trace!("parse_cass {:?}", scanner.peek());

    let mut children = vec![parse_identifier_usage(scanner)?];

    let ast_node_kind = if let Ok(Some(_)) = optional_token(scanner, Token::OpenParenthesis) {
        children.extend(parse_arguments(scanner)?);
        AstNodeKind::CallStatement
    } else {
        expect_token(scanner, Token::AssignOperator)?;
        children.push(parse_expression(scanner)?);
        AstNodeKind::AssignmentStatement
    };

    let interval = children[0]
        .interval
        .extend_clone(&children.last().unwrap().interval);
    Ok(AstNode {
        children,
        kind: ast_node_kind,
        interval,
    })
}

/// Parse the arguments of a function or procedure call as a vector of [AstNode]s.
/// There is no separate node for a parameter list, that is why we return a plain vector here.
fn parse_arguments(
    scanner: &mut Peekable<Scanner<impl Iterator<Item = Result<char>>>>,
) -> Result<Vec<AstNode>> {
    trace!("parse_arguments {:?}", scanner.peek());

    let mut children = Vec::new();
    let mut trailing_comma = false;

    loop {
        match scanner.peek() {
            Some((Ok(Token::CloseParenthesis), interval)) => {
                if trailing_comma {
                    return Err(parser_error(
                        interval.clone(),
                        ParserErrorKind::TrailingComma,
                    ));
                }

                expect_token(scanner, Token::CloseParenthesis)?;
                break;
            }
            Some((Ok(_), _)) => {
                children.push(parse_expression(scanner)?);
                if optional_token(scanner, Token::Comma)?.is_some() {
                    trailing_comma = true;
                } else {
                    trailing_comma = false;
                }
            }
            Some((Err(_), _)) => return Err(scanner.next().unwrap().0.unwrap_err()),
            None => return Err(Error::UnexpectedEndOfInput),
        }
    }

    Ok(children)
}

fn parse_type(
    scanner: &mut Peekable<Scanner<impl Iterator<Item = Result<char>>>>,
) -> Result<AstNode> {
    trace!("parse_type {:?}", scanner.peek());

    let (type_name, children, interval) = match scanner.peek() {
        Some((Ok(Token::Array), interval)) => {
            let interval = interval.clone();
            scanner.next();
            expect_token(scanner, Token::OpenBracket)?;
            let children = match scanner.peek() {
                Some((Ok(Token::CloseBracket), _)) => vec![],
                Some((Ok(_), _)) => {
                    let result = vec![parse_expression(scanner)?];
                    result
                }
                Some((Err(_), _)) => {
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
    trace!("parse_primitive_type {:?}", scanner.peek());

    match scanner.next() {
        Some((Ok(ref token @ Token::PredefinedIdentifier { ref lower_case, .. }), interval)) => {
            Ok(match lower_case.as_str() {
                "boolean" => PrimitiveTypeName::Boolean,
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
    trace!("parse_expression {:?}", scanner.peek());

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
                scanner.next().unwrap().0?;
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
    trace!("parse_simple_expression {:?}", scanner.peek());

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
    trace!("parse_unsigned_simple_expression {:?}", scanner.peek());

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
    trace!("parse_term {:?}", scanner.peek());

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
                scanner.next();

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
    trace!("parse_factor {:?}", scanner.peek());

    let factor = match scanner.peek() {
        Some((Ok(Token::Identifier { .. } | Token::PredefinedIdentifier { .. }), interval)) => {
            let interval = interval.clone();
            let identifier = parse_identifier_usage(scanner)?;

            if let Ok(Some(_)) = optional_token(scanner, Token::OpenParenthesis) {
                let mut children = vec![identifier];
                children.extend(parse_arguments(scanner)?);
                let interval = interval.extend_clone(&children.last().unwrap().interval);
                AstNode {
                    children,
                    kind: AstNodeKind::CallStatement,
                    interval,
                }
            } else {
                identifier
            }
        }
        Some((
            Ok(Token::IntegerLiteral(_) | Token::RealLiteral(_) | Token::StringLiteral(_)),
            _,
        )) => {
            let (token, interval) = scanner.next().unwrap();
            let (value, primitive_type) = match token.unwrap() {
                Token::IntegerLiteral(value) => (value, PrimitiveTypeName::Integer),
                Token::RealLiteral(value) => (value, PrimitiveTypeName::Real),
                Token::StringLiteral(value) => (value, PrimitiveTypeName::String),
                token => unreachable!("{token:?}"),
            };
            AstNode {
                children: Vec::new(),
                kind: AstNodeKind::Literal {
                    literal_type: TypeName::Primitive { primitive_type },
                    value,
                },
                interval,
            }
        }
        Some((Ok(Token::OpenParenthesis), _)) => {
            expect_token(scanner, Token::OpenParenthesis)?;
            let result = parse_expression(scanner)?;
            expect_token(scanner, Token::CloseParenthesis)?;
            result
        }
        Some((Ok(Token::NotOperator), _)) => {
            let interval = expect_token(scanner, Token::NotOperator)?;
            let mut result = parse_factor(scanner)?;
            result.interval = result.interval.extend_clone(&interval);
            result
        }
        Some((Ok(_), _)) => {
            let (other, interval) = scanner.next().unwrap();
            let other = other.unwrap();
            return Err(parser_error(
                interval,
                ParserErrorKind::ExpectedFactor { found: other },
            ));
        }
        Some((Err(_), _)) => return Err(scanner.next().unwrap().0.unwrap_err()),
        None => return Err(Error::UnexpectedEndOfInput),
    };

    Ok(if let Ok(Some(_)) = optional_token(scanner, Token::Dot) {
        let identifier = parse_identifier_usage(scanner)?;
        if identifier.get_identifier_lower_case() != Some("size") {
            return Err(parser_error(
                identifier.interval.clone(),
                ParserErrorKind::ExpectedSize { found: identifier },
            ));
        }
        let interval = factor.interval.extend_clone(&identifier.interval);
        AstNode {
            children: vec![factor, identifier],
            kind: AstNodeKind::DotOperator,
            interval,
        }
    } else {
        factor
    })
}

/// Parses an identifier or predefined identifier into an identifier.
fn parse_identifier_declaration(
    scanner: &mut Peekable<Scanner<impl Iterator<Item = Result<char>>>>,
) -> Result<AstNode> {
    trace!("parse_identifier_declaration {:?}", scanner.peek());

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
                symbol_index: 0,
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
                symbol_index: 0,
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

/// Parses an identifier or predefined identifier into an identifier or predefined identifier.
fn parse_identifier_usage(
    scanner: &mut Peekable<Scanner<impl Iterator<Item = Result<char>>>>,
) -> Result<AstNode> {
    trace!("parse_identifier_usage {:?}", scanner.peek());

    let result = match scanner.next() {
        Some((
            Ok(Token::Identifier {
                original,
                lower_case,
            }),
            interval,
        )) => AstNode::leaf(
            AstNodeKind::Identifier {
                original,
                lower_case,
                symbol_index: 0,
            },
            interval,
        ),
        Some((
            Ok(Token::PredefinedIdentifier {
                original,
                lower_case,
            }),
            interval,
        )) => AstNode::leaf(
            AstNodeKind::PredefinedIdentifier {
                original,
                lower_case,
                symbol_index: 0,
            },
            interval,
        ),
        Some((Ok(other), interval)) => {
            return Err(parser_error(
                interval,
                ParserErrorKind::ExpectedIdentifier { found: other },
            ))
        }
        Some((Err(error), _)) => return Err(error),
        None => return Err(Error::UnexpectedEndOfInput),
    };

    if let Some(_) = optional_token(scanner, Token::OpenBracket)? {
        let index_expression = parse_expression(scanner)?;
        let end_interval = expect_token(scanner, Token::CloseBracket)?;
        let interval = result.interval.extend_clone(&end_interval);
        Ok(AstNode {
            children: vec![result, index_expression],
            kind: AstNodeKind::IndexOperator,
            interval,
        })
    } else {
        Ok(result)
    }
}

/// Expect the next token to be the given token.
/// Note that this should only be used for tokens without attached data.
fn expect_token(
    scanner: &mut Peekable<Scanner<impl Iterator<Item = Result<char>>>>,
    expected: Token,
) -> Result<ScanInterval> {
    trace!("expect_token {:?}", scanner.peek());

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

/// Consume the next token if it is the given expected token.
/// Note that this should only be used for tokens without attached data.
fn optional_token(
    scanner: &mut Peekable<Scanner<impl Iterator<Item = Result<char>>>>,
    expected: Token,
) -> Result<Option<ScanInterval>> {
    trace!("optional_token {:?}", scanner.peek());

    if let Some((token, _)) = scanner.peek() {
        match token {
            Ok(token) => Ok(if &expected == token {
                let (_, interval) = scanner.next().unwrap();
                Some(interval)
            } else {
                None
            }),
            Err(_) => Err(scanner.next().unwrap().0.unwrap_err()),
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
