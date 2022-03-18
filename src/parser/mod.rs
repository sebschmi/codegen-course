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
    Identifier { value: String },
    /// An identifier of a variable, function, procedure or program.
    /// This identifier is predefined and thus has special rules on its value.
    PredefinedIdentifier { value: String },
    /// The name of a type.
    Type { type_name: TypeName, is_array: bool },
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
pub fn build_ast<CharacterIterator: Iterator<Item = Result<char>>>(
    mut scanner: Scanner<CharacterIterator>,
) -> Result<Box<AstNode>> {
    parse_program(&mut scanner)
}

fn parse_program<CharacterIterator: Iterator<Item = Result<char>>>(
    scanner: &mut Scanner<CharacterIterator>,
) -> Result<Box<AstNode>> {
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

    todo!("parse main {:?}", main_interval)
}

fn parse_procedure<CharacterIterator: Iterator<Item = Result<char>>>(
    scanner: &mut Scanner<CharacterIterator>,
    start_interval: ScanInterval,
) -> Result<Box<AstNode>> {
    todo!()
}

fn parse_function<CharacterIterator: Iterator<Item = Result<char>>>(
    scanner: &mut Scanner<CharacterIterator>,
    start_interval: ScanInterval,
) -> Result<Box<AstNode>> {
    todo!()
}

/// Parses an identifier or predefined identifier into an identifier.
fn parse_identifier<CharacterIterator: Iterator<Item = Result<char>>>(
    scanner: &mut Scanner<CharacterIterator>,
) -> Result<Box<AstNode>> {
    match scanner.next_with_interval() {
        Some((Ok(Token::Identifier(identifier)), interval)) => Ok(Box::new(AstNode::leaf(
            AstNodeKind::Identifier { value: identifier },
            interval,
        ))),
        Some((Ok(Token::PredefinedIdentifier(identifier)), interval)) => Ok(Box::new(
            AstNode::leaf(AstNodeKind::Identifier { value: identifier }, interval),
        )),
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
fn expect_token<CharacterIterator: Iterator<Item = Result<char>>>(
    scanner: &mut Scanner<CharacterIterator>,
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
fn expect_tokens<CharacterIterator: Iterator<Item = Result<char>>>(
    scanner: &mut Scanner<CharacterIterator>,
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
