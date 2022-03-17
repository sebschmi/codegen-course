/// A node of the abstract syntax tree.
pub struct AstNode {
    /// The children of the node.
    /// A valid number of children is decided by the kind of the node.
    children: Vec<Box<AstNode>>,

    /// The kind of the node.
    kind: AstNodeKind,

    /// The line this node starts in.
    start_line: usize,
    /// The line this node ends in.
    end_line: usize,
    /// The column this node starts in.
    start_column: usize,
    /// The column this node ends in.
    end_column: usize,
}

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

pub enum TypeName {
    Boolean,
    Integer,
    Real,
    String,
}
