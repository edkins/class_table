use num_bigint::BigInt;

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Expression {
    Empty,
    Null,
    Token(String),
    Integer(BigInt),
    U32(u32),
    Str(String),
    MemberAccess(Box<Expression>, String),
    Subscript(Box<Expression>, Vec<Expression>),
    Call(Box<Expression>, Vec<Expression>),
    And(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),
    Build(Box<Expression>, Vec<Vec<Expression>>),
    Block(Vec<Statement>, Box<Expression>),
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct ClassTable {
    pub header: Vec<Expression>,
    pub body: Vec<Vec<Expression>>,
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Statement {
    Expr(Expression),
    Let(Vec<Expression>, Expression, bool),
    Assign(String, Expression, Expression),
    For(Vec<Expression>, Vec<Statement>),
    If(Expression, Vec<Statement>, Vec<Statement>),
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct Function {
    pub header: Vec<Expression>,
    pub params: Vec<Vec<Expression>>,
    pub ret: Vec<Expression>,
    pub body: Expression,
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Declaration {
    Class(ClassTable),
    Fn(Function),
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct ProgramFile {
    pub declarations: Vec<Declaration>,
}
