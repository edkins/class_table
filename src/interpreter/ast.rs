use num_bigint::BigInt;

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Expression {
    Empty,
    Token(String),
    Integer(BigInt),
    U32(u32),
    MemberAccess(Box<Expression>, String),
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct ClassTable {
    pub header: Vec<Expression>,
    pub body: Vec<Vec<Expression>>,
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Statement {
    Expr(Expression),
    Let(Vec<Expression>, Expression),
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct Function {
    pub header: Vec<Expression>,
    pub params: Vec<Vec<Expression>>,
    pub ret: Vec<Expression>,
    pub body: Vec<Statement>,
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
