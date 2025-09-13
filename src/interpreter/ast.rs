use num_bigint::BigInt;

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Expression {
    Empty,
    Null,
    Builtin,
    Bool(bool),
    Token(String),
    Integer(BigInt),
    U32(u32),
    Str(String),
    SelfKeyword,
    List(Vec<Expression>),
    FieldAccess(Box<Expression>, String),
    MethodCall(Box<Expression>, String, Vec<Expression>),
    Subscript(Box<Expression>, Vec<Vec<Expression>>),
    Call(Box<Expression>, Vec<Expression>),
    And(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),
    Build(Box<Expression>, Vec<Vec<Expression>>),
    Block(Vec<Statement>, Box<Expression>),
    If(Box<Expression>, Box<Expression>, Box<Expression>),
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
    Loop(Vec<Statement>),
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct Function {
    pub header: Vec<Expression>,
    pub params: Vec<Vec<Expression>>,
    pub ret: Vec<Expression>,
    pub body: Expression,
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct Impl {
    pub header: Vec<Expression>,
    pub methods: Vec<Function>,
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct Trait {
    pub header: Vec<Expression>,
    pub methods: Vec<Function>,
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Declaration {
    Use(String, String),
    Class(ClassTable),
    Fn(Function),
    Impl(Impl),
    Trait(Trait),
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct ProgramFile {
    pub declarations: Vec<Declaration>,
}
