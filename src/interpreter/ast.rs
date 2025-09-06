use num_bigint::BigInt;

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum ClassCell {
    Empty,
    Token(String),
    Integer(BigInt),
    U32(u32),
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct ClassTable {
    pub header: Vec<ClassCell>,
    pub body: Vec<Vec<ClassCell>>,
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Statement {
    Expr(ClassCell),
    Let(Vec<ClassCell>, ClassCell),
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct Function {
    pub header: Vec<ClassCell>,
    pub params: Vec<Vec<ClassCell>>,
    pub ret: Vec<ClassCell>,
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
