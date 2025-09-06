use num_bigint::BigInt;

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum ClassCell {
    Empty,
    Token(String),
    Integer(BigInt),
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct ClassTable {
    pub header: Vec<ClassCell>,
    pub body: Vec<Vec<ClassCell>>,
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Declaration {
    Class(ClassTable),
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct ProgramFile {
    pub declarations: Vec<Declaration>,
}