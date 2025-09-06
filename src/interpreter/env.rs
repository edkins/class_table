use num_bigint::BigInt;

use crate::interpreter::ast::{ClassCell, Declaration, Function, ProgramFile, Statement};

pub struct Env {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Unit,
    Number(BigInt),
    U32(u32),
}

impl ProgramFile {
    pub fn lookup_fn(&self, name: &str) -> Option<&Function> {
        let search_value = ClassCell::Token(name.to_owned());
        self.declarations.iter().find_map(|decl| match decl {
            Declaration::Fn(func) if func.header[0] == search_value => Some(func),
            _ => None,
        })
    }
}

impl Function {
    fn check_args(&self, args: &[Value]) {
        if args.len() != self.params.len() {
            panic!("Argument count mismatch");
        }
        // TODO: type checking
    }
}

impl Env {
    pub fn new() -> Self {
        Env {}
    }

    pub fn run(&mut self, program: &ProgramFile, func: &str, args: &[Value]) -> Value {
        let function = program.lookup_fn(func).expect("Function not found");
        function.check_args(args);
        let mut result = Value::Unit;
        for stmt in &function.body {
            result = self.eval_stmt(stmt);
        }
        // TODO: type-check result
        result
    }

    pub fn eval_stmt(&mut self, stmt: &Statement) -> Value {
        match stmt {
            Statement::Expr(expr) => self.eval_expr(expr),
        }
    }

    pub fn eval_expr(&mut self, expr: &ClassCell) -> Value {
        match expr {
            ClassCell::Empty => Value::Unit,
            ClassCell::Token(s) => unimplemented!("Variable lookup for {}", s),
            ClassCell::Integer(n) => Value::Number(n.clone()),
            ClassCell::U32(n) => Value::U32(*n),
        }
    }
}
