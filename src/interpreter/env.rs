use std::collections::HashMap;

use num_bigint::BigInt;

use crate::interpreter::ast::{Declaration, Expression, Function, ProgramFile, Statement};

pub struct Env {
    vars: HashMap<String, VarValue>,
}

struct VarValue {
    value: Value,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Unit,
    Number(BigInt),
    U32(u32),
}

impl ProgramFile {
    pub fn lookup_fn(&self, name: &str) -> Option<&Function> {
        let search_value = Expression::Token(name.to_owned());
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
        Env {
            vars: HashMap::new(),
        }
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

    fn create_var(&mut self, name: String, value: Value) {
        self.vars.insert(name, VarValue { value });
    }

    fn lookup_var(&self, name: &str) -> Option<&Value> {
        self.vars.get(name).map(|var| &var.value)
    }

    pub fn eval_stmt(&mut self, stmt: &Statement) -> Value {
        match stmt {
            Statement::Expr(expr) => self.eval_expr(expr),
            Statement::Let(var, expr) => {
                let value = self.eval_expr(expr);
                match var.as_slice() {
                    &[Expression::Token(ref x)] => {
                        self.create_var(x.clone(), value);
                        Value::Unit
                    }
                    _ => panic!("Unsupported variable in let statement"),
                }
            }
        }
    }

    pub fn eval_expr(&mut self, expr: &Expression) -> Value {
        match expr {
            Expression::Empty => Value::Unit,
            Expression::Token(s) => self
                .lookup_var(s)
                .cloned()
                .unwrap_or_else(|| panic!("Variable {} not found", s)),
            Expression::Integer(n) => Value::Number(n.clone()),
            Expression::U32(n) => Value::U32(*n),
            Expression::MemberAccess(base, field) => unimplemented!("Member access not implemented"),
        }
    }
}
