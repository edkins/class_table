use std::collections::HashMap;

use num_bigint::BigInt;

use crate::interpreter::ast::{
    ClassTable, Declaration, Expression, Function, ProgramFile, Statement,
};

pub struct Env {
    program: ProgramFile,
    vars: HashMap<String, VarValue>,
}

struct VarValue {
    value: Value,
    mutable: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Unit,
    Number(BigInt),
    U32(u32),
    Str(String),
    Struct(String, Vec<Value>),
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
    pub fn new(program: ProgramFile) -> Self {
        Env {
            program,
            vars: HashMap::new(),
        }
    }

    pub fn run(&mut self, func: &str, args: &[Value]) -> Value {
        let function = self
            .program
            .lookup_fn(func)
            .expect("Function not found")
            .clone();
        function.check_args(args);
        for (arg, param) in args.iter().zip(&function.params) {
            let name = self.typed_to_name(param);
            self.create_var(name, arg.clone(), false);
        }

        let result = self.eval_expr(&function.body);
        // TODO: type-check result
        result
    }

    fn create_var(&mut self, name: String, value: Value, mutable: bool) {
        self.vars.insert(name, VarValue { value, mutable });
    }

    fn lookup_var(&self, name: &str) -> Option<&Value> {
        self.vars.get(name).map(|var| &var.value)
    }

    fn lookup_class(&self, name: &str) -> Option<&ClassTable> {
        let search_value = Expression::Token(name.to_owned());
        self.program
            .declarations
            .iter()
            .find_map(|decl| match decl {
                Declaration::Class(class) if class.header[0] == search_value => Some(class),
                _ => None,
            })
    }

    fn eval_stmt(&mut self, stmt: &Statement) -> Value {
        match stmt {
            Statement::Empty => Value::Unit,
            Statement::Expr(expr) => self.eval_expr(expr),
            Statement::Let(var, expr, mutable) => {
                let value = self.eval_expr(expr);
                match var.as_slice() {
                    &[Expression::Token(ref x)] => {
                        self.create_var(x.clone(), value, *mutable);
                        Value::Unit
                    }
                    _ => panic!("Unsupported variable in let statement"),
                }
            }
            _ => {
                unimplemented!()
            }
        }
    }

    fn eval_expr(&mut self, expr: &Expression) -> Value {
        match expr {
            Expression::Empty => Value::Unit,
            Expression::Token(s) => self
                .lookup_var(s)
                .cloned()
                .unwrap_or_else(|| panic!("Variable {} not found", s)),
            Expression::Integer(n) => Value::Number(n.clone()),
            Expression::U32(n) => Value::U32(*n),
            Expression::Str(s) => Value::Str(s.clone()),
            Expression::MemberAccess(base, field) => {
                let base_val = self.eval_expr(base);
                self.get_struct_member(base_val, field)
            }
            Expression::Build(cl, fields) => {
                let class_name = self.to_name(cl);
                let mut field_values = vec![Value::Unit; self.get_class_field_count(&class_name)];
                for field in fields {
                    match field.as_slice() {
                        &[Expression::Token(ref name), ref expr] => {
                            let value = self.eval_expr(expr);
                            field_values[self.get_class_field_index(&class_name, name)] = value;
                            // TODO: check it's the correct type
                        }
                        _ => panic!("Invalid field assignment in constructor"),
                    }
                }
                Value::Struct(class_name, field_values)
            }
            Expression::Block(stmts, expr) => {
                for stmt in stmts {
                    self.eval_stmt(stmt);
                }
                self.eval_expr(expr)
            }
            _ => unimplemented!("Expression type not supported"),
        }
    }

    fn to_name(&self, expr: &Expression) -> String {
        match expr {
            Expression::Token(s) => s.clone(),
            _ => panic!("Expected token"),
        }
    }

    fn typed_to_name(&self, expr: &[Expression]) -> String {
        match expr {
            [Expression::Token(s), _] => s.clone(),
            _ => panic!("Expected typed token"),
        }
    }

    fn get_class_field_count(&mut self, class_name: &str) -> usize {
        let cl = self.lookup_class(class_name).expect("No such class");
        cl.body.len()
    }

    fn get_class_field_index(&mut self, class_name: &str, field: &str) -> usize {
        // TODO: this assumes the first column is the one to look up. This may change with metaclasses.
        let search_value = Expression::Token(field.to_owned());
        let cl = self.lookup_class(class_name).expect("No such class");
        cl.body
            .iter()
            .position(|f| f[0] == search_value)
            .expect("Field not found")
    }

    fn get_struct_member(&mut self, structure: Value, field: &str) -> Value {
        match structure {
            Value::Struct(class_name, fields) => {
                let field_index = self.get_class_field_index(&class_name, field);
                fields[field_index].clone()
            }
            _ => panic!("Not a struct"),
        }
    }
}
