use std::{collections::HashMap, mem};

use num_bigint::BigInt;

use crate::interpreter::ast::{
    ClassTable, Declaration, Expression, Function, ProgramFile, Statement,
};

pub struct Env {
    program: ProgramFile,
    vars: HashMap<String, VarValue>,
    stack: Vec<HashMap<String, VarValue>>,
}

struct VarValue {
    value: Value,
    mutable: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Null,
    Number(BigInt),
    U32(u32),
    Str(String),
    Struct(String, Vec<Value>),
    List(Vec<Value>),
    Class(String),
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
            stack: Vec::new(),
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
        if self.vars.contains_key(&name) {
            panic!("Variable {} already exists", name);
        }
        self.vars.insert(name, VarValue { value, mutable });
    }

    fn lookup_var_or_global(&self, name: &str) -> Option<Value> {
        let local = self.vars.get(name).map(|var| &var.value);
        if local.is_some() {
            return local.cloned();
        }
        self.lookup_class(name).map(|_| Value::Class(name.to_owned()))
    }

    fn remove_var(&mut self, name: &str) {
        if self.vars.contains_key(name) {
            self.vars.remove(name);
        } else {
            panic!("Variable {} not found", name);
        }
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

    fn eval_stmt(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Expr(expr) => {
                self.eval_expr(expr);
            }
            Statement::Let(var, expr, mutable) => {
                let value = self.eval_expr(expr);
                match var.as_slice() {
                    &[Expression::Token(ref x)] => {
                        self.create_var(x.clone(), value, *mutable);
                    }
                    _ => panic!("Unsupported variable in let statement"),
                }
            }
            Statement::Assign(kind, lhs, rhs) => {
                let name = self.to_name(lhs);
                let value = self.eval_expr(rhs);
                match kind as &str {
                    "=" => {
                        let var = self.vars.get_mut(&name).expect("Variable not found");
                        if !var.mutable {
                            panic!("Cannot assign to immutable variable");
                        }
                        var.value = value;
                    }
                    _ => panic!("Unsupported assignment operator"),
                }
            }
            Statement::For(header, body) => {
                assert!(header.len() == 2);
                let name = self.to_name(&header[0]);
                let iterable = self.eval_expr(&header[1]);
                if let Value::List(items) = iterable {
                    for item in items {
                        self.create_var(name.clone(), item, false);
                        for stmt in body {
                            self.eval_stmt(stmt);
                        }
                        self.remove_var(&name);
                    }
                } else {
                    panic!("For loop expects a list");
                }
            }
            _ => {
                unimplemented!("{:?}", stmt)
            }
        }
    }

    fn eval_expr(&mut self, expr: &Expression) -> Value {
        match expr {
            Expression::Empty | Expression::Null => Value::Null,
            Expression::Token(s) => self
                .lookup_var_or_global(s)
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
                let mut field_values = vec![Value::Null; self.get_class_field_count(&class_name)];
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
            Expression::Call(f, args) => {
                let func = self.to_name(f);
                let arg_values = args.iter().map(|arg| self.eval_expr(arg)).collect::<Vec<_>>();
                let mut vars = HashMap::new();
                mem::swap(&mut self.vars, &mut vars);
                self.stack.push(vars);
                let result = self.run(&func, &arg_values);
                self.vars = self.stack.pop().expect("Stack underflow");
                result
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
            Value::Class(class_name) => {
                match field {
                    "classes" => Value::List(self.list_classes(&class_name)),
                    _ => panic!("Unknown class field {} for class {}", field, class_name),
                }
            }
            _ => panic!("Not a struct or class: {:?}", structure),
        }
    }

    fn list_classes(&self, metaclass: &str) -> Vec<Value> {
        let search_value = Expression::Token(metaclass.to_owned());
        self.program
            .declarations
            .iter()
            .filter_map(|decl| match decl {
                Declaration::Class(class) if class.header.len() >= 2 && class.header[1] == search_value => {
                    if let Expression::Token(ref name) = class.header[0] {
                        Some(Value::Class(name.clone()))
                    } else {
                        None
                    }
                }
                _ => None,
            })
            .collect()
    }
}
