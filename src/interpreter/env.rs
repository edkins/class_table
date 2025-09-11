use core::panic;
use std::{collections::HashMap, mem};

use num_bigint::BigInt;
use num_traits::ToPrimitive;

use crate::interpreter::ast::{
    ClassTable, Declaration, Expression, Function, ProgramFile, Statement,
};

pub struct Env {
    program: Vec<(String, ProgramFile)>,
    current_module: String,
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
    Bool(bool),
    Number(BigInt),
    U32(u32),
    Str(String),
    Struct(String, Vec<Value>),
    List(Vec<Value>),
    Class(String, String),
}

// pub struct LoadedClass {
//     module_name: String,
//     class_name: String,
//     metaclass_module_name: String,
//     metaclass_name: String,
//     table: ClassTable,
// }

impl Function {
    fn check_args(&self, args: &[Value]) {
        if args.len() != self.params.len() {
            panic!("Argument count mismatch");
        }
        // TODO: type checking
    }
}

impl Env {
    pub fn new(module_name: &str, program: ProgramFile) -> Self {
        Env {
            program: vec![(module_name.to_owned(), program)],
            current_module: module_name.to_owned(),
            vars: HashMap::new(),
            stack: Vec::new(),
        }
    }

    pub fn lookup_fn(&self, name: &str) -> Option<&Function> {
        let search_value = Expression::Token(name.to_owned());
        for (_, program) in &self.program {
            for decl in &program.declarations {
                if let Declaration::Fn(func) = decl {
                    if func.header[0] == search_value {
                        return Some(func);
                    }
                }
            }
        }
        None
    }

    fn run_builtin(&self, func: &str, args: &[Value]) -> Option<Value> {
        match func {
            "==" => {
                if args.len() != 2 {
                    panic!("Invalid number of arguments for == operator");
                }
                let left = &args[0];
                let right = &args[1];
                Some(Value::Bool(left == right))
            }
            "!=" => {
                if args.len() != 2 {
                    panic!("Invalid number of arguments for != operator");
                }
                let left = &args[0];
                let right = &args[1];
                Some(Value::Bool(left != right))
            }
            "<" => {
                if args.len() != 2 {
                    panic!("Invalid number of arguments for < operator");
                }
                let left = &args[0];
                let right = &args[1];
                match (left, right) {
                    (Value::Number(l), Value::Number(r)) => Some(Value::Bool(l < r)),
                    (Value::U32(l), Value::U32(r)) => Some(Value::Bool(l < r)),
                    _ => panic!("Invalid types for < operator"),
                }
            }
            ">" => {
                if args.len() != 2 {
                    panic!("Invalid number of arguments for > operator");
                }
                let left = &args[0];
                let right = &args[1];
                match (left, right) {
                    (Value::Number(l), Value::Number(r)) => Some(Value::Bool(l > r)),
                    (Value::U32(l), Value::U32(r)) => Some(Value::Bool(l > r)),
                    _ => panic!("Invalid types for > operator"),
                }
            }
            "len" => {
                if args.len() != 1 {
                    panic!("Invalid number of arguments for len function");
                }
                match &args[0] {
                    Value::Str(s) => Some(Value::Number(s.len().into())),
                    Value::List(l) => Some(Value::Number(l.len().into())),
                    _ => panic!("Invalid type for len function"),
                }
            }
            "startswith" => {
                if args.len() != 2 {
                    panic!("Invalid number of arguments for startswith function");
                }
                match (&args[0], &args[1]) {
                    (Value::Str(s), Value::Str(prefix)) => Some(Value::Bool(s.starts_with(prefix))),
                    _ => panic!("Invalid types for startswith function"),
                }
            }
            "from" => {
                if args.len() != 2 {
                    panic!("Invalid number of arguments for from function");
                }
                match (&args[0], &args[1]) {
                    (Value::Str(s), Value::Number(n)) => {
                        Some(Value::Str(s[n.to_usize().unwrap()..].to_owned()))
                    }
                    (Value::Str(s), Value::U32(n)) => {
                        Some(Value::Str(s[*n as usize..].to_owned()))
                    }
                    _ => panic!("Invalid types for from function"),
                }
            }
            _ => None
        }
    }

    pub fn run(&mut self, func: &str, args: &[Value]) -> Value {
        // check builtin functions
        if let Some(result) = self.run_builtin(func, args) {
            return result;
        }

        let function = self
            .lookup_fn(func)
            .unwrap_or_else(|| panic!("Function not found: {}", func))
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
        self.lookup_class(name).map(|_| Value::Class(self.current_module.clone(), name.to_owned()))
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
        for (module_name, program) in &self.program {
            if *module_name == self.current_module {
                for decl in &program.declarations {
                    if let Declaration::Class(class) = decl {
                        if class.header[0] == search_value {
                            return Some(class);
                        }
                    }
                }
            }
        }
        None
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
            Statement::Loop(body) => {
                loop {
                    for stmt in body {
                        self.eval_stmt(stmt);
                    }
                }
            }
        }
    }

    fn eval_expr(&mut self, expr: &Expression) -> Value {
        match expr {
            Expression::Empty | Expression::Null => Value::Null,
            Expression::Token(s) => self
                .lookup_var_or_global(s)
                .unwrap_or_else(|| panic!("Variable {} not found", s)),
            Expression::Bool(b) => Value::Bool(*b),
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
            Expression::And(left, right) => {
                let left_val = self.eval_expr(left);
                match left_val {
                    Value::Bool(true) => self.eval_expr(right),
                    Value::Bool(false) => Value::Bool(false),
                    _ => panic!("Invalid type for && operator"),
                }
            }
            Expression::Or(left, right) => {
                let left_val = self.eval_expr(left);
                match left_val {
                    Value::Bool(true) => Value::Bool(true),
                    Value::Bool(false) => self.eval_expr(right),
                    _ => panic!("Invalid type for || operator"),
                }
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
            Expression::If(cond, then_body, else_body) => {
                let condition = self.eval_expr(cond);
                match condition {
                    Value::Bool(true) => {
                        self.eval_expr(then_body)
                    }
                    Value::Bool(false) => {
                        self.eval_expr(else_body)
                    }
                    _ => {
                        unimplemented!("If condition not true or false: {:?}", condition)
                    }
                }
            }
            _ => unimplemented!("Expression type not supported: {:?}", expr),
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
            Value::Class(module_name, class_name) => {
                // TODO: consult metaclass
                match field {
                    "name" => Value::Str(class_name),
                    "classes" => Value::List(self.list_classes(&module_name, &class_name)),
                    _ => panic!("Unknown class field {} for class {}::{}", field, module_name, class_name),
                }
            }
            _ => panic!("Not a struct or class: {:?}", structure),
        }
    }

    fn list_classes(&self, metaclass_module: &str, metaclass: &str) -> Vec<Value> {
        let search_value = Expression::Token(metaclass.to_owned());
        let mut result = vec![];
        for (module_name, program) in &self.program {
            for decl in &program.declarations {
                // TODO: also allow classes to have a metaclass in a different module
                if let Declaration::Class(class) = decl && module_name == metaclass_module {
                    if class.header.len() >= 2 && class.header[1] == search_value {
                        result.push(Value::Class(module_name.clone(), self.to_name(&class.header[0])));
                    }
                }
            }
        }
        result
    }
}
