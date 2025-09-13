use core::panic;
use std::{collections::HashMap, mem};

use num_bigint::BigInt;
use num_traits::ToPrimitive;

use crate::interpreter::ast::{
    ClassTable, Declaration, Expression, Function, Impl, ProgramFile, Statement
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
    Int(BigInt),
    U32(u32),
    Str(String),
    Struct(String, Vec<Value>),
    Impl(String, Box<Value>),
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

enum Buildable {
    Class,
    Impl,
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
                    (Value::Int(l), Value::Int(r)) => Some(Value::Bool(l < r)),
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
                    (Value::Int(l), Value::Int(r)) => Some(Value::Bool(l > r)),
                    (Value::U32(l), Value::U32(r)) => Some(Value::Bool(l > r)),
                    _ => panic!("Invalid types for > operator"),
                }
            }
            "+" => {
                if args.len() != 2 {
                    panic!("Invalid number of arguments for + operator");
                }
                let left = &args[0];
                let right = &args[1];
                match (left, right) {
                    (Value::Int(l), Value::Int(r)) => Some(Value::Int(l + r)),
                    (Value::U32(l), Value::U32(r)) => Some(Value::U32(l + r)),
                    (Value::Str(l), Value::Str(r)) => Some(Value::Str(l.to_owned() + r)),
                    _ => panic!("Invalid types for + operator"),
                }
            }
            "assert" => {
                if args.len() != 1 {
                    panic!("Invalid number of arguments for assert function");
                }
                assert!(args[0] == Value::Bool(true));
                Some(Value::Null)
            }
            "len" => {
                if args.len() != 1 {
                    panic!("Invalid number of arguments for len function");
                }
                match &args[0] {
                    Value::Str(s) => Some(Value::Int(s.len().into())),
                    Value::List(l) => Some(Value::Int(l.len().into())),
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
                    (Value::Str(s), Value::Int(n)) => {
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

    pub fn run_method(&mut self, impl_name: Option<&str>, method: &str, args: &[Value]) -> Value {
        let method_impl = self
            .lookup_method_impl(impl_name, args.get(0), method)
            .unwrap_or_else(|| panic!("Method not found: {} for {:?} : {:?}", method, args.get(0), impl_name))
            .clone();
        method_impl.check_args(args);
        for (arg, param) in args.iter().zip(&method_impl.params) {
            let name = self.typed_to_name(param);
            self.create_var(name, arg.clone(), false);
        }

        let result = self.eval_expr(&method_impl.body);
        // TODO: type-check result
        result
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

    fn lookup_impl(&self, name: &str) -> Option<&Impl> {
        let search_value = Expression::Token(name.to_owned());
        for (module_name, program) in &self.program {
            if *module_name == self.current_module {
                for decl in &program.declarations {
                    if let Declaration::Impl(impl_def) = decl {
                        if impl_def.header[1] == search_value {
                            return Some(impl_def);
                        }
                    }
                }
            }
        }
        None
    }

    fn get_class(&self, value: &Value) -> String {
        match value {
            Value::Null => "null".to_owned(),
            Value::Bool(_) => "bool".to_owned(),
            Value::Int(_) => "int".to_owned(),
            Value::U32(_) => "u32".to_owned(),
            Value::Str(_) => "str".to_owned(),
            Value::Struct(class_name, _) => class_name.clone(),
            _ => panic!("Unable to get class of {:?}", value),
        }
    }

    fn lookup_method_impl(&self, impl_name: Option<&str>, value: Option<&Value>, method: &str) -> Option<&Function> {
        let search_value = Expression::Token(method.to_owned());
        if let Some(impl_name) = impl_name {
            let impl_def = self.lookup_impl(impl_name)?;
            for func in &impl_def.methods {
                if func.header[0] == search_value {
                    return Some(func);
                }
            }
        } else if let Some(value) = value {
            let class_name = self.get_class(value);
            let class_search_value = Expression::Token(class_name.to_owned());
            for (module_name, program) in &self.program {
                if *module_name == self.current_module {
                    for decl in &program.declarations {
                        if let Declaration::Impl(impl_def) = decl {
                            if impl_def.header[0] == class_search_value {
                                for func in &impl_def.methods {
                                    if func.header[0] == search_value {
                                        return Some(func);
                                    }
                                }
                            }
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
                    "+=" => {
                        let var = self.vars.get_mut(&name).expect("Variable not found");
                        if !var.mutable {
                            panic!("Cannot assign to immutable variable");
                        }
                        let new_value = match (&var.value, &value) {
                            (Value::Int(l), Value::Int(r)) => Value::Int(l + r),
                            (Value::U32(l), Value::U32(r)) => Value::U32(l + r),
                            (Value::Str(l), Value::Str(r)) => Value::Str(l.to_owned() + r),
                            _ => panic!("Invalid types for += operator"),
                        };
                        var.value = new_value;
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
            Expression::Integer(n) => Value::Int(n.clone()),
            Expression::U32(n) => Value::U32(*n),
            Expression::Str(s) => Value::Str(s.clone()),
            Expression::List(elements) => {
                let values = elements.iter().map(|e| self.eval_expr(e)).collect();
                Value::List(values)
            }
            Expression::FieldAccess(base, field) => {
                let base_val = self.eval_expr(base);
                self.get_struct_member(base_val, field)
            }
            Expression::Build(cl, fields) => {
                let class_name = self.to_name(cl);
                match self.get_buildable(&class_name) {
                    Some(Buildable::Class) => {
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
                    Some(Buildable::Impl) => {
                        assert!(fields.len() == 1);
                        assert!(fields[0].len() == 1);
                        // TODO: check the thing actually supports this impl
                        let value = self.eval_expr(&fields[0][0]);
                        Value::Impl(class_name, Box::new(value))
                    }
                    None => panic!("Class not found: {}", class_name),
                }
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
            Expression::MethodCall(base, method, args) => {
                let base_impl = self.eval_expr(base);
                let (impl_name, base_val) = if let Value::Impl(impl_name, base_val) = base_impl {
                    (Some(impl_name), *base_val)
                } else {
                    (None, base_impl)
                };
                let mut arg_values = vec![base_val];
                arg_values.extend(args.iter().map(|arg| self.eval_expr(arg)));
                let mut vars = HashMap::new();
                mem::swap(&mut self.vars, &mut vars);
                self.stack.push(vars);
                let result = self.run_method(impl_name.as_deref(), method, &arg_values);
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
            _ => panic!("Expected token, got {:?}", expr),
        }
    }

    fn typed_to_name(&self, expr: &[Expression]) -> String {
        match expr {
            [Expression::Token(s), _] => s.clone(),
            _ => panic!("Expected typed token"),
        }
    }

    fn get_buildable(&self, class_name: &str) -> Option<Buildable> {
        if self.lookup_class(class_name).is_some() {
            Some(Buildable::Class)
        } else if self.lookup_impl(class_name).is_some() {
            Some(Buildable::Impl)
        } else {
            None
        }
    }

    fn get_class_field_count(&self, class_name: &str) -> usize {
        let cl = self.lookup_class(class_name).expect("No such class");
        cl.body.len()
    }

    fn get_class_field_index(&self, class_name: &str, field: &str) -> usize {
        // TODO: this assumes the first column is the one to look up. This may change with metaclasses.
        let search_value = Expression::Token(field.to_owned());
        let cl = self.lookup_class(class_name).expect("No such class");
        cl.body
            .iter()
            .position(|f| f[0] == search_value)
            .expect("Field not found")
    }

    fn get_struct_member(&self, structure: Value, field: &str) -> Value {
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
