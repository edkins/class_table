use core::panic;
use std::{collections::HashMap, mem};

use num_traits::ToPrimitive;

use crate::interpreter::{
    ast::ProgramFile,
    check::{LoadedProgram, SyntacticProgram, Type, Value, VarName},
    check_expr::{CheckedExpr, CheckedFunction, CheckedStatement},
};

pub struct Env {
    program: LoadedProgram,
    current_module: String,
    vars: HashMap<VarName, VarValue>,
    stack: Vec<HashMap<VarName, VarValue>>,
}

struct VarValue {
    value: Value,
    mutable: bool,
}

impl CheckedFunction {
    fn check_args(&self, args: &[Value], program: &LoadedProgram) {
        if args.len() != self.params.len() {
            panic!(
                "Argument count mismatch: expected {}, got {}",
                self.params.len(),
                args.len()
            );
        }
        for (arg, param) in args.iter().zip(&self.params) {
            if !arg.is_instance(param.get_type(), program) {
                panic!(
                    "Argument type mismatch: expected {:?}, got {:?}",
                    param.get_type(),
                    arg
                );
            }
        }
    }
}

impl Env {
    pub fn new(programs: &[(String, ProgramFile)], main_module: &str) -> Self {
        let program = SyntacticProgram {
            modules: programs.iter().cloned().collect(),
        };
        let program = LoadedProgram::new(&program);
        Env {
            program,
            current_module: main_module.to_owned(),
            vars: HashMap::new(),
            stack: Vec::new(),
        }
    }

    fn run_builtin(&self, func: &str, args: &[Value]) -> Option<Value> {
        match func {
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
                    (Value::Str(s), Value::U32(n)) => Some(Value::Str(s[*n as usize..].to_owned())),
                    _ => panic!("Invalid types for from function"),
                }
            }
            _ => None,
        }
    }

    fn run_builtin_method(&self, method: &str, args: &[Value]) -> Value {
        match method {
            "==" => {
                if args.len() != 2 {
                    panic!("Invalid number of arguments for == operator");
                }
                let left = &args[0];
                let right = &args[1];
                Value::Bool(left == right)
            }
            "!=" => {
                if args.len() != 2 {
                    panic!("Invalid number of arguments for != operator");
                }
                let left = &args[0];
                let right = &args[1];
                Value::Bool(left != right)
            }
            "<" => {
                if args.len() != 2 {
                    panic!("Invalid number of arguments for < operator");
                }
                let left = &args[0];
                let right = &args[1];
                match (left, right) {
                    (Value::Int(l), Value::Int(r)) => Value::Bool(l < r),
                    (Value::U32(l), Value::U32(r)) => Value::Bool(l < r),
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
                    (Value::Int(l), Value::Int(r)) => Value::Bool(l > r),
                    (Value::U32(l), Value::U32(r)) => Value::Bool(l > r),
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
                    (Value::Int(l), Value::Int(r)) => Value::Int(l + r),
                    (Value::U32(l), Value::U32(r)) => Value::U32(l + r),
                    (Value::Str(l), Value::Str(r)) => Value::Str(l.to_owned() + r),
                    _ => panic!("Invalid types for + operator"),
                }
            }
            "field_names" => {
                if args.len() != 1 {
                    panic!("Invalid number of arguments for field_names method");
                }
                match &args[0] {
                    Value::Struct(class_module, class_name, fields) => {
                        let cl = self
                            .program
                            .lookup_class(class_module, class_name)
                            .expect("No such class");
                        let field_names = cl.field_names();
                        assert!(field_names.len() == fields.len());
                        Value::List(field_names.into_iter().map(Value::Str).collect())
                    }
                    _ => panic!("Invalid argument for field_names method"),
                }
            }
            _ => panic!("Unknown builtin method: {}", method),
        }
    }

    fn run_method(&mut self, implementing_class: &Type, method: &str, args: &[Value]) -> Value {
        let method_impl = self
            .lookup_checked_method_impl(implementing_class, method)
            .clone();
        method_impl.check_args(args, &self.program);
        for (arg, param) in args.iter().zip(&method_impl.params) {
            let name = param.name();
            self.create_var(name, arg.clone(), false);
        }

        if matches!(method_impl.body, CheckedExpr::Builtin(_)) {
            return self.run_builtin_method(method, args);
        }

        let result = self.eval_expr(&method_impl.body);
        assert!(
            result.is_instance(&method_impl.ret, &self.program),
            "Method return type mismatch: expected {:?}, got {:?}",
            method_impl.ret,
            result
        );
        result
    }

    pub fn run(&mut self, func: &str, args: &[Value]) -> Value {
        // check builtin functions
        if let Some(result) = self.run_builtin(func, args) {
            return result;
        }

        let function = self
            .program
            .lookup_checked_fn(&self.current_module, func)
            .unwrap_or_else(|| panic!("Function not found: {}", func))
            .clone();
        function.check_args(args, &self.program);
        for (arg, param) in args.iter().zip(&function.params) {
            let name = param.name();
            self.create_var(name, arg.clone(), false);
        }

        // TODO: type-check result
        self.eval_expr(&function.body)
    }

    fn create_var(&mut self, name: VarName, value: Value, mutable: bool) {
        if self.vars.contains_key(&name) {
            panic!("Variable {:?} already exists", name);
        }
        self.vars.insert(name, VarValue { value, mutable });
    }

    fn lookup_var_or_global(&self, name: &VarName) -> Option<Value> {
        let local = self.vars.get(name).map(|var| &var.value);
        if local.is_some() {
            return local.cloned();
        }
        match name {
            VarName::Name(name) => self
                .program
                .lookup_class(&self.current_module, name)
                .map(|_| Value::Class(self.current_module.clone(), name.to_owned())),
            VarName::SelfKeyword => panic!("'self' used outside of method"),
        }
    }

    fn remove_var(&mut self, name: &VarName) {
        if self.vars.contains_key(name) {
            self.vars.remove(name);
        } else {
            panic!("Variable {:?} not found", name);
        }
    }

    fn lookup_checked_method_impl(
        &self,
        implementing_class: &Type,
        method: &str,
    ) -> &CheckedFunction {
        match implementing_class {
            Type::Impl(_, _) | Type::Class(_, _, _) => {
                let impl_defs = self.program.lookup_type_as_impl(implementing_class);
                let mut candidates = vec![];
                for impl_def in impl_defs {
                    if let Some(m) = impl_def.lookup_checked_method_impl(method) {
                        candidates.push(m);
                    }
                }
                match candidates.len() {
                    0 => panic!(
                        "No method {} in implementing class {:?}",
                        method, implementing_class
                    ),
                    1 => candidates[0],
                    _ => panic!(
                        "Ambiguous method {} in implementing class {:?}: found {} candidates",
                        method,
                        implementing_class,
                        candidates.len()
                    ),
                }
            }
            _ => {
                panic!("Implementing class must be an impl or class type")
            }
        }
    }

    fn eval_stmt(&mut self, stmt: &CheckedStatement) {
        match stmt {
            CheckedStatement::Expr(expr) => {
                self.eval_expr(expr);
            }
            CheckedStatement::Let(var, t, expr, mutable) => {
                let value = self.eval_expr(expr).should_be_type(t, &self.program);
                self.create_var(VarName::Name(var.clone()), value, *mutable);
            }
            CheckedStatement::Assign(kind, lhs, rhs) => {
                let value = self.eval_expr(rhs);
                match kind as &str {
                    "=" => {
                        let var = self.vars.get_mut(lhs).expect("Variable not found");
                        if !var.mutable {
                            panic!("Cannot assign to immutable variable");
                        }
                        var.value = value;
                    }
                    "+=" => {
                        let var = self.vars.get_mut(lhs).expect("Variable not found");
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
            CheckedStatement::For(name, t, iterable_expr, body) => {
                let name = VarName::Name(name.clone());
                let iterable = self.eval_expr(iterable_expr);
                if let Value::List(items) = iterable {
                    for item in items {
                        assert!(item.is_instance(t, &self.program));
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
            CheckedStatement::Loop(body) => loop {
                for stmt in body {
                    self.eval_stmt(stmt);
                }
            },
        }
    }

    fn eval_expr(&mut self, expr: &CheckedExpr) -> Value {
        match expr {
            CheckedExpr::Null => Value::Null,
            CheckedExpr::Token(s, t) => self
                .lookup_var_or_global(&VarName::Name(s.clone()))
                .unwrap_or_else(|| panic!("Variable {} not found", s))
                .should_be_type(t, &self.program),
            CheckedExpr::Bool(b) => Value::Bool(*b),
            CheckedExpr::Integer(n) => Value::Int(n.clone()),
            CheckedExpr::U32(n) => Value::U32(*n),
            CheckedExpr::Str(s) => Value::Str(s.clone()),
            CheckedExpr::List(elements, t) => {
                let values = elements.iter().map(|e| self.eval_expr(e)).collect();
                Value::List(values).should_be_type(t, &self.program)
            }
            CheckedExpr::SelfKeyword(t) => self
                .lookup_var_or_global(&VarName::SelfKeyword)
                .expect("'self' used outside of method")
                .should_be_type(t, &self.program),
            CheckedExpr::FieldAccess(base, field, t) => {
                let base_val = self.eval_expr(base);
                self.get_struct_member(base_val, field)
                    .should_be_type(t, &self.program)
            }
            CheckedExpr::BuildClass(buildable, fields) => {
                if let Type::Class(class_module, class_name, args) = buildable
                    && args.is_empty()
                {
                    let mut field_values =
                        vec![Value::Null; self.get_class_field_count(class_module, class_name)];
                    for (name, expr) in fields {
                        let value = self.eval_expr(expr);
                        field_values[self.get_class_field_index(class_module, class_name, name)] =
                            value;
                    }
                    Value::Struct(class_module.clone(), class_name.clone(), field_values)
                } else {
                    panic!("Can only build non-generic classes this way");
                }
            }
            CheckedExpr::BuildImpl(buildable, value) => {
                if let Type::Impl(module, head) = buildable {
                    let value = self.eval_expr(value);
                    Value::Impl(module.clone(), head.clone(), Box::new(value))
                } else {
                    panic!("Can only build impls this way");
                }
            }
            CheckedExpr::Block(stmts, expr) => {
                for stmt in stmts {
                    self.eval_stmt(stmt);
                }
                self.eval_expr(expr)
            }
            CheckedExpr::And(left, right) => {
                let left_val = self.eval_expr(left);
                match left_val {
                    Value::Bool(true) => self.eval_expr(right),
                    Value::Bool(false) => Value::Bool(false),
                    _ => panic!("Invalid type for && operator"),
                }
            }
            CheckedExpr::Or(left, right) => {
                let left_val = self.eval_expr(left);
                match left_val {
                    Value::Bool(true) => Value::Bool(true),
                    Value::Bool(false) => self.eval_expr(right),
                    _ => panic!("Invalid type for || operator"),
                }
            }
            CheckedExpr::Call(f_module, f_name, args, t) => {
                let arg_values = args
                    .iter()
                    .map(|arg| self.eval_expr(arg))
                    .collect::<Vec<_>>();
                let mut vars = HashMap::new();
                mem::swap(&mut self.vars, &mut vars);
                self.stack.push(vars);
                let old_module = self.current_module.clone();
                self.current_module = f_module.clone();
                let result = self.run(f_name, &arg_values);
                self.vars = self.stack.pop().expect("Stack underflow");
                self.current_module = old_module;
                result.should_be_type(t, &self.program)
            }
            CheckedExpr::MethodCall(base, implementing_class, method, args, t) => {
                let base_impl = self.eval_expr(base);
                // let (impl_name, _impl_args, base_val) =
                //     if let Value::Impl(impl_module, impl_name, impl_args, base_val) = base_impl {
                //         (Some((impl_module, impl_name)), impl_args, *base_val)
                //     } else {
                //         (None, Vec::new(), base_impl)
                //     };
                let mut arg_values = vec![base_impl];
                arg_values.extend(args.iter().map(|arg| self.eval_expr(arg)));
                let mut vars = HashMap::new();
                mem::swap(&mut self.vars, &mut vars);
                self.stack.push(vars);
                let result = self.run_method(implementing_class, method, &arg_values);
                self.vars = self.stack.pop().expect("Stack underflow");
                result.should_be_type(t, &self.program)
            }
            CheckedExpr::If(cond, then_body, else_body, t) => {
                let condition = self.eval_expr(cond);
                match condition {
                    Value::Bool(true) => self.eval_expr(then_body).should_be_type(t, &self.program),
                    Value::Bool(false) => {
                        self.eval_expr(else_body).should_be_type(t, &self.program)
                    }
                    _ => {
                        unimplemented!("If condition not true or false: {:?}", condition)
                    }
                }
            }
            _ => unimplemented!("Expression type not supported: {:?}", expr),
        }
    }

    fn get_class_field_count(&self, class_module: &str, class_name: &str) -> usize {
        let cl = self
            .program
            .lookup_class(class_module, class_name)
            .unwrap_or_else(|| panic!("No such class: {}::{}", class_module, class_name));
        cl.num_fields()
    }

    fn get_class_field_index(&self, class_module: &str, class_name: &str, field: &str) -> usize {
        // TODO: this assumes the first column is the one to look up. This may change with metaclasses.
        let cl = self
            .program
            .lookup_class(class_module, class_name)
            .expect("No such class");
        cl.field_names()
            .iter()
            .position(|f| f == field)
            .expect("Field not found")
    }

    fn get_struct_member(&self, structure: Value, field: &str) -> Value {
        match structure {
            Value::Struct(class_module, class_name, fields) => {
                let field_index = self.get_class_field_index(&class_module, &class_name, field);
                fields[field_index].clone()
            }
            Value::Class(module_name, class_name) => {
                // TODO: consult metaclass
                match field {
                    "name" => Value::Str(class_name),
                    // "classes" => Value::List(self.list_classes(&module_name, &class_name)),
                    _ => panic!(
                        "Unknown class field {} for class {}::{}",
                        field, module_name, class_name
                    ),
                }
            }
            _ => panic!("Not a struct or class: {:?}", structure),
        }
    }

    // fn list_classes(&self, metaclass_module: &str, metaclass: &str) -> Vec<Value> {
    //     let search_value = Expression::Token(metaclass.to_owned());
    //     let mut result = vec![];
    //     for (module_name, program) in &self.program {
    //         for decl in &program.declarations {
    //             // TODO: also allow classes to have a metaclass in a different module
    //             if let Declaration::Class(class) = decl && module_name == metaclass_module && class.header.len() >= 2 && class.header[1] == search_value {
    //                 result.push(Value::Class(
    //                     module_name.clone(),
    //                     self.to_name(&class.header[0]).expect_name(),
    //                 ));
    //             }
    //         }
    //     }
    //     result
    // }
}

impl Value {
    fn get_class(&self) -> (String, String) {
        match self {
            Value::Null => ("std".to_owned(), "null".to_owned()),
            Value::Bool(_) => ("std".to_owned(), "bool".to_owned()),
            Value::Int(_) => ("std".to_owned(), "int".to_owned()),
            Value::U32(_) => ("std".to_owned(), "u32".to_owned()),
            Value::Str(_) => ("std".to_owned(), "str".to_owned()),
            Value::Struct(module_name, class_name, _) => (module_name.clone(), class_name.clone()),
            _ => panic!("Unable to get class of {:?}", self),
        }
    }

    fn should_be_type(self, typ: &Type, program: &LoadedProgram) -> Self {
        if self.is_instance(typ, program) {
            self
        } else {
            panic!("Type mismatch: expected {:?}, got {:?}", typ, self);
        }
    }

    fn is_instance(&self, typ: &Type, program: &LoadedProgram) -> bool {
        match typ {
            Type::Class(class_module, class_name, args) => {
                if class_module == "std" && class_name == "list" {
                    if args.len() != 1 {
                        panic!("List type must have one type argument");
                    }
                    if let Value::List(elements) = self {
                        return elements.iter().all(|e| e.is_instance(&args[0], program));
                    } else {
                        return false;
                    }
                }
                assert!(args.is_empty(), "Generic types not supported yet");
                let (my_module, my_class) = self.get_class();
                program.is_subclass(&my_module, &my_class, class_module, class_name)
            }
            Type::Impl(module, head) => {
                if let Value::Impl(impl_module, impl_head, _) = self {
                    module == impl_module && head == impl_head
                } else {
                    false
                }
            }
            Type::Literal(lit) => self == lit,
            _ => unimplemented!("Type checking not implemented for type {:?}", typ),
        }
    }
}
