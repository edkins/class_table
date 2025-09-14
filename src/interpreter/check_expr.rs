use std::collections::HashMap;

use num_bigint::BigInt;

use crate::interpreter::{
    ast::{Expression, Statement},
    check::{ArgCell, LoadedFunction, LoadedFunctionSignature, LoadedProgram, Type, VarName},
};

#[derive(Clone, Debug)]
pub struct CheckedFunction {
    pub params: Vec<ArgCell>,
    pub ret: Type,
    pub body: CheckedExpr,
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum CheckedExpr {
    Null,
    Bool(bool),
    Token(String, Type),
    Integer(BigInt),
    U32(u32),
    Str(String),
    SelfKeyword(Type),
    List(Vec<CheckedExpr>, Type),
    FieldAccess(Box<CheckedExpr>, String, Type),
    MethodCall(Box<CheckedExpr>, Type, String, Vec<CheckedExpr>, Type),
    // Subscript(Box<CheckedExpr>, Vec<CheckedExpr>, Type),
    Call(String, String, Vec<CheckedExpr>, Type),
    And(Box<CheckedExpr>, Box<CheckedExpr>),
    Or(Box<CheckedExpr>, Box<CheckedExpr>),
    BuildClass(Type, Vec<(String, CheckedExpr)>),
    BuildImpl(Type, Box<CheckedExpr>),
    Block(Vec<CheckedStatement>, Box<CheckedExpr>),
    If(Box<CheckedExpr>, Box<CheckedExpr>, Box<CheckedExpr>),
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum CheckedStatement {
    Expr(CheckedExpr),
    Let(String, Type, CheckedExpr, bool),
    Assign(String, CheckedExpr, CheckedExpr),
    For(String, Type, Vec<CheckedStatement>),
    Loop(Vec<CheckedStatement>),
}

#[derive(Clone)]
struct ExprContext<'a> {
    program: &'a LoadedProgram,
    current_module: String,
}

impl CheckedExpr {
    pub fn typ(&self) -> Type {
        match self {
            CheckedExpr::Null => Type::Class("std".to_owned(), "null".to_owned(), vec![]),
            CheckedExpr::Bool(_) | CheckedExpr::And(_, _) | CheckedExpr::Or(_, _) => {
                Type::Class("std".to_owned(), "bool".to_owned(), vec![])
            }
            CheckedExpr::Integer(_) => Type::Class("std".to_owned(), "int".to_owned(), vec![]),
            CheckedExpr::U32(_) => Type::Class("std".to_owned(), "u32".to_owned(), vec![]),
            CheckedExpr::Str(_) => Type::Literal(Expression::Str("".to_owned())),
            CheckedExpr::Token(_, t)
            | CheckedExpr::SelfKeyword(t)
            | CheckedExpr::List(_, t)
            | CheckedExpr::FieldAccess(_, _, t)
            | CheckedExpr::MethodCall(_, _, _, _, t)
            // | CheckedExpr::Subscript(_, _, t)
            | CheckedExpr::Call(_, _, _, t)
            | CheckedExpr::BuildClass(t, _)
            | CheckedExpr::BuildImpl(t, _) => t.clone(),
            CheckedExpr::Block(_, expr) => expr.typ(),
            CheckedExpr::If(_, then_branch, _) => then_branch.typ(),
        }
    }
}

fn generate_list_type(elem_types: &[Type]) -> Type {
    assert!(!elem_types.is_empty(), "Cannot infer type of empty list");
    let first_type = &elem_types[0];
    for t in &elem_types[1..] {
        assert!(
            t == first_type,
            "List elements must have the same type: {:?} vs {:?}",
            first_type,
            t
        );
    }
    Type::Class(
        "std".to_owned(),
        "list".to_owned(),
        vec![first_type.clone()],
    )
}

impl Expression {
    fn check(&self, ctx: ExprContext<'_>, env: &HashMap<VarName, Type>) -> CheckedExpr {
        match self {
            Expression::Empty | Expression::Null => CheckedExpr::Null,
            Expression::Builtin => panic!("Builtin expressions cannot be loaded"),
            Expression::Bool(b) => CheckedExpr::Bool(*b),
            Expression::Token(t) => CheckedExpr::Token(
                t.clone(),
                env.get(&VarName::Name(t.clone()))
                    .unwrap_or_else(|| panic!("Variable not found in env: {:?}", t))
                    .clone(),
            ),
            Expression::Integer(i) => CheckedExpr::Integer(i.clone()),
            Expression::U32(u) => CheckedExpr::U32(*u),
            Expression::Str(s) => CheckedExpr::Str(s.clone()),
            Expression::SelfKeyword => {
                let self_type = env
                    .get(&VarName::SelfKeyword)
                    .expect("Self used outside of method");
                CheckedExpr::SelfKeyword(self_type.clone())
            }
            Expression::List(elements) => {
                assert!(!elements.is_empty(), "Cannot infer type of empty list");
                let loaded_elements: Vec<CheckedExpr> =
                    elements.iter().map(|e| e.check(ctx.clone(), env)).collect();
                let list_type = generate_list_type(
                    &loaded_elements.iter().map(|e| e.typ()).collect::<Vec<_>>(),
                );
                CheckedExpr::List(loaded_elements, list_type)
            }
            Expression::FieldAccess(base, field_name) => {
                let base_loaded = base.check(ctx.clone(), env);
                let base_type = base_loaded.typ();
                let field_type = ctx.program.lookup_field(&base_type, field_name);
                CheckedExpr::FieldAccess(
                    Box::new(base_loaded),
                    field_name.clone(),
                    field_type.clone(),
                )
            }
            Expression::MethodCall(base, method, args) => {
                let base_loaded = base.check(ctx.clone(), env);
                let base_type = base_loaded.typ();
                let loaded_args: Vec<CheckedExpr> =
                    args.iter().map(|e| e.check(ctx.clone(), env)).collect();
                let program = ctx.program;
                let (implementor_type, method_sig) =
                    program.load_method_signature(&ctx.current_module, &base_type, method);

                assert!(
                    loaded_args.len() + 1 == method_sig.params.len(),
                    "Argument count mismatch in method call"
                );
                for (arg, param_type) in loaded_args.iter().zip(method_sig.params.iter()) {
                    assert!(
                        arg.typ().is_subtype_of(param_type.get_type(), program),
                        "Argument type mismatch in method call"
                    );
                }

                CheckedExpr::MethodCall(
                    Box::new(base_loaded),
                    implementor_type,
                    method.clone(),
                    loaded_args,
                    method_sig.ret.clone(),
                )
            }
            Expression::Subscript(_base, _indices) => {
                unimplemented!("Subscript loading not implemented yet");
            }
            Expression::Call(func, args) => {
                let loaded_args: Vec<CheckedExpr> =
                    args.iter().map(|e| e.check(ctx.clone(), env)).collect();
                let func_name = func.to_name().expect_name();
                let func_sig = ctx
                    .program
                    .load_function_signature(&ctx.current_module, &func_name);
                CheckedExpr::Call(
                    ctx.current_module.clone(),
                    func_name.clone(),
                    loaded_args,
                    func_sig.ret.clone(),
                )
            }
            Expression::And(lhs, rhs) => CheckedExpr::And(
                Box::new(lhs.check(ctx.clone(), env)),
                Box::new(rhs.check(ctx.clone(), env)),
            ),
            Expression::Or(lhs, rhs) => CheckedExpr::Or(
                Box::new(lhs.check(ctx.clone(), env)),
                Box::new(rhs.check(ctx.clone(), env)),
            ),
            Expression::Build(cl, fields) => {
                let buildable = Type::new(&**cl, &ctx.current_module, ctx.program);
                match &buildable {
                    Type::Class(cl_module, cl_name, cl_args) => {
                        assert!(
                            cl_args.is_empty(),
                            "Generic classes not supported in build expressions yet"
                        );
                        let mut field_values = Vec::new();
                        for field in fields {
                            assert!(field.len() == 2, "Invalid field in build expression");
                            let field_name = if let Expression::Token(name) = &field[0] {
                                name.clone()
                            } else {
                                panic!("Field name must be a token");
                            };
                            let field_value = field[1].check(ctx.clone(), env);
                            field_values.push((field_name, field_value));
                        }

                        let loaded_cl = ctx
                            .program
                            .lookup_class(&cl_module, &cl_name)
                            .expect("No such class");
                        assert!(
                            loaded_cl.body.len() == field_values.len(),
                            "Field count mismatch in build expression"
                        );

                        let field_hashmap = field_values.iter().cloned().collect::<HashMap<_, _>>();
                        for loaded_cl_row in &loaded_cl.body {
                            assert!(loaded_cl_row.len() == 2, "Expecting two columns");
                            let field_name = loaded_cl_row[0].unwrap_name();
                            let field_type = loaded_cl_row[1].unwrap_type();
                            assert!(
                                field_hashmap
                                    .get(&field_name)
                                    .expect("Field missing from builder")
                                    .typ()
                                    .is_subtype_of(&field_type, &ctx.program),
                                "Field type mismatch in build expression"
                            );
                        }

                        CheckedExpr::BuildClass(buildable.clone(), field_values)
                    }
                    Type::Impl(_, _) => {
                        assert!(fields.len() == 1 && fields[0].len() == 1);
                        let value = fields[0][0].check(ctx.clone(), env);
                        assert!(
                            value.typ().is_subtype_of(&buildable, &ctx.program),
                            "Impl build expression type mismatch"
                        );
                        CheckedExpr::BuildImpl(buildable.clone(), Box::new(value))
                    }
                    _ => panic!("Build expression must be a class"),
                }
            }
            Expression::Block(stmts, expr) => {
                let mut env_copy = env.clone();
                let loaded_stmts: Vec<CheckedStatement> = stmts
                    .iter()
                    .map(|s| s.check(ctx.clone(), &mut env_copy))
                    .collect();
                let loaded_expr = Box::new(expr.check(ctx, &env_copy));
                CheckedExpr::Block(loaded_stmts, loaded_expr)
            }
            Expression::If(cond, then_branch, else_branch) => CheckedExpr::If(
                Box::new(cond.check(ctx.clone(), env)),
                Box::new(then_branch.check(ctx.clone(), env)),
                Box::new(else_branch.check(ctx, env)),
            ),
        }
    }
}

impl Statement {
    fn check(&self, ctx: ExprContext<'_>, env: &mut HashMap<VarName, Type>) -> CheckedStatement {
        match self {
            Statement::Expr(expr) => CheckedStatement::Expr(expr.check(ctx, env)),
            Statement::Let(vars, expr, is_mut) => {
                assert!(vars.len() == 1);
                let var = vars.first().unwrap().to_name();
                let expr_checked = expr.check(ctx.clone(), env);
                let var_type = expr_checked.typ();
                assert!(var.is_name());
                assert!(
                    !env.contains_key(&var),
                    "Let variable shadows existing variable"
                );
                env.insert(var.clone(), var_type.clone());
                CheckedStatement::Let(var.expect_name(), var_type, expr_checked, *is_mut)
            }
            Statement::Assign(var, value, expr) => CheckedStatement::Assign(
                var.clone(),
                value.check(ctx.clone(), env),
                expr.check(ctx, env),
            ),
            Statement::For(vars, body) => {
                assert!(vars.len() == 2, "Multiple for bindings not supported yet");
                let var = vars[0].to_name();
                assert!(var.is_name());
                let list = &vars[1].check(ctx.clone(), env);
                assert!(
                    !env.contains_key(&var),
                    "For loop variable shadows existing variable"
                );
                let mut new_env = env.clone();
                new_env.insert(var.clone(), list.typ());
                let body_loaded = body
                    .iter()
                    .map(|s| s.check(ctx.clone(), &mut new_env))
                    .collect();
                CheckedStatement::For(var.expect_name(), list.typ(), body_loaded)
            }
            Statement::Loop(body) => {
                let body_loaded = body.iter().map(|s| s.check(ctx.clone(), env)).collect();
                CheckedStatement::Loop(body_loaded)
            }
        }
    }
}

impl LoadedProgram {
    pub fn lookup_field(&self, base: &Type, field_name: &str) -> Type {
        let cl = self.lookup_type_as_class(base);
        for row in &cl.body {
            if row[0].unwrap_name() == field_name.to_owned() {
                return row[1].unwrap_type();
            }
        }
        panic!("Cannot lookup field {:?} for {:?}", field_name, base);
    }

    pub fn load_method_signature(
        &self,
        current_module: &str,
        base: &Type,
        method_name: &str,
    ) -> (Type, LoadedFunctionSignature) {
        match base {
            Type::Class(_, _, args) if !args.is_empty() => {
                panic!(
                    "Method calls on generic classes not supported yet: {:?}",
                    base
                );
            }
            Type::Class(class_module, class_name, _) => {
                let (impl_class_module, impl_class_name, loaded_function) =
                    self.lookup_anon_impl(current_module, class_module, class_name, method_name);
                (
                    Type::Class(impl_class_module, impl_class_name, vec![]),
                    loaded_function.signature(),
                )
            }
            Type::Impl(impl_module, impl_name) => {
                let impl_decl = self
                    .lookup_impl(impl_module, impl_name)
                    .expect("No such impl");
                (
                    base.clone(),
                    impl_decl
                        .method_impls
                        .get(method_name)
                        .expect("No such method")
                        .signature(),
                )
            }
            _ => {
                unimplemented!(
                    "Method calls on non-class, non-impl types not supported yet: {:?}",
                    base
                );
            }
        }
    }

    pub fn load_function_signature(
        &self,
        module_name: &str,
        function_name: &str,
    ) -> LoadedFunctionSignature {
        self.lookup_fn(module_name, function_name)
            .unwrap_or_else(|| panic!("No such function: {}::{}", module_name, function_name))
            .signature()
    }
}

impl Type {
    pub fn is_subtype_of(&self, other: &Type, program: &LoadedProgram) -> bool {
        if self == other {
            return true;
        }
        if other == &Type::Class("introspect".to_owned(), "examine_struct".to_owned(), vec![]) {
            return true; // TODO: is this correct?
        }
        if let Type::Impl(other_module, other_name) = other
            && let Type::Class(self_module, self_name, self_args) = self
            && self_args.is_empty()
        {
            let other_impl = program
                .lookup_impl(other_module, other_name)
                .expect("No such impl");
            if other_impl.class_name == *self_name && other_impl.class_module == *self_module {
                return true;
            }
        }
        false
    }
}

impl LoadedFunction {
    pub fn check(
        &self,
        program: &LoadedProgram,
        current_type: Option<Type>,
        current_module: &str,
    ) -> CheckedFunction {
        let mut env = HashMap::new();
        for param in &self.params {
            env.insert(param.name().clone(), param.get_type().clone());
        }
        if let Some(t) = current_type {
            env.insert(VarName::SelfKeyword, t);
        }
        let ctx = ExprContext {
            program,
            current_module: current_module.to_owned(),
        };
        let body_loaded = self.body.check(ctx, &env);
        assert!(
            body_loaded.typ().is_subtype_of(&self.ret, program),
            "Function return type mismatch"
        );
        CheckedFunction {
            params: self.params.clone(),
            ret: self.ret.clone(),
            body: body_loaded,
        }
    }
}
