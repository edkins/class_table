use std::collections::HashMap;

use num_bigint::BigInt;

use crate::interpreter::{ast::{Expression, Statement}, check::{SyntacticProgram, Type}};

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum LoadedExpr {
    Null,
    Bool(bool),
    Token(String, Type),
    Integer(BigInt),
    U32(u32),
    Str(String),
    SelfKeyword(Type),
    List(Vec<LoadedExpr>, Type),
    FieldAccess(Box<LoadedExpr>, String, Type),
    MethodCall(Box<LoadedExpr>, Type, String, Vec<LoadedExpr>, Type),
    Subscript(Box<LoadedExpr>, Vec<LoadedExpr>, Type),
    Call(String, String, Vec<LoadedExpr>, Type),
    And(Box<LoadedExpr>, Box<LoadedExpr>),
    Or(Box<LoadedExpr>, Box<LoadedExpr>),
    Build(Type, Vec<(String, LoadedExpr)>),
    Block(Vec<LoadedStatement>, Box<LoadedExpr>),
    If(Box<LoadedExpr>, Box<LoadedExpr>, Box<LoadedExpr>),
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum LoadedStatement {
    Expr(LoadedExpr),
    Let(String, Type, LoadedExpr, bool),
    Assign(String, LoadedExpr, LoadedExpr),
    For(String, Type, Vec<LoadedStatement>),
    Loop(Vec<LoadedStatement>),
}

#[derive(Clone)]
struct ExprContext<'a> {
    program: &'a SyntacticProgram,
    current_type: Type,
    current_module: String,
}

impl LoadedExpr {
    pub fn typ(&self) -> Type {
        match self {
            LoadedExpr::Null => Type::Class("std".to_owned(), "null".to_owned(), vec![]),
            LoadedExpr::Bool(_)| LoadedExpr::And(_, _)|LoadedExpr::Or(_, _) => Type::Class("std".to_owned(), "bool".to_owned(), vec![]),
            LoadedExpr::Integer(_) => Type::Class("std".to_owned(), "int".to_owned(), vec![]),
            LoadedExpr::U32(_) => Type::Class("std".to_owned(), "u32".to_owned(), vec![]),
            LoadedExpr::Str(_) => Type::Literal(Expression::Str("".to_owned())),
            LoadedExpr::Token(_, t)| LoadedExpr::SelfKeyword(t)| LoadedExpr::List(_, t)| LoadedExpr::FieldAccess(_, _, t)| LoadedExpr::MethodCall(_, _, _, _, t) | LoadedExpr::Subscript(_, _, t)| LoadedExpr::Call(_, _, _, t) | LoadedExpr::Build(t, _) => t.clone(),
            LoadedExpr::Block(_, expr) => expr.typ(),
            LoadedExpr::If(_, then_branch, _) => then_branch.typ(),
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
    Type::Class("std".to_owned(), "list".to_owned(), vec![first_type.clone()])
}

impl Expression {
    pub fn load(&self, ctx: ExprContext<'_>, env: &HashMap<String, Type>) -> LoadedExpr {
        match self {
            Expression::Empty | Expression::Null => LoadedExpr::Null,
            Expression::Builtin => panic!("Builtin expressions cannot be loaded"),
            Expression::Bool(b) => LoadedExpr::Bool(*b),
            Expression::Token(t) => LoadedExpr::Token(t.clone(), env.get(t).expect("Variable not found in env").clone()),
            Expression::Integer(i) => LoadedExpr::Integer(i.clone()),
            Expression::U32(u) => LoadedExpr::U32(*u),
            Expression::Str(s) => LoadedExpr::Str(s.clone()),
            Expression::SelfKeyword => {
                panic!("Self keyword must be resolved to a type before loading")
            }
            Expression::List(elements) => {
                assert!(!elements.is_empty(), "Cannot infer type of empty list");
                let loaded_elements: Vec<LoadedExpr> =
                    elements.iter().map(|e| e.load(ctx.clone(), env)).collect();
                let list_type = generate_list_type(&loaded_elements.iter().map(|e| e.typ()).collect::<Vec<_>>());
                LoadedExpr::List(loaded_elements, list_type)
            }
            Expression::FieldAccess(base, field_name) => {
                let base_loaded = base.load(ctx.clone(), env);
                let base_type = base_loaded.typ();
                let field_type = ctx.program.lookup_field(&base_type, field_name);
                LoadedExpr::FieldAccess(
                    Box::new(base_loaded),
                    field_name.clone(),
                    field_type.clone(),
                )
            }
            Expression::MethodCall(base, method, args) => {
                let base_loaded = base.load(ctx.clone(), env);
                let base_type = base_loaded.typ();
                let loaded_args: Vec<LoadedExpr> = args.iter().map(|e| e.load(ctx.clone(), env)).collect();
                let program = ctx.program;
                let (implementor_type, method_sig) = program.load_method_signature(&base_type, method);

                assert!(loaded_args.len() == method_sig.params.len(), "Argument count mismatch in method call");
                for (arg, param_type) in loaded_args.iter().zip(method_sig.params.iter()) {
                    assert!(arg.typ().is_subtype_of(param_type.get_type(), program), "Argument type mismatch in method call");
                }

                LoadedExpr::MethodCall(
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
                let loaded_args: Vec<LoadedExpr> = args.iter().map(|e| e.load(ctx.clone(), env)).collect();
                let func_name = func.to_name().expect_name();
                let func_sig = ctx.program.load_function_signature(&ctx.current_module, &func_name);
                LoadedExpr::Call(ctx.current_module.clone(), func_name.clone(), loaded_args, func_sig.ret.clone())
            }
            Expression::And(lhs, rhs) => {
                LoadedExpr::And(Box::new(lhs.load(ctx.clone(), env)), Box::new(rhs.load(ctx.clone(), env)))
            }
            Expression::Or(lhs, rhs) => {
                LoadedExpr::Or(Box::new(lhs.load(ctx.clone(), env)), Box::new(rhs.load(ctx.clone(), env)))
            }
            Expression::Build(cl, fields) => {
                let buildable = Type::new(&**cl, &ctx.program);
                match &buildable {
                    Type::Class(cl_module, cl_name, cl_args) => {
                        assert!(cl_args.is_empty(), "Generic classes not supported in build expressions yet");
                        let mut field_values = Vec::new();
                        for field in fields {
                            assert!(field.len() == 2, "Invalid field in build expression");
                            let field_name = if let Expression::Token(name) = &field[0] {
                                name.clone()
                            } else {
                                panic!("Field name must be a token");
                            };
                            let field_value = field[1].load(ctx.clone(), env);
                            field_values.push((field_name, field_value));
                        }

                        let loaded_cl = ctx.program.load_class(&cl_module, &cl_name);
                        assert!(loaded_cl.body.len() == field_values.len(), "Field count mismatch in build expression");

                        let field_hashmap = field_values.iter().cloned().collect::<HashMap<_, _>>();
                        for loaded_cl_row in &loaded_cl.body {
                            assert!(loaded_cl_row.len() == 2, "Expecting two columns");
                            let field_name = loaded_cl_row[0].unwrap_name();
                            let field_type = loaded_cl_row[1].unwrap_type();
                            assert!(field_hashmap.get(&field_name).expect("Field missing from builder").typ().is_subtype_of(&field_type, &ctx.program), "Field type mismatch in build expression");
                        }
                        
                        LoadedExpr::Build(buildable.clone(), field_values)
                    }
                    _ => panic!("Build expression must be a class"),
                }
            }
            Expression::Block(stmts, expr) => {
                let loaded_stmts: Vec<LoadedStatement> =
                    stmts.iter().map(|s| s.load(ctx.clone(), env)).collect();
                let loaded_expr = Box::new(expr.load(ctx, env));
                LoadedExpr::Block(loaded_stmts, loaded_expr)
            }
            Expression::If(cond, then_branch, else_branch) => {
                LoadedExpr::If(
                    Box::new(cond.load(ctx.clone(), env)),
                    Box::new(then_branch.load(ctx.clone(), env)),
                    Box::new(else_branch.load(ctx, env)),
                )
            }
        }
    }
}

impl Statement {
    pub fn load(&self, ctx: ExprContext<'_>, env: &HashMap<String, Type>) -> LoadedStatement {
        match self {
            Statement::Expr(expr) => LoadedStatement::Expr(expr.load(ctx, env)),
            Statement::Let(vars, expr, is_mut) => {
                assert!(vars.len() == 1);
                let var = vars.first().unwrap().to_name().expect_name();
                let var_type = env.get(&var).expect("Variable not found in env");
                LoadedStatement::Let(var.clone(), var_type.clone(), expr.load(ctx, env), *is_mut)
            }
            Statement::Assign(var, value, expr) => {
                LoadedStatement::Assign(var.clone(), value.load(ctx.clone(), env), expr.load(ctx, env))
            }
            Statement::For(vars, body) => {
                assert!(vars.len() == 2, "Multiple for bindings not supported yet");
                let var = vars[0].to_name().expect_name();
                let list = &vars[1].load(ctx.clone(), env);
                assert!(!env.contains_key(&var), "For loop variable shadows existing variable");
                let mut new_env = env.clone();
                new_env.insert(var.clone(), list.typ());
                let body_loaded = body.iter().map(|s| s.load(ctx.clone(), &new_env)).collect();
                LoadedStatement::For(var.clone(), list.typ(), body_loaded)
            }
            Statement::Loop(body) => {
                let body_loaded = body.iter().map(|s| s.load(ctx.clone(), env)).collect();
                LoadedStatement::Loop(body_loaded)
            }
        }
    }
}