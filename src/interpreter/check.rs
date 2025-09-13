use std::collections::HashMap;

use crate::interpreter::ast::{ClassTable, Declaration, Expression, Function, ProgramFile};


#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum VarName {
    Name(String),
    SelfKeyword,
}

impl VarName {
    pub fn expect_name(&self) -> String {
        match self {
            VarName::Name(s) => s.clone(),
            VarName::SelfKeyword => panic!("Expected variable name, got 'self'"),
        }
    }
}

#[derive(Clone, Debug)]
enum LoadedDecl {
    Class(LoadedClass),
    Function(LoadedFunction),
    Impl(LoadedImpl),
    Trait(LoadedTrait),
}

#[derive(Clone, Debug)]
pub struct LoadedImpl {
    methods_impls: HashMap<String, LoadedFunction>,
}

#[derive(Clone, Debug)]
pub struct LoadedTrait {
    methods: HashMap<String, LoadedFunction>,
}

#[derive(Clone, Debug)]
pub struct LoadedClass {
    body: Vec<Vec<ClassCell>>,
}

#[derive(Clone, Debug)]
pub struct LoadedFunction {
    pub params: Vec<ArgCell>,
    pub body: Expression,
}

pub struct LoadedProgram {
    decls: HashMap<(String, String), LoadedDecl>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ArgCell {
    SelfKeyword,
    Arg(String, Buildable),
}

impl ArgCell {
    fn new(expr: &Expression, program: &SyntacticProgram) -> Self {
        match expr {
            Expression::Token(s) => {
                let buildable = program.lookup_class(s);
                ArgCell::Arg(s.clone(), Buildable::Class(buildable.0, buildable.1))
            }
            _ => panic!("Invalid argument expression: {:?}", expr),
        }
    }

    pub fn name(&self) -> VarName {
        match self {
            ArgCell::SelfKeyword => VarName::SelfKeyword,
            ArgCell::Arg(name, _) => VarName::Name(name.clone()),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum ClassCell {
    Name(String),
    Class(String, String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Buildable {
    Class(String, String),
    Impl(String, String, Vec<TraitArg>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TraitArg {
    Class(String),
}

pub struct SyntacticProgram {
    pub modules: HashMap<String, ProgramFile>,
}

enum ColumnSchema {
    Name,
    Type,
}

impl Buildable {
    fn new(expr: &Expression, program: &SyntacticProgram) -> Self {
        match expr {
            Expression::Token(s) => {
                let (module_name, class) = program.lookup_class(s);
                Buildable::Class(module_name, class)
            }
            Expression::Subscript(head, args) => {
                let head = head.to_name().expect_name();
                let args = args.iter().map(|e| e[0].to_trait_arg()).collect();
                if is_builtin_trait(&head) {
                    Buildable::Impl("std".to_owned(), head, args)
                } else {
                    let (module_name, _) = program.lookup_class(&head);
                    Buildable::Impl(module_name, head, args)
                }
            }
            _ => panic!("Invalid buildable expression: {:?}", expr),
        }
    }
}

impl LoadedDecl {
    fn as_opt_function(&self) -> Option<&LoadedFunction> {
        match self {
            LoadedDecl::Function(f) => Some(f),
            _ => None,
        }
    }

    fn as_opt_class(&self) -> Option<&LoadedClass> {
        match self {
            LoadedDecl::Class(c) => Some(c),
            _ => None,
        }
    }

    fn as_opt_impl(&self) -> Option<&LoadedImpl> {
        match self {
            LoadedDecl::Impl(i) => Some(i),
            _ => None,
        }
    }

    fn as_opt_trait(&self) -> Option<&LoadedTrait> {
        match self {
            LoadedDecl::Trait(t) => Some(t),
            _ => None,
        }
    }
}

impl LoadedImpl {
    pub fn lookup_method_impl(&self, name: &str) -> Option<&LoadedFunction> {
        self.methods_impls.get(name)
    }
}

impl LoadedFunction {
    fn from_fn(func: &Function, program: &SyntacticProgram) -> (String, Self) {
        assert!(func.header.len() == 1);
        let function_name = match &func.header[0] {
            Expression::Token(s) => s.clone(),
            _ => panic!("Invalid function header: {:?}", func.header),
        };
        let mut params = Vec::new();
        for param in &func.params {
            assert!(param.len() == 2);
            let param_name = match &param[0] {
                Expression::Token(s) => s.clone(),
                _ => panic!("Invalid function parameter: {:?}", param),
            };
            let param_type = match &param[1] {
                Expression::Token(s) => s.clone(),
                _ => panic!("Invalid function parameter: {:?}", param),
            };
            params.push(ArgCell::Arg(param_name, Buildable::Class(param_type, String::new())));
        }
        let loaded_function = LoadedFunction { params, body: func.body.clone() };
        (function_name, loaded_function)
    }
}

impl SyntacticProgram {
    fn lookup_class(&self, name: &str) -> (String, String) {
        let mut candidates = vec![];
        if matches!(name, "u32" | "str" | "bool") {
            candidates.push(("std".to_owned(), name.to_owned()));
        }
        for (module_name, module) in &self.modules {
            for decl in &module.declarations {
                if let Declaration::Class(class) = decl && class.header[0] == Expression::Token(name.to_owned()) {
                    candidates.push((module_name.clone(), name.to_owned()));
                }
            }
        }
        match candidates.len() {
            0 => panic!("Class not found: {}", name),
            1 => {candidates.pop().unwrap()}
            _ => panic!("Ambiguous class name: {} (candidates: {:?})", name, candidates),
        }
    }
}

impl ClassCell {
    fn new(expr: &Expression, schema: &ColumnSchema, program: &SyntacticProgram) -> Self {
        match (schema, expr) {
            (ColumnSchema::Name, Expression::Token(s)) => Self::Name(s.clone()),
            (ColumnSchema::Type, Expression::Token(s)) => {
                let (module_name, class) = program.lookup_class(&s);
                Self::Class(module_name, class)
            }
            _ => panic!("Invalid class cell expression: {:?}", expr),
        }
    }
}

impl LoadedClass {
    fn new(body: &[Vec<Expression>], program: &SyntacticProgram) -> Self {
        let mut class_body = Vec::new();
        let column_schema = vec![
            ColumnSchema::Name,
            ColumnSchema::Type,
        ]; // TODO: get from metaclass?
        for row in body {
            let mut class_row = Vec::new();
            assert!(row.len() == column_schema.len());
            for (cell, schema) in row.iter().zip(&column_schema) {
                let loaded_cell = ClassCell::new(cell, schema, program);
                class_row.push(loaded_cell);
            }
            class_body.push(class_row);
        }
        LoadedClass { body: class_body }
    }

    fn from_classtable(
        class: &ClassTable,
        program: &SyntacticProgram,
    ) -> (String, Self) {
        match &class.header[0] {
            Expression::Token(name) => (name.clone(), Self::new(&class.body, program)),
            _ => panic!("Invalid class header: {:?}", class.header),
        }
    }

    pub fn num_fields(&self) -> usize {
        self.body.len()
    }

    pub fn field_names(&self) -> Vec<String> {
        // TODO: what if metaclass changes order of fields or columns?
        self.body.iter().map(|row| {
            match &row[0] {
                ClassCell::Name(name) => name.clone(),
                _ => panic!("Expected field name, got {:?}", row[0]),
            }
        }).collect()
    }
}

impl Expression {
    pub fn to_name(&self) -> VarName {
        match self {
            Expression::Token(s) => VarName::Name(s.clone()),
            Expression::SelfKeyword => VarName::SelfKeyword,
            _ => panic!("Expected token, got {:?}", self),
        }
    }

    fn to_head_and_args(&self) -> (String, Vec<TraitArg>) {
        match self {
            Expression::Token(s) => (s.clone(), vec![]),
            Expression::Subscript(head, args) => {
                let head = head.to_name().expect_name();
                let args = args.iter().map(|e| e[0].to_trait_arg()).collect();
                (head, args)
            }
            _ => panic!("Expected head and args, got {:?}", self),
        }
    }

    fn to_trait_arg(&self) -> TraitArg {
        match self {
            Expression::Token(s) => TraitArg::Class(s.clone()),
            _ => panic!("Expected trait arg, got {:?}", self),
        }
    }
}

impl LoadedProgram {
    pub fn new(program: &SyntacticProgram) -> Self {
        let mut decls = HashMap::new();
        for (module_name, module) in &program.modules {
            for decl in &module.declarations {
                match decl {
                    Declaration::Class(class) => {
                        let (class_name, loaded_class) = LoadedClass::from_classtable(class, program);
                        assert!(!decls.contains_key(&(module_name.clone(), class_name.clone())));
                        decls.insert((module_name.clone(), class_name), LoadedDecl::Class(loaded_class));
                    }
                    Declaration::Fn(func) => {
                        let (function_name, loaded_function) = LoadedFunction::from_fn(func, program);
                        assert!(!decls.contains_key(&(module_name.clone(), function_name.clone())));
                        decls.insert((module_name.clone(), function_name), LoadedDecl::Function(loaded_function));
                    }
                    _ => unimplemented!()
                }
            }
        }
        LoadedProgram { decls }
    }

    pub fn lookup_fn(&self, module_name: &str, name: &str) -> Option<&LoadedFunction> {
        let search_key = (module_name.to_owned(), name.to_owned());
        self.decls.get(&search_key).and_then(LoadedDecl::as_opt_function)
    }

    pub fn lookup_class(&self, module: &str, name: &str) -> Option<&LoadedClass> {
        let search_key = (module.to_owned(), name.to_owned());
        self.decls.get(&search_key).and_then(LoadedDecl::as_opt_class)
    }

    pub fn lookup_impl(&self, module: &str, name: &str) -> Option<&LoadedImpl> {
        let search_key = (module.to_owned(), name.to_owned());
        self.decls.get(&search_key).and_then(LoadedDecl::as_opt_impl)
    }

    pub fn is_builtin_trait(&self, name: &str) -> bool {
        is_builtin_trait(name)
    }

    pub fn to_buildable(&self, current_module: &str, expr: &Expression) -> Buildable {
        let (head, args) = expr.to_head_and_args();
        if self.is_builtin_trait(&head) {
            Buildable::Impl(current_module.to_owned(), head, args)
        } else if self.lookup_class(&current_module, &head).is_some() {
            assert!(args.is_empty(), "Class cannot have trait args");
            Buildable::Class(current_module.to_owned(), head)
        } else if self.lookup_impl(&current_module, &head).is_some() {
            Buildable::Impl(current_module.to_owned(), head, args)
        } else {
            panic!("No class or impl found for: {}", head)
        }
    }
}

fn is_builtin_trait(name: &str) -> bool {
    matches!(name, "struct")
}
