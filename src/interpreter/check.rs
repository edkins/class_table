use std::collections::HashMap;

use crate::interpreter::ast::{
    ClassTable, Declaration, Expression, Function, Impl, ProgramFile, Trait,
};

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
    class_module: String,
    class_name: String,
    trait_name: Option<(String, String)>,
    method_impls: HashMap<String, LoadedFunction>,
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
    pub ret: Type,
    pub body: Expression,
}

pub struct LoadedProgram {
    decls: HashMap<(String, String), LoadedDecl>,
    anon_impls: Vec<LoadedImpl>,
    uses: Vec<(String, String, String)>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ArgCell {
    Arg(VarName, Type),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Type {
    Class(String, String, Vec<Type>),
    Trait(String, String),
}

impl ArgCell {
    pub fn name(&self) -> VarName {
        match self {
            ArgCell::Arg(name, _) => name.clone(),
        }
    }

    pub fn get_type(&self) -> &Type {
        match self {
            ArgCell::Arg(_, b) => b,
        }
    }

    fn new(
        expr: &[Expression],
        surrounding_class: Option<&Type>,
        program: &SyntacticProgram,
    ) -> Self {
        match expr {
            [Expression::Token(s), typ] => {
                let buildable = Type::new(typ, program);
                ArgCell::Arg(VarName::Name(s.clone()), buildable)
            }
            [Expression::SelfKeyword] => ArgCell::Arg(
                VarName::SelfKeyword,
                surrounding_class.expect("self used outside class").clone(),
            ),
            _ => panic!("Invalid argument expression: {:?}", expr),
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

const BUILTIN_CLASSES: &[&str] = &["null", "u32", "int", "str", "bool"];
const BUILTIN_TRAITS: &[&str] = &["struct"];

impl Type {
    fn new(expr: &Expression, program: &SyntacticProgram) -> Self {
        match expr {
            Expression::Token(s) => program.lookup_class(s),
            Expression::Subscript(head, args) => {
                if **head == Expression::Token("list".to_owned()) {
                    if args.len() != 1 || args[0].len() != 1 {
                        panic!("List type must have one type argument");
                    }
                    let elem_type = Type::new(&args[0][0], program);
                    return Type::Class("std".to_owned(), "list".to_owned(), vec![elem_type]);
                }
                unimplemented!("Generic types not supported yet");
                // let head = head.to_name().expect_name();
                // let args = args.iter().map(|e| e[0].to_trait_arg()).collect();
                // if BUILTIN_TRAITS.contains(&(&head as &str)) {
                //     Type::Trait("std".to_owned(), heads)
                // } else {
                //     let (module_name, _) = program.lookup_class(&head);
                //     Type::Trait(module_name, head)
                // }
            }
            _ => panic!("Invalid buildable expression: {:?}", expr),
        }
    }
}

impl LoadedImpl {
    fn from_impl(
        module_name: &str,
        impl_def: &Impl,
        program: &SyntacticProgram,
    ) -> (Option<String>, Self) {
        assert!(impl_def.header.len() == 3);
        let impl_name = match &impl_def.header[1] {
            Expression::Token(s) => Some(s.clone()),
            Expression::Empty => None,
            _ => panic!("Invalid impl header: {:?}", impl_def.header),
        };

        let trait_name = match &impl_def.header[2] {
            Expression::Token(s) => {
                if BUILTIN_TRAITS.contains(&(s as &str)) {
                    Some(("std".to_owned(), s.clone()))
                } else {
                    Some((module_name.to_owned(), s.clone()))
                }
            }
            Expression::Empty => None,
            _ => panic!("Invalid impl header: {:?}", impl_def.header),
        };

        let self_type = program.lookup_class(&impl_def.header[0].to_name().expect_name());
        let (class_module, class_name, _) = match &self_type {
            Type::Class(m, c, a) if a.is_empty() => (m.clone(), c.clone(), a.clone()),
            _ => panic!(
                "Impl self type must be a non-generic class: {:?}",
                self_type
            ),
        };

        let mut method_impls = HashMap::new();
        for method in &impl_def.methods {
            let (method_name, loaded_function) =
                LoadedFunction::from_fn(method, Some(&self_type), program);
            assert!(!method_impls.contains_key(&method_name));
            method_impls.insert(method_name, loaded_function);
        }
        (
            impl_name,
            LoadedImpl {
                class_module,
                class_name,
                trait_name,
                method_impls,
            },
        )
    }

    pub fn lookup_method_impl(&self, name: &str) -> Option<&LoadedFunction> {
        self.method_impls.get(name)
    }
}

impl LoadedTrait {
    fn from_trait(
        trait_module: &str,
        trait_def: &Trait,
        program: &SyntacticProgram,
    ) -> (String, Self) {
        assert!(trait_def.header.len() == 1);
        let trait_name = match &trait_def.header[0] {
            Expression::Token(s) => s.clone(),
            _ => panic!("Invalid trait header: {:?}", trait_def.header),
        };
        let mut methods = HashMap::new();
        for method in &trait_def.methods {
            let typ = Type::Trait(trait_module.to_owned(), trait_name.clone());
            let (method_name, loaded_function) =
                LoadedFunction::from_fn(method, Some(&typ), program);
            assert!(!methods.contains_key(&method_name));
            methods.insert(method_name, loaded_function);
        }
        (trait_name, LoadedTrait { methods })
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

impl LoadedFunction {
    fn from_fn(
        func: &Function,
        surrounding_class: Option<&Type>,
        program: &SyntacticProgram,
    ) -> (String, Self) {
        assert!(func.header.len() == 1);
        let function_name = match &func.header[0] {
            Expression::Token(s) => s.clone(),
            _ => panic!("Invalid function header: {:?}", func.header),
        };
        let mut params = Vec::new();
        for param in &func.params {
            let arg = ArgCell::new(param, surrounding_class.as_ref().cloned(), program);
            params.push(arg);
        }
        assert!(func.ret.len() == 1);
        let loaded_function = LoadedFunction {
            params,
            ret: Type::new(&func.ret[0], program),
            body: func.body.clone(),
        };
        (function_name, loaded_function)
    }

    fn implements(&self, other: &LoadedFunction) -> bool {
        if self.params.len() != other.params.len() {
            return false;
        }
        if other.body != Expression::Empty {
            return false;
        }
        for (p1, p2) in self.params.iter().zip(&other.params) {
            match (p1, p2) {
                (ArgCell::Arg(name1, b1), ArgCell::Arg(name2, b2)) => {
                    if name1 != name2 {
                        return false;
                    }
                    // TODO: type checking for self
                    if name1 != &VarName::SelfKeyword && b1 != b2 {
                        return false;
                    }
                }
            }
        }
        true
    }
}

impl SyntacticProgram {
    fn lookup_class(&self, name: &str) -> Type {
        let mut candidates = vec![];
        if BUILTIN_CLASSES.contains(&name) {
            candidates.push(Type::Class("std".to_owned(), name.to_owned(), vec![]));
        }
        for (module_name, module) in &self.modules {
            for decl in &module.declarations {
                if let Declaration::Class(class) = decl
                    && class.header[0] == Expression::Token(name.to_owned())
                {
                    candidates.push(Type::Class(module_name.clone(), name.to_owned(), vec![]));
                }
            }
        }
        match candidates.len() {
            0 => panic!("Class not found: {}", name),
            1 => candidates.pop().unwrap(),
            _ => panic!(
                "Ambiguous class name: {} (candidates: {:?})",
                name, candidates
            ),
        }
    }
}

impl ClassCell {
    fn new(expr: &Expression, schema: &ColumnSchema, program: &SyntacticProgram) -> Self {
        match (schema, expr) {
            (ColumnSchema::Name, Expression::Token(s)) => Self::Name(s.clone()),
            (ColumnSchema::Type, Expression::Token(s)) => match program.lookup_class(s) {
                Type::Class(module_name, class, args) => {
                    assert!(args.is_empty());
                    Self::Class(module_name, class)
                }
                _ => panic!("Expected class type, got {:?}", s),
            },
            _ => panic!("Invalid class cell expression: {:?}", expr),
        }
    }
}

impl LoadedClass {
    fn new(body: &[Vec<Expression>], program: &SyntacticProgram) -> Self {
        let mut class_body = Vec::new();
        let column_schema = vec![ColumnSchema::Name, ColumnSchema::Type]; // TODO: get from metaclass?
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

    fn from_classtable(class: &ClassTable, program: &SyntacticProgram) -> (String, Self) {
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
        self.body
            .iter()
            .map(|row| match &row[0] {
                ClassCell::Name(name) => name.clone(),
                _ => panic!("Expected field name, got {:?}", row[0]),
            })
            .collect()
    }

    pub fn is_struct(&self) -> bool {
        true // TODO: handle non-struct classes
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
        let mut anon_impls = vec![];
        let mut uses = vec![];

        for class_name in BUILTIN_CLASSES {
            decls.insert(
                ("std".to_owned(), class_name.to_string()),
                LoadedDecl::Class(LoadedClass { body: vec![] }),
            );
        }

        decls.insert(
            ("introspect".to_owned(), "examine_struct".to_owned()),
            LoadedDecl::Class(LoadedClass { body: vec![] }),
        );

        anon_impls.push(LoadedImpl {
            class_module: "introspect".to_owned(),
            class_name: "examine_struct".to_owned(),
            trait_name: None,
            method_impls: HashMap::from([(
                "field_names".to_owned(),
                LoadedFunction {
                    params: vec![ArgCell::Arg(
                        VarName::SelfKeyword,
                        Type::Class("introspect".to_owned(), "examine_struct".to_owned(), vec![]),
                    )],
                    ret: Type::Class(
                        "std".to_owned(),
                        "list".to_owned(),
                        vec![Type::Class("std".to_owned(), "str".to_owned(), vec![])],
                    ),
                    body: Expression::Builtin,
                },
            )]),
        });

        for (module_name, module) in &program.modules {
            for decl in &module.declarations {
                match decl {
                    Declaration::Class(class) => {
                        let (class_name, loaded_class) =
                            LoadedClass::from_classtable(class, program);
                        assert!(!decls.contains_key(&(module_name.clone(), class_name.clone())));
                        decls.insert(
                            (module_name.clone(), class_name),
                            LoadedDecl::Class(loaded_class),
                        );
                    }
                    Declaration::Fn(func) => {
                        let (function_name, loaded_function) =
                            LoadedFunction::from_fn(func, None, program);
                        assert!(!decls.contains_key(&(module_name.clone(), function_name.clone())));
                        decls.insert(
                            (module_name.clone(), function_name),
                            LoadedDecl::Function(loaded_function),
                        );
                    }
                    Declaration::Trait(trait_def) => {
                        let (trait_name, methods) =
                            LoadedTrait::from_trait(&module_name, trait_def, program);
                        assert!(!decls.contains_key(&(module_name.clone(), trait_name.clone())));
                        decls.insert(
                            (module_name.clone(), trait_name),
                            LoadedDecl::Trait(methods),
                        );
                    }
                    Declaration::Impl(impl_def) => {
                        let (impl_name, loaded_impl) =
                            LoadedImpl::from_impl(module_name, impl_def, program);
                        match impl_name {
                            Some(impl_name) => {
                                assert!(
                                    !decls.contains_key(&(module_name.clone(), impl_name.clone()))
                                );
                                decls.insert(
                                    (module_name.clone(), impl_name),
                                    LoadedDecl::Impl(loaded_impl),
                                );
                            }
                            None => {
                                anon_impls.push(loaded_impl);
                            }
                        }
                    }
                    Declaration::Use(use_module, use_decl) => {
                        uses.push((module_name.clone(), use_module.clone(), use_decl.clone()));
                    }
                }
            }
        }
        let result = LoadedProgram {
            decls,
            anon_impls,
            uses,
        };
        result.check_consistency();
        result
    }

    fn check_consistency(&self) {
        for (module_name, impl_name) in self.list_trait_impls() {
            let impl_def = self
                .lookup_impl(&module_name, &impl_name)
                .unwrap_or_else(|| panic!("Impl {}::{} not found", module_name, impl_name));
            let (trait_module, trait_name) = impl_def.trait_name.clone().unwrap();
            let trait_def = self
                .lookup_trait(&trait_module, &trait_name)
                .unwrap_or_else(|| {
                    panic!(
                        "Trait '{}::{}' not found for impl '{}::{}'",
                        trait_module, trait_name, module_name, impl_name
                    )
                });
            for (method_name, trait_method) in &trait_def.methods {
                let impl_method = impl_def.lookup_method_impl(method_name).unwrap_or_else(|| {
                    panic!(
                        "Method '{}' of trait '{}::{}' not implemented in impl '{}::{}'",
                        method_name, trait_module, trait_name, module_name, impl_name
                    )
                });
                assert!(
                    impl_method.implements(trait_method),
                    "Method '{}' of trait '{}::{}' in impl '{}::{}' does not match signature",
                    method_name,
                    trait_module,
                    trait_name,
                    module_name,
                    impl_name
                );
            }
            for method_name in impl_def.method_impls.keys() {
                if !trait_def.methods.contains_key(method_name) {
                    panic!(
                        "Method '{}' in impl '{}::{}' not found in trait '{}::{}'",
                        method_name, module_name, impl_name, trait_module, trait_name
                    );
                }
            }
        }
    }

    fn list_trait_impls(&self) -> Vec<(String, String)> {
        let mut result = vec![];
        for ((impl_module, impl_name), decl) in &self.decls {
            if let LoadedDecl::Impl(impl_def) = decl
                && impl_def.trait_name.is_some()
            {
                result.push((impl_module.clone(), impl_name.clone()));
            }
        }
        result
    }

    pub fn lookup_fn(&self, module_name: &str, name: &str) -> Option<&LoadedFunction> {
        let search_key = (module_name.to_owned(), name.to_owned());
        self.decls
            .get(&search_key)
            .and_then(LoadedDecl::as_opt_function)
    }

    pub fn lookup_class(&self, module: &str, name: &str) -> Option<&LoadedClass> {
        let search_key = (module.to_owned(), name.to_owned());
        self.decls
            .get(&search_key)
            .and_then(LoadedDecl::as_opt_class)
    }

    pub fn lookup_impl(&self, module: &str, name: &str) -> Option<&LoadedImpl> {
        let search_key = (module.to_owned(), name.to_owned());
        self.decls
            .get(&search_key)
            .and_then(LoadedDecl::as_opt_impl)
    }

    pub fn lookup_trait(&self, module: &str, name: &str) -> Option<&LoadedTrait> {
        let search_key = (module.to_owned(), name.to_owned());
        self.decls
            .get(&search_key)
            .and_then(LoadedDecl::as_opt_trait)
    }

    pub fn is_subclass(
        &self,
        subclass_module: &str,
        subclass_name: &str,
        superclass_module: &str,
        superclass_name: &str,
    ) -> bool {
        if subclass_module == superclass_module && subclass_name == superclass_name {
            return true;
        }
        self.visible_superclasses(None, subclass_module, subclass_name)
            .contains(&(superclass_module.to_owned(), superclass_name.to_owned()))
    }

    pub fn visible_superclasses(
        &self,
        source_module: Option<&str>,
        class_module: &str,
        class_name: &str,
    ) -> Vec<(String, String)> {
        // everything is a superclass of itself
        assert!(
            source_module.is_none()
                || self.can_see(source_module.unwrap(), class_module, class_name)
        );
        let mut result = vec![(class_module.to_owned(), class_name.to_owned())];

        if let Some(class) = self.lookup_class(class_module, class_name)
            && class.is_struct()
        {
            // If it's declared then it's a struct
            // If it's a struct then it's an examine_struct
            if source_module.is_none()
                || self.can_see(source_module.unwrap(), "introspect", "examine_struct")
            {
                result.push(("introspect".to_owned(), "examine_struct".to_owned()));
            }
        }
        result
    }

    fn can_see(&self, source_module: &str, module: &str, class: &str) -> bool {
        if module == "std" && BUILTIN_CLASSES.contains(&class) {
            return true;
        }
        if source_module == module {
            // TODO: should we check the class is actually real at this point?
            return true;
        }
        for (used_source, used_module, used_class) in &self.uses {
            if used_source == source_module && used_module == module && used_class == class {
                return true;
            }
        }
        false
    }

    pub fn lookup_anon_impl(
        &self,
        source_module: &str,
        class_module: &str,
        class_name: &str,
        method: &str,
    ) -> &LoadedFunction {
        let mut candidate_names = vec![];
        let mut candidates = vec![];
        let superclasses = self.visible_superclasses(Some(source_module), class_module, class_name);
        for (super_module, super_class) in superclasses {
            for impl_def in &self.anon_impls {
                if impl_def.class_module != super_module || impl_def.class_name != super_class {
                    continue;
                }
                if let Some(method_impl) = impl_def.lookup_method_impl(method) {
                    candidate_names.push(format!("{}::{}", super_module, super_class));
                    candidates.push(method_impl);
                }
            }
        }
        match candidates.len() {
            0 => panic!(
                "No anon impl for {}::{}::{}",
                class_module, class_name, method
            ),
            1 => candidates[0],
            _ => panic!(
                "Ambiguous anon impl for {}::{}::{} (candidates: {:?})",
                class_module, class_name, method, candidate_names
            ),
        }
    }

    pub fn to_buildable(&self, current_module: &str, expr: &Expression) -> Buildable {
        let (head, args) = expr.to_head_and_args();
        let mut candidates = vec![];
        for module in [current_module, "std"] {
            if self.lookup_class(module, &head).is_some() {
                candidates.push(Buildable::Class(module.to_owned(), head.clone()));
            }
            // if self.lookup_trait(module, &head).is_some() && !args.is_empty() {
            //     candidates.push(Buildable::Impl(module.to_owned(), head.clone(), args.clone()));
            // }
            if self.lookup_impl(module, &head).is_some() {
                candidates.push(Buildable::Impl(
                    module.to_owned(),
                    head.clone(),
                    args.clone(),
                ));
            }
        }
        match candidates.len() {
            0 => panic!("No such buildable: ({}|std)::{}", current_module, head),
            1 => candidates.pop().unwrap(),
            _ => panic!(
                "Ambiguous buildable: {} (candidates: {:?})",
                head, candidates
            ),
        }
    }
}
