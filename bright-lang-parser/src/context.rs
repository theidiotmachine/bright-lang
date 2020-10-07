use bright_lang_errs::source_location::SourceLocation;
use crate::UnsafeParseMode;
use std::collections::HashMap;
use std::collections::HashSet;
use std::cmp;

use bright_lang_ast::func::Func;
use bright_lang_ast::func::FuncDefn;
use bright_lang_errs::Error;
use bright_lang_types::MemberFunc;
use bright_lang_ast::GlobalVariableImport;
use bright_lang_ast::GlobalVariableDecl;
use bright_lang_ast::TraitImpl;
use bright_lang_types::TraitDecl;
use bright_lang_ast::TypeDecl;
use bright_lang_ast::func::GenericFunc;
use bright_lang_ast::ClosureRef;
use bright_lang_ast::func::LocalVar;
use bright_lang_types::TypeArg;
use bright_lang_ast::core::VariableMutability;
use bright_lang_types::QualifiedType;
use bright_lang_types::{Type, Mutability};

#[derive(Debug, Clone, PartialEq)]
enum ScopedVar{
    /// Actual local variable
    Local{
        mutability: VariableMutability,
        internal_name: String,
        r#type: QualifiedType,
        guard_type: Option<Type>,
    },
    /// Closure reference. Looks like a local, actually a member of the function's closure
    ClosureRef{
        mutability: VariableMutability,
        internal_name: String,
        r#type: QualifiedType,
        guard_type: Option<Type>,
    },
}

impl ScopedVar{
    pub fn is_var(&self) -> bool {
        match self {
            ScopedVar::Local{ mutability, internal_name: _, r#type: _, guard_type: _} => {
                *mutability == VariableMutability::Variable
            },
            ScopedVar::ClosureRef{mutability, internal_name: _, r#type: _, guard_type: _} => {
                *mutability == VariableMutability::Variable
            },
        }
    }
}

#[derive(Debug)]
struct Scope{
    pub var_names: HashMap<String, ScopedVar>,
}

impl<> Default for Scope<> {
    fn default() -> Self {
        Self {
            var_names: HashMap::new(),
        }
    }
}

#[derive(Debug)]
struct TypeScope{
    pub var_names: HashMap<String, TypeArg>,
}

impl<> Default for TypeScope<> {
    fn default() -> Self {
        Self {
            var_names: HashMap::new(),
        }
    }
}

/// Running state of the parser. Used to collect the AST as we build it.
#[derive(Debug)]
pub (crate) struct ParserContext {
    pub global_decls: Vec<GlobalVariableDecl>,
    pub global_imports: Vec<GlobalVariableImport>,
    pub func_decls: Vec<Func>,
    pub func_imports: Vec<FuncDefn>,
    pub generic_func_decls: Vec<GenericFunc>,
    pub generic_func_impls: HashSet<String>,
    pub errors: Vec<Error>,
    pub type_map: HashMap<String, TypeDecl>,
    pub trait_map: HashMap<String, TraitDecl>,
    pub exported_traits: HashSet<String>,
    pub trait_impl_map: HashMap<(String, String), TraitImpl>,
    pub import_namespace_map: HashMap<String, String>,
    pub unsafe_parse_mode: UnsafeParseMode,
    pub file_name: String,

    /// type variables
    type_var_stack: Vec<TypeScope>,

    /// Uniqueness counter
    counter: u64,

    /// This stack of local variables is used to understand the scoping rules. This is the complete set of
    /// of local variables in the function stack we are in. Unlike the variables in the ParserFuncContext, the 
    /// type will mutate depending on guards.
    func_var_stack: Vec<Scope>,
    /// This stack of local variables is used to understand the scoping rules. This is the complete set of
    /// of local variables in the block stack we are in. Unlike the variables in the ParserFuncContext, the 
    /// type will mutate depending on guards. This is how variables shadow each other.
    block_var_stack: Vec<Scope>,
}

impl ParserContext {
    pub (crate) fn new(unsafe_parse_mode: UnsafeParseMode, file_name: &str) -> ParserContext {
        let mut trait_map = HashMap::new();
        let trait_name = String::from("IsAStruct");
        trait_map.insert(trait_name.clone(), TraitDecl{name: trait_name, member_funcs: vec![]});
        ParserContext{
            global_decls: vec![],
            global_imports: vec![],
            func_decls: vec![],
            func_imports: vec![],
            generic_func_decls: vec![],
            generic_func_impls: HashSet::new(),
            errors: vec![],
            type_map: HashMap::new(),
            trait_map,
            exported_traits: HashSet::new(),
            trait_impl_map: HashMap::new(),
            import_namespace_map: HashMap::new(),
            unsafe_parse_mode,
            file_name: file_name.to_string(),
            type_var_stack: vec![],
            counter: 0,
            block_var_stack: Vec::new(),
            func_var_stack: Vec::new(),
            //temporaries_var_stack: Vec::new(),
        }
    }

    pub (crate) fn push_err(&mut self, err: Error) {
        self.errors.push(err);
    }

    pub fn get_fn_defn_from_decls(&self, name: &str) -> Option<FuncDefn> {
        self.func_decls.iter().find(|&x| x.defn.name == *name).map(|x| x.defn.clone())
    }

    pub fn get_generic_fn_from_generics(&self, name: &str) -> Option<GenericFunc> {
        self.generic_func_decls.iter().find(|&x| x.func.defn.name == *name).cloned()
    }

    pub fn get_fn_defn_from_imports(&self, name: &str) -> Option<FuncDefn> {
        self.func_imports.iter().find(|&x| x.name == *name).cloned()
    }

    pub (crate) fn push_type_scope(&mut self, args: &[TypeArg]) -> Vec<TypeArg> {
        let mut v: HashMap<String, TypeArg> = match self.type_var_stack.last() {
            None => HashMap::new(),
            Some(s) => {
                s.var_names.clone()
            } 
        };

        let mut out: Vec<TypeArg> = vec![];
        
        for arg in args {
            let type_arg = TypeArg{name: self.get_unique_name(&arg.name), constraint: arg.constraint.clone()};
            out.push(type_arg.clone());
            v.insert(arg.name.clone(), type_arg);
        }
        self.type_var_stack.push(TypeScope{var_names: v});
        out
    }

    ///A trait type scope does not need a unique name: traits are global things. I think?
    fn push_trait_type_scope(&mut self, arg: &TypeArg)  {
        let mut v: HashMap<String, TypeArg> = match self.type_var_stack.last() {
            None => HashMap::new(),
            Some(s) => {
                s.var_names.clone()
            } 
        };
   
        v.insert(arg.name.clone(), arg.clone());
        self.type_var_stack.push(TypeScope{var_names: v});
    }

    fn push_empty_type_scope(&mut self) {
        let v: HashMap<String, TypeArg> = match self.type_var_stack.last() {
            None => HashMap::new(),
            Some(s) => {
                s.var_names.clone()
            } 
        };
        
        self.type_var_stack.push(TypeScope{var_names: v});
    }

    pub (crate) fn pop_type_scope(&mut self) {
        self.type_var_stack.pop();
    }

    pub (crate) fn get_scoped_type(&mut self, var_name: &str) -> Option<&TypeArg> {
        match self.type_var_stack.last() {
            None => None,
            Some(s) => {
                s.var_names.get(var_name)
            } 
        }
    }

    fn has_type_stack(&self) -> bool {
        !self.type_var_stack.is_empty()
    }

    pub (crate) fn get_unique_name(&mut self, name: &str) -> String {
        let counter = self.counter;
        self.counter += 1;
        format!("{}#{}", name, counter)
    }

    fn get_global_decl(&self, id: &str) -> Option<&GlobalVariableDecl> {
        self.global_decls.iter().find(|&x| x.name == id)
    }

    fn push_empty_block_scope(&mut self) {
        self.block_var_stack.push(Scope::default());
    }

    fn push_empty_func_scope(&mut self) {
        self.func_var_stack.push(Scope::default());
    }

    pub (crate) fn push_block_scope(&mut self) {
        let o_head = self.block_var_stack.last();
        match o_head {
            None => self.push_empty_block_scope(),
            Some(head) => {
                let mut new_var_names: HashMap<String, ScopedVar> = HashMap::new();
                
                for (key, val) in head.var_names.iter() {
                    new_var_names.insert(key.clone(), val.clone());
                }

                self.block_var_stack.push(Scope{var_names: new_var_names});
            }
        }
    }

    pub (crate) fn push_func_scope(&mut self) {
        let o_head = self.func_var_stack.last();
        match o_head {
            None => {
                self.push_empty_func_scope();
                self.push_empty_block_scope();
            },
            Some(head) => {
                let mut new_var_names: HashMap<String, ScopedVar> = HashMap::new();
                
                for (key, val) in head.var_names.iter() {
                    let new_val = match val {
                        ScopedVar::Local{internal_name, r#type, guard_type, mutability} => 
                            ScopedVar::ClosureRef{internal_name: internal_name.clone(), r#type: r#type.clone(), guard_type: guard_type.clone(), mutability: *mutability},
                        ScopedVar::ClosureRef{internal_name, r#type, guard_type, mutability} => 
                            ScopedVar::ClosureRef{internal_name: internal_name.clone(), r#type: r#type.clone(), guard_type: guard_type.clone(), mutability: *mutability},
                    };
                    new_var_names.insert(key.clone(), new_val);
                }

                self.func_var_stack.push(Scope{var_names: new_var_names.clone()});

                self.block_var_stack.push(Scope{var_names: new_var_names});
            }
        }
    }

    pub (crate) fn pop_block_scope(&mut self) {
        self.block_var_stack.pop();
    }

    pub (crate) fn pop_func_scope(&mut self) {
        self.block_var_stack.pop();
        self.func_var_stack.pop();
    }

    pub (crate) fn add_var(&mut self, var_name: &String, internal_var_name: &String, r#type: &QualifiedType, mutability: VariableMutability)  {
        let o_head = self.func_var_stack.last_mut();
        match o_head {
            None => {},
            Some(head) => {
                head.var_names.insert(var_name.clone(), ScopedVar::Local{internal_name: internal_var_name.clone(), r#type: r#type.clone(), guard_type: None, mutability});
            }
        }

        let o_head = self.block_var_stack.last_mut();
        match o_head {
            None => {},
            Some(head) => {
                head.var_names.insert(var_name.clone(), ScopedVar::Local{internal_name: internal_var_name.clone(), r#type: r#type.clone(), guard_type: None, mutability});
            }
        }
    }

    fn find_named_scoped_var_given_internal_name(&self, internal_name: &str) -> (String, ScopedVar) {
        let o_head = self.block_var_stack.last();
        match o_head {
            None => panic!(),
            Some(head) => {
                head.var_names.iter().find(|(_, sv)| { 
                    match sv {
                        ScopedVar::Local{internal_name: this_internal_name, r#type: _, guard_type: _, mutability: _} => this_internal_name == internal_name,
                        ScopedVar::ClosureRef{internal_name: this_internal_name, r#type: _, guard_type: _, mutability: _} => this_internal_name == internal_name,
                    }
                }).map(|(n, sv)| (n.clone(), sv.clone())).unwrap()
            }
        }
    }

    fn patch_var(&mut self, 
        var_name: &String,
        new_var: &ScopedVar
    ) {
        let o_head = self.func_var_stack.last_mut();
        match o_head {
            None => panic!(),
            Some(head) => {
                head.var_names.insert(var_name.clone(), new_var.clone());
            }
        }

        let o_head = self.block_var_stack.last_mut();
        match o_head {
            None => panic!(),
            Some(head) => {
                head.var_names.insert(var_name.clone(), new_var.clone());
            }
        }
    }

    fn patch_guard_type(
        o_old_guard_type: &Option<Type>,
        new_guard_type: &Type, 
        loc: &SourceLocation,
        parser_context: &mut ParserContext
    ) -> Type {
        match o_old_guard_type {
            None => new_guard_type.clone(),
            Some(old_guard_type) => {
                if old_guard_type == new_guard_type {
                    parser_context.push_err(Error::TypeGuardReapply(*loc, new_guard_type.clone()));
                    new_guard_type.clone()
                } else {
                    match new_guard_type{
                        Type::Int(lower_to, upper_to) => {
                            match old_guard_type {
                                Type::Int(lower_from, upper_from) => {
                                    Type::Int(cmp::max(*lower_from, *lower_to), cmp::min(*upper_to, *upper_from))
                                },
                                _ => new_guard_type.clone()
                            }
                        },
                        _ => new_guard_type.clone()
                    }
                }
            }
        }
    }

    /// Apply a type guard to an existing variable.
    fn guard_var(&mut self, 
        internal_var_name: &String, 
        guard_type: &Type,
        loc: &SourceLocation,
    ) {
        //find the var in the block_var_stack
        let (var_name, shadowed_var) = self.find_named_scoped_var_given_internal_name(internal_var_name);

        let new_var = match shadowed_var {
            ScopedVar::Local{internal_name: _, r#type, guard_type: old_guard_type, mutability} => {
                ScopedVar::Local{internal_name: internal_var_name.clone(), r#type: r#type.clone(),
                    guard_type: Some(ParserContext::patch_guard_type(&old_guard_type, guard_type, loc, self)),
                    mutability
                }
            },
            ScopedVar::ClosureRef{internal_name: _, r#type, guard_type: old_guard_type, mutability} => {
                ScopedVar::ClosureRef{internal_name: internal_var_name.clone(), r#type: r#type.clone(), 
                    guard_type: Some(ParserContext::patch_guard_type(&old_guard_type, guard_type, loc, self)),
                    mutability
                }
            }
        };

        self.patch_var(&var_name, &new_var)
    }

    fn unguard_var(&mut self, 
        internal_var_name: &String, 
    )  {
        let (var_name, shadowed_var) = self.find_named_scoped_var_given_internal_name(internal_var_name);

        let o_new_var = match shadowed_var {
            ScopedVar::Local{internal_name: _, r#type, guard_type: o_guard_type, mutability} => {
                match o_guard_type {
                    Some(_) => Some(ScopedVar::Local{internal_name: internal_var_name.clone(), r#type: r#type.clone(), guard_type: None, mutability}),
                    None => None
                }
            },
            ScopedVar::ClosureRef{internal_name: _, r#type, guard_type: o_guard_type, mutability} => {
                match o_guard_type {
                    Some(_) => Some(ScopedVar::ClosureRef{internal_name: internal_var_name.clone(), r#type: r#type.clone(), guard_type: None, mutability}),
                    None => None
                }
            }
        };

        match o_new_var {
            Some(new_var) => self.patch_var(&var_name, &new_var),
            None => {}
        }
    }

    fn get_scoped_var(&self, var_name: &String) -> Option<&ScopedVar> {
        match self.block_var_stack.last() {
            None => None,
            Some(s) => {
                s.var_names.get(var_name)
            } 
        }
    }

    pub (crate) fn get_type_decl(&self, name: &str)-> Option<TypeDecl> {
        self.type_map.get(name).cloned()
    }

    fn append_type_decl_member_func(&mut self, type_name: &String, mf: &MemberFunc) {
        let td = self.type_map.get_mut(type_name).unwrap();
        match td {
            /*
            TypeDecl::Type{inner: _, type_args: _, export: _, member_funcs, constructor: _, under_construction: _} => {
                member_funcs.push(mf.clone());
            },
            */
            _ => unimplemented!()
        }
    }

    fn get_global_var(&self, name: &String) -> Option<&GlobalVariableDecl> {
        self.global_decls.iter().find(|&x| x.name == *name)
    }

    pub (crate) fn type_implements_trait(&self, t: &Type, trait_name: &String) -> bool {
        //first, has the type been patched with an implementation?
        if self.trait_impl_map.contains_key(&(t.get_type_name(), trait_name.clone())) {
            return true;
        }

        //no, so see if it's a type variable with a constraint
        match t {
            //yes it is in which case the constraint may implement the trait
            Type::VariableUsage{name: _, constraint} => constraint.contains_trait(trait_name),
            _ => false //give up
        }
    }

    /*
    /// Create a scope to keep track of any statements created for this statement
    fn open_temporary_scope(&mut self) -> () {
        self.temporaries_var_stack.push(Scope::default());
    }

    /// Copy the temporary scope into the parser func context, so we end up with variables in the AST
    fn close_temporary_scope(&mut self, parser_func_context: &mut ParserFuncContext) -> () {
        let scope = self.temporaries_var_stack.pop().unwrap();
        for (_, sv) in scope.var_names {
            match sv {
                ScopedVar::Local{internal_name, mutability, r#type, guard_type: _} => 
                    parser_func_context.add_var(&internal_name, &r#type, false, false, mutability),
                _ => unreachable!()
            }
        }
    }

    
    fn create_temporary(&mut self, r#type: &QualifiedType) -> String {
        let out = self.counter;
        self.counter += 1;
        let scope = self.temporaries_var_stack.last_mut().unwrap();
        let internal_var_name = format!("#{}", out);
        scope.var_names.insert(internal_var_name.clone(), ScopedVar::Local{internal_name: internal_var_name.clone(), r#type: r#type.clone(), guard_type: None, mutability: VariableMutability::Constant});
        internal_var_name
    }

    fn forget_temporary(&mut self, temporary_name: &String) -> () {
        let scope = self.temporaries_var_stack.last_mut().unwrap();
        scope.var_names.remove(temporary_name);
    }
    */
}

#[derive(Debug)]
pub (crate) struct ParserFuncContext{
    /// These are the local variables that are used to create the AST.
    pub local_vars: Vec<LocalVar>,
    pub local_var_map: HashMap<String, u32>,
    pub closure: Vec<ClosureRef>,
    pub given_func_return_type: QualifiedType,
    pub implied_func_return_type: QualifiedType,
    /// If we have entered a loop block
    pub in_iteration: bool,
    pub this_type: Option<QualifiedType>,
}

impl ParserFuncContext{
    pub fn new(this_type: &Option<QualifiedType>) -> ParserFuncContext{
        ParserFuncContext{
            local_vars: vec![],
            local_var_map: HashMap::new(),
            closure: vec![],
            given_func_return_type: QualifiedType::new(&Type::Undeclared, Mutability::Unknown),
            implied_func_return_type: QualifiedType::new(&Type::Undeclared, Mutability::Unknown),
            in_iteration: false,
            this_type: this_type.clone(),
        }
    }

    pub fn add_var(&mut self, internal_name: &String, var_type: &QualifiedType, closure_source: bool, arg: bool, mutability: VariableMutability) -> () {
        let idx = self.local_vars.len();
        self.local_var_map.insert(internal_name.clone(), idx as u32);
        self.local_vars.push(
            LocalVar{internal_name: internal_name.clone(), r#type: var_type.clone(), closure_source, arg, mutability}
        );
    }
}
