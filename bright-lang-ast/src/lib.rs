use std::collections::HashSet;
use crate::expr::Arena;
use bright_lang_types::TraitDecl;
use crate::core::VariableMutability;
use crate::expr::NodeIdx;
use bright_lang_types::UserClass;
use bright_lang_types::MemberFunc;
use bright_lang_types::Type;
use crate::func::GenericFunc;
use bright_lang_types::QualifiedType;
use crate::func::Func;
use std::collections::HashMap;

use serde::{Serialize, Deserialize};

use crate::func::FuncDefn;

pub mod func;
pub mod core;
pub mod expr;

/// Variable decl
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct GlobalVariableImport {
    pub name: String,
    pub r#type: QualifiedType,
    pub export: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ClosureRef {
    pub internal_name: String,
    pub r#type: QualifiedType,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TraitImpl{
    pub trait_name: String, 
    pub for_type: Type,
    pub export: bool,
    pub member_funcs: Vec<MemberFunc>, 
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TypeDecl{
    Alias{of: Type, export: bool},
    UserClass{export: bool, under_construction: bool, user_class: UserClass},
    Enum{export: bool},
}

/// Variable decl
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct GlobalVariableDecl {
    pub name: String,
    pub r#type: QualifiedType,
    pub init: Option<NodeIdx>,
    pub arena: Arena,
    pub export: bool,
    pub mutability: VariableMutability,
}

#[derive(PartialEq, Debug, Deserialize, Serialize)]
pub struct AST {
    ///global variables declared in this program
    pub global_decls: Vec<GlobalVariableDecl>,

    ///global variables imported in this program
    pub global_imports: Vec<GlobalVariableImport>,
    
    pub func_decls: Vec<Func>,
    pub func_imports: Vec<FuncDefn>,

    pub generic_func_decls: Vec<GenericFunc>,
    
    //start function
    pub start: String,

    pub type_map: HashMap<String, TypeDecl>,

    pub trait_map: HashMap<String, TraitDecl>,
    pub trait_impl_map: HashMap<(String, String), TraitImpl>,
    pub exported_traits: HashSet<String>,
}

/// The things exported from a compilation unit. Not really part of the AST but very related.
#[derive(Debug, Deserialize, Serialize, Default)]
pub struct Exports{
    pub global_decls: Vec<GlobalVariableDecl>,
    pub global_imports: Vec<GlobalVariableImport>,
    pub func_decls: Vec<FuncDefn>,
    pub func_imports: Vec<FuncDefn>,
    pub generic_func_decls: Vec<GenericFunc>,
    pub types: Vec<TypeDecl>,
    pub traits: Vec<TraitDecl>,
}

impl Exports{
    pub fn new() -> Exports {
        Exports{ global_decls: vec![], 
            global_imports: vec![], 
            func_decls: vec![], 
            func_imports: vec![], 
            generic_func_decls: vec![], 
            types: vec![],
            traits: vec![],
        }
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub struct Imports{
    pub exports: Exports,
    pub unique_name: String,
    pub module_name: String
}
