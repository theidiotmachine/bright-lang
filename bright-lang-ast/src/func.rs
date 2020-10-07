use crate::ClosureRef;
use crate::expr::Arena;
use bright_lang_types::TypeArg;
use serde::{Serialize, Deserialize};

use bright_lang_types::{QualifiedType, FuncType};

use std::collections::HashMap;

use crate::core::VariableMutability;
use crate::expr::{NodeIdx};

/// Simple func arg, one with a name and a type, e.g. x: number
#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct FuncArg {
    pub mutability: VariableMutability,
    pub name: String,
    pub r#type: QualifiedType,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct LocalVar{
    pub mutability: VariableMutability,
    pub internal_name: String,
    pub r#type: QualifiedType,
    pub closure_source: bool,
    pub arg: bool,
}

/// Function definition
#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct Func {
    pub defn: FuncDefn,
    pub arena: Arena,
    /// Function body entry point into the arena. None if imported.
    pub body: Option<NodeIdx>,
    pub local_vars: Vec<LocalVar>,
    pub closure: Vec<ClosureRef>,
    pub local_var_map: HashMap<String, u32>,
}

/// Function definition as seen from another module
#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct FuncDefn {
    pub name: String,
    pub return_type: QualifiedType,
    pub args: Vec<FuncArg>,
    pub export: bool,
    pub generic_impl: bool,
    //pub type_guard: Option<TypeGuard>,
    pub member_func: bool,
}

impl FuncDefn{
    pub fn get_arg_types(&self) -> Vec<QualifiedType> {
        let mut out: Vec<QualifiedType> = vec![];
        for arg in &self.args {
            out.push(arg.r#type.clone());
        }
        out
    }

    pub fn get_func_type(&self) -> FuncType {
        FuncType{out_type: self.return_type.clone(), in_types: self.get_arg_types()}
    }
}

/// A generic function that consumes type arguments and can generate a concrete function
#[derive(Clone, PartialEq, Debug, Deserialize, Serialize)]
pub struct GenericFunc{
    pub num_this_type_args: usize,
    pub type_args: Vec<TypeArg>,
    pub func: Func
}

