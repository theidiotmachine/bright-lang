use std::fmt::Display;
use std::fmt::Formatter;
use std::collections::HashMap;

use serde::{Serialize, Deserialize};

#[derive(Clone, PartialEq, Debug, Deserialize, Serialize, Hash)]
pub enum TypeConstraint{
    ///A union of constraints. Means at least one of these are true. If a union of traits, means at least 
    /// one is present. The member functions available are the intersection of traits in the union.
    Union(Vec<TypeConstraint>),
    ///An intersection of constraints. Means every one is true. If an intersection of traits, means
    /// all are present. The member functions available are the union of the traits in the intersection.
    Intersection(Vec<TypeConstraint>),
    ///Means implements this trait
    Trait(String),
    ///No condition, so accepts everything
    None
}


#[derive(Clone, PartialEq, Debug, Deserialize, Serialize, Hash)]
pub struct TypeArg{
    pub name: String,
    pub constraint: TypeConstraint,
}

// Type of a function.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FuncType{
    pub out_type: QualifiedType,
    pub in_types: Vec<QualifiedType>,
}

impl Display for FuncType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut vec: Vec<String> = vec![];
        for inner in &self.in_types{
            vec.push(format!("{}", inner));
        }
        write!(f, "({}) => {}", vec.join(","), self.out_type) 
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Type {
    ///boolean
    Bool,
    /// User enum class
    Enum{name: String, type_args: Vec<Type>},
    /// User enum class member type
    EnumMember{enum_name: String, member_name: String, enum_type_args: Vec<Type>},
    ///numeric literal of type 'number'
    FloatLiteral(f64),
    ///Func
    Func{func_type: Box<FuncType>},
    ///integer type. First number is lower bound, inclusive. Second number is upper bound, inclusive 
    Int(i128, i128),
    ///Type of a module. Not something that will ever appear at runtime.
    ModuleLiteral(String),
    /// bottom type
    Never,
    ///f64 number
    Number,
    ///string
    String,
    /// string literal
    StringLiteral(String),
    ///Tuple
    Tuple(Vec<Type>),
    ///type literal. Not something that will ever appear at runtime, but is useful for parts of the AST
    TypeLiteral(Box<QualifiedType>),
    /// not yet known - will be filled in by the type system
    Undeclared,
    /// top type
    Unknown,
    /// __array type
    UnsafeArray(Box<Type>),
    /// User class type
    UserClass{name: String, type_args: Vec<Type>},
    ///Unresolved type variable
    VariableUsage{name: String, constraint: TypeConstraint},
    /// unit type that actually manifests as nothing
    Void,
}

fn display_types(types: &Vec<Type>)->String{
    let mut vec: Vec<String> = vec![];
    for inner in types {
        vec.push(format!("{}", inner));
    }
    vec.join(",")
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Bool => write!(f, "Bool"),
            Type::Enum{name, type_args} => write!(f, "{}<{}>", name, display_types(type_args)),
            Type::EnumMember{enum_name, member_name, enum_type_args} => write!(f, "{}<{}>.{}", enum_name, display_types(enum_type_args), member_name),
            Type::FloatLiteral(n) => write!(f, "{}", n),
            Type::Func{func_type} => write!(f, "{}", func_type),
            Type::Int(lower, upper) => write!(f, "Int<{}, {}>", lower, upper),
            Type::ModuleLiteral(n) => write!(f, "module: {}", n),
            Type::Never => write!(f, "Never"),
            Type::Number => write!(f, "Number"),
            Type::String => write!(f, "String"),
            Type::StringLiteral(n) => write!(f, "\"{}\"", n),
            Type::Tuple(types) => write!(f, "Tuple<{}>", display_types(types)),
            Type::TypeLiteral(t) => write!(f, "type: {}", t),
            Type::Undeclared => write!(f, "Undeclared"),
            Type::Unknown => write!(f, "Unknown"),
            Type::UnsafeArray(t) => write!(f, "__Array<{}>", t),
            Type::UserClass{name, type_args} => write!(f, "{}<{}>", name, display_types(type_args)),
            Type::VariableUsage{name, constraint: _} => write!(f, "{}", name),
            Type::Void => write!(f, "Void"),
            
        }
    }
}

///Whether a type is const or mut
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Copy)]
pub enum Mutability {
    Const,
    Mut,
    MutRef
}

impl Display for Mutability {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Mutability::Const => write!(f, ""),
            Mutability::Mut => write!(f, "mut"),
            Mutability::MutRef => write!(f, "mut ref"),
        }
    }
}

///A type with a mutability modifier.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct QualifiedType{
    pub r#type: Type,
    pub mutability: Mutability,
}

impl Display for QualifiedType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.mutability, self.r#type)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Copy)]
pub enum Privacy{
    Public, Private, Protected
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MemberFunc{
    pub type_args: Vec<TypeArg>,
    pub func_type: FuncType,
    ///This is the full name with the type prefix in front of it.
    pub mangled_name: String,
    ///This is the name you would type.
    pub name: String,
    pub privacy: Privacy,
}

/// Data member of a struct.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Member{
    pub name: String,
    pub r#type: Type,
    pub privacy: Privacy,
}

impl Member {
    pub fn get_member_type_map(members: &Vec<Member>) -> HashMap<String, Type> {
        let mut out: HashMap<String, Type> = HashMap::new();
        for m in members {
            out.insert(m.name.clone(), m.r#type.clone());
        }
        out
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Copy)]
pub enum UserClassStorage{
    Heap,
    HeapRef,
    Stack
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UserClass{
    pub type_args: Vec<TypeArg>, 
    pub member_funcs: Vec<MemberFunc>, 
    pub constructor: Option<MemberFunc>, 
    pub members: Vec<Member>, 
    pub storage: UserClassStorage,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TraitMemberFunc{
    pub trait_name: String,
    pub type_args: Vec<TypeArg>,
    pub func_type: FuncType,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TraitDecl{
    pub name: String, 
    pub member_funcs: Vec<TraitMemberFunc>, 
}