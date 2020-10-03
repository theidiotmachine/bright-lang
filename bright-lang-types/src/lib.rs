use std::collections::HashSet;
use std::fmt::Display;
use std::fmt::Formatter;
use std::collections::HashMap;
use std::iter::FromIterator;

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

///This from [https://stackoverflow.com/questions/40718975/how-to-get-every-subset-of-a-vector-in-rust]
fn generate_power_set<T>(s: &[T]) -> Vec<Vec<T>> where T: Clone {
    (0..2usize.pow(s.len() as u32)).map(|i| {
         s.iter().enumerate().filter(|&(t, _)| (i >> t) % 2 == 1)
                             .map(|(_, element)| element.clone())
                             .collect()
     }).collect()
}   

impl TypeConstraint{
    ///whether or not this adheres to a marker trait
    pub fn contains_trait(&self, t: &str) -> bool {
        match self {
            TypeConstraint::Union(inner) => inner.iter().all(|x| x.contains_trait(t)),
            TypeConstraint::Intersection(inner) => inner.iter().any(|x| x.contains_trait(t)),
            TypeConstraint::Trait(i) => i == t,
            TypeConstraint::None => false
        }
    }

    pub fn get_all_contained_traits(&self) -> Vec<String> {
        match self {
            TypeConstraint::Union(inner) | TypeConstraint::Intersection(inner) => inner.iter().flat_map(|x| x.get_all_contained_traits()).collect(),
            TypeConstraint::Trait(i) => vec![i.clone()],
            TypeConstraint::None => vec![]
        }
    }

    pub fn conforms_to_trait_set(&self, ts: &HashSet<String>) -> bool {
        match self{
            TypeConstraint::Union(inner) => inner.iter().any(|x| x.conforms_to_trait_set(ts)),
            TypeConstraint::Intersection(inner) => inner.iter().all(|x| x.conforms_to_trait_set(ts)),
            TypeConstraint::Trait(i) => ts.contains(i),
            TypeConstraint::None => false
        }
    }

    ///Whether this is a subset of another trait. This is super expensive at the moment. I need some CS-hammer to optimise it. It basically
    /// goes through every combination of traits possible that this constraint conforms to, and checks that they are conformed to in the 
    /// other
    pub fn is_subset_of(&self, other: &TypeConstraint) -> bool {
        //first get all the contained traits of this
        let self_contained_traits = self.get_all_contained_traits();

        //now work through every combination of traits. If it's legitimate for this, it must be legitimate for the other
        let power_set = generate_power_set(&self_contained_traits);
        for s in power_set {
            let hs = HashSet::from_iter(s);
            if self.conforms_to_trait_set(&hs) && !other.conforms_to_trait_set(&hs) {
                return false;
            }
        }
        true
    }
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

impl Type{
    pub fn display_types(types: &[Type])->String{
        let mut vec: Vec<String> = vec![];
        for inner in types {
            vec.push(format!("{}", inner));
        }
        vec.join(",")
    }

    pub fn get_type_name(&self) -> String{
        match self {
            Type::Bool => String::from("Bool"),
            Type::Enum{name, type_args: _} => name.clone(),
            Type::EnumMember{enum_name, member_name, enum_type_args: _} => {format!("{}.{}", enum_name, member_name)},
            Type::FloatLiteral(_) => String::from("FloatLiteral"),
            Type::Func{func_type} => {
                let in_names: Vec<String> = func_type.in_types.iter().map(|x| x.r#type.get_type_name()).collect();
                let out_name = func_type.out_type.r#type.get_type_name();
                format!("Fn ({}) -> {}", in_names.join(", "), out_name)
            },
            Type::Int(_, _) => String::from("Int"),
            Type::Never => String::from("Never"),
            Type::Number => String::from("Number"),
            Type::ModuleLiteral(_) => String::from("ModuleLiteral"),
            Type::String => String::from("String"),
            Type::StringLiteral(_) => String::from("StringLiteral"),
            Type::Tuple(_) => String::from("Tuple"),
            Type::TypeLiteral(_) => String::from("TypeLiteral"),
            Type::Undeclared => String::from("Undeclared"),
            Type::Unknown => String::from("Unknown"),
            Type::UnsafeArray(_) => String::from("__Array"),
            Type::UserClass{name, type_args: _} => name.clone(),
            Type::VariableUsage{name, constraint: _} => name.clone(),
            Type::Void => String::from("Void"),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Bool => write!(f, "Bool"),
            Type::Enum{name, type_args} => write!(f, "{}<{}>", name, Type::display_types(type_args)),
            Type::EnumMember{enum_name, member_name, enum_type_args} => write!(f, "{}<{}>.{}", enum_name, Type::display_types(enum_type_args), member_name),
            Type::FloatLiteral(n) => write!(f, "{}", n),
            Type::Func{func_type} => write!(f, "{}", func_type),
            Type::Int(lower, upper) => write!(f, "Int<{}, {}>", lower, upper),
            Type::ModuleLiteral(n) => write!(f, "module: {}", n),
            Type::Never => write!(f, "Never"),
            Type::Number => write!(f, "Number"),
            Type::String => write!(f, "String"),
            Type::StringLiteral(n) => write!(f, "\"{}\"", n),
            Type::Tuple(types) => write!(f, "Tuple<{}>", Type::display_types(types)),
            Type::TypeLiteral(t) => write!(f, "type: {}", t),
            Type::Undeclared => write!(f, "Undeclared"),
            Type::Unknown => write!(f, "Unknown"),
            Type::UnsafeArray(t) => write!(f, "__Array<{}>", t),
            Type::UserClass{name, type_args} => write!(f, "{}<{}>", name, Type::display_types(type_args)),
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
    MutRef,
    Unknown
}

impl Display for Mutability {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Mutability::Const => write!(f, ""),
            Mutability::Mut => write!(f, "mut"),
            Mutability::MutRef => write!(f, "mut ref"),
            Mutability::Unknown => write!(f, "?"),
        }
    }
}

///A type with a mutability modifier.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct QualifiedType{
    pub r#type: Type,
    pub mutability: Mutability,
}

impl QualifiedType {
    /*
    pub fn get_pass_style(&self) -> PassStyle{ 
        self.r#type.get_pass_style()
    }
    */

    pub fn new(t: &Type, m: Mutability) -> QualifiedType  { 
        QualifiedType {r#type: t.clone(), mutability: m }
    }
    pub fn new_const(t: &Type) -> QualifiedType { 
        QualifiedType {r#type: t.clone(), mutability: Mutability::Const }
    }
    pub fn new_mut(t: &Type) -> QualifiedType { 
        QualifiedType {r#type: t.clone(), mutability: Mutability::Mut}
    }
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
    pub fn get_member_type_map(members: &[Member]) -> HashMap<String, Type> {
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