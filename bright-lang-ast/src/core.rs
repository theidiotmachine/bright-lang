use serde::{Serialize, Deserialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Copy)]
pub enum VariableMutability{
    Constant,
    Variable,
}