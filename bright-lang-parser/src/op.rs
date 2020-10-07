#[derive(Debug, PartialEq, Clone, Copy)]
pub (crate) enum Association{
    Left, 
    Right,
}

pub (crate) struct BinaryOperatorData{
    pub precedence: i32,
    pub association: Association,
    pub is_member: bool,
}