use serde::{Serialize, Deserialize};

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
/// A single character position in the
/// file including the line/column number
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl ::std::fmt::Display for Position {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

impl Position {
    #[inline]
    pub const fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
/// The start and end position of a token
/// including the line/column number
pub struct SourceLocation {
    pub start: Position,
    pub end: Position,
}

impl SourceLocation {
    pub const fn new_line_col(start_line: usize, start_column: usize, end_line: usize, end_column: usize) -> Self {
        Self { start: Position::new(start_line, start_column), end: Position::new(end_line, end_column) }
    }

    pub const fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    pub const fn new_start_position(start: Position) -> Self {
        Self { start, end: start }
    }

    pub const fn new_start_line_col(start_line: usize, start_column: usize,) -> Self {
        let pos = Position::new(start_line, start_column);
        Self { start: pos, end: pos }
    }

    pub fn extend_end_line_col(&mut self, end_line: usize, end_column: usize) {
        self.end = Position::new(end_line, end_column);
    }

}

impl ::std::fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
    }
}