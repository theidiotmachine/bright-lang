use std::fmt;
use logos::{Logos, Lexer};
use unicode_segmentation::UnicodeSegmentation;
use bright_lang_errs::source_location::SourceLocation;

#[derive(Logos, Debug, PartialEq, Clone)]
enum SingleLineCommentToken {
    #[token("\n")]
    NewLine,

    #[regex(r"[^\n]*")]
    AllTheThings,

    #[error]
    Error,
}

#[derive(Logos, Debug, PartialEq, Clone)]
enum MultiLineCommentToken {
    #[token("/*")]
    MultiLineCommentOpen,

    #[token("*/")]
    MultiLineCommentClose,

    #[regex(r"[^(/*)\n(*/)]*")]
    AllTheThings,

    #[token("\n")]
    NewLine,

    #[error]
    Error,
}

#[derive(Logos, Debug, PartialEq, Clone)]
enum StringToken {
    #[token("'")]
    SingleQuote,

    #[regex(r"[^(\\')'\n]*")]
    AllTheThings,

    #[token("\\'")]
    EscapedQuote,

    #[token("\n")]
    NewLine,

    #[error]
    Error,
}

#[derive(Logos, Debug, PartialEq, Clone)]
enum MainToken {
    #[regex(r"[ \t\f]+")]
    WhiteSpace,

    #[token("//")]
    SingleLineComment,

    #[token("/*")]
    MultiLineCommentOpen,

    #[token("\n")]
    NewLine,

    #[regex(r"[\p{L}_][\p{L}[:digit:]_]*")]
    Ident,

    #[regex(r"0x[0-9a-f_]+")]
    HexLiteral,

    #[regex(r"0b[01_]+")]
    BinLiteral,

    #[regex(r"0o[0-7_]+")]
    OctLiteral,

    #[regex(r"-?([0-9][0-9_]*([.][0-9_]*)?|[.][0-9_]+)")]
    DecLiteral,

    #[token("'")]
    SingleQuote,

    #[token("&&")]
    And,

    #[token("=")]
    Assign,

    #[token(":")]
    Colon,

    #[token(",")]
    Comma,
    
    #[token("/")]
    Divide,

    #[token(".`")]
    Dot,

    #[token("==")]
    Equal,

    #[token("=>")]
    FatArrow,
    
    #[token(">")]
    GreaterThan,

    #[token("&")]
    Intersection,

    #[token("<")]
    LessThan,

    #[token("-")]
    Minus,

    #[token("*")]
    Multiply,

    #[token("!")]
    Not,

    #[token("!=")]
    NotEqual,

    #[token("||")]
    Or,

    #[token("+")]
    Plus,

    #[token("(")]
    RoundOpen,

    #[token(")")]
    RoundClose,

    #[token("[")]
    SquareOpen,

    #[token("]")]
    SquareClose,

    #[token("{")]
    SquigglyOpen,

    #[token("}")]
    SquigglyClose,

    #[token(";")]
    Semicolon,

    #[token("->")]
    ThinArrow,

    #[token("|")]
    Union,

    #[token("Bool")]
    Bool,

    #[token("False")]
    False,

    #[token("Int")]
    Int,

    #[token("Never")]
    Never,

    #[token("Number")]
    Number,

    #[token("True")]
    True,

    #[token("Unknown")]
    Unknown,

    #[token("Void")]
    Void,
    
    #[token("as")]
    As,

    #[token("break")]
    Break,

    #[token("class")]
    Class,

    #[token("continue")]
    Continue,

    #[token("else")]
    Else,

    #[token("enum")]
    Enum,

    #[token("export")]
    Export,

    #[token("extends")]
    Extends,

    #[token("fn")]
    Fn,

    #[token("for")]
    For,

    #[token("from")]
    From,

    #[token("if")]
    If,

    #[token("import")]
    Import,

    #[token("heap")]
    Heap,

    #[token("implements")]
    Implements,

    #[token("interface")]
    Interface,

    #[token("let")]
    Let,

    #[token("mut")]
    Mut,

    #[token("of")]
    Of,

    #[token("private")]
    Private,

    #[token("public")]
    Public,

    #[token("ptr")]
    Ptr,

    #[token("ref")]
    Ref,

    #[token("return")]
    Return,

    #[token("shared")]
    Shared,

    #[token("stack")]
    Stack,

    #[token("switch")]
    Switch,

    #[token("this")]
    This,

    #[token("trait")]
    Trait,

    #[token("var")]
    Var,

    #[token("while")]
    While,
    
    #[error]
    Error,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Token {
    ///A new line, or \n
    NewLine,
    ///A 'symbol', meaning some arbitrary text
    Ident,

    HexLiteral,

    BinLiteral,

    OctLiteral,

    DecLiteral,

    And,

    Assign,

    Colon,

    Comma,
    
    Divide,

    Dot,

    Equal,

    Error,

    FatArrow,
    
    GreaterThan,

    Intersection,

    LessThan,

    Minus,

    Multiply,

    Not,

    NotEqual,

    Or,

    Plus,

    RoundOpen,

    RoundClose,

    SquareOpen,

    SquareClose,

    SquigglyOpen,

    SquigglyClose,

    Semicolon,

    StringLiteral,

    ThinArrow,

    Union,

    Bool,

    False,

    Int,

    Never,

    Number,

    True,

    Unknown,

    Void,
    
    As,

    Break,

    Class,

    Continue,

    Else,

    Enum,

    Export,

    Extends,

    Fn,

    For,

    From,

    If,

    /// `import` keyword
    Import,

    Heap,

    Implements,

    Interface,

    Let,

    Mut,

    Of,

    Private,

    Public,

    Ptr,

    Ref,

    Return,

    Shared,

    Stack,

    Switch,

    This,

    Trait,

    Var,

    While,
    
    EOF,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::NewLine => write!(f, "\\n"),
            Token::Ident | Token::HexLiteral | Token::BinLiteral | Token::OctLiteral | Token::DecLiteral => write!(f, "*literal*"),
            Token::And => write!(f, "&&"),
            Token::Assign => write!(f, "="),
            Token::Colon => write!(f, ":"),
            Token::Comma => write!(f, ","),
            Token::Divide => write!(f, "/"),
            Token::Dot => write!(f, "."),
            Token::Equal => write!(f, "=="),
            Token::Error => write!(f, "*error*"),
            Token::FatArrow => write!(f, "=>"),
            Token::GreaterThan => write!(f, "="),
            Token::Intersection => write!(f, "&"),
            Token::LessThan => write!(f, "<"),
            Token::Minus => write!(f, "-"),
            Token::Multiply => write!(f, "*"),
            Token::Not => write!(f, "!"),
            Token::NotEqual => write!(f, "!="),
            Token::Or => write!(f, "||"),
            Token::Plus => write!(f, "+"),
            Token::RoundOpen => write!(f, "("),
            Token::RoundClose => write!(f, ")"),
            Token::SquareOpen => write!(f, "["),
            Token::SquareClose => write!(f, "]"),
            Token::SquigglyOpen => write!(f, r"{{"),
            Token::SquigglyClose => write!(f, r"}}"),
            Token::Semicolon => write!(f, ";"),
            Token::StringLiteral => write!(f, "string literal"),
            Token::ThinArrow => write!(f, "->"),
            Token::Union => write!(f, "&"),
            Token::Bool => write!(f, "Bool"),
            Token::False => write!(f, "False"),
            Token::Int => write!(f, "Int"),
            Token::Never => write!(f, "Never"),
            Token::Number => write!(f, "Number"),
            Token::True => write!(f, "True"),
            Token::Unknown => write!(f, "Unknown"),
            Token::Void => write!(f, "Void"),
            Token::As => write!(f, "as"),
            Token::Break => write!(f, "break"),
            Token::Class => write!(f, "class"),
            Token::Continue => write!(f, "continue"),
            Token::Else => write!(f, "else"),
            Token::Enum => write!(f, "enum"),
            Token::Export => write!(f, "export"),
            Token::Extends => write!(f, "extends"),
            Token::Fn => write!(f, "fn"),
            Token::For => write!(f, "for"),
            Token::From => write!(f, "from"),
            Token::If => write!(f, "if"),
            Token::Import => write!(f, "import"),
            Token::Heap => write!(f, "heap"),
            Token::Implements => write!(f, "implements"),
            Token::Interface => write!(f, "interface"),
            Token::Let => write!(f, "let"),
            Token::Mut => write!(f, "mut"),
            Token::Of => write!(f, "of"),
            Token::Private=> write!(f, "private"),
            Token::Public => write!(f, "public"),
            Token::Ptr => write!(f, "ptr"),
            Token::Ref => write!(f, "ref"),
            Token::Return => write!(f, "return"),
            Token::Shared => write!(f, "shared"),
            Token::Stack => write!(f, "stack"),
            Token::Switch => write!(f, "switch"),
            Token::This => write!(f, "this"),
            Token::Trait => write!(f, "trait"),
            Token::Var => write!(f, "var"),
            Token::While => write!(f, "while"),
            Token::EOF => write!(f, "EOF"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TokenData{
    pub token: Token,
    pub loc: SourceLocation,
    pub text: Option<String>,
}

impl fmt::Display for TokenData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.token {
            Token::Ident | Token::HexLiteral | Token::BinLiteral | Token::OctLiteral 
                | Token::DecLiteral | Token::Error | Token::StringLiteral => write!(f, "{}", self.text.clone().unwrap()),
            _ => self.token.fmt(f),
        }
    }
}

enum Modes<'a> {
    Main(Lexer<'a, MainToken>),
    String(Lexer<'a, StringToken>),
    MultiLineComment(Lexer<'a, MultiLineCommentToken>),
    SingleLineComment(Lexer<'a, SingleLineCommentToken>),
}

impl<'a> Modes<'a> {
    fn new(s: &'a str) -> Self {
        Self::Main(MainToken::lexer(s))
    }
}

pub struct BrightLexer<'a>{
    mode: Modes<'a>,
    line_number: usize,
    col_number: usize,
    ml_depth: u32
}

impl<'a> BrightLexer<'a> {
    pub fn new(s: &'a str) -> Self {
        BrightLexer {
            mode: Modes::new(s),
            line_number: 1,
            col_number: 1,
            ml_depth: 0
        }
    }

    fn skip_ml_comment(&mut self) {
        match &mut self.mode {
            Modes::MultiLineComment(t) => {
                loop {
                    let r = t.next();
                    match r {
                        //EOF so bail
                        None => {
                            self.mode = Modes::Main(t.to_owned().morph());
                            return;
                        },
                        Some(MultiLineCommentToken::AllTheThings) => {
                            let c = t.slice().graphemes(true).count();
                            self.col_number += c;
                        },
                        //not sure what to do here.
                        Some(MultiLineCommentToken::Error) => {
                            self.col_number += t.slice().graphemes(true).count();
                        },
                        Some(MultiLineCommentToken::MultiLineCommentClose) => {
                            self.col_number += 2;
                            self.ml_depth -= 1;
                            if self.ml_depth == 0 {
                                self.mode = Modes::Main(t.to_owned().morph());
                                return;
                            }
                        },
                        Some(MultiLineCommentToken::MultiLineCommentOpen) => {
                            self.col_number += 2;
                            self.ml_depth += 1;
                        },
                        Some(MultiLineCommentToken::NewLine) => {
                            self.line_number += 1;
                            self.col_number = 1;
                        }
                    }
                }
            }
            _ => panic!()
        }
    }

    fn skip_sl_comment(&mut self) {
        match &mut self.mode {
            Modes::SingleLineComment(t) => {
                loop {
                    let r = t.next();
                    match r {
                        //EOF so bail
                        None => {
                            self.mode = Modes::Main(t.to_owned().morph());
                            return;
                        },
                        Some(SingleLineCommentToken::AllTheThings) => {
                            self.col_number += t.slice().graphemes(true).count();
                        },
                        //not sure what to do here.
                        Some(SingleLineCommentToken::Error) => {
                            self.col_number += t.slice().graphemes(true).count();
                        },
                        Some(SingleLineCommentToken::NewLine) => {
                            self.line_number += 1;
                            self.col_number = 1;
                            self.mode = Modes::Main(t.to_owned().morph());
                            return;
                        }
                    }
                }
            }
            _ => panic!()
        }
    }

    fn get_string_token(&mut self, line_number: usize, col_number: usize) -> TokenData {
        let mut text: String = String::from("");
        let mut loc = SourceLocation::new_start_line_col(line_number, col_number);

        match &mut self.mode {
            Modes::String(t) => {
                loop {
                    let r = t.next();
                    match r {
                        None => {
                            //EOF so bail
                            self.mode = Modes::Main(t.to_owned().morph());
                            return TokenData{token: Token::StringLiteral, loc, text: Some(text)};
                        },
                        Some(StringToken::AllTheThings) => {
                            let s = t.slice();
                            self.col_number += s.graphemes(true).count();
                            text += s;
                        },
                        Some(StringToken::Error) => {
                            //some error, so just return what we have
                            self.col_number += t.slice().graphemes(true).count();
                            loc.extend_end_line_col(self.line_number, self.col_number);
                            self.mode = Modes::Main(t.to_owned().morph());
                            return TokenData{token: Token::StringLiteral, loc, text: Some(text)};
                        },
                        Some(StringToken::EscapedQuote) => {
                            text += "'";
                            self.col_number += 2;
                        },
                        Some(StringToken::NewLine) => {
                            self.line_number += 1;
                            self.col_number = 1;
                            text += "\n";
                        },
                        Some(StringToken::SingleQuote) => {
                            //we're done here
                            self.col_number += 1;
                            loc.extend_end_line_col(self.line_number, self.col_number);
                            self.mode = Modes::Main(t.to_owned().morph());
                            return TokenData{token: Token::StringLiteral, loc, text: Some(text)};
                        },
                    }
                    
                }
            },
            _ => panic!()
        }
    }

    pub fn next(&mut self) -> TokenData {
        loop {
            match &mut self.mode {
                Modes::Main(t) => {
                    let r = t.next();
                    let slice = t.slice();
                    let line_number = self.line_number;
                    let col_number = self.col_number;
                    let mut loc = SourceLocation::new_start_line_col(line_number, col_number);
                    self.col_number += slice.graphemes(true).count();
                    loc.extend_end_line_col(self.line_number, self.col_number);

                    match r {
                        None => 
                            return TokenData{token: Token::EOF, loc, text: None},
                        Some(MainToken::And) => 
                            return TokenData{token: Token::And, loc, text: None},
                        Some(MainToken::As) => 
                            return TokenData{token: Token::As, loc, text: None},
                        Some(MainToken::Assign) => 
                            return TokenData{token: Token::Assign, loc, text: None},
                        Some(MainToken::BinLiteral) => 
                            return TokenData{token: Token::BinLiteral, loc, text: Some(String::from(slice))},
                        Some(MainToken::Bool) => 
                            return TokenData{token: Token::Bool, loc, text: None},
                        Some(MainToken::Break) => 
                            return TokenData{token: Token::Break, loc, text: None},
                        Some(MainToken::Class) => 
                            return TokenData{token: Token::Class, loc, text: None},
                        Some(MainToken::Colon) => 
                            return TokenData{token: Token::Colon, loc, text: None},
                        Some(MainToken::Comma) => 
                            return TokenData{token: Token::Comma, loc, text: None},
                        Some(MainToken::Continue) => 
                            return TokenData{token: Token::Continue, loc, text: None},
                        Some(MainToken::DecLiteral) => 
                            return TokenData{token: Token::DecLiteral, loc, text: Some(String::from(slice))},
                        Some(MainToken::Divide) => 
                            return TokenData{token: Token::Divide, loc, text: None},
                        Some(MainToken::Dot) => 
                            return TokenData{token: Token::Dot, loc, text: None},
                        Some(MainToken::Else) => 
                            return TokenData{token: Token::Else, loc, text: None},
                        Some(MainToken::Enum) => 
                            return TokenData{token: Token::Enum, loc, text: None},
                        Some(MainToken::Equal) => 
                            return TokenData{token: Token::Equal, loc, text: None},
                        Some(MainToken::Error) => 
                            return TokenData{token: Token::Error, loc, text: Some(String::from(slice))},
                        Some(MainToken::Export) => 
                            return TokenData{token: Token::Export, loc, text: None},
                        Some(MainToken::Extends) => 
                            return TokenData{token: Token::Extends, loc, text: None},
                        Some(MainToken::False) => 
                            return TokenData{token: Token::False, loc, text: None},
                        Some(MainToken::FatArrow) => 
                            return TokenData{token: Token::FatArrow, loc, text: None},
                        Some(MainToken::Fn) => 
                            return TokenData{token: Token::Fn, loc, text: None},
                        Some(MainToken::For) => 
                            return TokenData{token: Token::For, loc, text: None},
                        Some(MainToken::From) => 
                            return TokenData{token: Token::From, loc, text: None},
                        Some(MainToken::GreaterThan) => 
                            return TokenData{token: Token::GreaterThan, loc, text: None},
                        Some(MainToken::Heap) => 
                            return TokenData{token: Token::Heap, loc, text: None},
                        Some(MainToken::HexLiteral) => 
                            return TokenData{token: Token::HexLiteral, loc, text: Some(String::from(slice))},
                        Some(MainToken::If) => 
                            return TokenData{token: Token::If, loc, text: None},
                        Some(MainToken::Implements) => 
                            return TokenData{token: Token::Implements, loc, text: None},
                        Some(MainToken::Import) => 
                            return TokenData{token: Token::Import, loc, text: None},
                        Some(MainToken::Int) => 
                            return TokenData{token: Token::Int, loc, text: None},
                        Some(MainToken::Interface) => 
                            return TokenData{token: Token::Interface, loc, text: None},
                        Some(MainToken::Intersection) => 
                            return TokenData{token: Token::Intersection, loc, text: None},
                        Some(MainToken::LessThan) => 
                            return TokenData{token: Token::LessThan, loc, text: None},
                        Some(MainToken::Let) => 
                            return TokenData{token: Token::Let, loc, text: None},
                        Some(MainToken::Minus) => 
                            return TokenData{token: Token::Minus, loc, text: None},
                        Some(MainToken::MultiLineCommentOpen) => {
                            self.mode = Modes::MultiLineComment(t.to_owned().morph());
                            self.ml_depth = 1;
                        },
                        Some(MainToken::Multiply) => 
                            return TokenData{token: Token::Multiply, loc, text: None},
                        Some(MainToken::Mut) => 
                            return TokenData{token: Token::Mut, loc, text: None},
                        Some(MainToken::Never) => 
                            return TokenData{token: Token::Never, loc, text: None},
                        Some(MainToken::NewLine) => {
                            self.line_number += 1;
                            self.col_number = 1;
                            loc.extend_end_line_col(self.line_number, self.col_number);
                            return TokenData{token: Token::NewLine, loc, text: None};
                        },
                        Some(MainToken::Not) => 
                            return TokenData{token: Token::Not, loc, text: None},
                        Some(MainToken::NotEqual) => 
                            return TokenData{token: Token::NotEqual, loc, text: None},
                        Some(MainToken::Number) => 
                            return TokenData{token: Token::Number, loc, text: None},
                        Some(MainToken::OctLiteral) => 
                            return TokenData{token: Token::OctLiteral, loc, text: Some(String::from(slice))},
                        Some(MainToken::Of) => 
                            return TokenData{token: Token::Of, loc, text: None},
                        Some(MainToken::Or) => 
                            return TokenData{token: Token::Or, loc, text: None},
                        Some(MainToken::Plus) => 
                            return TokenData{token: Token::Plus, loc, text: None},
                        Some(MainToken::Private) => 
                            return TokenData{token: Token::Private, loc, text: None},
                        Some(MainToken::Ptr) => 
                            return TokenData{token: Token::Ptr, loc, text: None},
                        Some(MainToken::Public) => 
                            return TokenData{token: Token::Public, loc, text: None},
                        Some(MainToken::Ref) => 
                            return TokenData{token: Token::Ref, loc, text: None},
                        Some(MainToken::Return) => 
                            return TokenData{token: Token::Return, loc, text: None},
                        Some(MainToken::RoundClose) => 
                            return TokenData{token: Token::RoundClose, loc, text: None},
                        Some(MainToken::RoundOpen) => 
                            return TokenData{token: Token::RoundOpen, loc, text: None},
                        Some(MainToken::Semicolon) => 
                            return TokenData{token: Token::Semicolon, loc, text: None},
                        Some(MainToken::Shared) => 
                            return TokenData{token: Token::Shared, loc, text: None},
                        Some(MainToken::SingleLineComment) => {
                            self.mode = Modes::SingleLineComment(t.to_owned().morph());
                        },
                        Some(MainToken::SingleQuote) => {
                            self.mode = Modes::String(t.to_owned().morph());
                            return self.get_string_token(line_number, col_number);
                        },
                        Some(MainToken::SquareClose) => 
                            return TokenData{token: Token::SquareClose, loc, text: None},
                        Some(MainToken::SquareOpen) => 
                            return TokenData{token: Token::SquareOpen, loc, text: None},
                        Some(MainToken::SquigglyClose) => 
                            return TokenData{token: Token::SquigglyClose, loc, text: None},
                        Some(MainToken::SquigglyOpen) => 
                            return TokenData{token: Token::SquigglyOpen, loc, text: None},
                        Some(MainToken::Stack) => 
                            return TokenData{token: Token::Stack, loc, text: None},
                        Some(MainToken::Switch) => 
                            return TokenData{token: Token::Switch, loc, text: None},
                        Some(MainToken::Ident) => 
                            return TokenData{token: Token::Ident, loc, text: Some(String::from(slice))},
                        Some(MainToken::ThinArrow) => 
                            return TokenData{token: Token::ThinArrow, loc, text: None},
                        Some(MainToken::This) => 
                            return TokenData{token: Token::This, loc, text: None},
                        Some(MainToken::Trait) => 
                            return TokenData{token: Token::Trait, loc, text: None},
                        Some(MainToken::True) => 
                            return TokenData{token: Token::True, loc, text: None},
                        Some(MainToken::Union) => 
                            return TokenData{token: Token::Union, loc, text: None},
                        Some(MainToken::Unknown) => 
                            return TokenData{token: Token::Unknown, loc, text: None},
                        Some(MainToken::Var) => 
                            return TokenData{token: Token::Var, loc, text: None},
                        Some(MainToken::Void) => 
                            return TokenData{token: Token::Void, loc, text: None},
                        Some(MainToken::While) => 
                            return TokenData{token: Token::Var, loc, text: None},
                        Some(MainToken::WhiteSpace) => {}
                    }
                },
                Modes::String(_) => {
                    unreachable!();
                },
                Modes::MultiLineComment(_) => {
                    self.skip_ml_comment();
                },
                Modes::SingleLineComment(_) => {
                    self.skip_sl_comment();
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn simple() {
        use super::*;

        let mut lex = MainToken::lexer("fn hello(Привет: Number) -> Void");
        assert_eq!(lex.next(), Some(MainToken::Fn));
        assert_eq!(lex.span(), 0..2);
        assert_eq!(lex.next(), Some(MainToken::WhiteSpace));
        assert_eq!(lex.next(), Some(MainToken::Ident));
        assert_eq!(lex.slice(), "hello");
        assert_eq!(lex.next(), Some(MainToken::RoundOpen));
        assert_eq!(lex.next(), Some(MainToken::Ident));
        assert_eq!(lex.slice(), "Привет");
        assert_eq!(lex.next(), Some(MainToken::Colon));
        assert_eq!(lex.next(), Some(MainToken::WhiteSpace));
        assert_eq!(lex.next(), Some(MainToken::Number));
        assert_eq!(lex.next(), Some(MainToken::RoundClose));
        assert_eq!(lex.next(), Some(MainToken::WhiteSpace));
        assert_eq!(lex.next(), Some(MainToken::ThinArrow));
        assert_eq!(lex.next(), Some(MainToken::WhiteSpace));
        assert_eq!(lex.next(), Some(MainToken::Void));
    }

    #[test]
    fn numbers() {
        use super::*;

        let mut lex = MainToken::lexer("0x12a 0b0011_0111 0o124 -5 32 63.2 3. .4 -.5");
        assert_eq!(lex.next(), Some(MainToken::HexLiteral));
        assert_eq!(lex.slice(), "0x12a");
        assert_eq!(lex.next(), Some(MainToken::WhiteSpace));
        assert_eq!(lex.next(), Some(MainToken::BinLiteral));
        assert_eq!(lex.slice(), "0b0011_0111");
        assert_eq!(lex.next(), Some(MainToken::WhiteSpace));
        assert_eq!(lex.next(), Some(MainToken::OctLiteral));
        assert_eq!(lex.slice(), "0o124");
        assert_eq!(lex.next(), Some(MainToken::WhiteSpace));
        assert_eq!(lex.next(), Some(MainToken::DecLiteral));
        assert_eq!(lex.slice(), "-5");
        assert_eq!(lex.next(), Some(MainToken::WhiteSpace));
        assert_eq!(lex.next(), Some(MainToken::DecLiteral));
        assert_eq!(lex.slice(), "32");
        assert_eq!(lex.next(), Some(MainToken::WhiteSpace));
        assert_eq!(lex.next(), Some(MainToken::DecLiteral));
        assert_eq!(lex.slice(), "63.2");
        assert_eq!(lex.next(), Some(MainToken::WhiteSpace));
        assert_eq!(lex.next(), Some(MainToken::DecLiteral));
        assert_eq!(lex.slice(), "3.");
        assert_eq!(lex.next(), Some(MainToken::WhiteSpace));
        assert_eq!(lex.next(), Some(MainToken::DecLiteral));
        assert_eq!(lex.slice(), ".4");
        assert_eq!(lex.next(), Some(MainToken::WhiteSpace));
        assert_eq!(lex.next(), Some(MainToken::DecLiteral));
        assert_eq!(lex.slice(), "-.5");
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn semi_colons() {
        use super::*;
        let mut lex = MainToken::lexer("{a; b\n}");
        assert_eq!(lex.next(), Some(MainToken::SquigglyOpen));
        assert_eq!(lex.next(), Some(MainToken::Ident));
        assert_eq!(lex.next(), Some(MainToken::Semicolon));
        assert_eq!(lex.next(), Some(MainToken::WhiteSpace));
        assert_eq!(lex.next(), Some(MainToken::Ident));
        assert_eq!(lex.next(), Some(MainToken::NewLine));
        assert_eq!(lex.next(), Some(MainToken::SquigglyClose));
    }

    #[test]
    fn strings() {
        use super::*;
        let mut lex = MainToken::lexer(r"'aHY!\'oo' yay");
        assert_eq!(lex.next(), Some(MainToken::SingleQuote));
        let mut s_lexer = lex.morph::<StringToken>();
        assert_eq!(s_lexer.next(), Some(StringToken::AllTheThings));
        assert_eq!(s_lexer.slice(), "aHY!");
        assert_eq!(s_lexer.next(), Some(StringToken::EscapedQuote));
        assert_eq!(s_lexer.next(), Some(StringToken::AllTheThings));
        assert_eq!(s_lexer.slice(), "oo");
        assert_eq!(s_lexer.next(), Some(StringToken::SingleQuote));
        lex = s_lexer.morph();
        assert_eq!(lex.next(), Some(MainToken::WhiteSpace));
        assert_eq!(lex.next(), Some(MainToken::Ident));
        assert_eq!(lex.slice(), "yay");
    }

    #[test]
    fn sl_comment() {
        use super::*;
        let mut lex = MainToken::lexer("oops //all the // rest\n yay");
        assert_eq!(lex.next(), Some(MainToken::Ident));
        assert_eq!(lex.next(), Some(MainToken::WhiteSpace));
        assert_eq!(lex.next(), Some(MainToken::SingleLineComment));
        let mut sl_lexer = lex.morph::<SingleLineCommentToken>();
        assert_eq!(sl_lexer.next(), Some(SingleLineCommentToken::NewLine));
        lex = sl_lexer.morph();
        assert_eq!(lex.next(), Some(MainToken::WhiteSpace));
        assert_eq!(lex.next(), Some(MainToken::Ident));
        assert_eq!(lex.slice(), "yay");
    }

    #[test]
    fn ml_comment() {
        use super::*;

        let mut lex = MainToken::lexer("oops /*all the /*rest*/ */ yay");
        assert_eq!(lex.next(), Some(MainToken::Ident));
        assert_eq!(lex.next(), Some(MainToken::WhiteSpace));
        assert_eq!(lex.next(), Some(MainToken::MultiLineCommentOpen));
        let mut ml_lexer = lex.morph::<MultiLineCommentToken>();
        assert_eq!(ml_lexer.next(), Some(MultiLineCommentToken::MultiLineCommentOpen));
        assert_eq!(ml_lexer.next(), Some(MultiLineCommentToken::MultiLineCommentClose));
        assert_eq!(ml_lexer.next(), Some(MultiLineCommentToken::MultiLineCommentClose));
        lex = ml_lexer.morph();
        assert_eq!(lex.next(), Some(MainToken::WhiteSpace));
        assert_eq!(lex.next(), Some(MainToken::Ident));
        assert_eq!(lex.slice(), "yay");
    }

    #[test]
    fn lexer_simple() {
        use super::*;
        let mut lex = BrightLexer::new("fn fn fn");

        assert_eq!(lex.next(), TokenData{token:Token::Fn, loc: SourceLocation::new_line_col(1, 1, 1, 3), text: None});
        assert_eq!(lex.next(), TokenData{token:Token::Fn, loc: SourceLocation::new_line_col(1, 4, 1, 6), text: None});
        assert_eq!(lex.next(), TokenData{token:Token::Fn, loc: SourceLocation::new_line_col(1, 7, 1, 9), text: None});
        assert_eq!(lex.next(), TokenData{token:Token::EOF, loc: SourceLocation::new_line_col(1, 9, 1, 9), text: None});
    }

    #[test]
    fn lexer_sl_comment() {
        use super::*;
        let mut lex = BrightLexer::new("fn //can't see me\n fn");

        assert_eq!(lex.next(), TokenData{token:Token::Fn, loc: SourceLocation::new_line_col(1, 1, 1, 3), text: None});
        assert_eq!(lex.next(), TokenData{token:Token::Fn, loc: SourceLocation::new_line_col(2, 2, 2, 4), text: None});
        assert_eq!(lex.next(), TokenData{token:Token::EOF, loc: SourceLocation::new_line_col(2, 4, 2, 4), text: None});
    }

    #[test]
    fn lexer_ml_comment() {
        use super::*;
        let mut lex = BrightLexer::new("fn /*can't see me\n or me */ fn");

        assert_eq!(lex.next(), TokenData{token:Token::Fn, loc: SourceLocation::new_line_col(1, 1, 1, 3), text: None});
        assert_eq!(lex.next(), TokenData{token:Token::Fn, loc: SourceLocation::new_line_col(2, 11, 2, 13), text: None});
        assert_eq!(lex.next(), TokenData{token:Token::EOF, loc: SourceLocation::new_line_col(2, 13, 2, 13), text: None});
    }

    #[test]
    fn lexer_string() {
        use super::*;
        let mut lex = BrightLexer::new("fn 'now we come to \n \\'lunch\\'' fn");

        assert_eq!(lex.next(), TokenData{token:Token::Fn, loc: SourceLocation::new_line_col(1, 1, 1, 3), text: None});
        assert_eq!(lex.next(), TokenData{token:Token::StringLiteral, loc: SourceLocation::new_line_col(1, 4, 2, 12), text: Some(String::from("now we come to \n 'lunch'"))});
        assert_eq!(lex.next(), TokenData{token:Token::Fn, loc: SourceLocation::new_line_col(2, 13, 2, 15), text: None});
        assert_eq!(lex.next(), TokenData{token:Token::EOF, loc: SourceLocation::new_line_col(2, 15, 2, 15), text: None});
    }
}
