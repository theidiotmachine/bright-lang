use bright_lang_ast::func::GenericFunc;
use bright_lang_ast::GlobalVariableImport;
use bright_lang_ast::Exports;
use crate::context::ParserFuncContext;
use bright_lang_ast::expr::NodePtr;
use bright_lang_ast::expr::TypedExpr;
use std::collections::HashSet;
use std::collections::HashMap;
use std::cmp;

use bright_lang_ast::expr::{Arena, Expr};
use bright_lang_errs::source_location::Position;
use bright_lang_errs::source_location::SourceLocation;
use bright_lang_types::QualifiedType;
use bright_lang_types::{Type, Mutability};
use bright_lang_ast::func::FuncDefn;
use bright_lang_ast::func::Func;
use bright_lang_errs::Error;
use bright_lang_ast::Imports;
use bright_lang_ast::AST;
use bright_lang_lexer::{Token, TokenData, BrightLexer};

mod context;
mod top_level;
use crate::context::ParserContext;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum UnsafeParseMode{
    Safe,
    Unsafe,
    ExportsPhase,
}

pub trait Importer{
    fn import(&mut self, import_path_name: &String, from_path_name: &String) -> Result<Imports, String>;
}

#[derive(Debug, Clone, PartialEq)]
pub enum StartFuncType{
    ///The internal module start func
    Start,
    ///A full blown external start func
    WASMCallCtors
}

enum ImportFilter{
    All,
    Named(Vec<String>)
}

fn filter_imports(exports: Exports, imports: &ImportFilter) -> Exports {
    match imports {
        ImportFilter::All => (exports),
        ImportFilter::Named(_) => {panic!()}
    }
}

pub struct BrightParser<'a>{
    lexer: BrightLexer<'a>,
    look_ahead: TokenData,
}

impl<'a> BrightParser<'a>{
    pub fn new(
        mut lexer: BrightLexer<'a>
    ) -> BrightParser<'a> {
        let look_ahead = lexer.next();
        BrightParser{
            lexer, look_ahead
        }
    }

    pub fn new_from_text(text: &'a str) -> BrightParser<'a> {
        let lexer = BrightLexer::new(text);
        BrightParser::new(lexer)
    }

    /// Peek at the next token, without changing state.
    fn peek_next_token(& self) -> Token {
        self.look_ahead.token
    }

    /// Peek at the next token, without changing state.
    fn peek_next_item(& self) -> TokenData {
        self.look_ahead.clone()
    }

    fn skip_next_item(&mut self)  {
        self.look_ahead = self.lexer.next()
    }

    fn expect_token(&mut self, token: Token, parser_context: &mut ParserContext){
        let next = self.peek_next_item();
        if next.token == token {
            self.skip_next_item()
        } else {
            parser_context.push_err(Error::UnexpectedToken(next.loc, format!("Expected '{}', found '{}'", token.to_string(), next.to_string())));
        }
    }

    fn expect_string_literal(&mut self, message: &str, parser_context: &mut ParserContext) -> String {
        let next = self.peek_next_item();
        let token = next.token; 
        match token {
            Token::StringLiteral => next.text.unwrap(),   
            _ => {
                parser_context.push_err(Error::UnexpectedToken(next.loc, message.to_string()));
                String::from("")
            }
        }
    }

    pub fn parse_full(&mut self, 
        unsafe_parse_mode: UnsafeParseMode,
        importer: &mut dyn Importer,
        module_name: &str,
        start_func_type: StartFuncType,
        file_name: &str,
    ) -> (AST, Vec<Error>) {
        let mut parser_context = ParserContext::new(unsafe_parse_mode, file_name);
        
        let start_func_name = match start_func_type{
            StartFuncType::WASMCallCtors => String::from("__wasm_call_ctors"),
            StartFuncType::Start => format!("_start_{}", module_name)
        };

        self.parse_internal(&start_func_name, start_func_type, &mut parser_context, importer);
        
        (AST{start: start_func_name, 
            global_decls: parser_context.global_decls, 
            global_imports: parser_context.global_imports,
            func_decls: parser_context.func_decls, 
            func_imports: parser_context.func_imports, 
            generic_func_decls: parser_context.generic_func_decls,
            type_map: parser_context.type_map, 
            trait_map: parser_context.trait_map, 
            trait_impl_map: parser_context.trait_impl_map,
            exported_traits: parser_context.exported_traits
        }, parser_context.errors)
    }

    /// Consume a parser context, populate it.
    fn parse_internal(
        &mut self, 
        start_func_name: &str,
        start_func_type: StartFuncType,
        parser_context: &mut ParserContext,
        importer: &mut dyn Importer,
    ) {
        let mut init_body = Arena::new_block(SourceLocation::new(Position::new(0, 0), Position::new(0, 0)));
        
        let mut fake_parser_func_context = ParserFuncContext::new(&None);

        loop {
            if self.peek_next_token() == Token::EOF {
                break;
            } else {
                self.parse_global_statement(&mut init_body, &mut fake_parser_func_context, parser_context, importer);
            }
        }

        let start_function = Func{ 
            decl: FuncDefn{
                name: start_func_name.to_owned(), 
                return_type: QualifiedType::new_const(&Type::Void), 
                args: vec![], 
                export: start_func_type == StartFuncType::WASMCallCtors, 
                generic_impl: false, 
                //type_guard: None, 
                member_func: false,
            },
            arena: init_body,
            body: Some(NodePtr::new(0)),
            local_vars: vec![], 
            closure: vec![], 
            local_var_map: HashMap::new()
        };

        parser_context.func_decls.push(start_function);

        if !fake_parser_func_context.closure.is_empty() {
            parser_context.errors.push(Error::ClosureNotAllowed(SourceLocation::new(Position::new(0, 0), Position::new(0, 0))));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    struct DummyImporter{
    }

    impl Importer for DummyImporter{
        fn import(&mut self, _: &String, _: &String) -> Result<Imports, String> {
            Ok(Imports{exports: Exports::new(), module_name: String::from(""), unique_name: String::from("")})
        }
    }

    #[test]
    fn import_test() {
        let mut parser = BrightParser::new_from_text("import {*} from './dummy'");
        let (_, errs) = parser.parse_full(UnsafeParseMode::Safe, & mut DummyImporter{}, "test", StartFuncType::Start, "test.brt");
        assert_eq!(errs.len(), 0);
    }
}
