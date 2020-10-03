use bright_lang_ast::func::Func;
use bright_lang_ast::expr::Arena;
use crate::ParserFuncContext;
use bright_lang_ast::func::GenericFunc;
use bright_lang_ast::func::FuncDefn;
use bright_lang_ast::GlobalVariableImport;
use crate::filter_imports;
use bright_lang_errs::Error;
use crate::ImportFilter;
use crate::Importer;
use crate::ParserContext;
use bright_lang_lexer::{Token, TokenData, BrightLexer};
use crate::BrightParser;

impl<'a> BrightParser<'a> {
    fn parse_import_decl(&mut self,
        parser_context: &mut ParserContext,
        importer: &mut dyn Importer,
    )  {
        self.skip_next_item();
        self.expect_token(Token::SquigglyOpen, parser_context);
        let next = self.peek_next_item();
        self.skip_next_item();
        let import_filter: ImportFilter = match &next.token {
            Token::Multiply => {
                ImportFilter::All
            },
            Token::Ident => {
                parser_context.push_err(Error::NotYetImplemented(next.loc, String::from("Individual imports")));
                ImportFilter::Named(vec![])
            },
            _ => {
                parser_context.push_err(Error::UnexpectedToken(
                    next.loc,
                    format!("Expected '*' or ident; found {:?}", next),
                ));
                ImportFilter::Named(vec![])
            }
        };
        self.expect_token(Token::SquigglyClose, parser_context);
        
        //peek for 'as'?

        self.expect_token(Token::From, parser_context);
        let id_string = self.expect_string_literal("Expecting path name to import", parser_context);
        self.skip_next_item();
        if id_string.starts_with('.') {
            let r_imports = importer.import(&id_string, &(parser_context.file_name));
            let imports = match r_imports {
                Ok(imports) => imports,
                Err(e) => {
                    parser_context.push_err(Error::ImportFailed(next.loc, e));
                    return;
                }
            };
            
            let exports = imports.exports;
            let exports = filter_imports(exports, &import_filter);
            let namespace = imports.module_name;
            for g in &exports.global_decls {
                let import_name = format!("{}.{}", namespace, g.name);
                let decl = GlobalVariableImport{name: import_name.clone(), r#type: g.r#type.clone(), export: false};
                parser_context.global_imports.push(decl.clone());
            }
            
            for g in &exports.global_imports {
                let decl = GlobalVariableImport{name: g.name.clone(), r#type: g.r#type.clone(), export: false};
                parser_context.global_imports.push(decl.clone());
            }

            for f in &exports.func_decls {
                let import_name = format!("{}.{}", namespace, f.name);
                parser_context.func_imports.push(
                    FuncDefn{name: import_name.clone(), return_type: f.return_type.clone(), args: f.args.clone(), export: false, 
                        generic_impl: false, 
                        //type_guard: f.type_guard.clone(), 
                        member_func: f.member_func},
                ); 
            }

            for f in &exports.generic_func_decls {
                let import_name = format!("{}.{}", namespace, f.func.decl.name);
                parser_context.generic_func_decls.push(
                    GenericFunc{type_args: f.type_args.clone(), num_this_type_args: f.num_this_type_args, func: Func{
                        decl: FuncDefn{name: import_name.clone(), return_type: f.func.decl.return_type.clone(), 
                            args: f.func.decl.args.clone(), export: false, generic_impl: false, 
                            //type_guard: f.func.decl.type_guard.clone(),
                            member_func: f.func.decl.member_func},
                        arena: f.func.arena.clone(),
                        body: f.func.body,
                        local_vars: f.func.local_vars.clone(),
                        closure: f.func.closure.clone(),
                        local_var_map: f.func.local_var_map.clone(),
                    }}
                ); 
            }

            for f in &exports.func_imports {
                parser_context.func_imports.push(
                    FuncDefn{name: f.name.clone(), return_type: f.return_type.clone(), args: f.args.clone(), export: false, 
                        generic_impl: false, 
                        //type_guard: f.type_guard.clone(), 
                        member_func: f.member_func},
                ); 
            }

            parser_context.import_namespace_map.insert(namespace, imports.unique_name);
        } else {
            //it's an import from an external file
            parser_context.push_err(Error::NotYetImplemented(next.loc, String::from("Import external projects")));
        }
    }

    /// The root of the module is parsed and will run in its 'start' function.
    /// This parses that. The init_body param is that function body. 
    /// Because we put restrictions on this function - it can't have locals or a closure - 
    /// we fake that and will blow up if it violates that. 
    pub (crate) fn parse_global_statement(&mut self, 
        init_body: &mut Arena,
        fake_parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
        importer: &mut dyn Importer,
    ) {
        let next = self.peek_next_item();
        let token = &next.token;
        
        match token {
            Token::Import => {self.parse_import_decl(parser_context, importer);}
            /*
            Token::Keyword(ref k) => match k {
                Keyword::Export => self.parse_export_decl_main_phase(init_body, fake_parser_func_context, parser_context),
                Keyword::Fn => {
                    let o_func = self.parse_function_decl_main_phase(false, fake_parser_func_context, parser_context);
                    if let Some(f) = o_func { init_body.push(f) };
                },
                Keyword::Let => {
                    let var_decl = self.parse_variable_decl(true, false, fake_parser_func_context, parser_context);
                    init_body.push(var_decl);
                },
                Keyword::UnsafeStruct => self.parse_struct_decl(false, parser_context),
                Keyword::Alias => self.parse_alias(false, parser_context),
                Keyword::Type => self.parse_type_decl(false, ParserPhase::MainPhase, parser_context),
                Keyword::Trait => self.parse_trait_decl(false, parser_context),
                Keyword::Implement => self.parse_trait_impl(false, ParserPhase::MainPhase, parser_context),
                _ => { parser_context.errors.push(self.unexpected_token_error_raw(next.span, &next.location, "expecting valid statement")); self.skip_next_item(); }
            },
            */
            _ => {
                /*
                // if we don't know what this statement is, parse it as an expr
                let expr = self.parse_expr(fake_parser_func_context, parser_context);
                init_body.push(expr);

                // ugh this is like a hundred million lines to check the semi-colon
                let next = self.next_item();
                match next {
                    Ok(next) => {
                        if self.has_line_term {
                        } else if next.token.matches_punct(Punct::SemiColon) || next.token.is_eof() {
                            self.skip_next_item();
                        } else {
                            parser_context.errors.push(self.expected_token_error_raw(&next, &[&";"]));
                        }        
                    },
                    Err(e) => {
                        parser_context.errors.push(e)
                    }
                }
                */
            }
        }
    }
}