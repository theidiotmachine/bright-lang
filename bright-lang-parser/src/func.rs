use bright_lang_ast::expr::FuncObjectCreation;
use bright_lang_ast::expr::Expr;
use bright_lang_ast::expr::TypedExpr;
use bright_lang_ast::expr::NodeIdx;
use crate::cast::{cast_typed_expr, CastType};
use bright_lang_errs::source_location::SourceLocation;
use bright_lang_ast::func::FuncArg;
use bright_lang_ast::expr::Arena;
use bright_lang_lexer::{Token};
use bright_lang_ast::func::GenericFunc;
use std::collections::HashMap;
use bright_lang_ast::func::Func;
use bright_lang_ast::func::FuncDefn;
use bright_lang_errs::Error;
use bright_lang_types::FuncType;
use bright_lang_types::{TypeArg, Mutability};
use bright_lang_types::{QualifiedType, Type};
use bright_lang_ast::core::VariableMutability;
use crate::ParserContext;
use crate::ParserFuncContext;
use crate::BrightParser;
use crate::ParserPhase;

#[derive(Debug, PartialEq, Clone, Copy)]
pub (crate) enum FuncDeclContext {
    MemberFn,
    SharedMemberFn,
    Lambda,
    Global,
}

impl<'a> BrightParser<'a> {
    fn parse_function_decl_arg(&mut self,
        parser_context: &mut ParserContext,
    ) -> Option<FuncArg> {
        let next = self.peek_next_item();
        let token = next.token;
        let mut mutability = VariableMutability::Constant;
        if token == Token::Var {
            mutability = VariableMutability::Variable;
            self.skip_next_item();
        }

        let (name, _) = self.expect_ident(parser_context);
        let next = self.peek_next_item();
        let token = &next.token;
        match token {
            Token::Colon => {
                self.skip_next_item();
                let arg_type = self.parse_qualified_type(parser_context);
                
                let next = self.peek_next_item();
                let token = next.token;
                if token == Token::Comma || token == Token::NewLine {
                    self.skip_next_item();
                }
                Some(FuncArg{name, r#type: arg_type, mutability})
            },
            Token::Comma | Token::NewLine => {
                self.skip_next_item();
                Some(FuncArg{name, r#type: QualifiedType::new(&Type::Undeclared, Mutability::Unknown), mutability})
            },
            _ => {
                parser_context.push_err(Error::UnexpectedToken(next.loc, String::from("')' or ','"), next.to_string()));
                self.skip_next_item();
                None
            }
        }
    }

    /// register params in the local scope
    fn register_params(&mut self, 
        this_type: &Option<QualifiedType>,
        arg_list: &[FuncArg], 
        parser_func_context: &mut ParserFuncContext, 
        parser_context: &mut ParserContext,
    ) {
        if this_type.is_some() {
            let this_type = this_type.clone().unwrap();
            parser_context.add_var(&String::from("this"), &String::from("this"), &this_type, VariableMutability::Constant);
            parser_func_context.add_var(&String::from("this"), &this_type, false, true, VariableMutability::Constant);
        }

        for arg in arg_list {
            //vile hack
            if arg.name == "this" {
                continue;
            }
            let internal_name = parser_context.get_unique_name(&arg.name);
            parser_context.add_var(&arg.name, &internal_name, &arg.r#type, arg.mutability);
            //todo: constant params
            //todo: default params
            parser_func_context.add_var(&internal_name, &arg.r#type, false, true, arg.mutability);
        }
    }

    /// If you don't have a return value at the end of function, you get errors at run time. Also, it seems
    /// polite to make sure the return type is in fact correct.
    fn check_void_return_value(&mut self, 
        loc: SourceLocation,
        body_return_type: &QualifiedType,
        parser_func_context_inner: &ParserFuncContext,
        parser_context: &mut ParserContext,
    ) {
        match parser_func_context_inner.given_func_return_type.r#type {
            Type::Void => {},
            Type::Undeclared => {
                if body_return_type.r#type == Type::Void {
                    parser_context.push_err(Error::NoValueReturned(loc))
                }
            },
            _ => if body_return_type.r#type == Type::Void {
                parser_context.push_err(Error::NoValueReturned(loc))
            }
        }
    }

    ///Parse a func body to produce a full Func
    fn parse_func_body_internal(&mut self,
        func_defn: &mut FuncDefn,
        this_type: &Option<QualifiedType>,
        parser_context: &mut ParserContext,
    ) -> Func {
        let mut parser_func_context_inner = ParserFuncContext::new(this_type);
        
        parser_context.push_func_scope();

        self.register_params(this_type, &func_defn.args, &mut parser_func_context_inner, parser_context);

        parser_func_context_inner.given_func_return_type = func_defn.return_type.clone();
        
        let mut defn = func_defn.clone();
        let mut arena = Arena::new();
        let block_ni = self.parse_block(false, &mut arena, &mut parser_func_context_inner, parser_context);
        
        //if we never figured out a return type, then use this. It means we never encountered a return statement.
        if parser_func_context_inner.given_func_return_type.r#type == Type::Undeclared && parser_func_context_inner.implied_func_return_type.r#type == Type::Undeclared {
            //get a guess of the return type of the body based on the last elem
            let body_return_type = arena.a[block_ni.p].r#type.clone();
            parser_func_context_inner.implied_func_return_type = body_return_type;
        }
        //do some special checks on this
        self.check_void_return_value(arena.a[block_ni.p].loc, &parser_func_context_inner.implied_func_return_type, &parser_func_context_inner, parser_context);
        //fix up the decl to our implied value
        if defn.return_type.r#type == Type::Undeclared {
            defn.return_type = parser_func_context_inner.implied_func_return_type.clone();
            func_defn.return_type = parser_func_context_inner.implied_func_return_type.clone();
        }
        //and cast. we special case void because we drop unused values.
        let o_block = if defn.return_type.r#type == Type::Void {
            Some(block_ni)
        } else {
            Some(cast_typed_expr(&defn.return_type, block_ni, CastType::Implicit, &mut arena, parser_context))
        };

        parser_context.pop_func_scope();

        Func{
            defn,
            local_vars: parser_func_context_inner.local_vars, closure: parser_func_context_inner.closure, 
            local_var_map: parser_func_context_inner.local_var_map, body: o_block, arena,
        }
    }

    fn parse_function_decl_args(&mut self,
        this_type: &Option<QualifiedType>,
        parser_context: &mut ParserContext,
    ) -> Vec<FuncArg> {
        self.expect_token(Token::RoundOpen, parser_context);
        let mut args: Vec<FuncArg> = if this_type.is_some() { vec![FuncArg{name: String::from("this"), r#type: this_type.clone().unwrap(), mutability: VariableMutability::Constant}]} else { Vec::new() };
        loop {
            let next = self.peek_next_item();
            let token = &next.token;
            match token {
                Token::RoundClose => { self.skip_next_item(); return args; },
                Token::Ident | Token::Var => {
                    let arg = self.parse_function_decl_arg(parser_context);
                    if arg.is_some() {
                        args.push(arg.unwrap());
                    }
                },
                _ => {
                    parser_context.push_err(Error::UnexpectedToken(next.loc, String::from("')'"), next.to_string()));
                    self.skip_next_item();
                }
            }
        }
    }

    ///The core of the function parsing code.
    fn parse_func_decl_internal(&mut self,
        export: bool,
        phase: ParserPhase,
        prefix: &str,
        this_type: &Option<Type>,
        this_type_args: &Vec<TypeArg>,
        func_decl_context: FuncDeclContext,
        arena: &mut Arena,
        parser_func_context_outer: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> (Option<NodeIdx>, String, String, Vec<TypeArg>, FuncType) {
        if func_decl_context == FuncDeclContext::SharedMemberFn {
            self.expect_token(Token::Shared, parser_context);
        } 
        
        self.expect_token(Token::Fn, parser_context);
        
        let this_qualified_type = this_type.as_ref().map(|t| if self.peek_next_token() == Token::Mut {
            self.skip_next_item();
            QualifiedType::new_mut(&t)
        } else {
            QualifiedType::new_const(&t)
        } );
        
        let loc = self.peek_next_location();

        //First, parse the name.
        let (mangled_name, name) = {
            let next = self.peek_next_item();
            let token = next.token; 
            match token {
                Token::Ident => {
                    let i = next.text.unwrap();
                    self.skip_next_item();
                    let mut s = prefix.to_owned();
                    s.push_str(i.as_str());
                    (s, String::from(i.as_str()))
                }, 
                _ => {
                    let unique = parser_context.get_unique_name(&String::from("fn"));
                    (unique.clone(), unique)
                }
            }
        };
        
        //Generic? 
        let mut generic = false;
        let mut type_scope_pushed = false;
        let num_this_type_args = this_type_args.len();
        let type_args = {
            let next = self.peek_next_item();
            let token = next.token;
            if token == Token::LessThan {
                let type_args = self.parse_type_decl_args(parser_context);
                let mut type_args = parser_context.push_type_scope(&type_args);
                type_scope_pushed = true;
                generic = true;
                let mut o = this_type_args.clone();
                o.append(& mut type_args);
                o
            } else {
                if num_this_type_args > 0 {
                    generic = true;
                }
                this_type_args.clone()
            }
        };

        //now get the args
        let arg_list = self.parse_function_decl_args(&this_qualified_type, parser_context);

        //and the return type
        let next = self.peek_next_item();
        let token = next.token;
        let /*(*/return_type/*, type_guard)*/ = if token == Token::ThinArrow {
            self.skip_next_item();
            let t = self.parse_qualified_type(parser_context);

            /*
            let next = self.peek_next_item();
            let token = &next.token;
            let type_guard = if token.matches_keyword(Keyword::UnsafeTypeGuard) {
                self.parse_type_guard(&t, parser_context)
            } else {
                None
            };*/

            /*(*/t/*, type_guard)*/
        } else if token == Token::FatArrow {
            if export {
                parser_context.push_err(Error::ExportedFunctionFatArrow(next.loc));
            }
            self.skip_next_item();
            //(
                QualifiedType::new(&Type::Undeclared, Mutability::Unknown)
            //, None)
        } else {
            parser_context.push_err(Error::UnexpectedToken(next.loc, String::from("'->' or '=>'"), next.to_string()));
            self.skip_next_item();
            //(
                QualifiedType::new(&Type::Undeclared, Mutability::Unknown)
            //, None)
        };

        //here's the decl
        let mut func_defn = FuncDefn{
            name: mangled_name.to_string(), return_type, args: arg_list, export, generic_impl: generic, 
            //type_guard: type_guard, 
            member_func: this_type.is_some(),
        };
        
        //now parse the body if we need to
        let func = if generic || phase == ParserPhase::MainPhase{
            self.parse_func_body_internal(&mut func_defn, &this_qualified_type, parser_context)
        } else {
            Func{
                defn: func_defn,
                local_vars: vec![], closure: vec![], 
                local_var_map: HashMap::new(), body: None, arena: Arena::new()
            }
        };

        //deal with the results
        if generic {
            if type_scope_pushed {
                parser_context.pop_type_scope();
            }
            if !func.closure.is_empty() {
                parser_context.push_err(Error::NotYetImplemented(loc, String::from("Lambdas in generics")))
            }
            let func_type = func.defn.get_func_type();
            parser_context.generic_func_decls.push(GenericFunc{func, type_args: type_args.clone(), num_this_type_args});
            (None, mangled_name, name, type_args, func_type)
        } else {
            let func_closure = func.closure.clone();
            // now we have a closure of an inner function, we need to capture it. So look at every element
            for cr in &func_closure {
                // is it in our local vars?
                let o_local_var = parser_func_context_outer.local_vars.iter_mut().find(|lv| lv.internal_name == cr.internal_name);
                if o_local_var.is_some() {
                    // yes it is, so mark that local variable as a closure source
                    o_local_var.unwrap().closure_source = true;
                } else {
                    //not a local variable, so it's in the closure of this function, so make sure it's captured
                    parser_func_context_outer.closure.push(cr.clone())
                }
            }
            let func_type = func.defn.get_func_type();
            parser_context.func_decls.push(func); 
            (
                Some(arena.push(TypedExpr::new(
                    &Expr::FuncDecl(FuncObjectCreation{name: mangled_name.clone(), closure: func_closure}), 
                    &QualifiedType::new_const(&Type::Func{func_type: Box::new(func_type.clone())}), 
                    loc
                ))),
                mangled_name, name, vec![], func_type
            )
        }
    }

    pub(crate) fn parse_function_decl_main_phase(&mut self, 
        export: bool,
        arena: &mut Arena,
        parser_func_context_outer: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> Option<NodeIdx> {
        self.parse_func_decl_internal(export, ParserPhase::MainPhase, &String::from(""), &None, &vec![], FuncDeclContext::Global, arena, parser_func_context_outer, parser_context).0
    }
}