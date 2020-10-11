use bright_lang_ast::expr::TypedExpr;
use crate::cast::cast_typed_expr;
use crate::cast::{upcast_from_literal, CastType};
use bright_lang_ast::GlobalVariableDecl;
use crate::QualifiedType;
use bright_lang_ast::expr::{NodeIdx,Expr};
use bright_lang_ast::core::VariableMutability;
use crate::ParserContext;
use crate::ParserFuncContext;
use bright_lang_ast::expr::{Arena, ReturnExpr};
use crate::BrightParser;
use bright_lang_types::{Type, Mutability};
use bright_lang_lexer::{Token, TokenData, BrightLexer};
use bright_lang_errs::Error;

impl<'a> BrightParser<'a> {
    /// From wikipedia
    pub (crate) fn parse_expr(&mut self,
        arena: &mut Arena,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> NodeIdx {
        /*
        let expr = self.parse_primary(&None, parser_func_context, parser_context);
        self.parse_expr_1(&expr, 0, parser_func_context, parser_context)
        */
        NodeIdx::NULL
    }

    pub (crate) fn parse_block(&mut self,
        push_scope: bool,
        arena: &mut Arena,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> NodeIdx {
        let next = self.peek_next_item();
        let mut loc = next.loc;
  
        let token = next.token;
        
        let p = if token == Token::SquigglyOpen {
            let block_ptr = arena.push_block(loc);

            self.skip_next_item();
                
            let mut next = self.peek_next_item();
            let mut token = next.token;
            if push_scope {
                parser_context.push_block_scope();
            }

            while token != Token::SquigglyClose {
                let np = self.parse_expr(arena, parser_func_context, parser_context);
                arena.push_np_to_block(block_ptr, np);
                
                next = self.peek_next_item();
                token = next.token;
            }
            loc.extend_end_position(next.loc.end);
            self.skip_next_item();
            arena.close_block(loc, block_ptr);
            if push_scope {
                parser_context.pop_block_scope();
            }
            block_ptr
        } else {
            self.parse_expr(arena, parser_func_context, parser_context)
        };
        arena.set_return_expr(p, if push_scope { ReturnExpr::Block } else { ReturnExpr::Func });
        p
    }

    pub (crate) fn parse_variable_decl(&mut self, 
        global: bool,
        export: bool,
        arena: &mut Arena,
        parser_func_context: &mut ParserFuncContext,
        parser_context: &mut ParserContext,
    ) -> NodeIdx {
        let mut mutability = VariableMutability::Constant;
        let mut loc = self.peek_next_location();
        self.skip_next_item();

        let next = self.peek_next_item();
        if next.token == Token::Var {
            mutability = VariableMutability::Variable;
            self.skip_next_item();
        }

        let (id, _) = self.expect_ident(parser_context);
        self.skip_next_item();
        
        //check to see if this shadows a pre-existing global
        if global && parser_context.get_global_decl(&id).is_some() {
            parser_context.push_err(Error::DuplicateGlobalVariable(loc, id.to_string()));
        }

        let next = self.peek_next_item();
        let token = next.token;
        let mut var_type = match token {
            Token::Colon => {
                self.skip_next_item();
                self.parse_qualified_type(parser_context)
            },
            Token::Equal => {
                QualifiedType::new(&Type::Undeclared, Mutability::Unknown)
            },
            _ => {
                parser_context.push_err(Error::UnexpectedToken(next.loc, "variable type or value".to_owned(), next.to_string()));
                QualifiedType::new(&Type::Undeclared, Mutability::Unknown)
            }
        };

        self.expect_token(Token::Equal, parser_context);
        let mut init = self.parse_expr(arena, parser_func_context, parser_context);
        
        loc.end = arena.a[init.p].loc.end;

        if var_type.r#type == Type::Undeclared {
            if mutability == VariableMutability::Constant {
                var_type = arena.a[init.p].r#type.clone();
            } else {
                var_type = QualifiedType::new(&upcast_from_literal(&arena.a[init.p].r#type.r#type), arena.a[init.p].r#type.mutability);
            }
        }

        init = cast_typed_expr(&var_type, init, CastType::Implicit, arena, parser_context);

        self.expect_semicolon(parser_context);
        let name = id;
        if !global {
            let internal_name = parser_context.get_unique_name(&name);
            parser_context.add_var(&name, &internal_name, &var_type, mutability);
            parser_func_context.add_var(&internal_name, &var_type, false, false, mutability);

            if var_type.r#type.is_type_variable() {
                arena.push(TypedExpr::new(&Expr::UnresolvedLocalVariableInit{internal_name, var_type, init}, &QualifiedType::new_const(&Type::Void), loc))
            } else {
                arena.push(TypedExpr::new(&Expr::LocalVariableInit{internal_name, init}, &QualifiedType::new_const(&Type::Void), loc))
            } 
        } else {
            let decl = GlobalVariableDecl{name: name.clone(), r#type: var_type, export, mutability};
            parser_context.global_decls.push(decl);
            arena.push(TypedExpr::new(&Expr::GlobalVariableInit{name, init}, &QualifiedType::new_const(&Type::Void), loc))
        }
    }
}