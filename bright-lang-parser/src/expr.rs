use bright_lang_ast::expr::NodeIdx;
use crate::ParserContext;
use crate::ParserFuncContext;
use bright_lang_ast::expr::{Arena, ReturnExpr};
use crate::BrightParser;
use bright_lang_types::{Type, Mutability};
use bright_lang_lexer::{Token, TokenData, BrightLexer};

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
}