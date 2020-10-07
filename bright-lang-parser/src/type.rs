use bright_lang_types::{QualifiedType, Type};
use crate::op::{BinaryOperatorData, Association};
use bright_lang_errs::Error;
use bright_lang_errs::source_location::SourceLocation;
use bright_lang_types::{TypeArg, TypeConstraint, Mutability, S_32_MIN, S_32_MAX, PassStyle};
use bright_lang_lexer::{Token};
use bright_lang_ast::TypeDecl;
use crate::ParserContext;
use crate::BrightParser;
use crate::Commitment;

pub (crate) fn matches_type_constraint(
    arg_type: &Type,
    constraint: &TypeConstraint,
    parser_context: &ParserContext,
) -> bool {
    match constraint {
        TypeConstraint::None => true,
        TypeConstraint::Intersection(inner) => {
            inner.iter().all(|x| matches_type_constraint(arg_type, x, parser_context))
        },
        TypeConstraint::Union(inner) => {
            inner.iter().any(|x| matches_type_constraint(arg_type, x, parser_context))
        },
        TypeConstraint::Trait(t) => {
            parser_context.type_implements_trait(arg_type, t)
        }
    }
}

fn get_type_decl_binary_operator_data(token: Token) -> Option<BinaryOperatorData> {
    match token {
        Token::Union => Some(BinaryOperatorData{precedence: 7, association: Association::Left, is_member: false}),
        Token::Intersection => Some(BinaryOperatorData{precedence: 9, association: Association::Left, is_member: false}),
        _ => None
    }
}

impl<'a> BrightParser<'a> {
    fn parse_type_decl_constraint_primary(&mut self,
        parser_context: &mut ParserContext
    ) -> TypeConstraint {
        let err_out = TypeConstraint::None;
        let next = self.next_item();
        let loc = next.loc;
        let token = next.token;
        match token {
            Token::Ident => {
                let id = next.text.unwrap();
                let o_trait_decl = parser_context.trait_map.get(&id);
                match o_trait_decl {
                    Some(_) => TypeConstraint::Trait(id.to_string()),
                    None => {
                        parser_context.push_err(Error::UnrecognizedTypeArgConstraint(loc, id.clone()));
                        TypeConstraint::None
                    },
                }
            },
            _ => {
                parser_context.push_err(Error::UnrecognizedTypeArgConstraint(loc, next.to_string()));
                err_out
            }
        }
    }

    fn parse_type_decl_binary_op(&mut self, 
        op: Token,
        lhs: &TypeConstraint, 
        rhs: &TypeConstraint, 
        loc: &SourceLocation,
        parser_context: &mut ParserContext
    ) -> TypeConstraint {
        match op{
            //intersection - has all of the members
            Token::Intersection => {
                TypeConstraint::Intersection(vec![lhs.clone(), rhs.clone()])
            },
            //union - has the intersection of members
            Token::Union => {
                TypeConstraint::Union(vec![lhs.clone(), rhs.clone()])
            },
            _ => {
                parser_context.push_err(Error::UnexpectedToken(*loc, "expected | or &".to_string()));
                TypeConstraint::None
            }
        }
    }

    fn parse_type_decl_constraint_1(&mut self, 
        init_lhs: &TypeConstraint, 
        min_precedence: i32,
        parser_context: &mut ParserContext
    ) -> TypeConstraint {
        let mut lhs = init_lhs.clone();
        let lookahead_item = self.peek_next_item();
        let mut lookahead = lookahead_item.token;
        let lookahead_loc = lookahead_item.loc;
        
        // while lookahead is a binary operator whose precedence is >= min_precedence
        loop {
            let o_lookahead_data = get_type_decl_binary_operator_data(lookahead);
            match o_lookahead_data {
                None => break,
                Some(lookahead_data) => {
                    if lookahead_data.precedence >= min_precedence {
                        // op := lookahead
                        let op = lookahead;
                        let op_precedence = lookahead_data.precedence;
                        // advance to next token
                        self.skip_next_item();
                        // rhs := parse_primary ()
                        let r_rhs = self.parse_type_decl_constraint_primary(parser_context);
                        let mut rhs = r_rhs;
                        // lookahead := peek next token
                        let lookahead_item = self.peek_next_item();
                        lookahead = lookahead_item.token;
                        // while lookahead is a binary operator whose precedence is greater
                        // than op's, or a right-associative operator
                        // whose precedence is equal to op's
                        loop {
                            let o_lookahead_data = get_type_decl_binary_operator_data(lookahead);
                            match o_lookahead_data {
                                None => break,
                                Some(lookahead_data) => {
                                    if lookahead_data.precedence > op_precedence || (lookahead_data.association == Association::Right && lookahead_data.precedence == op_precedence) {
                                        //rhs := parse_expression_1 (rhs, lookahead's precedence)
                                        let new_rhs = self.parse_type_decl_constraint_1(&rhs, lookahead_data.precedence, parser_context);
                                        rhs = new_rhs;
                                        //lookahead := peek next token
                                        let lookahead_item = self.peek_next_item();
                                        lookahead = lookahead_item.token;
                                    } else {
                                        break;
                                    }
                                }
                            }                                
                        }
                        lhs = self.parse_type_decl_binary_op(op, &lhs, &rhs, &lookahead_loc, parser_context);
                    } else {
                        break;
                    }
                }
            }
        };
        lhs
    }

    pub (crate) fn parse_type_decl_constraint(&mut self, 
        parser_context: &mut ParserContext
    ) -> TypeConstraint {
        let primary = self.parse_type_decl_constraint_primary(parser_context);
        self.parse_type_decl_constraint_1(&primary, 0, parser_context)
    }

    ///Parse a set of type variables in angle brackets
    pub (crate) fn parse_type_decl_args(&mut self, 
        parser_context: &mut ParserContext
    ) -> Vec<TypeArg> {
        let mut out: Vec<TypeArg> = Vec::new();
        self.expect_token(Token::LessThan, parser_context);

        loop {
            let next = self.peek_next_item();
            let token = &next.token;
            match token {
                Token::GreaterThan => { self.skip_next_item(); return out; },
                Token::Ident => {
                    let n = next.text.unwrap();
                    self.skip_next_item();

                    let next = self.peek_next_item();
                    let token = next.token;
                    
                    if token == Token::Colon {
                        self.skip_next_item();
                        let constraint = self.parse_type_decl_constraint(parser_context);
                        out.push(TypeArg{name: n.to_string(), constraint});
                    } else {
                        out.push(TypeArg{name: n.to_string(), constraint: TypeConstraint::None});
                    }
                    if token == Token::Comma || token == Token::NewLine {
                        self.skip_next_item();
                    }
                },
                _ => {
                    self.skip_next_item();
                    parser_context.push_err(Error::UnrecognizedTypeArg(next.loc, next.to_string()));
                }
            }
        }
    }

    pub (crate) fn parse_type_args(&mut self,
        type_args: &[TypeArg],
        parser_context: &mut ParserContext,
    ) -> Vec<Type> {
        //skip '<'
        self.skip_next_item();
        let mut loc = self.peek_next_location();
        let mut idx = 0;
        let mut resolved_types = vec![];
        let num = type_args.len();

        //parse the explicit types
        while idx < num {
            let arg_type = self.parse_type(parser_context);
            if !matches_type_constraint(&arg_type, &(type_args[idx].constraint), parser_context) {
                parser_context.push_err(Error::FailedTypeArgConstraint(loc, arg_type.clone(), type_args[idx].name.clone()));
            }
            resolved_types.push(arg_type);
            let next = self.peek_next_item();
            let token = next.token;
            if token == Token::Comma {
                self.skip_next_item();
                loc = self.peek_next_location();
                idx += 1;
            } else if token == Token::GreaterThan {
                self.skip_next_item();
                if idx == num - 1 {
                    idx += 1;
                } else {
                    parser_context.push_err(Error::MissingTypeArgs(next.loc));
                    break;
                }
            }
        }

        while idx < num {
            resolved_types.push(Type::Undeclared);
            idx += 1;
        }

        resolved_types
    }

    pub(crate) fn parse_type_from_ident(&mut self, 
        ident: &str,
        commitment: Commitment,
        loc: SourceLocation,
        parser_context: &mut ParserContext,
    ) -> Option<Type> {
        let o_type = parser_context.get_type_decl(ident);
        match o_type {
            None => {
                let o_scoped_type = parser_context.get_scoped_type(ident);
                match o_scoped_type {
                    Some(scoped_type) => {
                        Some(Type::VariableUsage{name: scoped_type.name.clone(), constraint: scoped_type.constraint.clone()})
                    },
                    None => {
                        if commitment == Commitment::Committed {
                            parser_context.push_err(Error::InvalidType(loc, ident.to_owned()));
                            Some(Type::Undeclared)
                        } else {
                            None
                        }
                    }
                }
            },
            Some(user_type) => {
                match user_type {
                    TypeDecl::Alias{of, export: _} => Some(of),
                    TypeDecl::UserClass{user_class, export: _, under_construction: _,} => {
                        if !user_class.type_args.is_empty() {
                            let types = self.parse_type_args(&user_class.type_args, parser_context);
                            Some(Type::UserClass{name: ident.to_owned(), type_args: types})
                        } else {
                            Some(Type::UserClass{name: ident.to_owned(), type_args: vec![]})
                        }
                    },
                    TypeDecl::Enum{export: _} => unimplemented!()
                }
            }
        }
    }

    ///Parse a type, e.g. Int
    pub(crate) fn parse_type(&mut self, 
        parser_context: &mut ParserContext,
    ) -> Type {
        let next = self.next_item();
        let token = next.token;
        match token {
            Token::BinLiteral => {
                let number_i128 = next.parse_i128().unwrap();
                Type::Int(number_i128, number_i128)
            },
            Token::Bool => Type::Bool,
            Token::DecLiteral => {
                if next.text.clone().unwrap().contains('.') {
                    Type::FloatLiteral(next.text.unwrap().parse().unwrap())
                } else {
                    let number_i128 = next.parse_i128().unwrap();
                    Type::Int(number_i128, number_i128)
                }
            },
            Token::HexLiteral => {
                let number_i128 = next.parse_i128().unwrap();
                Type::Int(number_i128, number_i128)
            },
            Token::Ident => {
                self.parse_type_from_ident(&next.text.unwrap(), Commitment::Committed, next.loc, parser_context).unwrap()
            },
            Token::Int => {
                let next = self.peek_next_item();
                if next.token == Token::LessThan {
                    self.skip_next_item();
                    let next = self.next_item();
                    let token = next.token;
                    let lower = match token {
                        Token::BinLiteral | Token::HexLiteral | Token::OctLiteral => next.parse_i128().unwrap(),
                        Token::DecLiteral => {
                            if next.text.clone().unwrap().contains('.') {
                                parser_context.push_err(Error::UnexpectedToken(next.loc, String::from("expecting integer constant")));
                                S_32_MIN
                            } else {
                                next.parse_i128().unwrap()
                            }
                        },
                        _ => {
                            parser_context.push_err(Error::UnexpectedToken(next.loc, String::from("expecting integer constant")));
                            S_32_MIN
                        }
                    };
                    self.expect_token(Token::Comma, parser_context);
                    let next = self.next_item();
                    let token = next.token;
                    let upper = match token {
                        Token::BinLiteral | Token::HexLiteral | Token::OctLiteral => next.parse_i128().unwrap(),
                        Token::DecLiteral => {
                            if next.text.clone().unwrap().contains('.') {
                                parser_context.push_err(Error::UnexpectedToken(next.loc, String::from("expecting integer constant")));
                                S_32_MIN
                            } else {
                                next.parse_i128().unwrap()
                            }
                        },
                        _ => {
                            parser_context.push_err(Error::UnexpectedToken(next.loc, String::from("expecting integer constant")));
                            S_32_MIN
                        }
                    };
                    self.expect_token(Token::GreaterThan, parser_context);
                    Type::Int(lower, upper)
                } else {
                    Type::Int(S_32_MIN, S_32_MAX)
                }
            },
            Token::Never => Type::Never,
            Token::Number => Type::Number,
            Token::OctLiteral => {
                let number_i128 = next.parse_i128().unwrap();
                Type::Int(number_i128, number_i128)
            },
            Token::StringLiteral => Type::StringLiteral(next.text.unwrap()),
            Token::Unknown => Type::Unknown,
            Token::Void => Type::Void,
            _ => {
                parser_context.push_err(Error::InvalidType(next.loc, next.to_string()));
                Type::Undeclared
            }
        }
    }

    ///Parse a type with optional mutability qualifier, e.g. mut Int
    pub(crate) fn parse_qualified_type(&mut self, 
        parser_context: &mut ParserContext,
    ) -> QualifiedType {
        let next = self.peek_next_item();
        let token = next.token;
        let mut mutability = Mutability::Const;
        let mut_loc = next.loc;
        if token == Token::Mut {
            mutability = Mutability::Mut;
            self.skip_next_item();
        }
        let t = self.parse_type(parser_context);
        if t.get_pass_style() == PassStyle::SimpleValue && mutability == Mutability::Mut{
            parser_context.push_err(Error::UnnecessaryMut(mut_loc, t.clone()));
            mutability = Mutability::Const;
        }
        QualifiedType::new(&t, mutability)
    }
}