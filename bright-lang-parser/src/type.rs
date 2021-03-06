use bright_lang_types::StaticMemberFunc;
use bright_lang_types::MemberFunc;
use bright_lang_types::UserClass;
use crate::ParserPhase;
use bright_lang_types::TraitDecl;
use bright_lang_ast::TraitImpl;
use bright_lang_types::UserStruct;
use bright_lang_types::{Member, Privacy};
use bright_lang_types::{QualifiedType, Type};
use crate::op::{BinaryOperatorData, Association};
use bright_lang_errs::Error;
use bright_lang_errs::source_location::SourceLocation;
use bright_lang_types::{TypeArg, TypeConstraint, Mutability, S_32_MIN, S_32_MAX, PassStyle, UserClassStorage};
use bright_lang_lexer::{Token};
use bright_lang_ast::TypeDecl;
use crate::ParserContext;
use crate::BrightParser;
use crate::Commitment;
use crate::UnsafeParseMode;

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
                parser_context.push_err(Error::UnexpectedToken(*loc, "| or &".to_string(), op.to_string()));
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
                                        //rhs := parse_expression_1 (rhs, look ahead's precedence)
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
            if token == Token::Comma || token == Token::NewLine {
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
                    TypeDecl::Enum{export: _} => unimplemented!(),
                    TypeDecl::Struct{user_struct: _, under_construction: _, export: _} => Some(Type::UnsafeStruct{name: ident.to_owned()}),
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
                                parser_context.push_err(Error::UnexpectedToken(next.loc, String::from("integer constant"), next.to_string()));
                                S_32_MIN
                            } else {
                                next.parse_i128().unwrap()
                            }
                        },
                        _ => {
                            parser_context.push_err(Error::UnexpectedToken(next.loc, String::from("integer constant"), next.to_string()));
                            S_32_MIN
                        }
                    };
                    self.expect_comma(parser_context);
                    let next = self.next_item();
                    let token = next.token;
                    let upper = match token {
                        Token::BinLiteral | Token::HexLiteral | Token::OctLiteral => next.parse_i128().unwrap(),
                        Token::DecLiteral => {
                            if next.text.clone().unwrap().contains('.') {
                                parser_context.push_err(Error::UnexpectedToken(next.loc, String::from("integer constant"), next.to_string()));
                                S_32_MIN
                            } else {
                                next.parse_i128().unwrap()
                            }
                        },
                        _ => {
                            parser_context.push_err(Error::UnexpectedToken(next.loc, String::from("integer constant"), next.to_string()));
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
            Token::UnsafePtr => {
                if parser_context.unsafe_parse_mode == UnsafeParseMode::Safe {
                    parser_context.errors.push(Error::UnsafeCodeNotAllowed(next.loc));
                }
                Type::UnsafePtr
            },
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

    pub(crate) fn parse_alias(&mut self, 
        export: bool,
        parser_context: &mut ParserContext,
    ) {
        self.skip_next_item();
        let (id, _) = self.expect_ident(parser_context);
        self.skip_next_item();
        self.expect_token(Token::Equal, parser_context);
        let t = self.parse_type(parser_context);
        parser_context.type_map.insert(id, TypeDecl::Alias{of: t, export});
    }

    pub (crate) fn parse_struct_decl(&mut self,    
        export: bool,
        parser_context: &mut ParserContext,
    ) {
        let loc = self.peek_next_location();
        if parser_context.unsafe_parse_mode == UnsafeParseMode::Safe {
            parser_context.errors.push(Error::UnsafeCodeNotAllowed(loc));
        }
        self.skip_next_item();

        let (id, id_loc) = self.expect_ident(parser_context);

        if parser_context.type_map.contains_key(&id) {
            parser_context.push_err(Error::DuplicateTypeName(id_loc, id.clone()))
        }

        parser_context.push_empty_type_scope();

        let members = self.parse_class_members(parser_context);

        parser_context.type_map.insert(id.to_string(), TypeDecl::Struct{user_struct: UserStruct{members}, under_construction: true, export});

        parser_context.pop_type_scope();

        //register the fact that this is a struct and so has the IsAStruct trait
        let trait_name = String::from("IsAStruct");
        parser_context.trait_impl_map.insert((id.to_string(), trait_name.clone()), TraitImpl{trait_name, for_type: Type::UnsafeStruct{name: id.to_string()}, export: true, member_funcs: vec![]});

        //and register the type itself
        parser_context.type_map.insert(id, TypeDecl::Struct{user_struct: UserStruct{members: vec![]}, under_construction: false, export});
    }

    ///parse a `trait`, which is a reduced type class
    pub (crate) fn parse_trait_decl(&mut self, 
        export: bool,
        parser_context: &mut ParserContext,
    ) {
        self.skip_next_item();
        let (id, _) = self.expect_ident(parser_context);
        self.skip_next_item();
     
        let mut member_funcs = vec![];

        self.expect_token(Token::SquigglyOpen, parser_context);

        let mut next = self.peek_next_item();
        let mut token = next.token;
        let this_type = Type::VariableUsage{name: "This".to_owned(), constraint: TypeConstraint::Trait(id.clone())};
        parser_context.push_trait_type_scope(&TypeArg{name: "This".to_owned(), constraint: TypeConstraint::Trait(id.clone())});

        while token != Token::SquigglyClose {
            if token == Token::EOF {
                parser_context.push_err(Error::UnexpectedEoF(next.loc, String::from("'}'")));
                break;
            }

            match token {
                Token::Fn => {
                    let mf = self.parse_trait_func_decl_header(&QualifiedType::new_const(&this_type), &id, parser_context);
                    member_funcs.push(mf);
                },
                Token::Mut => {
                    self.skip_next_item();
                    next = self.peek_next_item();
                    token = next.token;
                    match token {
                        Token::Fn => {
                            let mf = self.parse_trait_func_decl_header(&QualifiedType::new_mut(&this_type), &id, parser_context);
                            member_funcs.push(mf);
                        },
                        _ => {
                            parser_context.push_err(Error::UnexpectedToken(next.loc, "fn".to_owned(), next.to_string()));
                            self.skip_next_item();
                        }
                    }
                },
                _ => {
                    parser_context.push_err(Error::UnexpectedToken(next.loc, "'mut' or 'fn'".to_owned(), next.to_string()));
                    self.skip_next_item();
                }
            }

            next = self.peek_next_item();
            token = next.token;
        }

        self.skip_next_item();
        parser_context.pop_type_scope();

        parser_context.trait_map.insert(id.clone(), TraitDecl{name: id, member_funcs, export});
    }

    fn parse_class_member(&mut self,
        parser_context: &mut ParserContext,
    ) -> Member {
        let mut next = self.peek_next_item();
        let mut privacy = Privacy::Unresolved;
        if next.token == Token::Private {
            privacy = Privacy::Private;
            next = self.next_item();
        } else if next.token == Token::Public {
            privacy = Privacy::Public;
            next = self.next_item();
        }

        let mut var = false;
        if next.token == Token::Var {
            if privacy == Privacy::Unresolved {
                privacy = Privacy::Private;
            }
            var = true;
            self.skip_next_item();
        } else if privacy == Privacy::Unresolved {
            privacy = Privacy::Public;
        }
        
        let (id, _) = self.expect_ident(parser_context);
        self.expect_token(Token::Colon, parser_context);
        let t = self.parse_qualified_type(parser_context);
        Member{var, privacy, name: id, r#type: t}
    }

    fn parse_class_members(&mut self,
        parser_context: &mut ParserContext,
    ) -> Vec<Member> {
        self.expect_token(Token::RoundOpen, parser_context);

        let mut members: Vec<Member> = vec![]; 
        loop {
            let next = self.peek_next_item();
            let token = next.token;
            match token {
                Token::RoundClose => { self.skip_next_item(); return members; },
                Token::Ident | Token::Var | Token::Public | Token::Private => {
                    let member = self.parse_class_member(parser_context);
                    members.push(member);
                },
                _ => {
                    parser_context.push_err(Error::UnexpectedToken(next.loc, String::from("')'"), next.to_string()));
                    self.skip_next_item();
                }
            }
        }
    }

    fn parse_class_body(&mut self,
        export: bool, 
        phase: ParserPhase,
        name: &str,
        this_type: &Type,
        this_type_args: &[TypeArg],
        parser_context: &mut ParserContext,
    ) {
        self.expect_token(Token::SquigglyOpen, parser_context);

        let prefix = format!("!{}__", name);
        let mut member_funcs: Vec<MemberFunc> = vec![];
        let mut static_member_funcs: Vec<StaticMemberFunc> = vec![];

        let mut next = self.peek_next_item();
        while next.token != Token::SquigglyClose {
            if next.token == Token::EOF {
                parser_context.push_err(Error::UnexpectedEoF(next.loc, String::from("expecting '}'")));
                break;
            }

            match next.token {
                Token::Fn => {
                    let mf = self.parse_member_function_decl(&prefix, &this_type, &this_type_args, Privacy::Public, export, phase, parser_context);
                    parser_context.append_type_decl_member_func(&name, &mf);
                    member_funcs.push(mf);
                },
                Token::Shared => unimplemented!(),
                _ => {
                    self.skip_next_item();
                    parser_context.push_err(Error::UnexpectedToken(next.loc, String::from("'}', fn or shared"), next.to_string()));
                }
            }
            next = self.peek_next_item();
        }
    }

    pub (crate) fn parse_class_decl(&mut self,
        export: bool, 
        phase: ParserPhase,
        parser_context: &mut ParserContext,
    ) {
        self.expect_token(Token::Class, parser_context);
        let mut user_class_storage = UserClassStorage::Unresolved;
        let mut next = self.peek_next_item();
        if next.token == Token::Stack {
            user_class_storage = UserClassStorage::Stack;
            self.skip_next_item();
        } else if next.token == Token::Heap {
            next = self.next_item();
            if next.token == Token::Ref {
                self.skip_next_item();
                user_class_storage = UserClassStorage::HeapRef;
            } else {
                user_class_storage = UserClassStorage::Heap;
            }
        }

        let (id, _) = self.expect_ident(parser_context);

        let mut generic = false;

        next = self.peek_next_item();
        let type_args = if next.token == Token::LessThan {
            let type_args = self.parse_type_decl_args(parser_context);
            let type_args = parser_context.push_type_scope(&type_args);
            generic = true;
            next = self.peek_next_item();
            type_args
        } else {
            vec![]
        };

        if next.token == Token::RoundOpen {
            let mut user_class = UserClass{
                type_args: type_args.clone(), member_funcs: vec![], members: vec![], static_member_funcs: vec![], storage: user_class_storage
            };
            parser_context.type_map.insert(id.clone(), TypeDecl::UserClass{export, under_construction: true, user_class: user_class.clone()});
            let members = self.parse_class_members(parser_context);
            user_class.members = members;
            parser_context.type_map.insert(id.clone(), TypeDecl::UserClass{export, under_construction: true, user_class});
            let user_type = Type::UserClass{name: id.clone(), type_args:
                type_args.iter().map(|type_arg| Type::VariableUsage{name: type_arg.name.clone(), constraint: type_arg.constraint.clone()}).collect(),
            };
            self.parse_class_body(export, phase, &id, &user_type, &type_args, parser_context);
            parser_context.finish_type_decl(&id);
        } else if next.token == Token::Implements {

        } else {
            parser_context.push_err(Error::UnexpectedToken(next.loc, "members or 'implements'".to_owned(), next.to_string()));
        }

        if generic {
            parser_context.pop_type_scope();
        }
    }
}