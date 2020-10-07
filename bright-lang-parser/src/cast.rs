use bright_lang_types::Mutability;
use bright_lang_types::PassStyle;
use bright_lang_ast::expr::TypedExpr;
use bright_lang_errs::Error;
use bright_lang_ast::expr::Expr;
use bright_lang_ast::expr::Arena;
use bright_lang_ast::expr::NodeIdx;
use bright_lang_types::QualifiedType;
use bright_lang_types::Type;

use crate::ParserContext;

/// Enum describing the types of implicit casts available
#[derive(Debug, Clone, PartialEq)]
pub (crate) enum TypeCast{
    /// This is an upcast that costs nothing at runtime.
    FreeUpcast(QualifiedType),
    /// One of the int casts that we support. This is a cast from a narrower int to a wider int. This may or may not be a thing at runtime.
    IntWiden(i128, i128),
    /// One of the int casts that we support. This is a cast from i32 to f64 and will definitely have runtime cost.
    IntToNumberWiden,
    /// Fail because of a const cast
    ConstFail,
    /// Not possible to cast these types
    None,
    /// Used by some places to indicate that the types are exact matches and don't need 
    /// a cast.
    NotNeeded,
    /// Means we must make a copy, because of const issues
    Clone,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub (crate) enum CastType{
    /// Implicit cast from normal flow of data
    Implicit,
    /// Explicit cast caused by an 'as' keyword
    Explicit,
}

#[derive(Debug, Clone, PartialEq, Copy)]
enum ConstCast{
    Fail,
    Pass,
    Clone
}

fn const_check(from_pass_style: PassStyle, to_pass_style: PassStyle, from_mutability: Mutability, to_mutability: Mutability) -> ConstCast {
    //you can't pass a const reference to a mut reference
    if (to_pass_style == PassStyle::Reference || to_pass_style == PassStyle::ValueHoldingReference) && to_mutability == Mutability::Mut && from_mutability == Mutability::Const && from_pass_style != PassStyle::SimpleValue {
        ConstCast::Fail
    //the object must be duplicated
    } else if to_pass_style == PassStyle::Value && from_pass_style == PassStyle::Value && to_mutability == Mutability::Mut && from_mutability == Mutability::Const {
        ConstCast::Clone
    //if we don't know, assume the worst
    } else if (to_pass_style == PassStyle::Unknown || from_pass_style == PassStyle::Unknown) && to_mutability == Mutability::Mut && from_mutability == Mutability::Const {
        ConstCast::Fail
    } else {
        ConstCast::Pass
    }
}

/// Try casting a type to another type.
pub (crate) fn try_cast(from: &QualifiedType, to: &QualifiedType, cast_type: CastType) -> TypeCast {
    if *from == *to {
        return TypeCast::NotNeeded;
    }

    let to_pass_style = to.get_pass_style();
    let from_pass_style = from.get_pass_style();

    let const_cast = const_check(from_pass_style, to_pass_style, from.mutability, to.mutability);
    if const_cast == ConstCast::Fail {
        return TypeCast::ConstFail;
    }

    let from_type = &from.r#type;
    let to_type = &to.r#type;

    if from_type == to_type {
        return match const_cast {
            ConstCast::Clone => TypeCast::Clone,
            ConstCast::Pass => TypeCast::NotNeeded,
            ConstCast::Fail => TypeCast::None
        };
    }

    if *from_type == Type::Unknown {
        // we can never widen from unknown
        return TypeCast::None;
    }

    if *from_type == Type::Never {
        // we can always cast from never, it's the bottom type
        return TypeCast::FreeUpcast(to.clone());
    }

    match to_type {
        //we can always widen to unknown, it's the top type
        Type::Unknown => TypeCast::FreeUpcast(QualifiedType::new(&Type::Unknown, from.mutability)),
        Type::Int(lower_to, upper_to) => {
            match from_type {
                Type::Int(lower_from, upper_from) => {
                    if lower_from >= lower_to && upper_from <= upper_to { TypeCast::IntWiden(*lower_to, *upper_to) } else { TypeCast::None }
                },
                _ => TypeCast::None,
            }
        },
        Type::Number => {
            match from_type {
                Type::Int(lower_from, lower_to) => if *lower_from >= -9007199254740991 && *lower_to <= 9007199254740991 { TypeCast::IntToNumberWiden } else { TypeCast::None},
                Type::FloatLiteral(_) => TypeCast::FreeUpcast(QualifiedType::new(&Type::Number, from.mutability)),
                _ => TypeCast::None,
            }
        },
        Type::String => {
            match from_type {
                Type::StringLiteral(_) => TypeCast::FreeUpcast(QualifiedType::new(&Type::String, from.mutability)),
                _ => TypeCast::None,
            }
        },
        Type::VariableUsage{name: to_name, constraint: to_constraint} => {
            match from_type {
                Type::VariableUsage{name: from_name, constraint: from_constraint} => {
                    if from_name == to_name {
                        match const_cast {
                            ConstCast::Clone => TypeCast::Clone,
                            ConstCast::Pass => TypeCast::NotNeeded,
                            ConstCast::Fail => TypeCast::None
                        }
                    } else if to_constraint.is_subset_of(from_constraint) {
                        match const_cast {
                            ConstCast::Clone => TypeCast::Clone,
                            ConstCast::Pass => TypeCast::FreeUpcast(to.clone()),
                            ConstCast::Fail => TypeCast::None
                        }
                    } else {
                        TypeCast::None
                    }
                },
                _ => TypeCast::None
            }
        }
        
        _ => TypeCast::None,
    }
}

pub (crate) fn cast_typed_expr(want: &QualifiedType, got_ni: NodeIdx, cast_type: CastType, arena: &mut Arena, parser_context: &mut ParserContext) -> NodeIdx {
    let type_cast = try_cast(&arena.a[got_ni.p].r#type, want, cast_type);
    let loc = arena.a[got_ni.p].loc;
    let got_mutability = arena.a[got_ni.p].r#type.mutability;
    match type_cast {
        TypeCast::FreeUpcast(_) => arena.push(TypedExpr::new(&Expr::FreeUpcast(got_ni), &want.clone(), loc)),
        TypeCast::IntWiden(_,_) => arena.push(TypedExpr::new(&Expr::IntWiden(got_ni), &want.clone(), loc)),
        TypeCast::IntToNumberWiden => arena.push(TypedExpr::new(&Expr::IntToNumber(got_ni), &QualifiedType::new(&Type::Number, got_mutability), loc)),
        TypeCast::None => {
            match cast_type {
                CastType::Implicit => 
                    parser_context.push_err(Error::TypeFailure(loc, want.clone(), arena.a[got_ni.p].r#type.clone())),
                CastType::Explicit  => 
                    parser_context.push_err(Error::CastFailure(loc, want.clone(), arena.a[got_ni.p].r#type.clone())),
            }
            arena.push(TypedExpr::new(&Expr::FreeUpcast(got_ni), &want.clone(), loc))
        },
        TypeCast::ConstFail => {
            parser_context.push_err(Error::ConstFailure(loc, want.clone(), arena.a[got_ni.p].r#type.clone()));
            got_ni
        },
        TypeCast::NotNeeded => {
            if cast_type == CastType::Explicit {
                parser_context.push_err(Error::CastNotNeeded(loc, want.r#type.clone()));
            }
            got_ni
        },
        TypeCast::Clone => panic!(),
    }
}