use std::collections::HashMap;
use std::io::stdin;
use crate::interpret::{RuntimeError, Value};
use crate::interpret::definitions::{Term, Type};
use crate::lang_obj::{FunctionSignature, Identifier, TypeIdentifier};

pub type FuncReturn = Result<Value, RuntimeError>;
pub type FuncArgs = Vec<Value>;

pub type PureFunc = Box<dyn Fn(FuncQuery) -> QueryResponse>;
pub struct PureFuncPackage {
    pub(crate) funcs: HashMap<Identifier, PureFunc>
}

pub enum FuncQuery {
    Meta,
    Call(FuncArgs)
}

pub enum QueryResponse {
    Meta(FunctionSignature),
    Ret(FuncReturn)
}

pub fn print(q: FuncQuery) -> QueryResponse {
    if let FuncQuery::Call(args) = q {
        QueryResponse::Ret({
           if args.len() != 1 {
               return QueryResponse::Ret(Err(RuntimeError::Semantic(
                   format!("print accepts only 1 argument, not {}", args.len())
               )))
           } else {
               let (arg_type, arg_term) = args.get(0).unwrap();
               let wrong_type_err = Err(RuntimeError::Semantic(
                   format!("print argument must be of type 'string'")
               ));
               if let Term::String(s) = arg_term {
                   // actually, we don't care
                   // what if we are using a type alias of string
                   // like type Text = string
                   // then it should be fine, right? as long as the actual type is a string
                   // if Type::string() != arg_type {
                   //     Err(RuntimeError::Interpreter(format!(
                   //         "Mismatched term and type! Bad!"
                   //     )))
                   // }
                   println!("{}", s);
                   Ok((
                       Type::empty_tuple(),
                       Term::empty_tuple()
                   ))
               } else {
                   wrong_type_err
               }
           }
       })
    } else {
        // meta
        QueryResponse::Meta(
            (
                Box::new(vec![("s".into(), None)]),
                None
            )
        )
    }
}

pub fn eprint(q: FuncQuery) -> QueryResponse {
    if let FuncQuery::Call(args) = q {
        QueryResponse::Ret({
            if args.len() != 1 {
                return QueryResponse::Ret(Err(RuntimeError::Semantic(
                    format!("eprint accepts only 1 argument, not {}", args.len())
                )))
            } else {
                let (arg_type, arg_term) = args.get(0).unwrap();
                let wrong_type_err = Err(RuntimeError::Semantic(
                    format!("eprint argument must be of type 'string'")
                ));
                if let Term::String(s) = arg_term {
                    // todo: unify string type
                    // if Type::Unit("string".into()) != arg_type {
                    //     Err(RuntimeError::Interpreter(format!(
                    //         "Mismatched term and type! Bad!"
                    //     )))
                    // }
                    eprintln!("{}", s);
                    Ok((
                        Type::empty_tuple(),
                        Term::empty_tuple()
                    ))
                } else {
                    wrong_type_err
                }
            }
        })
    } else {
        // meta
        QueryResponse::Meta(
            (
                Box::new(vec![("s".into(), None)]),
                None
            )
        )
    }
}

pub fn input(q: FuncQuery) -> QueryResponse {
    if let FuncQuery::Call(args) = q {
        QueryResponse::Ret({
           if args.len() > 0 {
               Err(RuntimeError::Semantic(format!(
                   "input() does not accept any arguments"
               )))
           } else {
               let mut s = String::new();
               match stdin().read_line(&mut s) {
                   Ok(_) => Ok((
                       Type::string(),
                       Term::String(s)
                   )),
                   Err(e) => Err(RuntimeError::Semantic(format!("input() error: {}", e)))
               }
           }
       })
    } else {
        QueryResponse::Meta((
            Box::new(vec![]),
            Some(TypeIdentifier::Name("string".into()))
        ))
    }
}

impl<T: Into<RuntimeError>> From<T> for QueryResponse {
    fn from(e: T) -> Self {
        QueryResponse::Ret(Err(e.into()))
    }
}