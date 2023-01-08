use std::collections::HashMap;
use crate::interpret::{RuntimeError, Value};
use crate::interpret::definitions::{Term, Type};
use crate::lang_obj::{FunctionSignature, Identifier};

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
                   // todo: unify string type
                   // if Type::Unit("string".into()) != arg_type {
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