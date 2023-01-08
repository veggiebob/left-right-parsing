use crate::interpret::definitions::Type::Product;
use crate::interpret::definitions::{function_param_type_id, function_return_type_id, HeapData, HeapID, LanguageObject, ListObject, ListType, NamedProductType, ProductObject, ProductType, ProductTypeKind, ProgramData, StackData, StackFrame, Term, TupleObject, TupleType, Type};
use crate::lang_obj::ListExprType::List;
use crate::lang_obj::{
    Expr, FunctionSignature, Identifier, ListExprType, Program, Statement, TypeIdentifier,
};
use std::any::TypeId;
use std::borrow::{Borrow, BorrowMut};
use std::cell::{Ref, RefCell};
use std::collections::{HashMap, VecDeque};
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use crate::interpret::stdlib::{print, PureFuncPackage, PureFunc, FuncQuery, QueryResponse};

pub mod definitions;
pub mod stdlib;

#[cfg(test)]
mod test;

// finds and/or loads programs
// uhhhhh not really sure no super good definitions yet
// hang tight 👍
#[derive(Eq, PartialEq, Hash, Debug, Clone)]
pub struct ImportStatement {
    // ???
    pub name: Identifier, // ??
}
trait ProgramAcquisitionService {
    fn find(import: ImportStatement) -> Program; // tentative
}

pub struct ProgramRetriever {
    // context?
}

type Value = (Type, Term);

#[derive(Debug, PartialEq, Eq)]
pub enum RuntimeError {
    Semantic(String),
    Interpreter(String), // an interpreter error. developer error.
}

#[derive(Debug, PartialEq)]
pub enum EvalResult {
    Term(Value),
    Call(Vec<StackFrame>, Vec<Statement>),
}

pub enum InterpreterAction {
    Return(Value),
    Call(Vec<StackFrame>, Vec<Statement>),
}

pub struct Interpreter<'a> {
    /// a stack containing scope data and variables
    pub stack: StackData,

    /// a heap
    pub heap: HeapData<'a>,

    /// a stack containing the remaining statements left to process
    /// in the program
    pub instruction_stack: Vec<Statement>,

    /// type data
    pub program_data: ProgramData,

    /// the program
    pub program: Program,

    /// other stuff:
    pub program_retriever: ProgramRetriever,
}

/// temporary infix operator for concatenating lists, solely for interpreter use
const LIST_CONCAT: &str = "«list_concatenator»";
const TUPLE_CONCAT: &str = "«tuple_concatenator»";

impl<'a> Interpreter<'a> {
    pub fn new(program: Program, program_retriever: ProgramRetriever) -> Interpreter<'a> {
        let mut interpreter = Interpreter {
            // there is no such thing as a main function! ha!
            // no, we run this program, exactly as we have been told.
            stack: StackData(vec![]),
            instruction_stack: vec![],
            heap: HeapData {
                data: HashMap::new(),
                pure_funcs: HashMap::new(),
                func_ptrs: HashMap::new()
            },
            program_data: ProgramData {
                types: HashMap::new(),
                constants: HashMap::new(), // globals
            },
            program,
            program_retriever,
        };
        interpreter.load_func_pkg("rust::stdlib".into(), PureFuncPackage {
            funcs: hashmap! {
                "print".into() => Box::new(print) as PureFunc
            }
        });
        interpreter
    }

    pub fn load_func_pkg(&mut self, identifier: Identifier, pkg: PureFuncPackage) {
        self.heap.pure_funcs.insert(identifier, pkg);
    }

    pub fn start(&mut self) -> Result<Option<Value>, RuntimeError> {
        // do any initialization steps
        let sf = self.create_inherited_stack_frame();
        // add a stack frame
        self.stack.0.push(sf);

        // push all the top-level statements onto the instruction stack
        for stmt in self.program.content.iter().rev() {
            self.instruction_stack.push(stmt.clone());
        }
        let mut return_value = None;
        while let Some(stmt) = self.instruction_stack.pop() {
            // otherwise, interpret it
            let res = self.interpret(&stmt, return_value.take())?;

            // if it requires additional computation, add it to the stack
            if let Some(InterpreterAction::Call(sf, inst_stmts)) = res {
                // the instruction calling others needs to be preserved
                self.instruction_stack.push(stmt);
                for stmt in inst_stmts.into_iter().rev() {
                    // the first instruction to run is at the end of the stack
                    self.instruction_stack.push(stmt);
                }
                self.stack.0.extend(sf); // add the stack frame
            } else if let Some(InterpreterAction::Return(term)) = res {
                return_value = Some(term);
                let _lost = self.stack.0.pop(); // drop the top stack frame
            }

            // println!("------");
            // println!(
            //     "Stack Frames:\n{}",
            //     self.stack
            //         .0
            //         .iter()
            //         .map(|sf| format!("{}", sf))
            //         .reduce(|acc, x| acc + "\n" + &x)
            //         .unwrap_or("".into())
            // );
            // println!(
            //     "Instructions: \n{}",
            //     self.instruction_stack
            //         .iter()
            //         .map(ToString::to_string)
            //         .rev()
            //         .reduce(|mut x, acc| x + ";\n" + &acc)
            //         .unwrap_or("<none>".to_string())
            // );
            // println!("Return Value: {:?}", return_value);
        }

        Ok(return_value)
    }

    // return a (revised statement (the one currently being run), a new stackframe with necessary locals, statements inside new scope)
    fn interpret(
        &mut self,
        stmt: &Statement,
        return_value: Option<Value>,
    ) -> Result<Option<InterpreterAction>, RuntimeError> {
        return match stmt {
            Statement::Import(imt) => {
                todo!("interpret import statement!")
            }
            Statement::Impure(expr) => {
                if let Some(t) = return_value {
                    return Ok(None);
                }
                match self.evaluate(expr)? {
                    EvalResult::Term(_t) => Ok(None), // we don't care what it evaluates to
                    EvalResult::Call(sf, stmts) => Ok(Some(InterpreterAction::Call(sf, stmts))),
                }
            }
            Statement::Assignment(ident, expr) => {
                self.top_stack()?.data.get(ident).ok_or(format!(
                    "Cannot assign a value {:?} that does not exist.",
                    ident
                ))?;
                match return_value {
                    None => {
                        match self.evaluate(expr)? {
                            EvalResult::Term(value) => {
                                self.top_stack_mut()?.data.insert(ident.clone(), value);
                                // assignment has completed, no further steps needed
                                Ok(None)
                            }
                            EvalResult::Call(sf, stmts) => {
                                // evaluation calls for more computation.
                                Ok(Some(InterpreterAction::Call(sf, stmts)))
                            }
                        }
                    }
                    Some(x) => {
                        self.top_stack_mut()?.data.insert(ident.clone(), x);
                        Ok(None)
                    }
                }
            }
            Statement::Let(ident, expr) => {
                if let Some(_) = self.stack.borrow().0.last().unwrap().data.get(ident) {
                    Err(RuntimeError::Semantic(format!(
                        "Tried to re-assign {:?}",
                        ident
                    )))
                } else {
                    if let Some(value) = return_value {
                        self.top_stack_mut()?.data.insert(ident.clone(), value);
                        return Ok(None);
                    }
                    match self.evaluate(expr)? {
                        EvalResult::Call(sf, stmts) => Ok(Some(InterpreterAction::Call(sf, stmts))),
                        EvalResult::Term(t) => {
                            self.stack
                                .0
                                .last_mut()
                                .unwrap()
                                .data
                                .insert(ident.clone(), t);
                            Ok(None)
                        }
                    }
                }
            }
            Statement::Ret(expr) => {
                if let Some(value) = return_value {
                    return Ok(Some(InterpreterAction::Return(value)));
                }
                match self.evaluate(expr)? {
                    EvalResult::Term(t) => Ok(Some(InterpreterAction::Return(t))),
                    EvalResult::Call(sf, stmts) => {
                        // need to handle return statement in evaluate
                        Ok(Some(InterpreterAction::Call(sf, stmts)))
                    }
                }
            }
        };
    }

    fn evaluate(&self, expr: &Expr) -> Result<EvalResult, RuntimeError> {
        // Now, this is where this gets language-dependent.
        // There also has to be a bit of collaboration between the parser
        // and the interpreter.
        // Of course, supposing imports are allowed, there is much more
        // that can be created, so this can remain fairly bare bones.
        match expr {
            Expr::Bool(b) => Ok(("bool".into(), Term::Bool(*b)).into()),
            Expr::Nat(x) => Ok(("nat".into(), Term::Nat(x.content).into()).into()),
            Expr::Str(s) => Ok(("string".into(), Term::String(s.content.clone())).into()),
            Expr::Infix(left, op, right) => {
                return match self.evaluate(left)? {
                    EvalResult::Call(sfs, mut stmts) => {
                        let sf = self.create_inherited_stack_frame();
                        let tmp_ident = sf.get_interpreter_identifier();

                        // the expression doesn't matter
                        // this captures the result of the left expression return statement
                        let capture = Statement::Let(
                            tmp_ident.clone(),
                            Expr::Variable(Identifier::Unit(String::new())),
                        );

                        let ret = Statement::Ret(Expr::Infix(
                            Expr::Variable(tmp_ident.clone()).into(),
                            op.clone(),
                            right.clone(),
                        ));

                        let mut stack_frames = vec![sf];
                        stack_frames.extend(sfs);

                        stmts.extend(vec![capture, ret]);

                        Ok(EvalResult::Call(stack_frames, stmts))
                    }
                    EvalResult::Term(left) => {
                        match self.evaluate(right)? {
                            EvalResult::Call(sfs, mut stmts) => {
                                let mut sf = self.create_inherited_stack_frame();

                                let left_ident = sf.get_interpreter_identifier();
                                sf.data.insert(left_ident.clone(), left);

                                let right_ident = sf.get_interpreter_identifier();

                                // the expression doesn't matter
                                // this captures the result of the left expression return statement
                                let capture = Statement::Let(
                                    right_ident.clone(),
                                    Expr::Variable(Identifier::Unit(String::new())),
                                );

                                let ret = Statement::Ret(Expr::Infix(
                                    Box::new(Expr::Variable(left_ident.clone())),
                                    op.clone(),
                                    Box::new(Expr::Variable(right_ident.clone())),
                                ));

                                let mut stack_frames = vec![sf];
                                stack_frames.extend(sfs);

                                stmts.extend(vec![capture, ret]);

                                Ok(EvalResult::Call(stack_frames, stmts))
                            }
                            EvalResult::Term((right_type, right_term)) => {
                                let (left_type, left_term) = left;
                                match op.as_str() {
                                    "," => Ok((
                                        Type::Product(ProductType {
                                            name: TypeIdentifier::Anonymous,
                                            data: ProductTypeKind::Tuple(TupleType {
                                                length: 2,
                                                types: vec![
                                                    Box::new(left_type.clone()),
                                                    Box::new(right_type),
                                                ],
                                            }),
                                        }),
                                        Term::Object(
                                            LanguageObject::Product(ProductObject::Tuple(
                                                TupleObject(vec![left_term.clone(), right_term]),
                                            ))
                                            .into(),
                                        ),
                                    )
                                        .into()),
                                    "+" => {
                                        if let Term::Nat(x) = left_term {
                                            if let Term::Nat(y) = right_term {
                                                Ok((
                                                    left_type, // assuming they are the same type
                                                    Term::Nat(x + y),
                                                )
                                                    .into())
                                            } else {
                                                Err(RuntimeError::Semantic(format!(
                                                    "Right side of + expression must be a number"
                                                )))
                                            }
                                        } else {
                                            Err(RuntimeError::Semantic(format!(
                                                "Left side of + expression must be a number"
                                            )))
                                        }
                                    }
                                    LIST_CONCAT => {
                                        let err = Err(RuntimeError::Semantic(format!("{} operator cannot be used between anything besides lists", LIST_CONCAT)));
                                        if left_type != right_type {
                                            return Err(RuntimeError::Semantic(format!("List contains multiple conflicting types, {:?} and {:?}", left_type, right_type)));
                                        }
                                        if let Term::Object(obj) = left_term {
                                            if let LanguageObject::Product(ProductObject::List(
                                                ListObject(mut left_items),
                                            )) = *obj
                                            {
                                                if let Term::Object(obj) = right_term {
                                                    if let LanguageObject::Product(
                                                        ProductObject::List(ListObject(
                                                            right_items,
                                                        )),
                                                    ) = *obj
                                                    {
                                                        left_items.extend(right_items);
                                                        Ok((
                                                            left_type.clone(), // cause they should be the same
                                                            Term::Object(Box::new(
                                                                LanguageObject::Product(
                                                                    ProductObject::List(
                                                                        ListObject(left_items),
                                                                    ),
                                                                ),
                                                            )),
                                                        )
                                                            .into())
                                                    } else {
                                                        err
                                                    }
                                                } else {
                                                    err
                                                }
                                            } else {
                                                err
                                            }
                                        } else {
                                            err
                                        }
                                    }
                                    TUPLE_CONCAT => {
                                        let err = Err(RuntimeError::Semantic(format!("{} operator cannot be used between anything besides tuples", TUPLE_CONCAT)));
                                        let l_type_err = Err(RuntimeError::Semantic(format!(
                                            "Left type is not tuple!"
                                        )));
                                        let r_type_err = Err(RuntimeError::Semantic(format!(
                                            "Right type is not tuple!"
                                        )));
                                        let mut left_types =
                                            if let Type::Product(ProductType { data, .. }) =
                                                left_type
                                            {
                                                if let ProductTypeKind::Tuple(tup_type) = data {
                                                    Ok(tup_type.types)
                                                } else {
                                                    l_type_err
                                                }
                                            } else {
                                                l_type_err
                                            }?;
                                        let right_types =
                                            if let Type::Product(ProductType { data, .. }) =
                                                right_type
                                            {
                                                if let ProductTypeKind::Tuple(tup_type) = data {
                                                    Ok(tup_type.types)
                                                } else {
                                                    r_type_err
                                                }
                                            } else {
                                                r_type_err
                                            }?;
                                        if let Term::Object(obj) = left_term {
                                            if let LanguageObject::Product(ProductObject::Tuple(
                                                TupleObject(mut left_items),
                                            )) = *obj
                                            {
                                                if let Term::Object(obj) = right_term {
                                                    if let LanguageObject::Product(
                                                        ProductObject::Tuple(TupleObject(
                                                            right_items,
                                                        )),
                                                    ) = *obj
                                                    {
                                                        left_items.extend(right_items);
                                                        left_types.extend(right_types);
                                                        Ok((
                                                            Type::Product(ProductType {
                                                                name: TypeIdentifier::Anonymous,
                                                                data: ProductTypeKind::Tuple(
                                                                    TupleType {
                                                                        length: left_types.len(),
                                                                        types: left_types,
                                                                    },
                                                                ),
                                                            }), // cause they should be the same
                                                            Term::Object(Box::new(
                                                                LanguageObject::Product(
                                                                    ProductObject::Tuple(
                                                                        TupleObject(left_items),
                                                                    ),
                                                                ),
                                                            )),
                                                        )
                                                            .into())
                                                    } else {
                                                        err
                                                    }
                                                } else {
                                                    err
                                                }
                                            } else {
                                                err
                                            }
                                        } else {
                                            err
                                        }
                                    }
                                    "$" | " " => {
                                        // call function!
                                        if let Term::Function(
                                            (params, ret_t),
                                            body,
                                            ret,
                                            captured,
                                        ) = left_term
                                        {
                                            // left side is a function
                                            let err = Err(RuntimeError::Semantic(format!(
                                                "Expected right side of operation to be a \
                                                    tuple."
                                            )));
                                            let type_err = Err(RuntimeError::Interpreter(format!(
                                                "Wrong type format for argument tuple."
                                            )));
                                            let right_types =
                                                if let Type::Product(ProductType { name, data }) =
                                                    right_type
                                                {
                                                    if let ProductTypeKind::Tuple(tuple_type) = data
                                                    {
                                                        Ok(tuple_type.types)
                                                    } else {
                                                        type_err
                                                    }
                                                } else {
                                                    type_err
                                                }?;
                                            if let Term::Object(o) = right_term {
                                                if let LanguageObject::Product(
                                                    ProductObject::Tuple(TupleObject(args)),
                                                ) = *o
                                                {
                                                    let mut sf =
                                                        self.create_inherited_stack_frame();
                                                    // todo: error for incorrect number of arguments
                                                    for cap in captured {
                                                        let c = self.top_stack()?.data.get(&cap)
                                                            .ok_or(RuntimeError::Interpreter(format!("Variable <<{:?}>> was supposedly captured, but does not exist", cap)))?
                                                            .clone();
                                                        sf.data.insert(cap.clone(), c);
                                                    }
                                                    // insert arguments into stack frame
                                                    for ((ident, typ), (arg_type, arg)) in params
                                                        .into_iter()
                                                        .zip(right_types.into_iter().zip(args))
                                                    {
                                                        // todo: check to see if type of parameter matches
                                                        //  arg type
                                                        sf.data.insert(ident, (*arg_type, arg));
                                                    }
                                                    let mut stmts: Vec<_> = body
                                                        .into_iter()
                                                        .map(|s| *s.clone())
                                                        .collect();
                                                    // add return statement
                                                    stmts.push(Statement::Ret(*ret.clone()));
                                                    Ok(EvalResult::Call(vec![sf], stmts))
                                                } else {
                                                    err
                                                }
                                            } else {
                                                err
                                            }
                                        } else if let Term::HeapPointer(ref ptr) = left_term {
                                            if let HeapID::ToFunc(pkg, ident) = ptr {
                                                if let Some(f) = self.heap.pure_funcs.get(pkg)
                                                    .and_then(|pkg| pkg.funcs.get(ident)) {
                                                    let type_err = Err(RuntimeError::Semantic(format!(
                                                        "Arguments to function were the wrong type"
                                                    )));
                                                    let term_err = Err(RuntimeError::Semantic(format!(
                                                        "Arguments to function were not a tuple"
                                                    )));
                                                    let right_types =
                                                        if let Type::Product(ProductType { name, data }) =
                                                            right_type
                                                        {
                                                            if let ProductTypeKind::Tuple(tuple_type) = data
                                                            {
                                                                Ok(tuple_type.types)
                                                            } else {
                                                                type_err
                                                            }
                                                        } else {
                                                            type_err
                                                        }?;
                                                    if let Term::Object(o) = right_term {
                                                        if let LanguageObject::Product(
                                                            ProductObject::Tuple(TupleObject(args)),
                                                        ) = *o
                                                        {
                                                            let arg_values = right_types.into_iter()
                                                                .map(|a| *a)
                                                                .zip(args).collect::<Vec<_>>();
                                                            let res = (*f)(FuncQuery::Call(arg_values));
                                                            if let QueryResponse::Ret(ret) = res {
                                                                Ok(EvalResult::Term(ret?))
                                                            } else {
                                                                Err(RuntimeError::Interpreter(format!(
                                                                    "Pure func {}::{} did not return the correct response!",
                                                                    pkg, ident
                                                                )))
                                                            }
                                                        } else {
                                                            term_err
                                                        }
                                                    } else {
                                                        term_err
                                                    }
                                                }
                                                else {
                                                    Err(RuntimeError::Interpreter(format!(
                                                        "Unable to find function pointer {}::{}",
                                                        pkg, ident
                                                    )))
                                                }
                                            } else {
                                                Err(RuntimeError::Semantic(format!(
                                                    "Expected left side heap pointer to point to function"
                                                )))
                                            }
                                        } else {
                                            Err(RuntimeError::Semantic(format!(
                                                "Expected left side of operation to be a function"
                                            )))
                                        }
                                    }
                                    _ => Err(RuntimeError::Semantic(format!(
                                        "Unrecognized operator: '{}'",
                                        op
                                    ))),
                                }
                            }
                        }
                    }
                };
            }
            Expr::List(exprs, list_type) => {
                let mut exprs = exprs.clone();
                if let Some(last) = exprs.pop() {
                    let mut sf = self.create_inherited_stack_frame();
                    let last_ident = sf.get_interpreter_identifier();

                    match self.evaluate(&*last)? {
                        EvalResult::Term((t_type, t_term)) => {
                            sf.data.insert(
                                last_ident.clone(),
                                (
                                    Type::Product(ProductType {
                                        name: TypeIdentifier::Anonymous,
                                        data: match list_type {
                                            ListExprType::List => {
                                                ProductTypeKind::List(ListType(t_type.into()))
                                            }
                                            ListExprType::Tuple => {
                                                ProductTypeKind::Tuple(TupleType {
                                                    length: 1,
                                                    types: vec![Box::new(t_type)],
                                                })
                                            }
                                        },
                                    }),
                                    Term::Object(
                                        match list_type {
                                            List => LanguageObject::Product(ProductObject::List(
                                                ListObject(vec![t_term]),
                                            )),
                                            ListExprType::Tuple => LanguageObject::Product(
                                                ProductObject::Tuple(TupleObject(vec![t_term])),
                                            ),
                                        }
                                        .into(),
                                    ),
                                ),
                            );
                            let stmts = vec![Statement::Ret(Expr::Infix(
                                Box::new(Expr::List(exprs, list_type.clone())),
                                if let List = list_type {
                                    LIST_CONCAT
                                } else {
                                    TUPLE_CONCAT
                                }
                                .into(),
                                Box::new(Expr::Variable(last_ident.clone())),
                            ))];
                            Ok(EvalResult::Call(vec![sf], stmts))
                        }
                        EvalResult::Call(sfs, mut stmts) => {
                            let sf = self.create_inherited_stack_frame();
                            let last_ident = sf.get_interpreter_identifier();

                            // captures return value
                            let capture = Statement::Let(last_ident.clone(), Expr::Str("".into()));
                            let ret = Statement::Ret(Expr::Infix(
                                Box::new(Expr::List(exprs, list_type.clone())),
                                if let List = list_type {
                                    LIST_CONCAT
                                } else {
                                    TUPLE_CONCAT
                                }
                                .into(),
                                Box::new(Expr::List(
                                    vec![Box::new(Expr::Variable(last_ident.clone()))],
                                    list_type.clone(),
                                )),
                            ));
                            stmts.extend(vec![capture, ret]);
                            let mut stack_frames = vec![sf];
                            stack_frames.extend(sfs);
                            Ok(EvalResult::Call(stack_frames, stmts))
                        }
                    }
                } else {
                    Ok((
                        Product(ProductType {
                            name: TypeIdentifier::Anonymous,
                            data: match list_type {
                                List => ProductTypeKind::List(ListType(Box::new(Type::unknown()))),
                                ListExprType::Tuple => ProductTypeKind::Tuple(TupleType {
                                    length: 0,
                                    types: vec![],
                                }),
                            },
                        }),
                        Term::Object(
                            match list_type {
                                List => {
                                    LanguageObject::Product(ProductObject::List(ListObject(vec![])))
                                }
                                ListExprType::Tuple => LanguageObject::Product(
                                    ProductObject::Tuple(TupleObject(vec![])),
                                ),
                            }
                            .into(),
                        ),
                    )
                        .into())
                }
            }
            Expr::Variable(ident) => {
                // look it up in stack
                let missing_err = RuntimeError::Semantic(format!(
                    "Unrecognized variable: <<{:?}>>",
                    ident
                ));
                let mut value = self
                    .stack
                    .0
                    .last()
                    .ok_or(RuntimeError::Interpreter("Empty stack??".into()))?
                    .data
                    .get(ident).map(|x| x.clone());
                for (pkg_ident, pkg) in self.heap.pure_funcs.iter() {
                    if let Some(f) = pkg.funcs.get(ident) {
                        value = Some((
                            Type::Pointer(Box::new(Product(ProductType {
                                name: "function".into(),
                                data: ProductTypeKind::Named(NamedProductType {
                                    fields: hashmap! {
                                        // todo: use function (f) to determine type
                                        function_param_type_id() => Box::new(Type::unknown()),
                                        function_return_type_id() => Box::new(Type::unknown())
                                    }
                                })
                            }))),
                            Term::HeapPointer(HeapID::ToFunc(pkg_ident.clone(), ident.clone()))
                        ))
                    }
                }
                let value = value.ok_or(missing_err)?;
                return Ok(EvalResult::Term(value));
            }
            Expr::Conditional(cond, then, otherwise) => match self.evaluate(cond)? {
                EvalResult::Term((_type, t)) => {
                    if let Term::Bool(b) = t {
                        self.evaluate(if b { then } else { otherwise })
                    } else {
                        Err(RuntimeError::Semantic(format!(
                            "Condition is not a boolean."
                        )))
                    }
                }
                EvalResult::Call(sfs, mut stmts) => {
                    let sf = self.create_inherited_stack_frame();
                    let cond_var = sf.get_interpreter_identifier();
                    let capture = Statement::Let(cond_var.clone(), Expr::Str("".into()));
                    let ret = Statement::Ret(Expr::Conditional(
                        Box::new(Expr::Variable(cond_var.clone())),
                        then.clone(),
                        otherwise.clone(),
                    ));
                    stmts.extend(vec![capture, ret]);
                    let mut stack_frames = vec![sf];
                    stack_frames.extend(sfs);
                    Ok(EvalResult::Call(stack_frames, stmts))
                }
            },
            Expr::Function(sig, body, ret) => {
                return Ok((
                    self.get_func_sig_type(sig),
                    Term::Function(
                        sig.clone(),
                        body.clone(),
                        ret.clone(),
                        self.top_stack()?.data.clone().into_keys().collect(),
                    ),
                )
                    .into())
            }
            Expr::Lambda(sig, ret) => {
                return Ok((
                    self.get_func_sig_type(sig),
                    Term::Function(
                        sig.clone(),
                        vec![],
                        ret.clone(),
                        self.top_stack()?.data.clone().into_keys().collect(),
                    ),
                )
                    .into())
            }
        }
    }

    fn get_type(&self, type_id: &TypeIdentifier) -> Type {
        Type::Unit(type_id.clone()) // technically works, won't work though
                                    // todo: look up type to find out what the actual type is
    }

    fn get_func_sig_type(&self, sig: &FunctionSignature) -> Type {
        let (params, ret_t) = sig;
        Product(ProductType {
            name: "function".into(),
            data: ProductTypeKind::Named(NamedProductType {
                fields: hashmap! {
                    function_return_type_id() => Box::new(ret_t.as_ref().map(|t| Type::Unit(t.clone())).unwrap_or(Type::unknown())),
                    function_param_type_id() => Box::new(
                        params.iter().map(|(_i, t)| t.as_ref().map(|t| t.clone()).map(Type::Unit).unwrap_or(Type::unknown())).collect::<Vec<_>>().into()
                    )
                },
            }),
        })
    }

    fn top_stack(&self) -> Result<&StackFrame, RuntimeError> {
        self.stack
            .0
            .last()
            .ok_or(RuntimeError::Interpreter(format!("Empty stack!")))
    }

    fn top_stack_mut(&mut self) -> Result<&mut StackFrame, RuntimeError> {
        self.stack
            .0
            .last_mut()
            .ok_or(RuntimeError::Interpreter(format!("Empty stack!")))
    }

    fn create_inherited_stack_frame(&self) -> StackFrame {
        let mut sf = StackFrame {
            data: HashMap::new(),
        };
        match self.top_stack() {
            Err(_) => sf,
            Ok(top_stack_frame) => {
                for (ident, term) in top_stack_frame.data.iter() {
                    sf.data.insert(ident.clone(), term.clone());
                }
                sf
            }
        }
    }
}

impl StackFrame {
    /// Get a new non-colliding identifier for interpreter use
    fn get_interpreter_identifier(&self) -> Identifier {
        Identifier::Temp(
            self.data
                .keys()
                .filter_map(|i| match i {
                    Identifier::Unit(_s) => None,
                    Identifier::Temp(x) => Some(*x),
                })
                .max()
                .map(|x| x + 1)
                .unwrap_or(0 as u64),
        )
    }
}
impl Display for StackFrame {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "StackFrame {}\n{}\n{}",
            "{",
            self.data
                .iter()
                .map(|(k, (t, o))| format!("   {}: {}::{}", k, o, t))
                .reduce(|acc, x| acc + ",\n" + &x)
                .unwrap_or("".into()),
            "}"
        )
    }
}

impl ProgramAcquisitionService for ProgramRetriever {
    fn find(import: ImportStatement) -> Program {
        todo!("no idea how to import things yet")
    }
}

impl From<String> for RuntimeError {
    fn from(x: String) -> RuntimeError {
        RuntimeError::Semantic(x)
    }
}

impl From<Value> for EvalResult {
    fn from(v: Value) -> Self {
        EvalResult::Term(v)
    }
}
