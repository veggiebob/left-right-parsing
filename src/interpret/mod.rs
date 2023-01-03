use std::borrow::{Borrow, BorrowMut};
use std::cell::{Ref, RefCell};
use std::collections::{HashMap, VecDeque};
use std::rc::Rc;
use crate::interpret::definitions::{HeapData, Kind, LanguageObject, ListObject, ProductObject, ProgramData, StackData, StackFrame, Term, TupleObject};
use crate::lang_obj::{Expr, Identifier, Program, Statement};

pub mod definitions;

#[cfg(test)]
mod test;

// finds and/or loads programs
// uhhhhh not really sure no super good definitions yet
// hang tight 👍
#[derive(Eq, PartialEq, Hash, Debug, Clone)]
pub struct ImportStatement {
    // ???
    pub name: Identifier // ??
}
trait ProgramAcquisitionService {
    fn find(import: ImportStatement) -> Program; // tentative
}

pub struct ProgramRetriever {
    // context?

}

type Variable = (Kind, Term);

#[derive(Debug, PartialEq, Eq)]
pub enum RuntimeError {
    Semantic(String),
    Interpreter(String) // an interpreter error. developer error.
}

#[derive(Debug, PartialEq)]
pub enum EvalResult {
    Term(Term),
    Call(Vec<StackFrame>, Vec<Statement>)
}

pub enum InterpreterAction {
    Return(Term),
    Call(Vec<StackFrame>, Vec<Statement>)
}

pub struct Interpreter {
    /// a stack containing scope data and variables
    pub stack: StackData,

    /// a heap
    pub heap: HeapData,

    /// a stack containing the remaining statements left to process
    /// in the program
    pub instruction_stack: Vec<Statement>,

    /// type data
    pub program_data: ProgramData,

    /// the program
    pub program: Program,

    /// other stuff:
    pub program_retriever: ProgramRetriever
}

/// temporary infix operator for concatenating lists, solely for interpreter use
const LIST_CONCAT: &str = "_list_concatenator";

impl Interpreter {
    pub fn new(program: Program, program_retriever: ProgramRetriever) -> Interpreter {
        Interpreter {
            // there is no such thing as a main function! ha!
            // no, we run this program, exactly as we have been told.
            stack: StackData(vec![]),
            instruction_stack: vec![],
            heap: HeapData {
                data: HashMap::new()
            },
            program_data: ProgramData {
                types: HashMap::new(),
                constants: HashMap::new() // globals
            },
            program,
            program_retriever
        }
    }

    pub fn start(&mut self) -> Result<Option<Term>, RuntimeError> {
        // do any initialization steps
        let sf = StackFrame {
            data: HashMap::new(),
            return_value: None
        };
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
        }

        Ok(return_value)
    }

    // return a (revised statement (the one currently being run), a new stackframe with necessary locals, statements inside new scope)
    fn interpret(&mut self, stmt: &Statement, return_value: Option<Term>) -> Result<Option<InterpreterAction>, RuntimeError> {
        return match stmt {
            Statement::Import(imt) => {
                todo!("interpret import statement!")
            },
            Statement::Impure(expr) => {
                if let Some(t) = return_value {
                    return Ok(None)
                }
                match self.evaluate(expr)? {
                    EvalResult::Term(_t) => Ok(None), // we don't care what it evaluates to
                    EvalResult::Call(sf, stmts) => {
                        Ok(Some(InterpreterAction::Call(sf, stmts)))
                    }
                }
            }
            Statement::Assignment(ident, expr) => {
                let stack = &self.stack;
                let contained_value = stack.0.last().unwrap().data.get(ident)
                    .ok_or(format!("Cannot assign a value {:?} that does not exist.", ident))?;
                match return_value {
                    None => {
                        match self.evaluate(expr)? {
                            EvalResult::Term(value) => {
                                self.stack.0.last_mut()
                                    .ok_or(RuntimeError::Interpreter("Empty stack!".into()))?
                                    .data.insert(ident.clone(), value);
                                // assignment has completed, no further steps needed
                                Ok(None)
                            },
                            EvalResult::Call(sf, stmts) => {
                                // evaluation calls for more computation.
                                Ok(Some(InterpreterAction::Call(sf, stmts)))
                            }
                        }
                    }
                    Some(x) => {
                        self.stack.0.last_mut()
                            .ok_or(RuntimeError::Interpreter("Empty stack!".into()))?
                            .data.insert(ident.clone(), x);
                        Ok(None)
                    }
                }
            }
            Statement::Let(ident, expr) => {
                if let Some(_) = self.stack.borrow().0.last().unwrap().data.get(ident) {
                    Err(RuntimeError::Semantic(format!("Tried to re-assign {:?}", ident)))
                } else {
                    if let Some(value) = return_value {
                        self.stack.0.last_mut().unwrap().data.insert(ident.clone(), value);
                        return Ok(None)
                    }
                    match self.evaluate(expr)? {
                        EvalResult::Call(sf, stmts) => {
                            Ok(Some(InterpreterAction::Call(sf, stmts)))
                        }
                        EvalResult::Term(t) => {
                            self.stack.0.last_mut().unwrap().data
                                .insert(ident.clone(), t);
                            Ok(None)
                        }
                    }
                }
            }
            Statement::Ret(expr) => {
                if let Some(value) = return_value {
                    return Ok(Some(InterpreterAction::Return(value)))
                }
                match self.evaluate(expr)? {
                    EvalResult::Term(t) => Ok(Some(InterpreterAction::Return(t))),
                    EvalResult::Call(sf, stmts) => {
                        // need to handle return statement in evaluate
                        Ok(Some(InterpreterAction::Call(sf, stmts)))
                    }
                }
            }
        }
    }

    fn evaluate(&self, expr: &Expr) -> Result<EvalResult, RuntimeError> {
        // Now, this is where this gets language-dependent.
        // There also has to be a bit of collaboration between the parser
        // and the interpreter.
        // Of course, supposing imports are allowed, there is much more
        // that can be created, so this can remain fairly bare bones.
        match expr {
            Expr::Nat(x) => Ok(Term::Nat(x.content).into()),
            Expr::Str(s) => Ok(Term::String(s.content.clone()).into()),
            Expr::Infix(left, op, right) => {
                return match self.evaluate(left)? {
                    EvalResult::Call(sfs, mut stmts) => {
                        let sf = StackFrame {
                            data: HashMap::new(),
                            return_value: None
                        };
                        let tmp_ident = sf.get_interpreter_identifier();

                        // the expression doesn't matter
                        // this captures the result of the left expression return statement
                        let capture = Statement::Let(tmp_ident.clone(),
                                                     Expr::Variable(Identifier::Unit(String::new())));

                        let ret = Statement::Ret(Expr::Infix(
                            Expr::Variable(tmp_ident.clone()).into(),
                            op.clone(),
                            right.clone()));

                        let mut stack_frames = vec![sf];
                        stack_frames.extend(sfs);

                        stmts.extend(vec![capture, ret]);

                        Ok(EvalResult::Call(stack_frames, stmts))
                    }
                    EvalResult::Term(left_term) => {
                        match self.evaluate(right)? {
                            EvalResult::Call(sfs, mut stmts) => {
                                let left_ident = Identifier::Temp(0);
                                let sf = StackFrame {
                                    data: {
                                        let mut h = HashMap::new();
                                        h.insert(left_ident.clone(), left_term);
                                        h
                                    },
                                    return_value: None
                                };
                                let right_ident = sf.get_interpreter_identifier();

                                // the expression doesn't matter
                                // this captures the result of the left expression return statement
                                let capture = Statement::Let(right_ident.clone(),
                                                             Expr::Variable(Identifier::Unit(String::new())));

                                let ret = Statement::Ret(Expr::Infix(
                                    Box::new(Expr::Variable(left_ident.clone())),
                                    op.clone(),
                                    Box::new(Expr::Variable(right_ident.clone()))
                                ));

                                let mut stack_frames = vec![sf];
                                stack_frames.extend(sfs);

                                stmts.extend(vec![capture, ret]);

                                Ok(EvalResult::Call(stack_frames, stmts))
                            },
                            EvalResult::Term(right_term) => {
                                match op.as_str() {
                                    "," => Ok(EvalResult::Term(
                                        Term::Object(
                                            LanguageObject::Product(
                                                ProductObject::Tuple(
                                                    TupleObject(vec![left_term, right_term])))
                                                .into()
                                        ))),
                                    "+" => todo!(),
                                    "$" | " " => {
                                        // call function!
                                        if let Term::Function((params, ret_t), body, ret, captured) = left_term {
                                            // left side is a function
                                            let err = Err(RuntimeError::Semantic(
                                                format!(
                                                    "Expected right side of operation to be a \
                                                    tuple")));
                                            if let Term::Object(o) = right_term {
                                                if let LanguageObject::Product(ProductObject::Tuple(args)) = *o {
                                                    let mut sf = StackFrame { // todo: inherit
                                                        data: HashMap::new(),
                                                        return_value: None
                                                    };
                                                    // todo: error for incorrect number of arguments
                                                    for cap in captured {
                                                        let c = self.top_stack()?.data.get(&cap)
                                                            .ok_or(RuntimeError::Interpreter(format!("Variable <<{:?}>> was supposedly captured, but does not exist", cap)))?
                                                            .clone();
                                                        sf.data.insert(cap, c);
                                                    }
                                                    for ((ident, _type), arg) in params.into_iter().zip(args.0.into_iter()) {
                                                        sf.data.insert(ident, arg);
                                                    }
                                                    let mut stmts: Vec<_> = body.into_iter().map(|s| *s).collect();
                                                    stmts.push(Statement::Ret(*ret));
                                                    Ok(EvalResult::Call(
                                                        vec![sf],
                                                        stmts
                                                    ))
                                                } else {
                                                    err
                                                }
                                            } else {
                                                err
                                            }
                                        } else {
                                            Err(RuntimeError::Semantic(format!("Expected left side of operation to be a function")))
                                        }
                                    }
                                    _ => Err(RuntimeError::Semantic(format!("Unrecognized operator: '{}'", op)))
                                }
                            }
                        }
                    }
                }
            }
            Expr::List(exprs) => {
                let mut exprs = exprs.clone();
                if let Some(last) = exprs.pop() {
                    let mut sf = StackFrame {
                        data: HashMap::new(),
                        return_value: None
                    };
                    let last_ident = sf.get_interpreter_identifier();

                    match self.evaluate(&*last)? {
                        EvalResult::Term(t) => {
                            sf.data.insert(
                                last_ident.clone(),
                                Term::Object(LanguageObject::Product(ProductObject::List(ListObject(vec![t]))).into())
                            );
                            let stmts = vec![
                                Statement::Ret(Expr::Infix(
                                    Box::new(Expr::List(exprs)),
                                    LIST_CONCAT.into(),
                                    Box::new(Expr::Variable(last_ident.clone()))
                                ))
                            ];
                            Ok(EvalResult::Call(vec![sf], stmts))
                        }
                        EvalResult::Call(sfs, mut stmts) => {
                            let sf = StackFrame {
                                data: HashMap::new(),
                                return_value: None,
                            };
                            let last_ident = sf.get_interpreter_identifier();

                            // captures return value
                            let capture = Statement::Let(last_ident.clone(), Expr::Str("".into()));
                            let ret = Statement::Ret(Expr::Infix(
                                Box::new(Expr::List(exprs)),
                                LIST_CONCAT.into(),
                                Box::new(Expr::List(vec![Box::new(Expr::Variable(last_ident.clone()))]))
                            ));
                            stmts.extend(vec![capture, ret]);
                            let mut stack_frames = vec![sf];
                            stack_frames.extend(sfs);
                            Ok(EvalResult::Call(stack_frames, stmts))
                        }
                    }
                } else {
                    Ok(EvalResult::Term(Term::Object(LanguageObject::Product(ProductObject::List(ListObject(vec![]))).into())))
                }
            },
            Expr::Variable(ident) => {
                // look it up in stack
                let value = self.stack.0.last()
                    .ok_or(RuntimeError::Interpreter("Empty stack??".into()))?
                    .data.get(ident)
                    .ok_or(RuntimeError::Semantic(format!("Unrecognized variable: <<{:?}>>", ident)))?;
                return Ok(EvalResult::Term(value.clone()));
            },
            Expr::Conditional(_, _, _) => todo!(),
            Expr::Function(sig, body, ret) => {
                return Ok(EvalResult::Term(Term::Function(
                    sig.clone(),
                    body.clone(),
                    ret.clone(),
                    self.top_stack()?.data.clone().into_keys().collect()
                )))
            },
            Expr::Lambda(sig, ret) => {
                return Ok(EvalResult::Term(Term::Function(
                    sig.clone(),
                    vec![],
                    ret.clone(),
                    self.top_stack()?.data.clone().into_keys().collect()
                )))
            },
        }
    }

    fn top_stack(&self) -> Result<&StackFrame, RuntimeError> {
        self.stack.0.last().ok_or(RuntimeError::Interpreter(format!("Empty stack!")))
    }

    fn top_stack_mut(&mut self) -> Result<&mut StackFrame, RuntimeError> {
        self.stack.0.last_mut().ok_or(RuntimeError::Interpreter(format!("Empty stack!")))
    }
}

impl StackFrame {

    /// Get a new non-colliding identifier for interpreter use
    fn get_interpreter_identifier(&self) -> Identifier {
        Identifier::Temp(self.data
            .keys()
            .filter_map(|i| match i {
                Identifier::Unit(_s) => None,
                Identifier::Temp(x) => Some(*x)
            })
            .max()
            .unwrap_or(0 as u64)
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

impl Into<EvalResult> for Term {
    fn into(self) -> EvalResult {
        EvalResult::Term(self)
    }
}