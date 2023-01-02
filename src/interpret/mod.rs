use std::borrow::{Borrow, BorrowMut};
use std::cell::{Ref, RefCell};
use std::collections::{HashMap, VecDeque};
use crate::interpret::definitions::{HeapData, ProgramData, StackData, StackFrame, Term};
use crate::lang_obj::{Expr, Identifier, Program, Statement};

pub mod definitions;

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

pub enum RuntimeError {
    Semantic(String),
    Interpreter(String) // an interpreter error. developer error.
}

pub enum EvalResult {
    Term(Term),
    Call(StackFrame, Vec<Statement>)
}

pub enum InterpreterAction {
    Return(Term),
    Call(StackFrame, Vec<Statement>)
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

    pub fn start(&mut self) -> Result<(), RuntimeError> {
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
                self.stack.0.push(sf); // add the stack frame
            } else if let Some(InterpreterAction::Return(term)) = res {
                return_value = Some(term);
                let _lost = self.stack.0.pop(); // drop the top stack frame
            }
        }

        Ok(())
    }

    // fn get_stack_value(&self, ident: &Identifier) -> Option<&RefCell<Term>> {
    //     self.stack.borrow_mut().0.last().unwrap().data.get(ident)
    // }

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
                                *contained_value.borrow_mut() = value;
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
                        *contained_value.borrow_mut() = x;
                        Ok(None)
                    }
                }
            }
            Statement::Let(ident, expr) => {
                if let Some(_) = self.stack.borrow().0.last().unwrap().data.get(ident) {
                    Err(RuntimeError::Semantic(format!("Tried to re-assign {:?}", ident)))
                } else {
                    if let Some(value) = return_value {
                        self.stack.0.last_mut().unwrap().data.insert(ident.clone(), RefCell::new(value));
                        return Ok(None)
                    }
                    match self.evaluate(expr)? {
                        EvalResult::Call(sf, stmts) => {
                            Ok(Some(InterpreterAction::Call(sf, stmts)))
                        }
                        EvalResult::Term(t) => {
                            self.stack.0.last_mut().unwrap().data
                                .insert(ident.clone(), RefCell::new(t));
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
                match op.as_str() {
                    "+" => {
                        // see if there is an implementation that exists for these two objects
                        // this requires checking their types, I guess
                        let left_t = self.evaluate(left)?;
                        let right_t = self.evaluate(right)?;
                        // are they addable?
                        todo!()
                    },
                    _ => Err(RuntimeError::Semantic(format!("Unrecognized operator: '{}'", op)))
                }
            }
            Expr::List(_) => todo!(),
            Expr::Variable(_) => todo!(),
            Expr::Conditional(_, _, _) => todo!(),
            Expr::Function(_, _, _, _) => todo!(),
            Expr::Lambda(_, _, _) => todo!(),
        }
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