use std::borrow::{Borrow, BorrowMut};
use std::collections::HashMap;
use crate::interpret::definitions::{HeapData, ProgramData, StackData, StackFrame};
use crate::lang_obj::{Identifier, Program, Statement};

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

pub struct Interpreter {
    /// a stack
    pub stack: StackData,

    /// a heap
    pub heap: HeapData,

    /// type data
    pub program_data: ProgramData,

    /// the program
    pub program: Program,

    /// other stuff:
    program_retriever: ProgramRetriever
}


impl Interpreter {
    pub fn new(program: Program, program_retriever: ProgramRetriever) -> Interpreter {
        Interpreter {
            // there is no such thing as a main function! ha!
            // no, we run this program, exactly as we have been told.
            stack: StackData(vec![]),
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

    pub fn start(&mut self) {
        // do any initialization steps
        self.stack.0.push(StackFrame {
            data: HashMap::new()
        });
        // run the program
        let mut stmts = vec![];
        std::mem::swap(&mut stmts, &mut self.program.content);
        self.interpret(&stmts);
        std::mem::swap(&mut self.program.content, &mut stmts);
    }

    fn interpret(&mut self, statements: &Vec<Statement>) {
        for stmt in statements {
            match stmt {
                Statement::Import(_) => todo!("can't import things yet"),
                Statement::Lambda(name, x, y, z) => todo!(),
                _ => todo!()
            }
        }
    }
}

impl ProgramAcquisitionService for ProgramRetriever {
    fn find(import: ImportStatement) -> Program {
        todo!("no idea how to import things yet")
    }
}