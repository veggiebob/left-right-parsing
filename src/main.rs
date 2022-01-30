#[macro_use] extern crate maplit;

use std::cell::RefCell;
use std::io::Write;
use std::ops::Add;
use std::process::exit;
use std::rc::Rc;
use crate::lang_obj::{Statement, Expr};
use crate::parse::*;
use crate::parse::structure_parsers::*;

pub mod parse;
pub mod lang_obj;
pub mod funcs;
pub mod semantic_validation;

#[cfg(test)]
pub mod test;

fn input() -> String {
    let mut content = String::new();

    // panic if for some reason the line cannot be read
    std::io::stdin().read_line(&mut content).unwrap();

    content
}

fn main() {

    // set up parsers
    // afaik this can't be done in a function reasonably with the structures I have

    // introduce the various configs for this parser:
    const INFIX_ADDITION_SYMBOL: &str = "+"; // for expressions, like "+" in "3 + 4"
    const INFIX_MULTIPLICATION_SYMBOL: &str = "*"; // for expressions like "*" in "3 * 4"
    const INFIX_SUBTRACTION_SYMBOL: &str = "-"; // for expressions like "-" in "4 - 3"
    const INFIX_FUNCTION_SYMBOL: &str = " "; // for expressions like "$" in "func $ [arg]"
    const INFIX_EQUALITY_SYMBOL: &str = "="; // for checking equality; for expressions like "=" in "x = y"
    const LIST_SEPARATOR_SYMBOL: char = ','; // for list expressions, like ',' in "[1, 2, 3]"
    const IDENTIFIER_ALLOWED_CHARS: &str = "_"; // also, by default, includes a-zA-Z
    const FUNCTION_ARGUMENT_SEPARATOR: char = ','; // seems simple
    const FUNCTION_ARGUMENT_INFIX_SYMBOL: &str = ":"; // like ":" in "func_name[arg1: type1, arg2: type2] => ..."

    ////////////////////////////////////////////////////
    // first, assemble only the finest expression parser
    ////////////////////////////////////////////////////

    // primitive parsers
    let string_parser = Rc::new(box_expr_parser!(StringParser()));
    let nat_parser = Rc::new(box_expr_parser!(NatParser()));
    let var_parser = Rc::new(box_expr_parser!(VariableParser::default(IdentifierParser::new(IDENTIFIER_ALLOWED_CHARS))));

    // start off with a couple simple parsers
    let root_parsers = RefCell::new(vec![
        &string_parser,
        &nat_parser,
        &var_parser
    ].into_iter().map(Rc::downgrade).collect());

    // create the expression parser
    let expr_parser = Rc::new(ExprParser {
        parsers: root_parsers
    });

    // create a couple recursive parsers
    let parenthetical_parser = Rc::new(box_expr_parser!(ParentheticalParser {
            expr_parser: Rc::clone(&expr_parser)
        }));
    let list_parser = Rc::new(box_expr_parser!(ListParser {
            separator: LIST_SEPARATOR_SYMBOL,
            expr_parser: Rc::clone(&expr_parser)
        }));
    let infix_addition = Rc::new(box_expr_parser!(InfixParser {
            expr_parser: Rc::clone(&expr_parser),
            infix: String::from(INFIX_ADDITION_SYMBOL),
        }));
    let infix_multiplication = Rc::new(box_expr_parser!(InfixParser {
            expr_parser: Rc::clone(&expr_parser),
            infix: String::from(INFIX_MULTIPLICATION_SYMBOL)
        }));
    let infix_invocation = Rc::new(box_expr_parser!(InfixParser {
            expr_parser: Rc::clone(&expr_parser),
            infix: String::from(INFIX_FUNCTION_SYMBOL)
        }));
    let infix_subtraction = Rc::new(box_expr_parser!(InfixParser {
            expr_parser: Rc::clone(&expr_parser),
            infix: String::from(INFIX_SUBTRACTION_SYMBOL)
        }));
    let infix_equality = Rc::new(box_expr_parser!(InfixParser {
            expr_parser: Rc::clone(&expr_parser),
            infix: String::from(INFIX_EQUALITY_SYMBOL)
        }));
    let condition_parser = Rc::new(box_expr_parser!(ConditionalParser {
            expr_parser: Rc::clone(&expr_parser)
        }));
    // add the recursive parsers
    expr_parser.parsers.borrow_mut().extend(vec![
        // single-instance
        &parenthetical_parser,
        &condition_parser,

        // configured
        &infix_addition,
        &infix_multiplication,
        &infix_subtraction,
        &infix_invocation,
        &infix_equality,
        &list_parser,
    ].into_iter().map(Rc::downgrade).collect::<Vec<_>>());

    /////////////////////////////////////////////////////
    // now, we append the statement parsers
    /////////////////////////////////////////////////////

    // create the shallow arg parser for function signatures
    let arg_name_parser = Rc::new(box_expr_parser!(VariableParser::default(IdentifierParser::new(IDENTIFIER_ALLOWED_CHARS))));
    let arg_type_parser = Rc::new(box_expr_parser!(VariableParser::default(IdentifierParser::new(IDENTIFIER_ALLOWED_CHARS))));
    let arg_name_type_parser = Rc::new(ExprParser {
        parsers: RefCell::new(vec![
            Rc::downgrade(&arg_name_parser),
            Rc::downgrade(&arg_type_parser)
        ])
    });
    let arg_infix = Rc::new(box_expr_parser!(InfixParser {
            expr_parser: Rc::clone(&arg_name_type_parser),
            infix: String::from(FUNCTION_ARGUMENT_INFIX_SYMBOL)
        }));
    let arg_parser = Rc::new(ExprParser {
        parsers: RefCell::new(vec![
            Rc::downgrade(&arg_infix)
        ])
    });
    let arg_parser = Rc::new(ListParser {
        expr_parser: arg_parser,
        separator: FUNCTION_ARGUMENT_SEPARATOR
    });

    let stmt_parser = Rc::new(StatementParser {
        parsers: RefCell::new(vec![])
    });

    // create function definition parser
    let fn_parser = Rc::new(box_stmt_parser!(FnDefParser {
            id_parser: Rc::new(IdentifierParser::new(IDENTIFIER_ALLOWED_CHARS)),
            statement_parser: Rc::clone(&stmt_parser),
            type_parser: Rc::new(TypeParser(IdentifierParser::new(IDENTIFIER_ALLOWED_CHARS))),
            expr_parser: Rc::clone(&expr_parser),
            arg_parser
        }));

    // create let statement parser
    let let_parser = Rc::new(box_stmt_parser!(LetParser {
            id_parser: Rc::new(IdentifierParser::new(IDENTIFIER_ALLOWED_CHARS)),
            expr_parser: Rc::clone(&expr_parser)
        }));

    stmt_parser.parsers.borrow_mut().extend(vec![
        Rc::downgrade(&fn_parser),
        Rc::downgrade(&let_parser)
    ]);

    let args: Vec<_> = std::env::args().collect();

    enum ParseMode {
        Expression,
        Statement,
        Program
    };

    enum RunMode {
        Interactive,
        Evaluation(String)
    };

    // 'i' for interactive
    let run_mode = if args.len() > 1 {
        let arg = args.get(1).unwrap();
        if arg == "i" {
            RunMode::Interactive
        } else if arg == "e" {
            if args.len() > 3 {
                RunMode::Evaluation(args[3..].join(" ").to_string())
            } else {
                panic!("Expected a third argument for evaluation mode")
            }
        } else {
            panic!("Expected 'i' or 'e' for a run mode.");
        }
    } else {
        fail()
    };

    let parse_mode = if args.len() > 2 {
        let arg = args.get(2).unwrap();
        if arg == "expr" {
            ParseMode::Expression
        } else if arg == "stmt" {
            ParseMode::Statement
        } else if arg == "prgm" {
            ParseMode::Program
        } else {
            fail()
        }
    } else {
        fail()
    };

    match run_mode {
        RunMode::Interactive => {
            println!("Parse demo!");
            match parse_mode {
                ParseMode::Expression => println!("Input an expression."),
                ParseMode::Statement => println!("Input a statement."),
                ParseMode::Program => panic!("Programs not implemented yet! Sorry!")
            }
            println!("Enter `:q` to leave.");
            loop {
                print!("> ");
                std::io::stdout().flush().unwrap(); // display >
                let obj = input().trim().to_string(); // strip off extra space at beginning or end (parser is sensitive about that)
                if obj == ":q".to_string() {
                    break;
                }
                // if anything, print the most user-friendly part here
                match parse_mode {
                    ParseMode::Expression => {
                        println!(
                            "{}",
                            ParseResult(
                                expr_parser.parse(&obj, true, ParseMetaData::new())
                            ).to_string()
                        );
                    },
                    ParseMode::Statement => {
                        println!(
                            "{}",
                            ParseResult(
                                stmt_parser.parse(&obj, true, ParseMetaData::new())
                            ).to_string()
                        );
                    },
                    ParseMode::Program => {
                        panic!("Program parse mode still in progress!");
                    }
                }
            }
            println!("Leaving demo.");
        },
        RunMode::Evaluation(content) => {
            let content = content.trim();
            let content = String::from(content);
            // print the most program-readable stuff
            match parse_mode {
                ParseMode::Expression => {
                    println!(
                        "{}",
                        ParseResult(
                            expr_parser.parse(&content, true, ParseMetaData::new())
                        ).to_string()
                    );
                },
                ParseMode::Statement => {
                    println!(
                        "{}",
                        ParseResult(
                            stmt_parser.parse(&content, true, ParseMetaData::new())
                        ).to_string()
                    );
                },
                ParseMode::Program => {
                    panic!("Program parse mode still in progress!");
                }
            }
        }
    }
}

fn fail() -> ! {
    panic!("Expected 2 arguments: <i|e> <expr|stmt|prgm> <content>")
}