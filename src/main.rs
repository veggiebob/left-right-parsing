#[macro_use] extern crate maplit;

use std::cell::RefCell;
use std::collections::HashSet;
use std::hash::Hash;
use std::io::Write;
use std::ops::Add;
use std::process::exit;
use std::rc::Rc;
use std::sync::Arc;
use crate::lang_obj::{Expr, ParseError, Program, Statement};
use crate::lang_obj::formatting::{Format, HTML, JSON, Style, Text};
use crate::parse::*;
use crate::parse::structure_parsers::*;
use crate::program_parsing::ProgramParser;
use crate::semantic_validation::{ProgramValidator, Validator};

pub mod ez_parse;
pub mod parse;
pub mod lang_obj;
pub mod funcs;
pub mod semantic_validation;
pub mod interpret;

pub mod templates;

fn input() -> String {
    let mut content = String::new();

    // panic if for some reason the line cannot be read
    std::io::stdin().read_line(&mut content).unwrap();

    content
}

fn main() {
    crate::templates::language_1(run_main_program);
}

fn run_main_program(prgm_parser: ProgramParser, stmt_parser: Rc<StatementParser>, expr_parser: Rc<ExprParser>) {

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
            if args.len() > 4 {
                RunMode::Evaluation(args[4..].join(" ").to_string())
            } else {
                println!("Expected at least 4 arguments for evaluation mode");
                fail()
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

    let prgm_validator = ProgramValidator;
    let parse_validate_program = |obj: &String| {
        ParseResult(
            prgm_parser.parse(obj, true, ParseMetaData::new())
                .and_then(|hs| {
            let mut errors = String::new();
            let h = hs.into_iter().filter(|(prgm, used)| {
                let validation = prgm_validator.validate(prgm);
                if let Err(ref err) = validation {
                    errors += &*format!("{}\n", err);
                }
                validation.is_ok()
            }).collect::<HashSet<_>>();
            if h.len() > 0 {
                Ok(h)
            } else {
                Err(ParseError::from(format!("No parses matched after validation. Errors incurred: {}", errors)))
            }
        }))
    };


    match run_mode {
        RunMode::Interactive => {
            println!("Parse demo!");
            match parse_mode {
                ParseMode::Expression => println!("Input an expression."),
                ParseMode::Statement => println!("Input a statement."),
                ParseMode::Program => println!("Input a program. Enter '.' to do an evaluation.\n\
                    (Semantic validation enabled!)")
            }
            println!("Enter `:q` to leave.");
            let mut obj = String::new();
            loop {
                print!("> ");
                std::io::stdout().flush().unwrap(); // display >
                let input_line = input().trim().to_string(); // strip off extra space at beginning or end (parser is sensitive about that)
                if input_line == ":q".to_string() {
                    break;
                }
                let mut evaluating = false;
                if let ParseMode::Program = parse_mode {
                    if input_line.as_str() != "." {
                        obj += &*(input_line + "\n");
                        continue;
                    } else {
                        // println!("evaluating >{}<", obj);
                        evaluating = true;
                    }
                } else {
                    obj = input_line;
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
                        let res = parse_validate_program(&obj);
                        match &res.0 {
                            Ok(_) => {
                                println!(
                                    "{}",
                                    res.to_string()
                                );
                            },
                            Err(e) => println!("{:?}", e)
                        }
                    }
                }
                if evaluating {
                    obj = String::new();
                }
            }
            println!("Leaving demo.");
        },
        RunMode::Evaluation(content) => {
            let json_fmt = JSON::new(true);
            let html_fmt_o = HTML {
                style: Style {
                    color_wheel: vec![]
                }
            };
            let html_fmt = html_fmt_o.clone();

            type Formatter<T> = Box<dyn Fn(&T) -> String>;

            // create formatters
            let expr_fmt: Formatter<Expr> = if args.len() > 3 {
                let arg = args.get(3).unwrap();
                (match arg.as_str() {
                    "debug" => Box::new(|x: &Expr| x.to_string()) as Formatter<Expr>,
                    "json" => Box::new(move |x: &Expr| json_fmt.format(x).content) as Formatter<Expr>,
                    "js" => panic!("Sorry, js isn't implemented yet"),
                    "html" => Box::new(move |x: &Expr| html_fmt.clone().format(x).content) as Formatter<Expr>,
                    x => panic!("Expected argument 3 (format mode) to be one of ['format', 'json', 'js', 'html'], not {}", x)
                })
            } else {
                fail()
            };

            let html_fmt = html_fmt_o.clone();
            let stmt_fmt: Formatter<Statement> = if args.len() > 3 {
                let arg = args.get(3).unwrap();
                (match arg.as_str() {
                    "debug" => Box::new(|x: &Statement| x.to_string()) as Formatter<Statement>,
                    "json" => Box::new(move |x: &Statement| json_fmt.format(x).content) as Formatter<Statement>,
                    "js" => panic!("Sorry, js isn't implemented yet"),
                    "html" => Box::new(move |x: &Statement| html_fmt.format(x).content) as Formatter<Statement>,
                    x => panic!("Expected argument 3 (format mode) to be one of ['format', 'json', 'js', 'html'], not {}", x)
                })
            } else {
                fail()
            };

            let html_fmt = html_fmt_o.clone();
            let prgm_fmt = if args.len() > 3 {
                let arg = args.get(3).unwrap();
                (match arg.as_str() {
                    "debug" => Box::new(|x: &Program| x.to_string()) as Formatter<Program>,
                    "json" => Box::new(move |x: &Program| json_fmt.format(x).content) as Formatter<Program>,
                    "js" => panic!("Sorry, js isn't implemented yet"),
                    "html" => Box::new(move |x: &Program| html_fmt.format(x).content) as Formatter<Program>,
                    x => panic!("Expected argument 3 (format mode) to be one of ['format', 'json', 'js', 'html'], not {}", x)
                })
            } else {
                fail()
            };

            let content = content.trim();
            let content = String::from(content);
            // println!("content: '{}'", &content);
            // print the most program-readable stuff
            match parse_mode {
                ParseMode::Expression => {
                    println!(
                        "{}",
                        multi_format(
                            &ParseResult(
                                expr_parser.parse(&content, true, ParseMetaData::new())
                            ),
                            &expr_fmt
                        )
                    );
                },
                ParseMode::Statement => {
                    println!(
                        "{}",
                        multi_format(
                            &ParseResult(
                                stmt_parser.parse(&content, true, ParseMetaData::new())
                            ),
                            &stmt_fmt
                        )
                    );
                },
                ParseMode::Program => {
                    // program mode is special and needs an extra space for some reason? oh well
                    let res = parse_validate_program(&format!("{} ", &content));
                    println!(
                        "{}",
                        multi_format(
                            &res,
                            &prgm_fmt
                        )
                    );
                }
            }
        }
    }
}

fn multi_format<E: Hash + Eq, F: Fn(&E) -> String>(parsed: &ParseResult<E>, fmtr: &F) -> String {
    match &parsed.0 {
        Ok(ps) => {
            let mut root = String::new();
            for (e, _used) in ps {
                root = root + &fmtr(e) + "\n";
            }
            root
        },
        Err(e) => {
            format!("{:?}", e)
        }
    }
}

fn fail() -> ! {
    panic!("Expected 4 arguments: <i|e> <expr|stmt|prgm> <json|default|js|...> <content>")
}
