use std::cell::RefCell;
use std::collections::HashSet;
use std::process::Output;
use std::rc::{Rc, Weak};
use std::task::Context;
use crate::funcs::{char_at, expect_str, take, take_while};
use crate::lang_obj::{Expr, LONat, LOString, ParseError};

pub trait Parser {

    /// Type of object to produce during parsing
    type Output;

    /// Use this parser to generate an object, or not.
    /// If so, return the object and how many characters were used.
    /// Otherwise, report the error.
    /// consume:  whether or not the parser should consume the string.
    /// NOTE: true meaning it MUST consume the ENTIRE string
    /// false means nothing; it's indeterminate about the behavior of the parsing
    fn parse(&self, content: &String, consume: bool, context: ParseMetaData) -> Result<HashSet<(Self::Output, usize)>, ParseError>;
}

pub type AnyParser = Box<dyn Parser<Output=Expr>>;

#[macro_export]
macro_rules! create_parser {
    ($parser:expr) => {
        Box::new($parser) as Box<dyn Parser<Output=Expr>>
    }
}

pub struct ExprParser {
    pub parsers: RefCell<Vec<Weak<AnyParser>>>
}
pub struct StringParser();
pub struct NatParser();

/// Because of the way parsing works,
/// all infix operators are right-associative.
/// That means the first element is always treated as a term when there is some ambiguity.
/// For example, "1 + 2 + 3" will be parsed as "1 + (2 + 3)"
/// This cannot be changed with the current implementation.
pub struct InfixParser {
    pub expr_parser: Rc<ExprParser>,
    pub infix: String,
}
pub struct ParentheticalParser {
    pub expr_parser: Rc<ExprParser>
}

/// For miscellaneous parsing data.
/// To ensure scope-consistency, I'm not gonna pass this mutably anywhere
#[derive(Clone, Copy)]
pub struct ParseMetaData {
    pub depth: u32,
    pub was_infix: bool
}

impl Parser for ExprParser {
    type Output = Expr;

    fn parse(&self, content: &String, consume: bool, meta: ParseMetaData) -> Result<HashSet<(Self::Output, usize)>, ParseError> {
        // in order to parse an expression, we need to know how to parse it.
        // what are we going to expect?
        // in the face of elegant syntax, we can't expect anything, we just need to try possibilities
        // if there's an ambiguous result, we throw an error! it can't be parsed.

        // there are relatively few expression patterns, so we will just try all of them!
        // bonus: can users define their own expression patterns?
        let mut out = HashSet::new();
        let mut err_messages = "".to_string();
        for parser in self.parsers.borrow().iter() {
            match parser.upgrade() {
                Some(parser) => {
                    match parser.parse(content, consume, meta.increment_depth_no_change_infix()) {
                        Ok(result) => {
                            for r in result {
                                out.insert(r);
                            }
                        }
                        Err(e) => {
                            // println!("Unable to parse because {:?}", e);
                            // fail silently (unless debugging) because failures might not mean anything
                            err_messages += &*("\n".to_string() + e.message.as_str());
                        }
                    }
                },
                None => {
                    println!("Skipping over a parser because its pointer has degraded!");
                }
            };
        }
        let possible_parses = out.len();
        if possible_parses == 0 {
            Err(format!("No valid expressions. {} tried. {}", self.parsers.borrow().len(), err_messages).into())
        } else {
            Ok(out)
        }
    }
}

impl Parser for StringParser {
    type Output = Expr;
    // naive implementation with the assumption that no double quotes (") are present in the string
    fn parse(&self, content: &String, consume: bool, meta: ParseMetaData) -> Result<HashSet<(Self::Output, usize)>, ParseError> {
        if let Some((q, s)) = crate::funcs::take(content, 1) {
            if q != "\"" {
                Err(ParseError {
                    location: Some(0),
                    range: None,
                    message: "Expected a \" to start string.".to_string()
                })
            } else {
                let (string_content, end) = crate::funcs::take_while(&s, |&c| c != '"');
                let str_len = string_content.len();
                if end.len() == 0 || crate::funcs::char_at(&end, 0).unwrap() != '"' {
                    Err(ParseError {
                        location: Some(1 + str_len + 1),
                        range: Some((0, 1 + str_len + 1)),
                        message: "Expected a closing \" to end string literal.".to_string()
                    })
                } else {
                    let used = 1 + str_len + 1;
                    if consume && used < content.len() {
                        Err(
                            "Could not consume all content when parsing string.".into()
                        )
                    } else {
                        Ok(hashset![(
                            Expr::Str(LOString {
                                content: string_content
                            }),
                            used
                        )])
                    }
                }
            }
        } else {
            Err(ParseError {
                location: Some(0),
                range: None,
                message: "Expected a string longer than length 1.".to_string()
            })
        }
    }
}

impl Parser for NatParser {
    type Output = Expr;

    fn parse(&self, content: &String, consume: bool, meta: ParseMetaData) -> Result<HashSet<(Self::Output, usize)>, ParseError> {
        let (n, _) = crate::funcs::take_while(content, |c| c.is_digit(10));
        if n.len() > 0 {
            if consume && n.len() < content.len() {
                Err("Could not consume entire content when parsing nat.".into())
            } else {
                Ok(hashset![(Expr::Nat(LONat {
                        content: n.parse().unwrap()
                    }),
                    n.len()
                )])
            }
        } else {
            Err(ParseError {
                location: Some(0),
                range: None,
                message: "Expected digit".to_string()
            })
        }
    }
}

impl Parser for InfixParser {
    type Output = Expr;

    fn parse(&self, content: &String, consume: bool, meta: ParseMetaData) -> Result<HashSet<(Self::Output, usize)>, ParseError> {
        // <expr> <infix> <expr>

        if meta.was_infix {
            return Err(
                format!("Just parsed an infix, not going to parse another one!").into()
            )
        }

        // parse first expression
        // not consuming because even if it's ordered to consume all, the decision happens lower
        let meta = meta.increment_depth();
        self.expr_parser.parse(content, false, meta.with_infix()).and_then(|parses| {
            let good_parses = parses.into_iter()
                .map(|(expr, used)| {
                    let rest = take(content, used).unwrap().1;
                    let (b, rest) = take_while(&rest, |c| c.is_whitespace());
                    let index = used + b.len();
                    let used = used + b.len() + self.infix.len();
                    // parse infix
                    expect_str(&rest, self.infix.as_str(),
                               ParseError {
                                   location: Some(index),
                                   range: None,
                                   message: format!("Expected an operator {}", self.infix)
                               },
                        "Expected more string space".into()
                    ).map(|rest| {
                        let (white, rest) = take_while(&rest, |c| c.is_whitespace());
                        (expr, rest, used + white.len())
                    })
                })
                .filter_map(|res| {
                    // parse next expression (through errors)
                    res
                        .map(|(expr1, str, used1)| {
                            // now, here, we must consume if it is wished
                            self.expr_parser.parse(&str, consume, meta)
                                .map(|parses|
                                    parses.into_iter()
                                        // filter out results if it doesn't consume the rest of the string
                                        // I'm not sure if this is necessary, but I'll leave it here
                                        .filter(|p| !consume || p.1 == str.len())
                                        .map(|(expr2, used2)| {
                                            (
                                                Expr::Infix(
                                                    Box::new(expr1.clone()),
                                                    self.infix.clone(),
                                                    Box::new(expr2)
                                                ),
                                                used1 + used2
                                            )
                                        })
                                        .collect::<HashSet<_>>()
                                )
                        })
                        .ok()
                })
                .filter_map(Result::ok)
                .flatten()
                .collect::<HashSet<_>>();
            if good_parses.len() == 0 {
                Err("No possible parses".into())
            } else {
                Ok(good_parses)
            }
        })
    }
}

impl Parser for ParentheticalParser {
    type Output = Expr;

    fn parse(&self, content: &String, consume: bool, meta: ParseMetaData) -> Result<HashSet<(Self::Output, usize)>, ParseError> {
        if let Some('(') = char_at(content, 0) {
            // find the matching parenthesis
            let depth = &mut 0;
            // includes '(', does not include ')'
            let expr_str = take_while(content, |&c| {
                if c == '(' {
                    *depth += 1;
                } else if c == ')' {
                    *depth -= 1;
                }
                *depth > 0
            });
            if *depth > 0 {
                Err("Could not find matching ')'".into())
            } else {
                if consume && expr_str.0.len() + 1 < content.len() {
                    Err(
                        "Could not consume entire content when parsing parenthetical expression".into()
                    )
                } else {
                    self.expr_parser.parse(
                        &take(&expr_str.0, 1).unwrap().1, // remove the first '('
                        true, meta.increment_depth())
                        .map(|solutions| {
                            solutions.into_iter().map(
                                |(expr, used)| {
                                    (expr, used + 2) // include parentheses
                                }
                            ).collect()
                        })
                }
            }
        } else {
            Err("Expected '(', got nothing".into())
        }
    }
}


impl ParseMetaData {
    pub fn new() -> ParseMetaData {
        ParseMetaData {
            depth: 0,
            was_infix: false
        }
    }

    /// increments depth. NOTE: RESETS WAS_INFIX FOR CONVENIENCE
    pub fn increment_depth(self) -> ParseMetaData {
        let mut meta = self;
        meta.depth += 1;
        meta.was_infix = false;
        meta
    }

    /// same as incrementing depth but doesn't change was_infix
    pub fn increment_depth_no_change_infix(self) -> ParseMetaData {
        if self.was_infix {
            self.increment_depth().with_infix()
        } else {
            self.increment_depth()
        }
    }

    /// indicates that an infix was just passed
    pub fn with_infix(self) -> ParseMetaData {
        let mut meta = self;
        meta.was_infix = true;
        meta
    }
}