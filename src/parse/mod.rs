use std::cell::RefCell;
use std::cmp::min;
use std::collections::HashSet;
use std::hash::Hash;
use std::iter::TakeWhile;
use std::ops::Add;
use std::process::Output;
use std::rc::{Rc, Weak};
use std::task::Context;
use crate::ez_parse::funcs::{CatParser, concat};
use crate::ez_parse::cycles::*;

use crate::funcs::{char_at, expect_str, take, take_while};
use crate::lang_obj::{Expr, LONat, LOString, ParseError};
use crate::lang_obj::Expr::Infix;

pub mod structure_parsers;
pub mod program_parsing;

pub trait Parser {
    /// Type of object to produce during parsing
    type Output;

    /// Use this parser to generate an object, or not.
    /// If so, return the object and how many characters were used.
    /// Otherwise, report the error.
    /// consume:  whether or not the parser should consume the string.
    /// NOTE: true meaning it MUST consume the ENTIRE string
    /// false means nothing; it's indeterminate about the behavior of the parsing
    fn parse(&self,
             content: &String,
             consume: bool,
             context: ParseMetaData) -> Result<HashSet<(Self::Output, usize)>, ParseError>;

    fn parse_all(&self, content: &str) -> Result<HashSet<(Self::Output, usize)>, ParseError> {
        let s = content.to_owned();
        self.parse(&s, true, ParseMetaData::new())
    }
}

/// Optional-use struct. Provides more implementation for the output of a parse operation.
#[derive(Eq, PartialEq, Debug)]
pub struct ParseResult<O: Hash + Eq>(pub Result<HashSet<(O, usize)>, ParseError>);

impl<O: Hash + Eq + Clone> Clone for ParseResult<O> {
    fn clone(&self) -> Self {
        ParseResult(self.0.clone())
    }
}

impl<T: Hash + Eq> ParseResult<T> {
    pub fn empty() -> ParseResult<bool> {
        ParseResult(Ok(hashset![(true, 0)]))
    }
    pub fn len(&self) -> usize {
        match &self.0 {
            Ok(hs) => hs.len(),
            Err(_err) => 0
        }
    }
    pub fn filter_inner<F: Fn(&(T, usize)) -> bool>(self, f: F) -> Self {
        ParseResult(self.0.map(|hs| hs.into_iter().filter(|e|
            f(e)
        ).collect()))
    }
}

impl<T: ToString + Hash + Eq + Clone> ToString for ParseResult<T> {
    fn to_string(&self) -> String {
        match self.simplified_results() {
            Ok(hs) => {
                if hs.len() > 1 {
                    let mut out = format!("{} possibilities:\n", hs.len());
                    for e in hs {
                        // out += & *format!("{} <<<{}>>>", e.to_string(), used);
                        out += &*format!("{}", e.to_string());
                        out += "\n";
                    }
                    out
                } else if hs.len() == 0 {
                    panic!("This should be impossible!");
                } else {
                    let mut out = String::new();
                    for e in hs {
                        out = e.to_string();
                        break;
                    }
                    out
                }
            },
            Err(err) => format!("{:?}", err)
        }
    }
}

impl<T: Hash + Eq> ParseResult<T> {
    pub fn map_inner<X: Hash + Eq, F: Fn(T) -> X>(self, f: F) -> ParseResult<X> {
        ParseResult(
            self.0.map(
                |hs|
                    hs.into_iter()
                        .map(|(e, used)| (f(e), used)).collect()
            )
        )
    }
}

impl<T: Hash + Eq + Clone> ParseResult<T> {
    /// quick way to combine parses.
    /// if you just do the types, it should be pretty straight forward
    pub fn chain<S, F, C, R>(
        self,
        content: &String,
        consume: bool,
        context: ParseMetaData,
        parse_func: F,
        constructor: C
    ) -> ParseResult<R>
        where F: Fn(&T, String, ParseMetaData) -> Option<Vec<(S, usize)>>,
              S: Hash + Eq,
              C: Fn(T, S) -> R,
              R: Hash + Eq
    {
        let results: Result<HashSet<_>, ParseError> = self.0
            .and_then(
                // if it wasn't an error to begin with
                |ps| {
                    let ps: HashSet<(R, usize)> = ps.into_iter().filter_map(
                        |(e, used1)| {
                            // if used1 >= content.len() {
                            //     Err(ParseError::from("")) // expected more to be parsed!
                            // epsilon parser!
                            // } else {
                                // take the rest of the string (after one of the previous parse results)
                                if let Some((_, rest)) = take(content, used1) {
                                    // calculate the possibilities based on this parse
                                    parse_func(&e, rest, context.clone()).map( // if there are any
                                        |rs|
                                            rs.into_iter()
                                                // add the usages together
                                                .map(|(s, used2)| (s, used1 + used2))
                                                // filter out the ones that don't consume everything if they were supposed to
                                                .filter(|(s, used)| !consume || *used >= content.len())
                                                // add the two parse results together with function
                                                .map(|(s, used)| (constructor(e.clone(), s), used))
                                                .collect::<Vec<(R, usize)>>()
                                    )
                                } else {
                                    None // for some reason there were not enough characters left
                                }
                            // }
                        }
                    )
                        .flatten()
                        .collect();
                    if ps.len() > 0 {
                        Ok(ps)
                    } else {
                        Err("No possibilities".into())
                    }
                }
            );
        ParseResult(results)
    }

    pub fn parse_any_whitespace(self, content: &String, consume: bool, context: ParseMetaData) -> ParseResult<T> {
        let any_whitespace = TakeWhileParser::whitespace(LengthQualifier::GEQ(0));
        self.chain( // parse any amount of space (including 0)
            content,
            consume,
            context.clone(),
            chainable(|_x, next, meta| {
                any_whitespace.parse(&next, consume, context.clone())
            }),
            |x, _| x
        )
    }

    pub fn parse_some_whitespace(self, content: &String, consume: bool, context: ParseMetaData) -> ParseResult<T> {
        let some_whitespace = TakeWhileParser::whitespace(LengthQualifier::GEQ(1));
        self.chain( // parse any amount of space (including 0)
            content,
            consume,
            context.clone(),
            chainable(|_x, next, meta| {
                some_whitespace.parse(&next, consume, context.clone())
            }),
            |x, _| x
        )
    }

    pub fn parse_static_text(self, content: &String, consume: bool, context: ParseMetaData, phrase: &str) -> ParseResult<T> {
        self.chain(
            content,
            consume,
            context,
            |_x, next, _meta| {
                expect_str::<ParseError>(&next, phrase,
                    format!("Expected a \"{}\"", phrase).into(),
                    format!("Expected more characters, specifically \"{}\"", phrase).into())
                    .map(|_| vec![((), phrase.len())]).ok()
            },
            |x, _| x
        )
    }

    pub fn parse_expression<R, F>(self,
                            content: &String,
                            consume: bool,
                            context: ParseMetaData,
                            expr_parser: Rc<ExprParser>,
                            constructor: F
    ) -> ParseResult<R>
        where F: Fn(T, Expr) -> R,
              R: Hash + Eq
    {
        self.chain(
            content,
            consume,
            context.clone(),
            chainable(|_prev, next, _meta| {
                expr_parser.parse(&next, consume, context.clone())
            }),
            constructor
        )
    }

    /// by being able to clone both, you can add them together
    pub fn merge(&self, other: &ParseResult<T>) -> ParseResult<T> {
        match &self.0 {
            Ok(stuff) => {
                let mut s = stuff.clone();
                match &other.0 {
                    Ok(stuff2) => {
                        for e in stuff2.clone() {
                            s.insert(e);
                        }
                    },
                    Err(e) => {}
                }
                ParseResult(Ok(s))
            },
            Err(err) => {
                match &other.0 {
                    Ok(stuff) => ParseResult(Ok(stuff.clone())),
                    Err(err2) => ParseResult(Err(
                        ParseError {
                            location: None,
                            range: None,
                            message: format!("{}\n{}", err.message, err2.message)
                        }
                    ))
                }
            }
        }
    }

    pub fn simplified_results(&self) -> Result<HashSet<T>, ParseError> {
        self.0.as_ref()
            .map_err(|e| e.clone())
            .map(|hs|
                hs.iter().map(|(x, _used)| x.clone()).collect()
            )
    }
}

/// function to convert the usual parse function into the acceptable chain function
/// The reason I didn't change the "chain" function is because I wanted it to be
/// more generic and support more formats of parsing besides those functions
/// that strictly implement the Parser trait
pub fn chainable<S, F, T>(f: F) -> impl Fn(&T, String, ParseMetaData) -> Option<Vec<(S, usize)>>
    where F: Fn(&T, String, ParseMetaData) -> Result<HashSet<(S, usize)>, ParseError>,
{
    move |i, content, meta| {
        f(i, content, meta).ok().map(|e| e.into_iter().collect())
    }
}

pub type GenericExprParser = Box<dyn Parser<Output=Expr>>;

#[macro_export]
macro_rules! box_expr_parser {
    ($parser:expr) => {
        Box::new($parser) as Box<dyn Parser<Output=Expr>>
    }
}

pub struct ExprParser {
    pub parsers: RefCell<Vec<Weak<GenericExprParser>>>
}

/// produces LO Strings
#[derive(Clone, Copy)]
pub struct LOStringParser();

/// Produces actual strings
#[derive(Clone, Copy)]
pub struct StringParser();

#[derive(Clone, Copy)]
pub struct LONatParser();

#[derive(Clone, Copy)]
pub struct NatParser();

pub struct ListParser {
    pub expr_parser: Rc<ExprParser>,
    pub separator: char
}

/// Because of the way parsing works,
/// all infix operators are right-associative.
/// That means the first element is always treated as a term when there is some ambiguity.
/// For example, "1 + 2 + 3" will be parsed as "1 + (2 + 3)"
/// This cannot be changed with the current implementation.
pub struct InfixParser {
    pub expr_parser: Rc<ExprParser>,
    pub infix: String,
}

/// Parse very simple things, just expressions inside parenthesis
/// example: "(123)"
///          "(9 + 21)"
pub struct ParentheticalParser {
    pub expr_parser: Rc<ExprParser>
}

/// Parse if statements.
pub struct ConditionalParser {
    pub expr_parser: Rc<ExprParser>
}

/// For miscellaneous parsing data.
/// To ensure scope-consistency, I'm not gonna pass this mutably anywhere
#[derive(Clone, Debug)]
pub struct ParseMetaData {
    pub depth: u32,
    /// Represents when the last iteration involved a zero-width parse operation.
    /// Zero-width parse operations are only allowed once, which forces right-associativity
    /// for infix operations. A zero-width parse operation occurs if the parse operation
    /// takes a recursive step before "consuming" any characters.
    /// Example: Parsing the expression `A + B` (where `A` and `B` are expressions) requires
    /// that a recursive step be taken immediately, so no characters are consumed. If this
    /// flag is not in place properly, stack overflow and infinite recursion *will* occur.
    pub was_infix: bool,

    pub path: Vec<ParseDecision>,
    pub last_path: Vec<ParseDecision>,
    pub history: Vec<Vec<ParseDecision>>,
}

#[derive(Clone, Debug, Copy, Eq, PartialEq, Hash)]
pub enum ParseDecision {
    UnionLeft,
    UnionRight,
    Recur
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
        let mut num_tried = 0;
        for parser in self.parsers.borrow().iter() {
            match parser.upgrade() {
                Some(parser) => {
                    num_tried += 1;
                    match parser.parse(content, consume, meta.clone().increment_depth_no_change_infix()) {
                        Ok(result) => {
                            for r in result {
                                out.insert(r);
                            }
                        },
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
            Err(format!("No valid expressions. {} tried. {}", num_tried, err_messages).into())
        } else {
            Ok(out)
        }
    }
}

impl Parser for LOStringParser {
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


impl Parser for StringParser {
    type Output = String;
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
                        Ok(hashset![(string_content, used)])
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

impl Parser for LONatParser {
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

impl Parser for NatParser {
    type Output = u64;

    fn parse(&self, content: &String, consume: bool, meta: ParseMetaData) -> Result<HashSet<(Self::Output, usize)>, ParseError> {
        let (n, _) = crate::funcs::take_while(content, |c| c.is_digit(10));
        if n.len() > 0 {
            if consume && n.len() < content.len() {
                Err("Could not consume entire content when parsing nat.".into())
            } else {
                Ok(hashset![(n.parse().unwrap(), n.len())])
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
        ParseResult(self.expr_parser.parse(&content, false, meta.clone().with_infix()))
            .parse_any_whitespace(&content, false, meta.clone())
            .parse_static_text(&content, false, meta.clone(), &self.infix)
            .parse_any_whitespace(&content, false, meta.clone())
            .chain(
                &content,
                consume,
                meta.clone().increment_depth(),
                chainable(|_prev_expr, next, meta| {
                    self.expr_parser.parse(&next, consume, meta)
                }),
                |expr1, expr2| {
                    Infix(
                        expr1.into(),
                        self.infix.clone(),
                        expr2.into()
                    )
                }
            )
            .0
    }
}

impl Parser for ParentheticalParser {
    type Output = Expr;

    fn parse(&self, content: &String, consume: bool, meta: ParseMetaData) -> Result<HashSet<(Self::Output, usize)>, ParseError> {
        // (<expr>)
        if let Some('(') = char_at(content, 0) {
            if let Some((_, rest)) = take(&content, 1) {
                // '(' used
                // even though we're only excluding one character, we can't guarantee it has a matching ')'
                self.expr_parser.parse(&rest, false, meta.increment_depth())
                    .and_then(|possibles| {
                        let ps: HashSet<(Expr, usize)> = possibles.into_iter()
                            .filter_map(|(expr, used)| {
                                    if let Some(')') = take(&rest, used).and_then(|s| char_at(&s.1, 0)) {
                                        // complete parse!
                                        let used = 2 + used; // include parenthesis
                                        if consume && used < rest.len() {
                                            None
                                        } else {
                                            Some((
                                                expr,
                                                used
                                            ))
                                        }
                                    } else {
                                        None
                                    }
                            })
                            .collect();
                        if ps.len() == 0 {
                            Err("No possibilities!".into())
                        } else {
                            Ok(ps)
                        }
                    })
            } else {
                Err("String is not long enough.".into())
            }
        } else {
            Err("Expected '(', got nothing".into())
        }
    }
}

impl Parser for ListParser {
    type Output = Expr;

    fn parse(&self, content: &String, consume: bool, context: ParseMetaData) -> Result<HashSet<(Self::Output, usize)>, ParseError> {
        let whitespace_parser = TakeWhileParser::whitespace(LengthQualifier::GEQ(0)); // consume any amount of whitespace!
        expect_str(content,
                   "[",
                    "ListParser: expecting '['".into(),
                        "ListParser: expected longer string".into())
            .and_then(|rest| {

                // check if it's empty
                let empty_list = ParseResult::<bool>::empty()
                    .parse_any_whitespace(&rest, false, context.clone())
                    .parse_static_text(&rest, consume, context.clone(), "]");
                if empty_list.0.is_ok() {
                    return empty_list.map_inner(|_b| Expr::List(vec![])).0
                        .map(|hs|
                        hs.into_iter().map(|(e, used)| (e, used + 1)).collect())
                }

                // used 1
                let meta = context.increment_depth();

                // this first step seems redundant, but it's easier to compare to what the
                // next item in the list is
                let mut parsed = match self.expr_parser
                    .parse(&rest, false, meta.clone()) {
                    Ok(p) => p.into_iter().map(
                        |(expr, used)| {
                            (vec![expr], used)
                        }
                    ).collect::<HashSet<_>>(),
                    Err(e) => return Err(e)
                };
                let mut finished = HashSet::new();
                while parsed.len() > 0 {
                    parsed = parsed.into_iter().filter_map(
                        |(exprs, prev_used)| {
                            if let Some((_prev_exprs, next)) = take(&rest, prev_used) {
                                match char_at(&next, 0) {
                                    Some(c) => {
                                        if c == self.separator {
                                            // branch!
                                            if let Some((_comma, next)) = take(&next, 1) { // take off a comma
                                                let result = whitespace_parser.parse(&next, false, meta.clone());
                                                let result = ParseResult(result);
                                                result.chain(
                                                    &next,
                                                    false,
                                                    meta.clone(),
                                                    chainable(|_spaces, next, meta|
                                                        self.expr_parser.parse(&next, false, meta)
                                                            .map(|ps|
                                                                // add 1 for the unaccounted-for comma
                                                                ps.into_iter()
                                                                    // add previous expression length + 1 for comma
                                                                    .map(|(expr, used)| (expr, prev_used + 1 + used))
                                                                    .collect()
                                                            )
                                                    ),
                                                    |_spaces, expr| {
                                                        // this is the core of branching
                                                        let mut exprs = exprs.clone(); // include all the past expressions
                                                        exprs.extend(vec![expr]); // extend this one with the next expression
                                                        exprs
                                                    }
                                                ).0.ok()
                                            } else {
                                                None // expected an expression after the comma!
                                            }
                                        } else if c == ']' {
                                            if !consume || prev_used >= rest.len() - 1 {
                                                // add one for ']'
                                                finished.insert((exprs, prev_used + 1));
                                            }
                                            None
                                        } else {
                                            None // was not a separator nor ']', so
                                        }
                                    },
                                    _ => None // there was not another character
                                }
                            } else {
                                None // did not find an end to the list
                            }
                        }
                    )
                        .flatten()
                        .collect::<HashSet<_>>();
                }
                if finished.len() > 0 {
                    Ok(finished.into_iter().map(
                        |(exprs, used)| {
                            // add one to used for '['
                            (Expr::List(exprs.into_iter().map(Box::new).collect()), used + 1)
                        }
                    ).collect())
                } else {
                    Err("ListParser: No possible parses!".into())
                }
            })
    }
}

impl Parser for ConditionalParser {
    type Output = Expr;

    fn parse(&self, content: &String, consume: bool, meta: ParseMetaData) -> Result<HashSet<(Self::Output, usize)>, ParseError> {
        let expr_parser = Rc::clone(&self.expr_parser);
        ParseResult::<bool>::empty()
            .parse_static_text(&content, false, meta.clone(), "if")
            .parse_any_whitespace(&content, false, meta.clone())
            .parse_expression(
                content,
                false,
                meta.clone(),
                Rc::clone(&expr_parser),
                |_, expr|
                    expr
            )
            .parse_any_whitespace(&content, false, meta.clone())
            .parse_expression(
                content,
                false,
                meta.clone(),
                Rc::clone(&expr_parser),
                |cond, then_true|
                    (cond, then_true)
            )
            .parse_any_whitespace(&content, false, meta.clone())
            .parse_static_text(&content, false, meta.clone(), "else")
            .parse_any_whitespace(&content, false, meta.clone())
            .parse_expression(content, consume, meta.clone(), Rc::clone(&expr_parser), |(cond, then), then_else|
                Expr::Conditional(
                    cond.into(),
                    then.into(),
                    then_else.into()
                )
            )
            .0
    }
}


impl ParseMetaData {
    pub fn new() -> ParseMetaData {
        ParseMetaData {
            depth: 0,
            was_infix: false,
            last_path: vec![],
            path: vec![],
            history: vec![],
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

    pub fn add_decision(self, parse_decision: ParseDecision) -> ParseMetaData {
        let mut meta = self.clone();
        meta.path.push(parse_decision);
        meta
    }

    pub fn same_paths(&self) -> bool {
        self.path == self.last_path
    }

    pub fn rotate_paths(self) -> ParseMetaData {
        let mut meta = self.clone();
        // std::mem::swap(&mut meta.path, &mut meta.last_path);
        meta.history.push(meta.path.clone());
        meta.last_path = vec![];
        for d in meta.path {
            meta.last_path.push(d);
        }
        meta.path = vec![];
        meta
    }

    pub fn max_history_cycles(&self) -> usize {
        maximum_repeated_subseq(&self.history)
        // match find_first_repeating_subsequence(&self.history) {
        //     Some((subseq, _i)) => max_repeats(&self.history, &subseq),
        //     None => 0
        // }
    }
}


// miscellaneous parsers (for simple things)

/// describes a number of times for something.
pub enum LengthQualifier {
    /// Exactly that many times
    /// (regex: /{x}/)
    Exactly(usize),

    /// Less than or equal to this amount of times
    /// (regex: /{,x}/)
    /// (bool is whether or not 0 is allowed)
    LEQ(usize, bool),

    /// Greater than or equal to this amount of times
    /// (regex: /{x,}/)
    GEQ(usize)
}

pub struct TakeWhileParser {
    pub func: Box<dyn Fn(&char) -> bool>,
    pub amount: LengthQualifier
}

impl TakeWhileParser {
    pub fn whitespace(amount: LengthQualifier) -> TakeWhileParser {
        TakeWhileParser {
            func: Box::new(|c: &char| c.is_whitespace()),
            amount
        }
    }
}

impl Parser for TakeWhileParser {
    type Output = String;

    fn parse(&self, content: &String, consume: bool, context: ParseMetaData) -> Result<HashSet<(Self::Output, usize)>, ParseError> {
        let (max_possible, rest) = take_while(content, &self.func);
        if consume && max_possible.len() < content.len() {
            return Err("TakeWhileParser did not consume all the characters.".into());
        }
        match &self.amount {
            &LengthQualifier::Exactly(x) => {
                if consume && x < content.len() {
                    Err(format!("Tried to consume but the exact amount required, {}, was not the \
                    length of the string. (bozo)", x).into())
                } else if max_possible.len() >= x {
                    Ok(hashset![
                        (take(&max_possible, x).unwrap().0, x)
                    ])
                } else {
                    Err(format!("This TakeWhileParser requires exactly {} characters to be taken, \
                    not enough were able to be taken", x).into())
                }
            },
            &LengthQualifier::LEQ(x, zero) => {
                if consume {
                    if x < content.len() {
                        Err(format!("Needs to consume the length \
                        of the string. {} < {}", x, content.len()).into())
                    } else if max_possible.len() < content.len() {
                        Err(format!("Unable to consume whole string.").into())
                    } else {
                        // now we know:
                        // x == content.len()
                        // max_possible.len() >= content.len()
                        Ok(hashset![
                            (
                                max_possible,
                                content.len()
                            )
                        ])
                    }
                } else {
                    if max_possible.len() == 0 && zero {
                        Ok(hashset![("".to_string(), 0)])
                    } else {
                        let mut possible = hashset![];
                        if zero {
                            possible.insert(("".to_string(), 0));
                        }
                        if max_possible.len() > 0 {
                            for n in 1..=min(x, max_possible.len()) {
                                possible.insert(
                                    (
                                        take(&max_possible, n).unwrap().0,
                                        n
                                    )
                                );
                            }
                        }
                        Ok(possible)
                    }
                }
            },
            &LengthQualifier::GEQ(x) => {
                if consume {
                    if x > content.len() {
                        Err("Can't consume more than the entire string.".into())
                    } else if max_possible.len() < content.len() {
                        Err("Can't consume the entire string.".into())
                    } else {
                        Ok(hashset![
                            (
                                max_possible,
                                content.len()
                            )
                        ])
                    }
                } else {
                    if max_possible.len() < x {
                        Err(format!("Did not consume enough characters to meet the {} minimum requirement", x).into())
                    } else {
                        let mut possible = hashset![];
                        for n in x..=max_possible.len() {
                            possible.insert((
                                take(&max_possible, n).unwrap().0,
                                n
                            ));
                        }
                        Ok(possible)
                    }
                }
            }
        }
    }
}