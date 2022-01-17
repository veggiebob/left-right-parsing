use std::cell::RefCell;
use std::cmp::min;
use std::collections::HashSet;
use std::hash::Hash;
use std::iter::TakeWhile;
use std::process::Output;
use std::rc::{Rc, Weak};
use std::task::Context;

use crate::funcs::{char_at, expect_str, take, take_while};
use crate::lang_obj::{Expr, LONat, LOString, ParseError};
use crate::lang_obj::Expr::Infix;

pub mod structure_parsers;

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

/// Optional-use struct. Provides more implementation for the output of a parse operation.
#[derive(Eq, PartialEq, Debug)]
pub struct ParseResult<O: Hash + Eq>(pub Result<HashSet<(O, usize)>, ParseError>);

impl<O: Hash + Eq + Clone> Clone for ParseResult<O> {
    fn clone(&self) -> Self {
        ParseResult(self.0.clone())
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
                            if used1 >= content.len() {
                                None // expected more to be parsed!
                            } else {
                                if let Some((_, rest)) = take(content, used1) {
                                    parse_func(&e, rest, context).map(
                                        |rs|
                                            rs.into_iter()
                                                .map(|(s, used2)| (s, used1 + used2))
                                                .filter(|(s, used)| !consume || *used == content.len())
                                                .map(|(s, used)| (constructor(e.clone(), s), used))
                                                .collect::<Vec<(R, usize)>>()
                                    )
                                } else {
                                    None // for some reason there were not enough characters left
                                }
                            }
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
            context,
            chainable(|_x, next, meta| {
                any_whitespace.parse(&next, consume, context)
            }),
            |x, _| x
        )
    }

    pub fn parse_some_whitespace(self, content: &String, consume: bool, context: ParseMetaData) -> ParseResult<T> {
        let some_whitespace = TakeWhileParser::whitespace(LengthQualifier::GEQ(1));
        self.chain( // parse any amount of space (including 0)
            content,
            consume,
            context,
            chainable(|_x, next, meta| {
                some_whitespace.parse(&next, consume, context)
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
pub struct StringParser();
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
        ParseResult(self.expr_parser.parse(&content, false, meta.with_infix()))
            .parse_any_whitespace(&content, false, meta)
            .parse_static_text(&content, false, meta, &self.infix)
            .parse_any_whitespace(&content, false, meta)
            .chain(
                &content,
                consume,
                meta.increment_depth(),
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
                // used 1
                let meta = context.increment_depth();

                // this first step seems redundant, but it's easier to compare to what the
                // next item in the list is
                let mut parsed = match self.expr_parser
                    .parse(&rest, false, meta) {
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
                                                let result = whitespace_parser.parse(&next, false, meta);
                                                let result = ParseResult(result);
                                                result.chain(
                                                    &next,
                                                    false,
                                                    meta,
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
                if consume && x != content.len() {
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