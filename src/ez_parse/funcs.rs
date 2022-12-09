use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt::Debug;
use std::hash::Hash;
use std::marker::PhantomData;
use std::rc::Rc;
use crate::parse::{ParseDecision, Parser, ParseResult};
use crate::{chainable, ParseError, ParseMetaData};
use crate::funcs::{expect_str, take};

type ParserRef<P> = Rc<RefCell<P>>;
pub fn parser_ref<P: Parser>(p: P) -> ParserRef<P> {
    return Rc::new(RefCell::new(p));
}

/// Parser that concatenates two parsers together. This doesn't provide any
/// branching mechanisms or anything; it's mostly for convenience.
/// p1 and p2 are obvious, joiner is the function that operates on the outputs
/// of the two parsers.
#[derive(Clone)]
pub struct CatParser<P1, P2, J>
    where
        P1: Parser,
        P2: Parser,
        P1::Output: Hash + Eq + Clone,
        P2::Output: Hash + Eq + Clone {
    pub p1: ParserRef<P1>,
    pub p2: ParserRef<P2>,
    pub joiner: Box<J>
}

#[derive(Clone)]
pub struct UnionParser<P1, P2>
    where
        P1: Parser,
        P2: Parser {
    pub p1: ParserRef<P1>,
    pub p2: ParserRef<P2>
}

#[derive(Clone)]
pub struct KleeneParser<P>
    where
        P: Parser {
    p: ParserRef<P>
}

/// very simple parser that merely determines if the string is the next part
/// in the source to be parsed. Its return value is usually not useful.
#[derive(Clone)]
pub struct SimpleStrParser {
    pub str: String
}

/// Parses epsilon :)
#[derive(Clone, Copy)]
pub struct EpsilonParser;

// type ParseFunc<T, F: Fn(&String, bool, ParseMetaData) -> Result<HashSet<(T, usize)>, ParseError>> = F;

/// has a RefCell for a function, so that it can be interchanged easily.
pub struct FunctionParser<T>(pub RefCell<Box<dyn Fn(&String, bool, ParseMetaData) -> Result<HashSet<(T, usize)>, ParseError>>>);

impl<T> Parser for FunctionParser<T>
{
    type Output = T;
    fn parse(&self, content: &String, consume: bool, context: ParseMetaData) -> Result<HashSet<(Self::Output, usize)>, ParseError> {
        let result = self.0.borrow()(content, consume, context);
        result
    }
}

impl Parser for EpsilonParser {
    type Output = ();
    fn parse(&self, content: &String, consume: bool, context: ParseMetaData) -> Result<HashSet<(Self::Output, usize)>, ParseError> {
        if consume && content.len() > 0 {
            Err(ParseError::from("Could not consume string with epsilon parser"))
        } else {
            Ok(hashset!{((), 0)})
        }
    }
}

impl SimpleStrParser {
    pub fn new(str: &String) -> SimpleStrParser {
        SimpleStrParser {
            str: str.clone()
        }
    }
}

impl From<&str> for SimpleStrParser {
    fn from(s: &str) -> Self {
        let s = s.to_string();
        SimpleStrParser::new(&s)
    }
}

impl<P1: Parser<Output=I1>, P2: Parser<Output=I2>, I1: Hash + Eq + Clone, I2: Hash + Eq + Clone, O: Hash + Eq, J: Fn(I1, I2) -> O> Parser for CatParser<P1, P2, J> {
    type Output = O;
    fn parse(&self, content: &String, consume: bool, context: ParseMetaData) -> Result<HashSet<(Self::Output, usize)>, ParseError> {
        // println!("Cat parser with {} left, no-width={}", content, &context.was_infix);
        let pr = ParseResult(self.p1.borrow().parse(content, false, context.clone().add_decision(ParseDecision::Recur)));
        let meta = context.increment_depth();
        let res = pr.chain(
            content,
            consume,
            meta,
            chainable(|a, rest, c| self.p2.borrow().parse(&rest, consume, c)),
            &self.joiner);
        res.0
    }
}

/// Just an Either type
#[derive(Hash, Eq, PartialEq, Debug, Copy, Clone)]
pub enum UnionResult<L, R> {
    Left(L),
    Right(R)
}

impl<P1: Parser, P2: Parser> Parser for UnionParser<P1, P2>
    where
        P1::Output: Hash + Eq + Clone,
        P2::Output: Hash + Eq + Clone
{
    type Output = UnionResult<P1::Output, P2::Output>;
    fn parse(&self, content: &String, consume: bool, context: ParseMetaData) -> Result<HashSet<(Self::Output, usize)>, ParseError> {
        let meta = context.clone().increment_depth();
        // println!("Union of two parsers! with {} remaining, no-width={}", content, context.clone().was_infix);
        let p1_out = ParseResult(self.p1.borrow().parse(content, consume, meta.clone().add_decision(ParseDecision::UnionLeft)));
        let p2_out = ParseResult(self.p2.borrow().parse(content, consume, meta.clone().add_decision(ParseDecision::UnionRight)));
        // multiply the contents, or not
        match p1_out.0 {
            Ok(r1) => {
                match p2_out.0 {
                    Err(e) =>
                        // p1 had results, but not p2
                        ParseResult(Ok(r1)).map_inner(UnionResult::Left).0,
                    Ok(r2) => {
                        // cartesian product
                        let mut left: HashSet<_> = r1.into_iter()
                            .map(|(x, len)| (Self::Output::Left(x.clone()), len)).collect();
                        let right: HashSet<_> = r2.into_iter()
                            .map(|(x, len)| (UnionResult::Right(x.clone()), len)).collect();
                        left.extend(right);
                        Ok(left)
                    }
                }
            },
            Err(e1) => {
                match p2_out.0 {
                    Err(e2) =>
                        // no results, so error
                        Err(ParseError::from(format!("Neither case in the union was fulfilled. Left: {:?}; Right: {:?}", e1, e2))),
                    Ok(r2) =>
                        // p2 had results, p1 didn't
                        ParseResult(Ok(r2)).map_inner(UnionResult::Right).0
                }
            }
        }
    }
}

impl<P: Parser> Parser for KleeneParser<P>
    where
        P::Output: Hash + Eq + Clone
{

    // Kleene operation: K = e|PK
    // consume: operates the same, but filters all possibilities where the whole string is not consumed.

    type Output = Vec<P::Output>; // can be empty
    fn parse(&self, content: &String, consume: bool, context: ParseMetaData) -> Result<HashSet<(Self::Output, usize)>, ParseError> {

        let mut all_parses = hashset!{(vec![], 0)};

        {
            let meta = context.increment_depth();
            // original parse
            let mut res = ParseResult(self.p.borrow().parse(content, false, meta.clone()))
                .map_inner(|x| vec![x]).0;
            // if the original parse works...
            while let Ok(hs) = &res {
                let mut new_res = hashset!{};
                let meta = meta.clone().increment_depth();
                // iterate through these possibilities
                for (previous, len) in hs {
                    all_parses.insert((previous.clone(), *len));
                    match take(content, *len) {
                        Some((_before, rest)) => {
                            // parse the next, maybe. The inductive step.
                            let next_res = self.p.borrow().parse(&rest, false, meta.clone());
                            match next_res {
                                Ok(hs2) => {
                                    for (e, used) in hs2 {
                                        let mut p = previous.clone();
                                        p.push(e);
                                        new_res.insert((p, *len + used));
                                    }
                                },
                                Err(e) => {}
                            }
                        },
                        None => {

                        }
                    }
                }
                // conditionally update res with new parses (inductive step)
                if new_res.len() > 0 {
                    res = Ok(new_res);
                } else {
                    res = Err(ParseError::from("no more possibilities"));
                }
            }
        }
        if consume {
            all_parses = all_parses.into_iter().filter(|(_, len)| *len == content.len()).collect();
            if all_parses.len() == 0 {
                return Err(ParseError::from("Could not apply Kleene star operation on the rest of string."));
            }
        }
        Ok(all_parses)
    }
}

impl Parser for SimpleStrParser {
    type Output = String;
    fn parse(&self, content: &String, consume: bool, context: ParseMetaData) -> Result<HashSet<(Self::Output, usize)>, ParseError> {
        let res = expect_str(content, &self.str, ParseError {
            location: None,
            range: None,
            message: format!("Expected '{}'", self.str)
        }, ParseError {
            location: None,
            range: None,
            message: format!("Expected '{}'", self.str)
        }).map(|s| hashset!{(self.str.clone(), self.str.len())});
        match res {
            Ok(hs) => {
                if !consume || self.str.len() == content.len() {
                    Ok(hs)
                } else {
                    Err(ParseError::from(format!("String '{}' did not consume all the content", self.str)))
                }
            },
            e => e
        }
    }
}

// evil, do not use (causes infinite recursion (but also causes program to compile!))
// impl<T> Parser for &T
//     where T: Parser
// {
//     type Output = T::Output;
//     fn parse(&self, content: &String, consume: bool, context: ParseMetaData) -> Result<HashSet<(Self::Output, usize)>, ParseError> {
//         self.parse(content, consume, context)
//     }
// }

// Regular Operations of DFAs:
// union
// concatenation
// Kleene * (hint: LengthQualifier)
// Intersection (we won't do this one, because it doesn't make sense with data)
// Complement (this doesn't seem useful so I'm not going to make this one, but keep it around)

/// Concatenate two parsers, with a joiner `j`.
pub fn concat<P1, P2, O, J>(p1: ParserRef<P1>, p2: ParserRef<P2>, j: Box<J>) -> CatParser<P1, P2, J>
    where
        P1: Parser,
        P2: Parser,
        P1::Output: Hash + Eq + Clone,
        P2::Output: Hash + Eq + Clone,
        J: Fn(P1::Output, P2::Output) -> O {
    CatParser {
        p1,
        p2,
        joiner: j
    }
}

pub fn union<P1, P2>(p1: ParserRef<P1>, p2: ParserRef<P2>) -> UnionParser<P1, P2>
    where
        P1: Parser,
        P2: Parser {
    UnionParser {
        p1,
        p2
    }
}

pub fn kleene<P>(p: ParserRef<P>) -> KleeneParser<P>
    where
        P: Parser
{
    KleeneParser {
        p
    }
}

////////////////////////////////////////////////
// The following are all convenience functions
////////////////////////////////////////////////

#[derive(Clone)]
pub struct MappedParser<P, F, T>
    where
        P: Parser,
        F: Fn(P::Output) -> T
{
    p: ParserRef<P>,
    f: Box<F>
}

impl<P, F, T> Parser for MappedParser<P, F, T>
    where
        P: Parser,
        F: Fn(P::Output) -> T,
        P::Output: Hash + Eq,
        T: Hash + Eq
{
    type Output = T;
    fn parse(&self, content: &String, consume: bool, context: ParseMetaData) -> Result<HashSet<(Self::Output, usize)>, ParseError> {
        ParseResult(self.p.borrow().parse(content, consume, context)).map_inner(&self.f).0
    }
}

pub fn map<P, F, T>(p: ParserRef<P>, f: F) -> MappedParser<P, F, T>
    where
        P: Parser,
        F: Fn(P::Output) -> T
{
    MappedParser {
        p,
        f: Box::new(f)
    }
}

type OptionalParser<P: Parser> = MappedParser<UnionParser<P, EpsilonParser>, fn(<UnionParser<P, EpsilonParser> as Parser>::Output) -> Option<P::Output>, Option<P::Output>>;

/// Convenience for union with nothing, or "optional"
pub fn optional<P>(p: ParserRef<P>) -> OptionalParser<P>
    where
        P: Parser,
        P::Output: Hash + Eq + Clone
{
    MappedParser {
        p: parser_ref(UnionParser {
            p1: p,
            p2: parser_ref(EpsilonParser)
        }),
        f: Box::new(|m| match m {
            UnionResult::Left(x) => Some(x),
            UnionResult::Right(_e) => None
        })
    }
}

/// Creates a parser using an inner parser, and parsing the left and right strings.
/// This is another convenience function. For example, parse "(123)" with a number parser,
/// and left="(" and right=")"
pub fn enclose_with<P, O>(parser: P, left: &String, right: &String) -> impl Parser<Output=O>
    where P: Parser<Output=O>,
          O: Hash + Eq + Clone + Debug,
{
    let left_side_parser = concat(
        parser_ref(SimpleStrParser::new(left)),
        parser_ref(parser),
        Box::new(|left_string, o| o)
    );
    let whole_parser = concat(
        parser_ref(left_side_parser),
        parser_ref(SimpleStrParser::new(right)),
        Box::new(|o, right_string| o)
    );
    whole_parser
}

pub fn enclose_with2<P, O>(parser: ParserRef<P>, left: &String, right: &String) -> impl Parser<Output=O>
    where P: Parser<Output=O>,
          O: Hash + Eq + Clone + Debug,
{
    let left_side_parser = concat(
        parser_ref(SimpleStrParser::new(left)),
        parser,
        Box::new(|left_string, o| o)
    );
    let whole_parser = concat(
        parser_ref(left_side_parser),
        parser_ref(SimpleStrParser::new(right)),
        Box::new(|o, right_string| o)
    );
    whole_parser
}

pub struct NoWidthParserFlag<P: Parser>(pub ParserRef<P>);
impl<P: Parser> Parser for NoWidthParserFlag<P> {
    type Output = P::Output;
    fn parse(&self, content: &String, consume: bool, context: ParseMetaData) -> Result<HashSet<(Self::Output, usize)>, ParseError> {
        if context.was_infix {
            Err(ParseError::from("Tried to use a no-width parser twice in a row"))
        } else {
            self.0.borrow().parse(content, consume, context.with_infix())
        }
    }
}

pub fn flag_0_width<P>(p: ParserRef<P>) -> NoWidthParserFlag<P>
    where
        P: Parser
{
    NoWidthParserFlag(p)
}

pub fn fst<U, V>(u: U, _v: V) -> U {
    u
}
pub fn snd<U, V>(_u: U, v: V) -> V {
    v
}

