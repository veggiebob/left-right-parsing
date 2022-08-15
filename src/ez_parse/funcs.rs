
use std::collections::HashSet;
use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;
use crate::parse::{Parser, ParseResult};
use crate::{chainable, ParseError, ParseMetaData};
use crate::funcs::{expect_str, take};

// we are going to operate on ParseResults as the main parse object
// now, we will abstract away into fun parsing objects and meta parsing methods
// such as enclosure

/// Parser that concatenates two parsers together. This doesn't provide any
/// branching mechanisms or anything; it's mostly for convenience.
/// p1 and p2 are obvious, joiner is the function that operates on the outputs
/// of the two parsers.
#[derive(Clone)]
pub struct CatParser<P1, P2, J>
    where
        P1: Parser,
        P2: Parser {
    pub p1: P1,
    pub p2: P2,
    pub joiner: Box<J>
}

#[derive(Clone)]
pub struct UnionParser<P1, P2>
    where
        P1: Parser,
        P2: Parser {
    pub p1: P1,
    pub p2: P2
}

#[derive(Clone)]
pub struct KleeneParser<P>
    where
        P: Parser {
    p: P
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

// impl<P1: Clone + Copy, P2: Clone + Copy, J> Copy for CatParser<P1, P2, J> where Box<J>: Clone + Copy {}
// impl<P1: Clone, P2: Clone> Copy for UnionParser<P1, P2> {}
// impl<P: Clone> Copy for KleeneParser<P> {}

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
        let pr = ParseResult(self.p1.parse(content, false, context));
        let res = pr.chain(
            content,
            consume,
            context,
            chainable(|a, rest, c| self.p2.parse(&rest, consume, context)),
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
        let p1_out = ParseResult(self.p1.parse(content, consume, context));
        let p2_out = ParseResult(self.p2.parse(content, consume, context));
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
            let meta = context;
            // original parse
            let mut res = ParseResult(self.p.parse(content, false, meta))
                .map_inner(|x| vec![x]).0;
            // if the original parse works...
            while let Ok(hs) = &res {
                let mut new_res = hashset!{};
                let meta = meta.increment_depth();
                // iterate through these possibilities
                for (previous, len) in hs {
                    all_parses.insert((previous.clone(), *len));
                    match take(content, *len) {
                        Some((_before, rest)) => {
                            // parse the next, maybe. The inductive step.
                            let next_res = self.p.parse(&rest, false, meta);
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
        expect_str(content, &self.str, ParseError {
            location: None,
            range: None,
            message: format!("Expected '{}'", self.str)
        }, ParseError {
            location: None,
            range: None,
            message: format!("Expected '{}'", self.str)
        }).map(|s| hashset!{(self.str.clone(), self.str.len())})
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
pub fn concat<P1, P2, I1, I2, O, J>(p1: P1, p2: P2, j: Box<J>) -> CatParser<P1, P2, J>
    where
        P1: Parser,
        P2: Parser,
        J: Fn(I1, I2) -> O {
    CatParser {
        p1,
        p2,
        joiner: j
    }
}

pub fn union<P1, P2>(p1: P1, p2: P2) -> UnionParser<P1, P2>
    where
        P1: Parser,
        P2: Parser {
    UnionParser {
        p1,
        p2
    }
}

pub fn kleene<P>(p: P) -> KleeneParser<P>
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
    p: P,
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
        ParseResult(self.p.parse(content, consume, context)).map_inner(&self.f).0
    }
}

pub fn map<P, F, T>(p: P, f: F) -> MappedParser<P, F, T>
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
pub fn optional<P>(p: P) -> OptionalParser<P>
    where
        P: Parser,
        P::Output: Hash + Eq + Clone
{
    MappedParser {
        p: UnionParser {
            p1: p,
            p2: EpsilonParser
        },
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
        SimpleStrParser::new(left),
        parser,
        Box::new(|left_string, o| o)
    );
    let whole_parser = concat(
        left_side_parser,
        SimpleStrParser::new(right),
        Box::new(|o, right_string| o)
    );
    whole_parser
}

pub fn fst<U, V>(u: U, _v: V) -> U {
    u
}
pub fn snd<U, V>(_u: U, v: V) -> V {
    v
}

