
use std::collections::HashSet;
use std::fmt::Debug;
use std::hash::Hash;
use crate::parse::{Parser, ParseResult};
use crate::{chainable, ParseError, ParseMetaData};
use crate::funcs::expect_str;

// we are going to operate on ParseResults as the main parse object
// now, we will abstract away into fun parsing objects and meta parsing methods
// such as enclosure

pub struct CatParser<P1: Parser<Output=I1>, P2: Parser<Output=I2>, I1, I2, J> {
    pub p1: P1,
    pub p2: P2,
    pub joiner: Box<J>
}

pub struct SimpleStrParser {
    pub str: String
}

impl SimpleStrParser {
    pub fn new(str: &String) -> SimpleStrParser {
        SimpleStrParser {
            str: str.clone()
        }
    }
}

impl<P1: Parser<Output=I1>, P2: Parser<Output=I2>, I1: Hash + Eq + Clone, I2: Hash + Eq + Clone, O: Hash + Eq, J: Fn(I1, I2) -> O> Parser for CatParser<P1, P2, I1, I2, J> {
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

pub fn concat<P1, P2, I1, I2, O, J>(p1: P1, p2: P2, j: Box<J>) -> CatParser<P1, P2, I1, I2, J>
    where P1: Parser<Output=I1>,
          P2: Parser<Output=I2>,
          J: Fn(I1, I2) -> O {
    CatParser {
        p1,
        p2,
        joiner: j
    }
}

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