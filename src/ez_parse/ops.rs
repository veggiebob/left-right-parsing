use std::collections::HashSet;
use std::ops::Add;
use crate::ez_parse::funcs::{CatParser, concat, SimpleStrParser};
use crate::{ParseError, ParseMetaData, Parser};

pub struct EZ<T>(pub T);

impl<P1: Parser, P2: Parser> Add<EZ<P2>> for EZ<P1> {
    type Output = CatParser<P1, P2, fn(P1::Output, P2::Output) -> (P1::Output, P2::Output)>;
    fn add(self, rhs: EZ<P2>) -> Self::Output {
        concat(self.0, rhs.0, Box::new(|a, b| (a, b)))
    }
}

// impl<P1: Parser, P2: Parser> Add<P2> for EZ<P1> {
//     type Output = CatParser<P1, P2, fn(P1::Output, P2::Output) -> (P1::Output, P2::Output)>;
//     fn add(self, rhs: P2) -> Self::Output {
//         concat(self.0, rhs, Box::new(|a, b| (a, b)))
//     }
// }

impl<P: Parser> Add<String> for EZ<P> {
    type Output = CatParser<P, SimpleStrParser, fn(P::Output, String) -> (P::Output, String)>;
    fn add(self, rhs: String) -> Self::Output {
        concat(self.0, SimpleStrParser::new(&rhs), Box::new(|a, b| (a, b)))
    }
}

// impl<P: Parser, S: Into<String>> Add<S> for EZ<P> {
//     type Output = CatParser<P, SimpleStrParser, fn(P::Output, String) -> (P::Output, String)>;
//     fn add(self, rhs: S) -> Self::Output {
//         self + rhs.into()
//     }
// }

// impl Parser for &str {
//     type Output = String;
//     fn parse(&self, content: &String, consume: bool, context: ParseMetaData) -> Result<HashSet<(Self::Output, usize)>, ParseError> {
//         let s = self.to_string();
//         SimpleStrParser::new(&s).parse(content, consume, context)
//     }
// }
//
// impl Parser for String {
//     type Output = String;
//     fn parse(&self, content: &String, consume: bool, context: ParseMetaData) -> Result<HashSet<(Self::Output, usize)>, ParseError> {
//         SimpleStrParser::new(&self).parse(content, consume, context)
//     }
// }