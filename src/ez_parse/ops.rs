use std::ops::Add;
use crate::ez_parse::funcs::{CatParser, concat, SimpleStrParser};
use crate::Parser;

pub struct EZ<T>(pub T);

impl<P1: Parser, P2: Parser> Add<EZ<P2>> for EZ<P1> {
    type Output = CatParser<P1, P2, P1::Output, P2::Output, fn(P1::Output, P2::Output) -> (P1::Output, P2::Output)>;
    fn add(self, rhs: EZ<P2>) -> Self::Output {
        concat(self.0, rhs.0, Box::new(|a, b| (a, b)))
    }
}

impl<P1: Parser, P2: Parser> Add<P2> for EZ<P1> {
    type Output = CatParser<P1, P2, P1::Output, P2::Output, fn(P1::Output, P2::Output) -> (P1::Output, P2::Output)>;
    fn add(self, rhs: P2) -> Self::Output {
        concat(self.0, rhs, Box::new(|a, b| (a, b)))
    }
}

// impl<P: Parser> Add<&str> for EZ<P> {
//     type Output = CatParser<P, Parser<Output=(P::Output, &str)>, P::Output, &str, fn(P::Output, &str) -> (P::Output, &str)>;
//     fn add(self, rhs: &str) -> Self::Output {
//         let s = rhs.to_string();
//         self + EZ(SimpleStrParser::new(&s))
//     }
// }