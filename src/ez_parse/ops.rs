use std::collections::HashSet;
use std::hash::Hash;
use std::ops::{Add, Div, Mul};
use std::rc::Rc;
use crate::ez_parse::funcs::{CatParser, concat, map, MappedParser, parser_ref, ParserRef, SimpleStrParser, union, UnionParser, UnionResult};
use crate::{ParseError, ParseMetaData, Parser};

pub struct EZ<T>(pub T);

impl<P: Parser> EZ<P> {
    pub fn parse<T>(&self, content: &String, consume: bool, meta: T) -> Result<HashSet<(P::Output, usize)>, ParseError>
        where T: Into<ParseMetaData> {
        self.0.parse(content, consume, meta.into())
    }
    pub fn parse_all(&self, content: &str) -> Result<HashSet<(P::Output, usize)>, ParseError> {
        self.0.parse_all(content)
    }
}

impl<P: Parser> Parser for EZ<P> {
    type Output = P::Output;

    fn parse(&self, content: &String, consume: bool, context: ParseMetaData) -> Result<HashSet<(Self::Output, usize)>, ParseError> {
        self.0.parse(content, consume, context)
    }
}

/*

CONCATENATION

*/

impl<P1: Parser, P2: Parser> Add<EZ<P2>> for EZ<P1>
    where
        P1::Output: Hash + Eq + Clone,
        P2::Output: Hash + Eq + Clone
{
    type Output = EZ<CatParser<P1, P2, fn(P1::Output, P2::Output) -> (P1::Output, P2::Output)>>;
    fn add(self, rhs: EZ<P2>) -> Self::Output {
        EZ(
            concat(parser_ref(self.0), parser_ref(rhs.0), Box::new(|a, b| (a, b)))
        )
    }
}

impl<P1: Parser, P2: Parser> Add<&ParserRef<P2>> for EZ<P1>
where
    P1::Output: Hash + Eq + Clone,
    P2::Output: Hash + Eq + Clone
{
    type Output = EZ<CatParser<P1, P2, fn(P1::Output, P2::Output) -> (P1::Output, P2::Output)>>;
    fn add(self, rhs: &ParserRef<P2>) -> Self::Output {
        EZ(
            concat(parser_ref(self.0), Rc::clone(rhs), Box::new(|a, b| (a, b)))
        )
    }
}
impl<P1: Parser, P2: Parser> Add<EZ<P2>> for &ParserRef<P1>
    where
        P1::Output: Hash + Eq + Clone,
        P2::Output: Hash + Eq + Clone
{
    type Output = EZ<CatParser<P1, P2, fn(P1::Output, P2::Output) -> (P1::Output, P2::Output)>>;
    fn add(self, rhs: EZ<P2>) -> Self::Output {
        EZ(
            concat(Rc::clone(self), parser_ref(rhs.0), Box::new(|a, b| (a, b)))
        )
    }
}

/// important note: this implementation removes the string in the object result
/// (this assumes you don't care about the string being parsed, it's just used as a sentinel)
impl<P: Parser> Add<String> for EZ<P>
    where
        P::Output: Hash + Eq + Clone
{
    type Output = EZ<CatParser<P, SimpleStrParser, fn(P::Output, String) -> P::Output>>;
    fn add(self, rhs: String) -> Self::Output {
        EZ(
            concat(parser_ref(self.0), parser_ref(SimpleStrParser::new(&rhs)), Box::new(|a, _b| a))
        )
    }
}

/// important note: this implementation removes the string in the object result
/// (this assumes you don't care about the string being parsed, it's just used as a sentinel)
impl<P: Parser> Add<&str> for EZ<P>
    where
        P::Output: Hash + Eq + Clone
{
    type Output = EZ<CatParser<P, SimpleStrParser, fn(P::Output, String) -> P::Output>>;
    fn add(self, rhs: &str) -> Self::Output {
        let s = rhs.to_owned();
        EZ(
            concat(parser_ref(self.0), parser_ref(SimpleStrParser::new(&s)), Box::new(|a, _b| a))
        )
    }
}

impl<T: Parser> Add<EZ<T>> for &str
    where T::Output: Hash + Eq + Clone {
    type Output = EZ<CatParser<SimpleStrParser, T, fn(String, T::Output) -> T::Output>>;

    fn add(self, rhs: EZ<T>) -> Self::Output {
        let s = self.to_owned();
        EZ(
            concat(
                parser_ref(SimpleStrParser::new(&s)),
                parser_ref(rhs.0),
                Box::new(|_a, b| b)
            )
        )
    }
}

/*

UNION

*/

impl<P1: Parser, P2: Parser> Mul<EZ<P2>> for EZ<P1> {
    type Output = EZ<UnionParser<P1, P2>>;

    fn mul(self, rhs: EZ<P2>) -> Self::Output {
        EZ(union(parser_ref(self.0), parser_ref(rhs.0)))
    }
}

/// div operator for union of parsers which output the SAME TYPE (trivial combination)
impl<P1, P2> Div<EZ<P2>> for EZ<P1>
where
    P1: Parser,
    P2: Parser<Output=P1::Output>,
    P1::Output: Hash + Eq + Clone {
    type Output = EZ<MappedParser<UnionParser<P1, P2>, fn(UnionResult<P1::Output, P2::Output>) -> P1::Output, P1::Output>>;

    fn div(self, rhs: EZ<P2>) -> Self::Output {
        EZ(map(
            parser_ref(
                union(
                    parser_ref(self.0),
                    parser_ref(rhs.0)
                )
            ),
            UnionResult::join
        ))
    }
}

#[cfg(test)]
mod test {
    use crate::ez_parse::funcs::{map, parser_ref, UnionResult};
    use crate::ez_parse::funcs::UnionResult::{Left, Right};
    use crate::ez_parse::ops::EZ;
    use crate::lang_obj::Expr::{Nat, Str};
    use crate::lang_obj::{Expr, LONat, LOString};
    use crate::parse::{NatParser, ParseMetaData, Parser, StringParser, TakeWhileParser};

    #[test]
    fn concat() {
        let n = NatParser();
        let p = EZ(n) + ",";

        assert_eq!(p.parse_all("1234,"), Ok(hashset!{(Nat(LONat { content: 1234 }), 5)}));

        assert!(p.parse_all("123").is_err());
    }

    #[test]
    fn union() {
        let quoted_n = "\"" + EZ(NatParser()) + "\"";
        let s = StringParser();
        let p = EZ(quoted_n) * EZ(s);
        let p = map(parser_ref(p), UnionResult::join);
        println!("{:?}", p.parse_all("\"1234\""));
        assert_eq!(p.parse_all("\"1234\""),
                   Ok(hashset!{
                       (Nat(LONat { content: 1234 }), 6),
                       (Str(LOString { content: "1234".to_string() }), 6)}))
    }
}