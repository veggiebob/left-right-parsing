use crate::ez_parse::funcs::{concat, map, parser_ref, union, CatParser, IntoParserRef, MappedParser, ParserRef, SimpleStrParser, UnionParser, UnionResult, snd, fst};
use crate::{ParseError, ParseMetaData, Parser};
use std::collections::HashSet;
use std::hash::Hash;
use std::iter::TakeWhile;
use std::ops::{Add, Div, Mul};
use std::rc::Rc;
use crate::parse::{LengthQualifier, TakeWhileParser};

pub struct EZ<T>(pub T);

impl<T: Clone> Clone for EZ<T> {
    fn clone(&self) -> Self {
        EZ(self.0.clone())
    }
}

impl<P: Parser> EZ<P> {
    pub fn parse<T>(
        &self,
        content: &String,
        consume: bool,
        meta: T,
    ) -> Result<HashSet<(P::Output, usize)>, ParseError>
    where
        T: Into<ParseMetaData>,
    {
        self.0.parse(content, consume, meta.into())
    }
    pub fn parse_all(&self, content: &str) -> Result<HashSet<(P::Output, usize)>, ParseError> {
        self.0.parse_all(content)
    }
}

impl<P: Parser> Parser for EZ<P> {
    type Output = P::Output;

    fn parse(
        &self,
        content: &String,
        consume: bool,
        context: ParseMetaData,
    ) -> Result<HashSet<(Self::Output, usize)>, ParseError> {
        self.0.parse(content, consume, context)
    }
}

// redundant, since IntoParserRef<P> is already implemented for any P: Parser
// impl<P: Parser> IntoParserRef<P> for EZ<P> {
//     fn into(self) -> ParserRef<P> {
//         parser_ref(self.0)
//     }
// }

/*

CONCATENATION

*/

impl<P1: Parser, P2: Parser> Add<EZ<P2>> for EZ<P1>
where
    P1::Output: Hash + Eq + Clone,
    P2::Output: Hash + Eq + Clone,
{
    type Output = EZ<CatParser<P1, P2, fn(P1::Output, P2::Output) -> (P1::Output, P2::Output)>>;
    fn add(self, rhs: EZ<P2>) -> Self::Output {
        EZ(concat(parser_ref(self.0), parser_ref(rhs.0), |a, b| (a, b)))
    }
}

impl<P1: Parser, P2: Parser> Add<&ParserRef<P2>> for EZ<P1>
where
    P1::Output: Hash + Eq + Clone,
    P2::Output: Hash + Eq + Clone,
{
    type Output = EZ<CatParser<P1, P2, fn(P1::Output, P2::Output) -> (P1::Output, P2::Output)>>;
    fn add(self, rhs: &ParserRef<P2>) -> Self::Output {
        EZ(concat(parser_ref(self.0), Rc::clone(rhs), |a, b| (a, b)))
    }
}
impl<P1: Parser, P2: Parser> Add<EZ<P2>> for &ParserRef<P1>
where
    P1::Output: Hash + Eq + Clone,
    P2::Output: Hash + Eq + Clone,
{
    type Output = EZ<CatParser<P1, P2, fn(P1::Output, P2::Output) -> (P1::Output, P2::Output)>>;
    fn add(self, rhs: EZ<P2>) -> Self::Output {
        EZ(concat(Rc::clone(self), parser_ref(rhs.0), |a, b| (a, b)))
    }
}

/// important note: this implementation removes the string in the object result
/// (this assumes you don't care about the string being parsed, it's just used as a sentinel)
impl<P: Parser> Add<String> for EZ<P>
where
    P::Output: Hash + Eq + Clone,
{
    type Output = EZ<CatParser<P, SimpleStrParser, fn(P::Output, String) -> P::Output>>;
    fn add(self, rhs: String) -> Self::Output {
        EZ(concat(
            parser_ref(self.0),
            parser_ref(SimpleStrParser::new(&rhs)),
            |a, _b| a,
        ))
    }
}

/// important note: this implementation removes the string in the object result
/// (this assumes you don't care about the string being parsed, it's just used as a sentinel)
impl<P: Parser> Add<&str> for EZ<P>
where
    P::Output: Hash + Eq + Clone,
{
    type Output = EZ<CatParser<P, SimpleStrParser, fn(P::Output, String) -> P::Output>>;
    fn add(self, rhs: &str) -> Self::Output {
        let s = rhs.to_owned();
        EZ(concat(self.0, SimpleStrParser::new(&s), |a, _b| a))
    }
}

impl<T: Parser> Add<EZ<T>> for &str
where
    T::Output: Hash + Eq + Clone,
{
    type Output = EZ<CatParser<SimpleStrParser, T, fn(String, T::Output) -> T::Output>>;

    fn add(self, rhs: EZ<T>) -> Self::Output {
        let s = self.to_owned();
        EZ(concat(SimpleStrParser::new(&s), rhs.0, |_a, b| b))
    }
}

// special codes! (using char)

impl<T: Parser> Add<EZ<T>> for char
where
    T::Output: Hash + Eq + Clone
{
    type Output = EZ<CatParser<TakeWhileParser, EZ<T>, fn(String, T::Output) -> T::Output>>;
    fn add(self, rhs: EZ<T>) -> Self::Output {
        if self != ' ' {
            panic!("Parser character cannot be \'{}\'.", self);
        }
        EZ(concat(
            TakeWhileParser::whitespace(LengthQualifier::GEQ(0)),
            rhs,
                |_w, expr| expr))
    }
}

impl<T: Parser> Add<char> for EZ<T>
    where
        T::Output: Hash + Eq + Clone
{
    type Output = EZ<CatParser<EZ<T>, TakeWhileParser, fn(T::Output, String) -> T::Output>>;
    fn add(self, rhs: char) -> Self::Output {
        if rhs != ' ' {
            panic!("Parser character cannot be \'{}\'.", rhs);
        }
        EZ(concat(
            self,
            TakeWhileParser::whitespace(LengthQualifier::GEQ(0)),
            |expr, _w| expr))
    }
}

/*

UNION

*/

impl<P1: Parser, P2: Parser> Mul<EZ<P2>> for EZ<P1> {
    type Output = EZ<UnionParser<P1, P2>>;

    fn mul(self, rhs: EZ<P2>) -> Self::Output {
        EZ(union(self.0, rhs.0))
    }
}

/// div operator for union of parsers which output the SAME TYPE (trivial combination)
impl<P1, P2> Div<EZ<P2>> for EZ<P1>
where
    P1: Parser,
    P2: Parser<Output = P1::Output>,
    P1::Output: Hash + Eq + Clone,
{
    type Output = EZ<
        MappedParser<
            UnionParser<P1, P2>,
            fn(UnionResult<P1::Output, P2::Output>) -> P1::Output,
            P1::Output,
        >,
    >;

    fn div(self, rhs: EZ<P2>) -> Self::Output {
        EZ(map(union(self.0, rhs.0), UnionResult::join))
    }
}

#[cfg(test)]
mod test {
    use crate::ez_parse::funcs::UnionResult::{Left, Right};
    use crate::ez_parse::funcs::{map, parser_ref, UnionResult};
    use crate::ez_parse::ops::EZ;
    use crate::lang_obj::Expr::{Nat, Str};
    use crate::lang_obj::{Expr, LONat, LOString};
    use crate::parse::{LONatParser, LOStringParser, ParseMetaData, Parser, TakeWhileParser};

    #[test]
    fn concat() {
        let n = LONatParser();
        let p = EZ(n) + ",";

        assert_eq!(
            p.parse_all("1234,"),
            Ok(hashset! {(Nat(LONat { content: 1234 }), 5)})
        );

        assert!(p.parse_all("123").is_err());
    }

    #[test]
    fn union() {
        let quoted_n = "\"" + EZ(LONatParser()) + "\"";
        let s = LOStringParser();
        let p = quoted_n * EZ(s);
        let p = map(p, UnionResult::join);
        println!("{:?}", p.parse_all("\"1234\""));
        assert_eq!(
            p.parse_all("\"1234\""),
            Ok(hashset! {
            (Nat(LONat { content: 1234 }), 6),
            (Str(LOString { content: "1234".to_string() }), 6)})
        )
    }
}
