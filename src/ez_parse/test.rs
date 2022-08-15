use crate::ez_parse::funcs::*;
use crate::{Expr, NatParser, ParseMetaData, Parser, StringParser};
use crate::Expr::{Nat, Str};
use crate::ez_parse::funcs::UnionResult::{Left, Right};
use crate::ez_parse::ops::EZ;
use crate::lang_obj::{LONat, LOString};

// these tests work, just make them assert the results

#[test]
fn concat_test1() {
    let source = String::from("1234\"abcd\"");
    let p1 = NatParser();
    let p2 = StringParser();
    let p = concat(p1, p2, Box::new(|nat: Expr, str: Expr| (nat, str)));
    let res = p.parse(&source, true, ParseMetaData::new());
    assert_eq!(res, Ok(hashset!{((Expr::Nat(LONat::from(1234)), Expr::Str(LOString::from("abcd".to_string()))), 10)}))
}

#[test]
fn test_simple_str_parser() {
    let source = String::from("(");
    let p = SimpleStrParser::new(&source);
    let res = p.parse(&source, true, ParseMetaData::new());
    assert_eq!(res, Ok(hashset!{("(".to_string(), 1)}))
}

#[test]
fn enclose_test1() {
    let source = String::from("(1234)");
    let p = NatParser();
    let left = "(".to_string();
    let right = ")".to_string();
    let e = enclose_with(p, &left, &right);
    let res = e.parse(&source, true, ParseMetaData::new());
    // println!("{:?}", res);
    assert_eq!(res, Ok(hashset!{(Expr::Nat(LONat::from(1234)), 6)}));
}

#[test]
fn ez_add_parsers() {
    let source = String::from("1234\"abcd\"");
    let p1 = NatParser();
    let p2 = StringParser();
    let p = EZ(p1) + EZ(p2);
    let res = p.parse(&source, true, ParseMetaData::new());
    assert_eq!(res, Ok(hashset!{((Nat(LONat { content: 1234 }), Str(LOString { content: "abcd".to_string() })), 10)}))
}

#[test]
fn ez_union() {
    // parse this string (the string '"1234"') in two different ways:
    // 1: " + number + "
    // 2: StringParser

    let source = String::from("\"1234\"");
    let quote = "\"".to_string();
    let p1 = enclose_with(NatParser(), &quote, &quote);
    let p2 = StringParser();
    let p = union(p1, p2);
    let res = p.parse(&source, true, ParseMetaData::new());
    // println!("{:?}", res);
    assert_eq!(res,
               Ok(hashset!{
                   (Right(Str(LOString { content: "1234".to_string() })), 6),
                   (Left(Nat(LONat { content: 1234 })), 6)
               })
    )
}

#[test]
fn ez_kleene() {
    let source = String::from("1,2,3,");

    let np = NatParser();
    let separator = ",".to_string();
    let i = concat(np, SimpleStrParser::new(&separator), Box::new(|a, _sep| a));
    let series_i = kleene(i);
    let res = series_i.parse(&source, true, ParseMetaData::new());
    // println!("{:?}", res);
    assert_eq!(res, Ok(hashset!{
        (vec![Nat(LONat { content: 1 })], 2),
        (vec![], 0),
        (vec![Nat(LONat { content: 1 }), Nat(LONat { content: 2 })], 4),
        (vec![Nat(LONat { content: 1 }), Nat(LONat { content: 2 }), Nat(LONat { content: 3 })], 6)
    }))
}