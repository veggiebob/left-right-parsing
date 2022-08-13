use crate::ez_parse::funcs::*;
use crate::{Expr, NatParser, ParseMetaData, Parser, StringParser};
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
    println!("{:?}", res);
}