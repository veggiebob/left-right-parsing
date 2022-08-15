use std::collections::HashSet;
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
    let res = series_i.parse(&source, false, ParseMetaData::new());
    // produces any number of correct answers
    assert_eq!(res, Ok(hashset!{
        (vec![Nat(LONat { content: 1 })], 2),
        (vec![], 0),
        (vec![Nat(LONat { content: 1 }), Nat(LONat { content: 2 })], 4),
        (vec![Nat(LONat { content: 1 }), Nat(LONat { content: 2 }), Nat(LONat { content: 3 })], 6)
    }));

    // assert that when it parses the whole string it only returns the one, whole correct answer.
    let res = series_i.parse(&source, true, ParseMetaData::new());
    assert_eq!(res, Ok(hashset!{(vec![Nat(LONat { content: 1 }), Nat(LONat { content: 2 }), Nat(LONat { content: 3 })], 6)}));

    // allowed to have 0 parses
    let source = String::from("3");
    let res = series_i.parse(&source, false, ParseMetaData::new());
    assert_eq!(res, Ok(hashset!{(vec![], 0)}));
}

#[test]
fn ez_list_parser() {
    // this will demonstrate how to easily create a robust list parser,
    // for elements which are numbers
    let k_spaces = || kleene(SimpleStrParser::from(" ")); // K* for space

    let separator = ",";

    let expr_p = NatParser(); // <expr> = \d+
    let elem_p = concat(
        expr_p,
        concat(
            concat(k_spaces(), SimpleStrParser::from(separator), Box::new(fst)),
            k_spaces(),
            Box::new(|a, b| a)),
        Box::new(fst)); // \s*<expr>,\s*
    let elem_series = concat(
        kleene(elem_p),
        optional(expr_p), // allows for a hanging separator (ex. `1,2,3,`)
        // add the last element (optionally) at the end.
        Box::new(|v: Vec<Expr>, end| {
            Expr::List(match end {
                Some(end) => {
                    let mut vs = v;
                    vs.push(end);
                    vs
                },
                None => v
            }.into_iter().map(Box::new).collect())
        })
    ); // ((<expr>\s*,\s*)*)(<expr>?)

    // \s*((<expr>\s*,\s*)*)(<expr>?)\s*
    let padded_elem_series = concat(
        k_spaces(),
        concat(elem_series, k_spaces(), Box::new(fst)),
        Box::new(snd));

    // <left> = \[, <right> = \]
    let (left, right) = ("[".to_string(), "]".to_string());

    // final parser:
    // <left>\s*((<expr>\s*,\s*)*)(<expr>?)\s*<right>
    let p = enclose_with(padded_elem_series, &left, &right);



    // ok that took more than I thought, but it's still easy to understand.
    let source = String::from("[ 1 ,2, 3]");
    let res = p.parse(&source, true, ParseMetaData::new());
    assert_eq!(res, Ok(hashset!{
        (Expr::List(
            vec![
                Nat(LONat { content: 1 }),
                Nat(LONat { content: 2 }),
                Nat(LONat { content: 3 })
            ]
            .into_iter().map(Box::new).collect()), 10)
    }));

    // notice it also handles empty lists, with space inside (or not)
    let source = String::from("[ ]");
    let res = p.parse(&source, true, ParseMetaData::new());
    assert_eq!(res, Ok(hashset!{(Expr::List(vec![]), 3)}));

    // notice it also handles single-element lists
    let source = String::from("[3]");
    let res = p.parse(&source, true, ParseMetaData::new());
    assert_eq!(res, Ok(hashset!{(Expr::List(vec![Box::new(Expr::Nat(LONat { content: 3 }))]), 3)}));
}

#[test]
fn deep_expr_test() {
    // this will demonstrate how to easily create a robust list parser,
    // for elements which are numbers
    let k_spaces = || kleene(SimpleStrParser::from(" ")); // K* for space

    let separator = " ";

    let expr_p = NatParser(); // <expr> = \d+
    let elem_p = concat(
        expr_p,
        concat(
            concat(k_spaces(), SimpleStrParser::from(separator), Box::new(fst)),
            k_spaces(),
            Box::new(|a, b| a)),
        Box::new(fst)); // \s*<expr>,\s*

    let elem_series = concat(
        kleene(elem_p),
        optional(expr_p), // allows for a hanging separator (ex. `1,2,3,`)
        // add the last element (optionally) at the end.
        Box::new(|v: Vec<Expr>, end| {
            Expr::List(match end {
                Some(end) => {
                    let mut vs = v;
                    vs.push(end);
                    vs
                },
                None => v
            }.into_iter().map(Box::new).collect())
        })
    ); // ((<expr>\s*,\s*)*)(<expr>?)

    // \s*((<expr>\s*,\s*)*)(<expr>?)\s*
    let padded_elem_series = concat(
        k_spaces(),
        concat(elem_series, k_spaces(), Box::new(fst)),
        Box::new(snd));

    // <left> = \[, <right> = \]
    let (left, right) = ("[".to_string(), "]".to_string());

    // final parser:
    // <left>\s*((<expr>\s*,\s*)*)(<expr>?)\s*<right>
    let p = enclose_with(padded_elem_series, &left, &right);

    // ok that took more than I thought, but it's still easy to understand.
    let source = String::from("[ 1 2  3]");
    let res = p.parse(&source, true, ParseMetaData::new());
    assert_eq!(res, Ok(hashset!{
        (Expr::List(
            vec![
                Nat(LONat { content: 1 }),
                Nat(LONat { content: 2 }),
                Nat(LONat { content: 3 }),
            ]
            .into_iter().map(Box::new).collect()), 10)
    }));

    // notice it also handles empty lists, with space inside (or not)
    let source = String::from("[ ]");
    let res = p.parse(&source, true, ParseMetaData::new());
    assert_eq!(res, Ok(hashset!{(Expr::List(vec![]), 3)}));

    // notice it also handles single-element lists
    let source = String::from("[3]");
    let res = p.parse(&source, true, ParseMetaData::new());
    assert_eq!(res, Ok(hashset!{(Expr::List(vec![Box::new(Expr::Nat(LONat { content: 3 }))]), 3)}));
}
