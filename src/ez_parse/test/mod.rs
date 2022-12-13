use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;
use crate::ez_parse::funcs::*;
use crate::{Expr, NatParser, ParseError, ParseMetaData, Parser, StringParser};
use crate::Expr::{Nat, Str};
use crate::ez_parse::funcs::UnionResult::{Left, Right};
use crate::ez_parse::ops::EZ;
use crate::lang_obj::{Identifier, LONat, LOString};

// these tests work, just make them assert the results

#[test]
fn concat_test1() {
    let source = String::from("1234\"abcd\"");
    let p1 = NatParser();
    let p2 = StringParser();
    let p = concat(parser_ref(p1), parser_ref(p2), Box::new(|nat: Expr, str: Expr| (nat, str)));
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
    let p = union(parser_ref(p1), parser_ref(p2));
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
    let i = concat(parser_ref(np), parser_ref(SimpleStrParser::new(&separator)), Box::new(|a, _sep| a));
    let series_i = kleene(parser_ref(i));
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
    let k_spaces = || kleene(parser_ref(SimpleStrParser::from(" "))); // K* for space

    let separator = ",";

    let expr_p = NatParser(); // <expr> = \d+
    let elem_p = concat(
        parser_ref(expr_p),
        parser_ref(concat(
            parser_ref(concat(parser_ref(k_spaces()), parser_ref(SimpleStrParser::from(separator)), Box::new(fst))),
            parser_ref(k_spaces()),
            Box::new(|a, b| a))),
        Box::new(fst)); // \s*<expr>,\s*
    let elem_series = concat(
        parser_ref(kleene(parser_ref(elem_p))),
        parser_ref(optional(parser_ref(expr_p))), // allows for a hanging separator (ex. `1,2,3,`)
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
        parser_ref(k_spaces()),
        parser_ref(concat(parser_ref(elem_series), parser_ref(k_spaces()), Box::new(fst))),
        Box::new(snd));

    // <left> = \[, <right> = \]
    let (left, right) = ("[".to_string(), "]".to_string());

    // final parser:
    // <left>\s*((<expr>\s*,\s*)*)(<expr>?)\s*<right>
    let p = enclose_with(padded_elem_series, &left, &right);


    // ok that took more than I thought, but it's still easy to understand.
    let source = String::from("[ 1 ,2, 3]");
    let res = p.parse(&source, true, ParseMetaData::new());
    assert_eq!(res, Ok(hashset! {
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
    assert_eq!(res, Ok(hashset! {(Expr::List(vec![]), 3)}));

    // notice it also handles single-element lists
    let source = String::from("[3]");
    let res = p.parse(&source, true, ParseMetaData::new());
    assert_eq!(res, Ok(hashset! {(Expr::List(vec![Box::new(Expr::Nat(LONat { content: 3 }))]), 3)}));

    // hanging separator
    let source = "[4,]".to_string();
    let res = p.parse(&source, true, ParseMetaData::new());
    assert_eq!(res, Ok(hashset! {(Expr::List(vec![Box::new(Expr::Nat(LONat { content: 4 }))]), 4)}));
}

#[test]
fn recursive_parser() {
    let base_parser = parser_ref(SimpleStrParser::from("+"));
    let placeholder: FunctionParser<String> = FunctionParser(RefCell::new(Box::new(|content, consume, meta| Err(ParseError::from("no impl")))));
    // *placeholder.0.borrow_mut() = Box::new(|content, consume, meta| Ok(hashset!{("hi".to_string(), 2)}));
    // use epsilon as a placeholder
    let placeholder = parser_ref(placeholder);
    let parser = concat(
        Rc::clone(&base_parser),
        Rc::clone(&placeholder),
        Box::new(|a: String, b: String| a + &b));

    let parser = parser_ref(parser);
    let p = Rc::clone(&parser);
    *placeholder.borrow().0.borrow_mut() = Box::new(
        move |content, consume, meta| {
            match p.borrow().parse(content, consume, meta) {
                Err(_e) => Ok(hashset! {(String::new(), 0)}),
                x => x
            }
        }
    );

    // now we parse...?
    let source = "+".to_string();
    let res = parser.borrow().parse(&source, true, ParseMetaData::new());
    assert_eq!(res, Ok(hashset!{("+".to_string(), 1)}));

    let source = "++".to_string();
    let res = parser.borrow().parse(&source, true, ParseMetaData::new());
    assert_eq!(res, Ok(hashset!{("++".to_string(), 2)}));

    let source = "+++".to_string();
    let res = parser.borrow().parse(&source, true, ParseMetaData::new());
    assert_eq!(res, Ok(hashset!{("+++".to_string(), 3)}));
}

#[test]
fn concat_epsilon_test() {
    // important test, because this helped fix a bug in ParseResult::chain
    let p = concat(parser_ref(SimpleStrParser::from("a")), parser_ref(EpsilonParser), Box::new(fst));
    let source = "a".to_string();
    let res = p.parse(&source, true, ParseMetaData::new());
    println!("{:?}", res);
    assert_eq!(res, Ok(hashset!{("a".to_string(), 1)}))
}

#[test]
fn recursive_test_1() {
    let s = SimpleStrParser::new(&"1".to_string());
    let base_expr = parser_ref(s);
    let placeholder: FunctionParser<Expr> = FunctionParser(RefCell::new(Box::new(|_, _, _| Err("yeet".into()))));
    let placeholder = parser_ref(placeholder);
    let (left_paren, right_paren) = ("(".to_string(), ")".to_string());
    let expr_parser = parser_ref(map(
        parser_ref(union(base_expr, parser_ref(enclose_with2(Rc::clone(&placeholder), &left_paren, &right_paren)))),
        Box::new(|u| {
            match u {
                Left(_one) => Nat(1.into()),
                Right(expr) => expr
            }
        })
    ));
    let _self_parser = Rc::clone(&expr_parser);
    *placeholder.borrow().0.borrow_mut() = Box::new(move |content, consume, meta| {
        if meta.was_infix {
            Err("tried to use self parser again".into())
        } else {
            _self_parser.borrow().parse(content, consume, meta.with_infix())
        }
    });

    let src = "1".to_string();
    let res = expr_parser.borrow().parse(&src, true, ParseMetaData::new());
    assert_eq!(res, Ok(hashset!{(Nat(LONat { content: 1 }), 1)}));

    let src = "(1)".to_string();
    let res = expr_parser.borrow().parse(&src, true, ParseMetaData::new());
    assert_eq!(res, Ok(hashset!{(Nat(LONat { content: 1 }), 3)}));
}


#[test]
fn deep_expr_test() {
    // our play-parser only parses numbers (1234) or an underscore (_)
    // let base_expr = parser_ref(map(
    //     parser_ref(union(
    //     parser_ref(NatParser()),
    //     parser_ref(SimpleStrParser::from("_")))),
    //     |p| match p {
    //         UnionResult::Left(nat) => nat,
    //         UnionResult::Right(_underscore) => Expr::Variable(Identifier::Unit("_".to_string()))
    //     }
    // ));
    let base_expr = parser_ref(NatParser());

    // create a placeholder function parser, which will eventually hold our actual expression parser
    // (the goal of the expression parser is to union it with itself)
    let placeholder = || parser_ref(
        FunctionParser(RefCell::new(Box::new(|_, _, _| Err(ParseError::from("placeholder")))))
    );

    let p_expr = placeholder();

    let infix = parser_ref(concat(
        Rc::clone(&p_expr),
        parser_ref(concat(
                parser_ref(SimpleStrParser::from("+")),
                Rc::clone(&p_expr),
                Box::new(snd)
            )),
        Box::new(|left, right| Expr::Infix(Box::new(left), "+".to_string(), Box::new(right)))
    ));

    let expr =
            parser_ref(map(
                parser_ref(union(
                    parser_ref(map(
                        parser_ref(union(infix, base_expr)),
                        Box::new(|u| {
                            // println!("joining an infix or base <{:?}>", u);
                            match u {
                                Left(expr) => expr,
                                Right(expr) => expr,
                            }
                        }))),
                    Rc::clone(&p_expr)
                )),
                |e| {
                    // println!("Joining an infix or another expression <{:?}>", e);
                    match e {
                        Left(infix) => infix,
                        Right(other) => other
                    }
                }
            ));

    let _expr = Rc::clone(&expr);
    *p_expr.borrow().0.borrow_mut() = Box::new(move |content, consume, meta| {
        // println!("Function parser being called for {} with meta {:?}", content, meta);
        if meta.same_paths() || meta.max_history_cycles() > 2 {
            Err("Followed the exact same path twice".into())
        } else {
            let meta = meta.rotate_paths();
            _expr.borrow().parse(content, consume, meta)
        }
    });

    ///////////////////////////////
    let source = "1+2+3+4".to_string();
    let res = expr.borrow().parse(&source, true, ParseMetaData::new());
    println!("{:?}", res);
}

#[cfg(test)]
mod json;