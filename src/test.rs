use std::borrow::Borrow;
use std::cell::RefCell;
use std::rc::{Rc, Weak};
use crate::box_parser;
use crate::parse::{chainable, LengthQualifier, ListParser, ParseMetaData, Parser, ParseResult, TakeWhileParser};
use crate::funcs::{join, substring, take, take_while};
use crate::lang_obj::{Expr, Identifier, LONat, LOString};
use crate::lang_obj::Expr::{Infix, Nat, Str};
use crate::parse::{GenericExprParser, ExprParser, InfixParser, NatParser, ParentheticalParser, StringParser};
use crate::parse::LengthQualifier::LEQ;

#[test]
fn misc_string_tests() {
    let original = "नमस्ते्";
    let chars: Vec<char> = original.chars().collect();
    let end = join(chars);
    assert_eq!(String::from(original), end);
}

#[test]
fn take_test() {
    let s = "hello".to_string();
    assert_eq!(take(&s, 1), Some(("h".to_string(), "ello".to_string())));
    assert_eq!(take(&s, 2), Some(("he".to_string(), "llo".to_string())));
    assert_eq!(take(&s, 6), None);
}

#[test]
fn take_while_test() {
    let s = "abc123d4e5.f,(\"532\",);".to_string();
    assert_eq!(take_while(&s, |c| c.is_digit(10)).0, "".to_string());
    assert_eq!(take_while(&s, |c| c.is_alphabetic()).0, "abc".to_string());
    assert_eq!(take_while(&s, |c| c.is_alphanumeric()).0, "abc123d4e5".to_string());
}

#[test]
fn parse_str_nat() {
    let context = ParseMetaData::new();

    let s = "\"abcd\" abc!".to_string();
    assert_eq!(
        StringParser().parse(&s, false, context).unwrap(),
        hashset![(
            Expr::Str(LOString {
                content: "abcd".to_string()
            }),
            6 as usize
        )]
    );

    let s = "\"this is the good string haha! yay! 'yeet'!\"".to_string();
    assert_eq!(
        StringParser().parse(&s, false, context).unwrap(),
        hashset![(
            Expr::Str(LOString {
                content: substring(&s, 1, s.len() - 2).unwrap()
            }),
            s.len()
        )]
    );

    let s = "143abc".to_string();
    assert_eq!(
        NatParser().parse(&s, false, context).unwrap(),
        hashset![(
            Expr::Nat(LONat {
                content: 143
            }),
            3
        )]
    );
}

#[test]
fn test_expr_parse() {

    // first make our expression parser
    let string_parser = Rc::new(box_parser!(StringParser()));
    let nat_parser = Rc::new(box_parser!(NatParser()));

    let root_parsers = RefCell::new(vec![
        &string_parser,
        &nat_parser
    ].into_iter().map(Rc::downgrade).collect());

    let parser = ExprParser {
        parsers: root_parsers
    };
    let context = ParseMetaData::new();

    let content = "\"test string\"".to_string();
    let result = parser.parse(&content, true, context);
    // println!("{:?}", result);
    assert_eq!(
        result,
        Ok(hashset![(Str("test string".to_string().into()), 13)])
    )
}

#[test]
fn test_infix_parse_1() {
    // first make our expression parser
    let string_parser = Rc::new(box_parser!(StringParser()));
    let nat_parser = Rc::new(box_parser!(NatParser()));

    let root_parsers = RefCell::new(vec![
        &string_parser,
        &nat_parser
    ].into_iter().map(Rc::downgrade).collect());

    let expr_parser = Rc::new(ExprParser {
        parsers: root_parsers
    });

    let parser = InfixParser {
        expr_parser: Rc::clone(&expr_parser),
        infix: "+".to_string(),
    };

    let context = ParseMetaData::new();
    let test = "\"test string\" + \"yay\"".to_string();
    // println!("{:#?}", parser.parse(&test));
    let expected_result =
        Ok(hashset![
            (
                Expr::Infix(
                    Str(LOString {
                            content: "test string".to_string()
                    }).into(),
                    "+".to_string(),
                    Str(LOString {
                        content: "yay".to_string()
                    }).into()
                ),
                21
            )
        ]);
    assert_eq!(
        parser.parse(&test, true, context),
        expected_result
    );

    let test = "\"test string1\" + 36".to_string();
    // println!("{:#?}", parser.parse(&test));
    assert_eq!(
        parser.parse(&test, true, context),
        Ok(
            hashset!{
                (
                    Infix(
                        Str(
                            LOString {
                                content: "test string1".to_string(),
                            },
                        ).into(),
                        "+".to_string(),
                        Nat(
                            LONat {
                                content: 36,
                            },
                        ).into(),
                    ),
                    19,
                )
            },
        )
    );

    let test = "\"test string1\"+ 36".to_string();
    // println!("{:#?}", parser.parse(&test));
    assert_eq!(
        parser.parse(&test, true, context),
        Ok(
            hashset!{
                (
                    Infix(
                        Str(
                            LOString {
                                content: "test string1".to_string(),
                            },
                        ).into(),
                        "+".to_string(),
                        Nat(
                            LONat {
                                content: 36,
                            },
                        ).into(),
                    ),
                    18,
                )
            },
        )
    );
}

#[test]
fn test_extended_expressions() {
    let string_parser = Rc::new(box_parser!(StringParser()));
    let nat_parser = Rc::new(box_parser!(NatParser()));

    // start off with a couple simple parsers
    let root_parsers = RefCell::new(vec![
        &string_parser,
        &nat_parser
    ].into_iter().map(Rc::downgrade).collect());

    // collect them together with an ExprParser
    let expr_parser = Rc::new(ExprParser {
        parsers: root_parsers
    });

    // create a parenthetical parser, that uses the expr_parser to parse interior expressions
    let parenthetical_parser = Rc::new(box_parser!(ParentheticalParser {
        expr_parser: Rc::clone(&expr_parser)
    }));

    // add the parenthetical parser to the list of parsers in the expression parser
    expr_parser.parsers.borrow_mut().push(Rc::downgrade(&parenthetical_parser));

    // to be clear, we are testing the expression parser
    let parser = expr_parser;
    let context = ParseMetaData::new();

    let test = "((32))".to_string();
    assert_eq!(parser.parse(&test, true, context), Ok(hashset![(Expr::Nat(LONat {
        content: 32
    }), 6)]));

    let test = "32".to_string();
    assert_eq!(parser.parse(&test, true, context), Ok(hashset![(Expr::Nat(LONat {
        content: 32
    }), 2)]));
    // this test passing successfully demonstrates that a parser can use itself *inside itself*
    // note that the parenthetical parser is being used inside the expression parser, which is
    // used by the parenthetical parser, which is used by the expression parser

    /*

    order of events:

    1. "please parse this expression, using anything that works"

    "((32))" -> expression parser
       -> tries numerical parse, fails
       -> tries string parse, fails
       -> tries parenthetical parse:

       2. what is the value of the parse that didn't fail?
         "((32))" -> parenthetical parser
            - deconstruction -
            "(32)" -> expression parser
              -> tries numerical parse, fails
              -> tries string parse, fails
              -> tries parenthetical parse, succeeds:
                "(32)" -> parenthetical parser
                - deconstruction -
                "32" -> expression parser
                  -> tries numerical parse, succeeds:
                    -> returns 32 as a Language Object
                  -> tries string parse, fails
                  -> tries parenthetical parse, fails
                  (note that all parses are tried every time, no matter what.
                   I have no idea how real parsing works, but I like this, since
                   it will tell me if I have ambiguous syntax)
     */
}

#[test]
fn parse_deep_infix() {
    // combine infix and parenthetical parser for big expression parsing power!
    let string_parser = Rc::new(box_parser!(StringParser()));
    let nat_parser = Rc::new(box_parser!(NatParser()));

    // start off with a couple simple parsers
    let root_parsers = RefCell::new(vec![
        &string_parser,
        &nat_parser
    ].into_iter().map(Rc::downgrade).collect());

    // collect them together with an ExprParser
    let parser = Rc::new(ExprParser {
        parsers: root_parsers
    });

    // create a parenthetical parser, that uses the expr_parser to parse interior expressions
    let parenthetical_parser = Rc::new(box_parser!(ParentheticalParser {
        expr_parser: Rc::clone(&parser)
    }));

    // add the parenthetical parser to the list of parsers in the expression parser
    parser.parsers.borrow_mut().push(Rc::downgrade(&parenthetical_parser));

    // same with infix
    let infix_parser = Rc::new(box_parser!(InfixParser {
        expr_parser: Rc::clone(&parser),
        infix: "+".to_string(),
    }));
    parser.parsers.borrow_mut().push(Rc::downgrade(&infix_parser));

    // test time!
    let context = ParseMetaData::new();

    let test = "(32) + 12".to_string();
    assert_eq!(parser.parse(&test, true, context), Ok(hashset![(Expr::Infix(
        Expr::Nat(LONat {
            content: 32
        }).into(),
        "+".to_string(),
        Expr::Nat(12.into()).into()
    ), 9)]));


    let test = "12 + 12 + 12".to_string();
    assert_eq!(
        parser.parse(&test, true, context),
        Ok(
            hashset![
                (
                    Expr::Infix(
                        Expr::Nat(12.into()).into(),
                        "+".to_string(),
                        Expr::Infix(
                            Expr::Nat(12.into()).into(),
                            "+".to_string(),
                            Expr::Nat(12.into()).into()
                        ).into()
                    ),
                    12
                )
            ]
        )
    );

    // now let's try forcing left-precedence
    let test = "(12 + 12) + 12".to_string();
    assert_eq!(
        parser.parse(&test, true, context),
        Ok(
            hashset![
                (
                    Expr::Infix(
                        Expr::Infix(
                            Expr::Nat(12.into()).into(),
                            "+".to_string(),
                            Expr::Nat(12.into()).into()
                        ).into(),
                        "+".to_string(),
                        Expr::Nat(12.into()).into()
                    ),
                    14
                )
            ]
        )
    );

}

#[test]
fn list_test_1() {
    let string_parser = Rc::new(box_parser!(StringParser()));
    let nat_parser = Rc::new(box_parser!(NatParser()));

    // start off with a couple simple parsers
    let root_parsers = RefCell::new(vec![
        &string_parser,
        &nat_parser
    ].into_iter().map(Rc::downgrade).collect());

    // collect them together with an ExprParser
    let expr_parser = Rc::new(ExprParser {
        parsers: root_parsers
    });

    let parser = ListParser {
        expr_parser: Rc::clone(&expr_parser),
        separator: ','
    };

    let test = "[1,2, 3]".to_string();
    assert_eq!(
        parser.parse(&test, true, ParseMetaData::new()),
        Ok(hashset![
            (
                Expr::List(vec![
                    Nat(LONat { content: 1 }).into(),
                    Nat(LONat { content: 2 }).into(),
                    Nat(LONat { content: 3 }).into()]),
                8
            )
        ])
    );
}

#[test]
pub fn list_test_2() {
    let string_parser = Rc::new(box_parser!(StringParser()));
    let nat_parser = Rc::new(box_parser!(NatParser()));

    // start off with a couple simple parsers
    let root_parsers = RefCell::new(vec![
        &string_parser,
        &nat_parser
    ].into_iter().map(Rc::downgrade).collect());

    // collect them together with an ExprParser
    let expr_parser = Rc::new(ExprParser {
        parsers: root_parsers
    });

    let parser = ListParser {
        expr_parser: Rc::clone(&expr_parser),
        separator: ','
    };
    let test = "[\"a\",2,\"b\",4, 800808,01401740]".to_string();
    assert_eq!(
        parser.parse(&test, true, ParseMetaData::new()),
        Ok(hashset![
            (
                Expr::List(vec![
                    Str(LOString { content: "a".into() }),
                    Nat(LONat { content: 2 }),
                    Str(LOString { content: "b".into() }),
                    Nat(LONat { content: 4 }),
                    Nat(LONat { content: 800808 }),
                    Nat(LONat { content: 1401740 })
                ].into_iter().map(Box::new).collect()),
                30)
        ])
    );
}

#[test]
fn parse_deep_list_expressions() {
    // combine infix and parenthetical parser for big expression parsing power!
    let string_parser = Rc::new(box_parser!(StringParser()));
    let nat_parser = Rc::new(box_parser!(NatParser()));

    // start off with a couple simple parsers
    let root_parsers = RefCell::new(vec![
        &string_parser,
        &nat_parser
    ].into_iter().map(Rc::downgrade).collect());

    // collect them together with an ExprParser
    let parser = Rc::new(ExprParser {
        parsers: root_parsers
    });

    // create a parenthetical parser, that uses the expr_parser to parse interior expressions
    let parenthetical_parser = Rc::new(box_parser!(ParentheticalParser {
        expr_parser: Rc::clone(&parser)
    }));

    // add the parenthetical parser to the list of parsers in the expression parser
    parser.parsers.borrow_mut().push(Rc::downgrade(&parenthetical_parser));

    // same with infix
    let infix_parser = Rc::new(box_parser!(InfixParser {
        expr_parser: Rc::clone(&parser),
        infix: "+".to_string(),
    }));
    parser.parsers.borrow_mut().push(Rc::downgrade(&infix_parser));

    // work in progress (add list parser here)
    let list_parser = Rc::new(box_parser!(ListParser {
        separator: ' ', // ooh! space separator!
        expr_parser: Rc::clone(&parser)
    }));
    parser.parsers.borrow_mut().push(Rc::downgrade(&list_parser));

    // ready to test!
    let test = "[1 + 2 3 \n\"hello\" + 1]".to_string();
    assert_eq!(
        parser.parse(&test, true, ParseMetaData::new()),
        Ok(hashset![
            (
                Expr::List(vec![
                    Infix(
                        Nat(LONat { content: 1 }).into(),
                        "+".to_string(),
                        Nat(LONat { content: 2 }).into()
                    ).into(),
                    Nat(LONat { content: 3 }).into(),
                    Infix(
                        Str(LOString { content: "hello".into() }).into(),
                        "+".to_string(),
                        Nat(LONat { content: 1 }).into()
                    ).into()
                ]),
                22
            )
        ])
    )
}

#[test]
fn chain_test() {
    let num_parser = Rc::new(box_parser!(NatParser()));
    let parenthetical_parser = ParentheticalParser {
        expr_parser: Rc::new(ExprParser {
            parsers: RefCell::new(vec![
                Rc::downgrade(&num_parser)
            ])
        })
    };

    let test = "func_name(12)".to_string();
    // supposing "func_name" has already been parsed, parse the rest of the string!
    let parses = ParseResult(
        // create a fake parse result
        Ok(hashset![
            (Identifier::Unit("func_name".to_string()), 9)
        ])
    )
        // chain it with a parenthetical parse
        .chain(
        &test,
        true,
        ParseMetaData::new(),
        // using 'chainable' to chain the output of the parse into the right type
        chainable(
            |ident, content, meta| {
                parenthetical_parser.parse(&content, true, meta.increment_depth())
            }
        ),
        |e, s| (e, s)
    );
    assert_eq!(parses, ParseResult(Ok(hashset![((Identifier::Unit("func_name".to_string()), Nat(12.into())), 13)])));
}

#[test]
fn take_while_parser_test() {
    let space_parser = TakeWhileParser {
        func: Box::new(|c: &char| c.is_whitespace()),
        amount: LengthQualifier::GEQ(0)
    };

    let test = " ,".to_string();
    let meta = ParseMetaData::new();
    assert_eq!(
        space_parser.parse(&test, false, meta.clone()),
        Ok(hashset![
            ("".to_string(), 0),
            (" ".to_string(), 1)
        ])
    );

    let test = "  \ta".to_string();
    let space_parser = TakeWhileParser {
        func: Box::new(|c: &char| c.is_whitespace()),
        amount: LengthQualifier::LEQ(3, true)
    };
    assert_eq!(
        space_parser.parse(&test, false, meta.clone()),
        Ok(hashset![
            ("".to_string(), 0),
            (" ".to_string(), 1),
            ("  ".to_string(), 2),
            ("  \t".to_string(), 3)
        ])
    );
}