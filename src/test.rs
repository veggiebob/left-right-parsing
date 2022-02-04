use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::{Rc, Weak};
use std::sync::Arc;

use crate::{box_expr_parser, box_stmt_parser};
use crate::funcs::{join, substring, take, take_while};
use crate::lang_obj::{Expr, Identifier, LONat, LOString, Program};
use crate::lang_obj::Expr::{Conditional, Infix, List, Nat, Str, Variable};
use crate::lang_obj::formatting::{Format, JSON};
use crate::lang_obj::Identifier::Unit;
use crate::lang_obj::Statement;
use crate::lang_obj::Statement::{FnDef, Let};
use crate::parse::{chainable, ConditionalParser, LengthQualifier, ListParser, ParseMetaData, Parser, ParseResult, TakeWhileParser};
use crate::parse::{ExprParser, GenericExprParser, InfixParser, NatParser, ParentheticalParser, StringParser};
use crate::parse::LengthQualifier::LEQ;
use crate::parse::structure_parsers::{FnDefParser, IdentifierParser, LetParser, StatementParser, TypeParser, VariableParser};
use crate::program_parsing::ProgramParser;
use crate::semantic_validation::{ExpressionValidator, ProgramValidator, ScopeFrame, Validator};

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
fn test_0_depth_parser() {

    // first make our expression parser
    let string_parser = Rc::new(box_expr_parser!(StringParser()));
    let nat_parser = Rc::new(box_expr_parser!(NatParser()));

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
fn identifier_parse() {
    const INFIX_ADDITION_SYMBOL: &str = "+"; // for expressions, like "+" in "3 + 4"
    const INFIX_MULTIPLICATION_SYMBOL: &str = "*"; // for expressions like "*" in "3 * 4"
    const INFIX_SUBTRACTION_SYMBOL: &str = "-"; // for expressions like "-" in "4 - 3"
    const INFIX_FUNCTION_SYMBOL: &str = " "; // for expressions like "$" in "func $ [arg]"
    const INFIX_EQUALITY_SYMBOL: &str = "="; // for checking equality; for expressions like "=" in "x = y"
    const LIST_SEPARATOR_SYMBOL: char = ','; // for list expressions, like ',' in "[1, 2, 3]"
    const IDENTIFIER_ALLOWED_CHARS: &str = "_"; // also, by default, includes a-zA-Z
    const FUNCTION_ARGUMENT_SEPARATOR: char = ','; // seems simple
    const FUNCTION_ARGUMENT_INFIX_SYMBOL: &str = ":"; // like ":" in "func_name[arg1: type1, arg2: type2] => ..."

    ////////////////////////////////////////////////////
    // first, assemble only the finest expression parser
    ////////////////////////////////////////////////////

    // primitive parsers
    let string_parser = Rc::new(box_expr_parser!(StringParser()));
    let nat_parser = Rc::new(box_expr_parser!(NatParser()));
    let var_parser = Rc::new(box_expr_parser!(VariableParser::default(IdentifierParser::new(IDENTIFIER_ALLOWED_CHARS))));

    // start off with a couple simple parsers
    let root_parsers = RefCell::new(vec![
        &string_parser,
        &nat_parser,
        &var_parser
    ].into_iter().map(Rc::downgrade).collect());

    // create the expression parser
    let expr_parser = Rc::new(ExprParser {
        parsers: root_parsers
    });

    let test = "abc".to_string();
    assert_eq!(expr_parser.parse(&test, true, ParseMetaData::new()).unwrap(), hashset![(Variable(Identifier::Unit("abc".into())), 3)]);
}

#[test]
fn test_infix_parse_1() {
    // first make our expression parser
    let string_parser = Rc::new(box_expr_parser!(StringParser()));
    let nat_parser = Rc::new(box_expr_parser!(NatParser()));

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
    let string_parser = Rc::new(box_expr_parser!(StringParser()));
    let nat_parser = Rc::new(box_expr_parser!(NatParser()));

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
    let parenthetical_parser = Rc::new(box_expr_parser!(ParentheticalParser {
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
    let string_parser = Rc::new(box_expr_parser!(StringParser()));
    let nat_parser = Rc::new(box_expr_parser!(NatParser()));

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
    let parenthetical_parser = Rc::new(box_expr_parser!(ParentheticalParser {
        expr_parser: Rc::clone(&parser)
    }));

    // add the parenthetical parser to the list of parsers in the expression parser
    parser.parsers.borrow_mut().push(Rc::downgrade(&parenthetical_parser));

    // same with infix
    let infix_parser = Rc::new(box_expr_parser!(InfixParser {
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
fn parse_spaced_infix() {
    // combine infix and parenthetical parser for big expression parsing power!
    let string_parser = Rc::new(box_expr_parser!(StringParser()));
    let nat_parser = Rc::new(box_expr_parser!(NatParser()));

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
    let parenthetical_parser = Rc::new(box_expr_parser!(ParentheticalParser {
        expr_parser: Rc::clone(&parser)
    }));

    // add the parenthetical parser to the list of parsers in the expression parser
    parser.parsers.borrow_mut().push(Rc::downgrade(&parenthetical_parser));

    // same with infix
    let infix_parser = Rc::new(box_expr_parser!(InfixParser {
        expr_parser: Rc::clone(&parser),
        infix: " ".to_string(),
    }));
    parser.parsers.borrow_mut().push(Rc::downgrade(&infix_parser));

    // test time!
    let context = ParseMetaData::new();
    let test = "12 13".to_string();
    assert_eq!(
        parser.parse(&test, true, context),
        Ok(
            hashset!{
                (
                    Infix(
                        Nat(
                            LONat {
                                content: 12,
                            },
                        ).into(),
                        " ".into(),
                        Nat(
                            LONat {
                                content: 13,
                            },
                        ).into(),
                    ),
                    5,
                ),
            },
        )
    );

    let test = "12  13".to_string();
    assert_eq!(
        parser.parse(&test, true, context),
        Ok(
            hashset!{
                (
                    Infix(
                        Nat(
                            LONat {
                                content: 12,
                            },
                        ).into(),
                        " ".into(),
                        Nat(
                            LONat {
                                content: 13,
                            },
                        ).into(),
                    ),
                    6,
                ),
            },
        )
    );

    let test = "12   13".to_string();
    assert_eq!(
        parser.parse(&test, true, context),
        Ok(
            hashset!{
                (
                    Infix(
                        Nat(
                            LONat {
                                content: 12,
                            },
                        ).into(),
                        " ".into(),
                        Nat(
                            LONat {
                                content: 13,
                            },
                        ).into(),
                    ),
                    7,
                ),
            },
        )
    );
}

#[test]
fn list_test_1() {
    let string_parser = Rc::new(box_expr_parser!(StringParser()));
    let nat_parser = Rc::new(box_expr_parser!(NatParser()));

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

    // test single element in list
    let test = "[1]".to_string();
    assert_eq!(
        parser.parse(&test, true, ParseMetaData::new()),
        Ok(hashset![
            (
                Expr::List(vec![
                    Nat(LONat { content: 1 }).into()]),
                3
            )
        ])
    );

    let test = "[]".to_string();
    assert_eq!(
        parser.parse(&test, true, ParseMetaData::new()),
        Ok(hashset![
            (
                Expr::List(vec![]),
                2
            )
        ])
    );
}

#[test]
pub fn list_test_2() {
    let string_parser = Rc::new(box_expr_parser!(StringParser()));
    let nat_parser = Rc::new(box_expr_parser!(NatParser()));

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
    let string_parser = Rc::new(box_expr_parser!(StringParser()));
    let nat_parser = Rc::new(box_expr_parser!(NatParser()));

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
    let parenthetical_parser = Rc::new(box_expr_parser!(ParentheticalParser {
        expr_parser: Rc::clone(&parser)
    }));

    // add the parenthetical parser to the list of parsers in the expression parser
    parser.parsers.borrow_mut().push(Rc::downgrade(&parenthetical_parser));

    // same with infix
    let infix_parser = Rc::new(box_expr_parser!(InfixParser {
        expr_parser: Rc::clone(&parser),
        infix: "+".to_string(),
    }));
    parser.parsers.borrow_mut().push(Rc::downgrade(&infix_parser));

    let list_parser = Rc::new(box_expr_parser!(ListParser {
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
    );

    // parse recursive lists
    let test = "[[1] [2] [3] \"matrix!\"]".to_string();
    assert_eq!(
        parser.parse(&test, true, ParseMetaData::new()),
        Ok(hashset!{(List(vec![
            List(vec![Nat(LONat { content: 1 }).into()]).into(),
            List(vec![Nat(LONat { content: 2 }).into()]).into(),
            List(vec![Nat(LONat { content: 3 }).into()]).into(),
            Str(LOString { content: "matrix!".to_string() }).into()
        ].into()), 23)})
    );
}

#[test]
fn chain_test() {
    let num_parser = Rc::new(box_expr_parser!(NatParser()));
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

#[test]
fn conditional_if_else_test() {
    // fairly ominous parser

    let string_parser = Rc::new(box_expr_parser!(StringParser()));
    let nat_parser = Rc::new(box_expr_parser!(NatParser()));

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
    let parenthetical_parser = Rc::new(box_expr_parser!(ParentheticalParser {
        expr_parser: Rc::clone(&parser)
    }));

    // add the parenthetical parser to the list of parsers in the expression parser
    parser.parsers.borrow_mut().push(Rc::downgrade(&parenthetical_parser));

    // same with infix
    let infix_parser = Rc::new(box_expr_parser!(InfixParser {
        expr_parser: Rc::clone(&parser),
        infix: "+".to_string(),
    }));
    parser.parsers.borrow_mut().push(Rc::downgrade(&infix_parser));

    let list_parser = Rc::new(box_expr_parser!(ListParser {
        separator: ',', // ooh! space separator!
        expr_parser: Rc::clone(&parser)
    }));
    parser.parsers.borrow_mut().push(Rc::downgrade(&list_parser));

    let condition_parser = Rc::new(box_expr_parser!(ConditionalParser {
        expr_parser: Rc::clone(&parser)
    }));
    parser.parsers.borrow_mut().push(Rc::downgrade(&condition_parser));

    // test time!
    let ctx = ParseMetaData::new();

    let test = "if 13 14 else 15".to_string();
    // println!("length of test: {}", test.len());
    // println!("{:#?}", parser.parse(&test, true, ctx));
    assert_eq!(
        parser.parse(&test, true, ctx),
        Ok(
            hashset!{
                (
                    Expr::Conditional(
                        Nat(
                            LONat {
                                content: 13,
                            },
                        ).into(),
                        Nat(
                            LONat {
                                content: 14,
                            },
                        ).into(),
                        Nat(
                            LONat {
                                content: 15,
                            },
                        ).into(),
                    ),
                    test.len()
                ),
            },
        )
    );

    let test = "if [13, 11] 14 else 15".to_string();
    // println!("length of test: {}", test.len());
    // println!("{:#?}", parser.parse(&test, true, ctx));
    assert_eq!(
        parser.parse(&test, true, ctx),
        Ok(
            hashset!{
                (
                    Expr::Conditional(
                        List(vec![
                            Nat(LONat { content: 13 }).into(),
                            Nat(LONat { content: 11 }).into()
                        ]).into(),
                        Nat(
                            LONat {
                                content: 14,
                            },
                        ).into(),
                        Nat(
                            LONat {
                                content: 15,
                            },
                        ).into(),
                    ),
                    test.len()
                ),
            },
        )
    );

    // elseif!
    let test = "if [13, 11] 14 elseif \"true\" \"true\" else \"false\"".to_string();
    // println!("length of test: {}", test.len());
    // println!("{:#?}", parser.parse(&test, true, ctx));
    assert_eq!(
        parser.parse(&test, true, ctx),
        Ok(
            hashset!{
                (
                    Expr::Conditional(
                        List(vec![
                            Nat(LONat { content: 13 }).into(),
                            Nat(LONat { content: 11 }).into()
                        ]).into(),
                        Nat(
                            LONat {
                                content: 14,
                            },
                        ).into(),
                        Expr::Conditional(
                            Str(LOString { content: "true".to_string() }).into(),
                            Str(LOString { content: "true".to_string() }).into(),
                            Str(LOString { content: "false".to_string() }).into()
                        ).into()
                    ),
                    test.len()
                ),
            },
        )
    );
}

#[test]
fn test_let_parse() {

    let string_parser = Rc::new(box_expr_parser!(StringParser()));
    let nat_parser = Rc::new(box_expr_parser!(NatParser()));

    let root_parsers = RefCell::new(vec![
        &string_parser,
        &nat_parser
    ].into_iter().map(Rc::downgrade).collect());

    let expr_parser = ExprParser {
        parsers: root_parsers
    };

    let let_parser = LetParser {
        id_parser: Rc::new(IdentifierParser::new("")),
        expr_parser: Rc::new(expr_parser)
    };

    let context = ParseMetaData::new();

    let test = "let x = 32".to_string();
    // println!("{:?}", let_parser.parse(&test, true, context))
    assert_eq!(
        let_parser.parse(&test, true, context),
        Ok(hashset!{(Let(Unit("x".to_string()), Nat(LONat { content: 32 })), test.len())})
    );

    let test = "let x=\"hello world\"".to_string();
    assert_eq!(
        let_parser.parse(&test, true, context),
        Ok(hashset!{(Let(Unit("x".to_string()), Str(LOString { content: "hello world".to_string() })), test.len())})
    );
}

#[test]
fn weirder_let_parse_tests() {

    let string_parser = Rc::new(box_expr_parser!(StringParser()));
    let nat_parser = Rc::new(box_expr_parser!(NatParser()));

    let root_parsers = RefCell::new(vec![
        &string_parser,
        &nat_parser
    ].into_iter().map(Rc::downgrade).collect());

    let expr_parser = ExprParser {
        parsers: root_parsers
    };

    let let_parser = LetParser {
        id_parser: Rc::new(IdentifierParser::new(".=-2")), // new charset
        expr_parser: Rc::new(expr_parser)
    };

    let context = ParseMetaData::new();

    let test = "let .-. = 32".to_string();
    // println!("{:?}", let_parser.parse(&test, true, context))
    assert_eq!(
        let_parser.parse(&test, true, context),
        Ok(hashset!{(Let(Unit(".-.".to_string()), Nat(LONat { content: 32 })), test.len())})
    );

    let test = "let hello=2=\"hello world\"".to_string();
    assert_eq!(
        let_parser.parse(&test, true, context),
        Ok(hashset!{(Let(Unit("hello=2".to_string()), Str(LOString { content: "hello world".to_string() })), test.len())})
    );
}

#[test]
fn fn_def_parse_test() {
    // create a basic expression parser (shallow, no recursion)
    let string_parser = Rc::new(box_expr_parser!(StringParser()));
    let nat_parser = Rc::new(box_expr_parser!(NatParser()));

    let root_parsers = RefCell::new(vec![
        &string_parser,
        &nat_parser
    ].into_iter().map(Rc::downgrade).collect());

    let expr_parser = Rc::new(ExprParser {
        parsers: root_parsers
    });

    // create arg parser to parse things like [var_name : var_type, var_name2 : var_type2]
    let arg_name_parser = Rc::new(box_expr_parser!(VariableParser::default(IdentifierParser::new("_"))));
    let arg_type_parser = Rc::new(box_expr_parser!(VariableParser::default(IdentifierParser::new("_"))));
    let arg_name_type_parser = Rc::new(ExprParser {
        parsers: RefCell::new(vec![
            Rc::downgrade(&arg_name_parser),
            Rc::downgrade(&arg_type_parser)
        ])
    });
    let arg_infix = Rc::new(box_expr_parser!(InfixParser {
        expr_parser: Rc::clone(&arg_name_type_parser),
        infix: ":".to_string()
    }));
    let arg_parser = Rc::new(ExprParser {
        parsers: RefCell::new(vec![
            Rc::downgrade(&arg_infix)
        ])
    });
    let arg_parser = Rc::new(ListParser {
        expr_parser: arg_parser,
        separator: ','
    });

    let stmt_parser = Rc::new(StatementParser {
        parsers: RefCell::new(vec![])
    });

    // create function definition parser
    let fn_parser = Rc::new(box_stmt_parser!(FnDefParser {
        id_parser: Rc::new(IdentifierParser::new("_")),
        statement_parser: Rc::clone(&stmt_parser),
        type_parser: Rc::new(TypeParser(IdentifierParser::new(""))),
        expr_parser: Rc::clone(&expr_parser),
        arg_parser
    }));

    // create let statement parser
    let let_parser = Rc::new(box_stmt_parser!(LetParser {
        id_parser: Rc::new(IdentifierParser::new("_")),
        expr_parser: Rc::clone(&expr_parser)
    }));

    stmt_parser.parsers.borrow_mut().extend(vec![
        Rc::downgrade(&fn_parser),
        Rc::downgrade(&let_parser)
    ]);

    let context = ParseMetaData::new();
    let test = "fn_name[var_a: nice, var_b: nicee] => \"hello world\"".to_string();
    // println!("test length: {}", test.len());
    // println!("{:?}", fn_parser.parse(&test, true, context));
    assert_eq!(
        fn_parser.parse(&test, true, context),
        Ok(hashset!{
            (
                FnDef(
                    "fn_name".into(),
                    vec![
                        (Unit("var_a".into()), "nice".into()),
                        (Unit("var_b".into()), "nicee".into())
                    ],
                    Str(
                        LOString { content: "hello world".into() }
                    ),
                    vec![].into()
                ),
                51
            )
        })
    );

    let test = "fn_name[var_a : nice, var_b : nicee]=> 32".to_string();
    // println!("test length: {}", test.len());
    // println!("{:?}", fn_parser.parse(&test, true, context));
    assert_eq!(
        fn_parser.parse(&test, true, context),
        Ok(hashset!{
            (
                FnDef(
                    "fn_name".to_string(),
                    vec![
                        (Unit("var_a".into()), "nice".to_string()),
                        (Unit("var_b".into()), "nicee".to_string())
                    ],
                    Nat(LONat::from(32)),
                    vec![].into()
                ),
                41
            )
        })
    );

    let test = "fn_names [var_a : nice, var_b : nicee]=> 32".to_string();
    // println!("test length: {}", test.len());
    // println!("{:?}", fn_parser.parse(&test, true, context));
    assert_eq!(
        fn_parser.parse(&test, true, context),
        Ok(hashset!{
            (
                FnDef(
                    "fn_names".to_string(),
                    vec![
                        (Unit("var_a".into()), "nice".to_string()),
                        (Unit("var_b".into()), "nicee".to_string())
                    ],
                    Nat(LONat::from(32)),
                    vec![].into()
                ),
                43
            )
        })
    );

    let test = "fn_name[var_a : nice, var_b : nicee]=> 32\"hi\"".to_string();
    // println!("{:?}", fn_parser.parse(&test, true, context));
    assert!(fn_parser.parse(&test, true, context).is_err());

    // deeper!!!

    // vanity tests ;)
    let test = "fn_name[var_a : nice] => 32 where { }".to_string();
    // println!("length of test: {}", test.len());
    // println!("{:?}", fn_parser.parse(&test, true, context));
    assert_eq!(
        fn_parser.parse(&test, true, context),
        Ok(hashset!{
            (
                FnDef("fn_name".into(), vec![(Unit("var_a".into()), "nice".into())], Nat(LONat { content: 32 }), vec![].into()), 37)})
    );

    let test = "fn_name[var_a : nice] => 32 where {\
     let x = 5\
     }".to_string();
    // println!("length of test: {}", test.len());
    // println!("{:?}", fn_parser.parse(&test, true, context));

    assert_eq!(
        fn_parser.parse(&test, true, context),
        Ok(hashset!{(FnDef("fn_name".into(), vec![(Unit("var_a".into()), "nice".into())],
            Nat(LONat { content: 32 }), vec![Let(Unit("x".into()), Nat(LONat { content: 5 }))].into()), 45)})
    );

    let test = "fn_name[var_a : nice] => 32 where {\
     let x = 5\
     y[x:nat]=>\"hello world\"
     }".to_string();
    // println!("length of test: {}", test.len());
    // println!("{:?}", fn_parser.parse(&test, true, context));

    assert_eq!(
        fn_parser.parse(&test, true, context),
        Ok(hashset!{(FnDef(
            "fn_name".into(),
            vec![(Unit("var_a".into()), "nice".into())],
            Nat(LONat { content: 32 }),
            vec![
                FnDef(
                    "y".into(),
                    vec![(Unit("x".into()), "nat".into())],
                    Str(LOString { content: "hello world".into() }),
                    vec![].into()),
                Let(Unit("x".into()), Nat(LONat { content: 5 }))
            ].into()), 74)})

    )
}


#[test]
fn omega_gigachad_function_test() {

    // introduce the various configs for this parser:

    const INFIX_ADDITION_SYMBOL: &str = "+"; // for expressions, like "+" in "3 + 4"
    const INFIX_MULTIPLICATION_SYMBOL: &str = "*"; // for expressions like "*" in "3 * 4"
    const INFIX_SUBTRACTION_SYMBOL: &str = "-"; // for expressions like "-" in "4 - 3"
    const INFIX_FUNCTION_SYMBOL: &str = " "; // for expressions like "$" in "func $ [arg]"
    const INFIX_EQUALITY_SYMBOL: &str = "="; // for checking equality; for expressions like "=" in "x = y"
    const LIST_SEPARATOR_SYMBOL: char = ','; // for list expressions, like ',' in "[1, 2, 3]"
    const IDENTIFIER_ALLOWED_CHARS: &str = "_"; // also, by default, includes a-zA-Z
    const FUNCTION_ARGUMENT_SEPARATOR: char = ','; // seems simple
    const FUNCTION_ARGUMENT_INFIX_SYMBOL: &str = ":"; // like ":" in "func_name[arg1: type1, arg2: type2] => ..."

    ////////////////////////////////////////////////////
    // first, assemble only the finest expression parser
    ////////////////////////////////////////////////////

    // primitive parsers
    let string_parser = Rc::new(box_expr_parser!(StringParser()));
    let nat_parser = Rc::new(box_expr_parser!(NatParser()));
    let var_parser = Rc::new(box_expr_parser!(VariableParser::default(IdentifierParser::new(IDENTIFIER_ALLOWED_CHARS))));

    // start off with a couple simple parsers
    let root_parsers = RefCell::new(vec![
        &string_parser,
        &nat_parser,
        &var_parser
    ].into_iter().map(Rc::downgrade).collect());

    // create the expression parser
    let expr_parser = Rc::new(ExprParser {
        parsers: root_parsers
    });

    // create a couple recursive parsers
    let parenthetical_parser = Rc::new(box_expr_parser!(ParentheticalParser {
        expr_parser: Rc::clone(&expr_parser)
    }));
    let list_parser = Rc::new(box_expr_parser!(ListParser {
        separator: LIST_SEPARATOR_SYMBOL,
        expr_parser: Rc::clone(&expr_parser)
    }));
    let infix_addition = Rc::new(box_expr_parser!(InfixParser {
        expr_parser: Rc::clone(&expr_parser),
        infix: String::from(INFIX_ADDITION_SYMBOL),
    }));
    let infix_multiplication = Rc::new(box_expr_parser!(InfixParser {
        expr_parser: Rc::clone(&expr_parser),
        infix: String::from(INFIX_MULTIPLICATION_SYMBOL)
    }));
    let infix_invocation = Rc::new(box_expr_parser!(InfixParser {
        expr_parser: Rc::clone(&expr_parser),
        infix: String::from(INFIX_FUNCTION_SYMBOL)
    }));
    let infix_subtraction = Rc::new(box_expr_parser!(InfixParser {
        expr_parser: Rc::clone(&expr_parser),
        infix: String::from(INFIX_SUBTRACTION_SYMBOL)
    }));
    let infix_equality = Rc::new(box_expr_parser!(InfixParser {
        expr_parser: Rc::clone(&expr_parser),
        infix: String::from(INFIX_EQUALITY_SYMBOL)
    }));
    let condition_parser = Rc::new(box_expr_parser!(ConditionalParser {
        expr_parser: Rc::clone(&expr_parser)
    }));
    // add the recursive parsers
    expr_parser.parsers.borrow_mut().extend(vec![
        // single-instance
        &parenthetical_parser,
        &condition_parser,

        // configured
        &infix_addition,
        &infix_multiplication,
        &infix_subtraction,
        &infix_invocation,
        &infix_equality,
        &list_parser,
    ].into_iter().map(Rc::downgrade).collect::<Vec<_>>());

    /////////////////////////////////////////////////////
    // now, we append the statement parsers
    /////////////////////////////////////////////////////

    // create the shallow arg parser for function signatures
    let arg_name_parser = Rc::new(box_expr_parser!(VariableParser::default(IdentifierParser::new(IDENTIFIER_ALLOWED_CHARS))));
    let arg_type_parser = Rc::new(box_expr_parser!(VariableParser::default(IdentifierParser::new(IDENTIFIER_ALLOWED_CHARS))));
    let arg_name_type_parser = Rc::new(ExprParser {
        parsers: RefCell::new(vec![
            Rc::downgrade(&arg_name_parser),
            Rc::downgrade(&arg_type_parser)
        ])
    });
    let arg_infix = Rc::new(box_expr_parser!(InfixParser {
        expr_parser: Rc::clone(&arg_name_type_parser),
        infix: String::from(FUNCTION_ARGUMENT_INFIX_SYMBOL)
    }));
    let arg_parser = Rc::new(ExprParser {
        parsers: RefCell::new(vec![
            Rc::downgrade(&arg_infix)
        ])
    });
    let arg_parser = Rc::new(ListParser {
        expr_parser: arg_parser,
        separator: FUNCTION_ARGUMENT_SEPARATOR
    });

    let stmt_parser = Rc::new(StatementParser {
        parsers: RefCell::new(vec![])
    });

    // create function definition parser
    let fn_parser = Rc::new(box_stmt_parser!(FnDefParser {
        id_parser: Rc::new(IdentifierParser::new(IDENTIFIER_ALLOWED_CHARS)),
        statement_parser: Rc::clone(&stmt_parser),
        type_parser: Rc::new(TypeParser(IdentifierParser::new(IDENTIFIER_ALLOWED_CHARS))),
        expr_parser: Rc::clone(&expr_parser),
        arg_parser
    }));

    // create let statement parser
    let let_parser = Rc::new(box_stmt_parser!(LetParser {
        id_parser: Rc::new(IdentifierParser::new(IDENTIFIER_ALLOWED_CHARS)),
        expr_parser: Rc::clone(&expr_parser)
    }));

    stmt_parser.parsers.borrow_mut().extend(vec![
        Rc::downgrade(&fn_parser),
        Rc::downgrade(&let_parser)
    ]);


    ////////////////////////////////////////////////////////////////////////////////////////////////

    // BEGIN TESTING FUNCTIONS!!!

    ////////////////////////////////////////////////////////////////////////////////////////////////

    let parser = stmt_parser; // we are parsing functions!
    let context = ParseMetaData::new();

    // multiply_by_two [x: double] => x + x

    // bad fibo
    let test = String::from("fibonacci [x: nat] => go x
        where {
            go [y: nat] => y * (go [y-1])
        }");

    // vanity test
    // println!("length: {}", test.len());
    // println!("{:#?}", parser.parse(&test, true, context));

    assert_eq!(
        parser.parse(&test, true, context),

        // almost straight from {:#?}
        Ok(
            hashset!{
                (
                    FnDef(
                        "fibonacci".to_string(),
                        vec![
                            (
                                Unit(
                                    "x".to_string(),
                                ),
                                "nat".to_string(),
                            ),
                        ],
                        Infix(
                            Variable(
                                Unit(
                                    "go".into(),
                                ),
                            ).into(),
                            " ".into(),
                            Variable(
                                Unit(
                                    "x".into(),
                                ),
                            ).into(),
                        ),
                        vec![
                            FnDef(
                                "go".into(),
                                vec![
                                    (
                                        Unit(
                                            "y".into(),
                                        ),
                                        "nat".into(),
                                    ),
                                ],
                                Infix(
                                    Variable(
                                        Unit(
                                            "y".into(),
                                        ),
                                    ).into(),
                                    "*".into(),
                                    Infix(
                                        Variable(
                                            Unit(
                                                "go".into(),
                                            ),
                                        ).into(),
                                        " ".into(),
                                        List(
                                            vec![
                                                Infix(
                                                    Variable(
                                                        Unit(
                                                            "y".into(),
                                                        ),
                                                    ).into(),
                                                    "-".into(),
                                                    Nat(
                                                        LONat {
                                                            content: 1,
                                                        },
                                                    ).into(),
                                                ).into(),
                                            ],
                                        ).into(),
                                    ).into(),
                                ),
                                vec![].into(),
                            ),
                        ].into(),
                    ),
                    94,
                )
            },
        )
    );


    // actual fibo!
    let test = String::from("factorial [x: nat] => if x=0 1 else go x
        where {
            go [y: nat] => y * (go [y-1])
        }");

    // vanity test
    // println!("length: {}", test.len());
    // println!("{:#?}", parser.parse(&test, true, context));

    // vanity json test
    // let (expr, _) = parser.parse(&test, true, ParseMetaData::new()).unwrap().into_iter().next().unwrap();
    // println!("json: {}", JSON::new(false).format(&expr));


    assert_eq!(
        parser.parse(&test, true, context),
        Ok(
            hashset!{
                (
                    FnDef(
                        "factorial".into(),
                        vec![
                            (
                                Unit(
                                    "x".into(),
                                ),
                                "nat".into(),
                            ),
                        ],
                        Conditional(
                            Infix(
                                Variable(
                                    Unit(
                                        "x".into(),
                                    ),
                                ).into(),
                                "=".into(),
                                Nat(
                                    LONat {
                                        content: 0,
                                    },
                                ).into(),
                            ).into(),
                            Nat(
                                LONat {
                                    content: 1,
                                },
                            ).into(),
                            Infix(
                                Variable(
                                    Unit(
                                        "go".into(),
                                    ),
                                ).into(),
                                " ".into(),
                                Variable(
                                    Unit(
                                        "x".into(),
                                    ),
                                ).into(),
                            ).into(),
                        ),
                        vec![
                            FnDef(
                                "go".into(),
                                vec![
                                    (
                                        Unit(
                                            "y".into(),
                                        ),
                                        "nat".into(),
                                    ),
                                ],
                                Infix(
                                    Variable(
                                        Unit(
                                            "y".into(),
                                        ),
                                    ).into(),
                                    "*".into(),
                                    Infix(
                                        Variable(
                                            Unit(
                                                "go".into(),
                                            ),
                                        ).into(),
                                        " ".into(),
                                        List(
                                            vec![
                                                Infix(
                                                    Variable(
                                                        Unit(
                                                            "y".into(),
                                                        ),
                                                    ).into(),
                                                    "-".into(),
                                                    Nat(
                                                        LONat {
                                                            content: 1,
                                                        },
                                                    ).into(),
                                                ).into(),
                                            ],
                                        ).into(),
                                    ).into(),
                                ),
                                vec![].into(),
                            ),
                        ].into(),
                    ),
                    test.len(),
                ),
            },
        )
    );

    // empty lists DO work!
    // let test = "add_things[] => 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10".to_string();
    // println!("length: {}", test.len());
    // println!("{:#?}", parser.parse(&test, true, context));

    // let test = "f[] => []
    // let x = 3
    // let y = 4".to_string();
    // println!("{}", ParseResult(parser.parse(&test, false, ParseMetaData::new())).to_string());

    // let test = "f[x:t] => f x".to_string();
    // println!("{}", ParseResult(parser.parse(&test, false, ParseMetaData::new())).to_string());
}

#[test]
fn test_program_parsing() {
    // introduce the various configs for this parser:

    const INFIX_ADDITION_SYMBOL: &str = "+"; // for expressions, like "+" in "3 + 4"
    const INFIX_MULTIPLICATION_SYMBOL: &str = "*"; // for expressions like "*" in "3 * 4"
    const INFIX_SUBTRACTION_SYMBOL: &str = "-"; // for expressions like "-" in "4 - 3"
    const INFIX_FUNCTION_SYMBOL: &str = " "; // for expressions like "$" in "func $ [arg]"
    const INFIX_EQUALITY_SYMBOL: &str = "="; // for checking equality; for expressions like "=" in "x = y"
    const LIST_SEPARATOR_SYMBOL: char = ','; // for list expressions, like ',' in "[1, 2, 3]"
    const IDENTIFIER_ALLOWED_CHARS: &str = "_"; // also, by default, includes a-zA-Z
    const FUNCTION_ARGUMENT_SEPARATOR: char = ','; // seems simple
    const FUNCTION_ARGUMENT_INFIX_SYMBOL: &str = ":"; // like ":" in "func_name[arg1: type1, arg2: type2] => ..."

    ////////////////////////////////////////////////////
    // first, assemble only the finest expression parser
    ////////////////////////////////////////////////////

    // primitive parsers
    let string_parser = Rc::new(box_expr_parser!(StringParser()));
    let nat_parser = Rc::new(box_expr_parser!(NatParser()));
    let var_parser = Rc::new(box_expr_parser!(VariableParser::default(IdentifierParser::new(IDENTIFIER_ALLOWED_CHARS))));

    // start off with a couple simple parsers
    let root_parsers = RefCell::new(vec![
        &string_parser,
        &nat_parser,
        &var_parser
    ].into_iter().map(Rc::downgrade).collect());

    // create the expression parser
    let expr_parser = Rc::new(ExprParser {
        parsers: root_parsers
    });

    // create a couple recursive parsers
    let parenthetical_parser = Rc::new(box_expr_parser!(ParentheticalParser {
        expr_parser: Rc::clone(&expr_parser)
    }));
    let list_parser = Rc::new(box_expr_parser!(ListParser {
        separator: LIST_SEPARATOR_SYMBOL,
        expr_parser: Rc::clone(&expr_parser)
    }));
    let infix_addition = Rc::new(box_expr_parser!(InfixParser {
        expr_parser: Rc::clone(&expr_parser),
        infix: String::from(INFIX_ADDITION_SYMBOL),
    }));
    let infix_multiplication = Rc::new(box_expr_parser!(InfixParser {
        expr_parser: Rc::clone(&expr_parser),
        infix: String::from(INFIX_MULTIPLICATION_SYMBOL)
    }));
    let infix_invocation = Rc::new(box_expr_parser!(InfixParser {
        expr_parser: Rc::clone(&expr_parser),
        infix: String::from(INFIX_FUNCTION_SYMBOL)
    }));
    let infix_subtraction = Rc::new(box_expr_parser!(InfixParser {
        expr_parser: Rc::clone(&expr_parser),
        infix: String::from(INFIX_SUBTRACTION_SYMBOL)
    }));
    let infix_equality = Rc::new(box_expr_parser!(InfixParser {
        expr_parser: Rc::clone(&expr_parser),
        infix: String::from(INFIX_EQUALITY_SYMBOL)
    }));
    let condition_parser = Rc::new(box_expr_parser!(ConditionalParser {
        expr_parser: Rc::clone(&expr_parser)
    }));
    // add the recursive parsers
    expr_parser.parsers.borrow_mut().extend(vec![
        // single-instance
        &parenthetical_parser,
        &condition_parser,

        // configured
        &infix_addition,
        &infix_multiplication,
        &infix_subtraction,
        &infix_invocation,
        &infix_equality,
        &list_parser,
    ].into_iter().map(Rc::downgrade).collect::<Vec<_>>());

    /////////////////////////////////////////////////////
    // now, we append the statement parsers
    /////////////////////////////////////////////////////

    // create the shallow arg parser for function signatures
    let arg_name_parser = Rc::new(box_expr_parser!(VariableParser::default(IdentifierParser::new(IDENTIFIER_ALLOWED_CHARS))));
    let arg_type_parser = Rc::new(box_expr_parser!(VariableParser::default(IdentifierParser::new(IDENTIFIER_ALLOWED_CHARS))));
    let arg_name_type_parser = Rc::new(ExprParser {
        parsers: RefCell::new(vec![
            Rc::downgrade(&arg_name_parser),
            Rc::downgrade(&arg_type_parser)
        ])
    });
    let arg_infix = Rc::new(box_expr_parser!(InfixParser {
        expr_parser: Rc::clone(&arg_name_type_parser),
        infix: String::from(FUNCTION_ARGUMENT_INFIX_SYMBOL)
    }));
    let arg_parser = Rc::new(ExprParser {
        parsers: RefCell::new(vec![
            Rc::downgrade(&arg_infix)
        ])
    });
    let arg_parser = Rc::new(ListParser {
        expr_parser: arg_parser,
        separator: FUNCTION_ARGUMENT_SEPARATOR
    });

    let stmt_parser = Rc::new(StatementParser {
        parsers: RefCell::new(vec![])
    });

    // create function definition parser
    let fn_parser = Rc::new(box_stmt_parser!(FnDefParser {
        id_parser: Rc::new(IdentifierParser::new(IDENTIFIER_ALLOWED_CHARS)),
        statement_parser: Rc::clone(&stmt_parser),
        type_parser: Rc::new(TypeParser(IdentifierParser::new(IDENTIFIER_ALLOWED_CHARS))),
        expr_parser: Rc::clone(&expr_parser),
        arg_parser
    }));

    // create let statement parser
    let let_parser = Rc::new(box_stmt_parser!(LetParser {
        id_parser: Rc::new(IdentifierParser::new(IDENTIFIER_ALLOWED_CHARS)),
        expr_parser: Rc::clone(&expr_parser)
    }));

    stmt_parser.parsers.borrow_mut().extend(vec![
        Rc::downgrade(&fn_parser),
        Rc::downgrade(&let_parser)
    ]);

    let program_parser = ProgramParser {
        stmt_parser: Arc::new(Rc::clone(&stmt_parser))
    };

    /////////////////////////////////////////////////////////////////
    //             LET THE TESTS BEGIN                             //
    /////////////////////////////////////////////////////////////////

    // let test = "f[x:t]=>f x g[y:t] => y + \"hello\" + 3".to_string();
    // todo: if the to_string method changes, come change this assertion ig
    let test = "f[x:t]=>f+x ".to_string();
    assert_eq!({
                   let (prgm, _used) = ParseResult(
                       program_parser.parse(&test, true, ParseMetaData::new())
                   ).0.unwrap().into_iter().next().unwrap();
                    prgm.content.get(0).unwrap().to_string()
               }, "function f (x: t) => (f + x)");
}

#[test]
pub fn test_program_validation() {

    // I want an easy way to validate programs, so I'll start with the parser:


    // introduce the various configs for this parser:

    const INFIX_ADDITION_SYMBOL: &str = "+"; // for expressions, like "+" in "3 + 4"
    const INFIX_MULTIPLICATION_SYMBOL: &str = "*"; // for expressions like "*" in "3 * 4"
    const INFIX_SUBTRACTION_SYMBOL: &str = "-"; // for expressions like "-" in "4 - 3"
    const INFIX_FUNCTION_SYMBOL: &str = " "; // for expressions like "$" in "func $ [arg]"
    const INFIX_EQUALITY_SYMBOL: &str = "="; // for checking equality; for expressions like "=" in "x = y"
    const LIST_SEPARATOR_SYMBOL: char = ','; // for list expressions, like ',' in "[1, 2, 3]"
    const IDENTIFIER_ALLOWED_CHARS: &str = "_"; // also, by default, includes a-zA-Z
    const FUNCTION_ARGUMENT_SEPARATOR: char = ','; // seems simple
    const FUNCTION_ARGUMENT_INFIX_SYMBOL: &str = ":"; // like ":" in "func_name[arg1: type1, arg2: type2] => ..."

    ////////////////////////////////////////////////////
    // first, assemble only the finest expression parser
    ////////////////////////////////////////////////////

    // primitive parsers
    let string_parser = Rc::new(box_expr_parser!(StringParser()));
    let nat_parser = Rc::new(box_expr_parser!(NatParser()));
    let var_parser = Rc::new(box_expr_parser!(VariableParser::default(IdentifierParser::new(IDENTIFIER_ALLOWED_CHARS))));

    // start off with a couple simple parsers
    let root_parsers = RefCell::new(vec![
        &string_parser,
        &nat_parser,
        &var_parser
    ].into_iter().map(Rc::downgrade).collect());

    // create the expression parser
    let expr_parser = Rc::new(ExprParser {
        parsers: root_parsers
    });

    // create a couple recursive parsers
    let parenthetical_parser = Rc::new(box_expr_parser!(ParentheticalParser {
        expr_parser: Rc::clone(&expr_parser)
    }));
    let list_parser = Rc::new(box_expr_parser!(ListParser {
        separator: LIST_SEPARATOR_SYMBOL,
        expr_parser: Rc::clone(&expr_parser)
    }));
    let infix_addition = Rc::new(box_expr_parser!(InfixParser {
        expr_parser: Rc::clone(&expr_parser),
        infix: String::from(INFIX_ADDITION_SYMBOL),
    }));
    let infix_multiplication = Rc::new(box_expr_parser!(InfixParser {
        expr_parser: Rc::clone(&expr_parser),
        infix: String::from(INFIX_MULTIPLICATION_SYMBOL)
    }));
    let infix_invocation = Rc::new(box_expr_parser!(InfixParser {
        expr_parser: Rc::clone(&expr_parser),
        infix: String::from(INFIX_FUNCTION_SYMBOL)
    }));
    let infix_subtraction = Rc::new(box_expr_parser!(InfixParser {
        expr_parser: Rc::clone(&expr_parser),
        infix: String::from(INFIX_SUBTRACTION_SYMBOL)
    }));
    let infix_equality = Rc::new(box_expr_parser!(InfixParser {
        expr_parser: Rc::clone(&expr_parser),
        infix: String::from(INFIX_EQUALITY_SYMBOL)
    }));
    let condition_parser = Rc::new(box_expr_parser!(ConditionalParser {
        expr_parser: Rc::clone(&expr_parser)
    }));
    // add the recursive parsers
    expr_parser.parsers.borrow_mut().extend(vec![
        // single-instance
        &parenthetical_parser,
        &condition_parser,

        // configured
        &infix_addition,
        &infix_multiplication,
        &infix_subtraction,
        &infix_invocation,
        &infix_equality,
        &list_parser,
    ].into_iter().map(Rc::downgrade).collect::<Vec<_>>());

    /////////////////////////////////////////////////////
    // now, we append the statement parsers
    /////////////////////////////////////////////////////

    // create the shallow arg parser for function signatures
    let arg_name_parser = Rc::new(box_expr_parser!(VariableParser::default(IdentifierParser::new(IDENTIFIER_ALLOWED_CHARS))));
    let arg_type_parser = Rc::new(box_expr_parser!(VariableParser::default(IdentifierParser::new(IDENTIFIER_ALLOWED_CHARS))));
    let arg_name_type_parser = Rc::new(ExprParser {
        parsers: RefCell::new(vec![
            Rc::downgrade(&arg_name_parser),
            Rc::downgrade(&arg_type_parser)
        ])
    });
    let arg_infix = Rc::new(box_expr_parser!(InfixParser {
        expr_parser: Rc::clone(&arg_name_type_parser),
        infix: String::from(FUNCTION_ARGUMENT_INFIX_SYMBOL)
    }));
    let arg_parser = Rc::new(ExprParser {
        parsers: RefCell::new(vec![
            Rc::downgrade(&arg_infix)
        ])
    });
    let arg_parser = Rc::new(ListParser {
        expr_parser: arg_parser,
        separator: FUNCTION_ARGUMENT_SEPARATOR
    });

    let stmt_parser = Rc::new(StatementParser {
        parsers: RefCell::new(vec![])
    });

    // create function definition parser
    let fn_parser = Rc::new(box_stmt_parser!(FnDefParser {
        id_parser: Rc::new(IdentifierParser::new(IDENTIFIER_ALLOWED_CHARS)),
        statement_parser: Rc::clone(&stmt_parser),
        type_parser: Rc::new(TypeParser(IdentifierParser::new(IDENTIFIER_ALLOWED_CHARS))),
        expr_parser: Rc::clone(&expr_parser),
        arg_parser
    }));

    // create let statement parser
    let let_parser = Rc::new(box_stmt_parser!(LetParser {
        id_parser: Rc::new(IdentifierParser::new(IDENTIFIER_ALLOWED_CHARS)),
        expr_parser: Rc::clone(&expr_parser)
    }));

    stmt_parser.parsers.borrow_mut().extend(vec![
        Rc::downgrade(&fn_parser),
        Rc::downgrade(&let_parser)
    ]);

    let program_parser = ProgramParser {
        stmt_parser: Arc::new(Rc::clone(&stmt_parser))
    };

    /////////////////////////////////////////////////////////////////
    //             LET THE TESTS BEGIN                             //
    /////////////////////////////////////////////////////////////////
    let eval = |s: &str| {
        let s = format!("{} ", s);
        // figure out how to get consume to be true
        let res = program_parser.parse(&s, true, ParseMetaData::new());
        if res.is_err() {
            let no_consume = program_parser.parse(&s, false, ParseMetaData::new());
            println!("Length of string: {}", s.len());
            println!("Without consuming, it would have produced: {:?}",
                     no_consume.map(|hs|hs.into_iter().map(
                         |(p,u)|
                             format!("\n{}\t\t{}", p.to_string(), u)
                     ).collect::<Vec<_>>()));
            println!("error: {:?}", res);
            panic!(res)
        } else {
            res.unwrap()
        }
    };

    let validate = |p: HashSet<(Program, usize)>| {
        let mut errors = String::new();
        let results = p.into_iter()
            .map(|(prgm, _used)| prgm)
            .filter(|program| {
                let pv = ProgramValidator;
                match pv.validate(program) {
                    Ok(_) => true,
                    Err(e) => {
                        errors += &*format!("Removed {} because {}\n", program.to_string(), e);
                        false
                    }
                }
            }).collect::<HashSet<_>>();
        if results.len() == 0 {
            panic!("{}", errors);
        } else {
            results
        }
    };

    let prgm1 = eval("f[x:t, z:t] => z * z where { let y = x * x }");
    for r in validate(prgm1) {
        println!("{}", r.to_string());
    }

    let prgm2 = eval("let y = 5\nlet x = y");
    for r in validate(prgm2) {
        println!("{}", r.to_string());
    }
}

#[test]
pub fn test_validation_expr() {
    let expr_v = ExpressionValidator(ScopeFrame {
        globals: hashset![],
        locals: hashmap![
            Identifier::Unit("hi".to_string()) => 1,
            Identifier::Unit("-.-".to_string()) => 2,
        ]
    });

    let expr = Expr::Infix(
        Expr::Variable(Identifier::Unit("hi".into())).into(),
        "+".into(),
        Expr::Nat(64.into()).into()
    );
    expr_v.validate(&expr).unwrap();


    let expr = Expr::Infix(
        Expr::Variable(Identifier::Unit("-.-".into())).into(),
        "+".into(),
        Expr::Variable(Identifier::Unit("ok".into())).into()
    );
    assert!(expr_v.validate(&expr).is_err());
}