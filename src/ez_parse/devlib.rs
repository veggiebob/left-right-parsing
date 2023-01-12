/*
This will, might, eventually contain the parser for a reasonable programming language.
The testing module will demonstrate its uses.
 */

use crate::ez_parse::funcs::UnionResult::{Left, Right};
use crate::ez_parse::funcs::{concat, map, parser_ref, FunctionParser, ParserRef, SimpleStrParser, kleene, optional, fst};
use crate::ez_parse::ops::EZ;
use crate::lang_obj::{Expr, ListExprType};
use crate::parse::{
    LengthQualifier, NatParser, ParentheticalParser, StringParser, TakeWhileParser,
};
use std::iter::TakeWhile;
use std::rc::Rc;

fn get_simple_expr_parser() -> ParserRef<FunctionParser<Expr>> {
    let parser = FunctionParser::<Expr>::placeholder::<Expr>();

    let sp = || EZ(TakeWhileParser::whitespace(LengthQualifier::GEQ(0)));
    let nat_parser = EZ(map(NatParser(), |x| Expr::Nat(x.into())));
    let string_parser = EZ(map(StringParser(), |s| Expr::Str(s.into())));
    let bool_parser = EZ(map(
        EZ(SimpleStrParser::from("true")) * EZ(SimpleStrParser::from("false")),
        |u| match u {
            Left(_b) => Expr::Bool(true),
            Right(_b) => Expr::Bool(false),
        },
    ));

    // infix parser
    let op_parser = TakeWhileParser {
        amount: LengthQualifier::GEQ(1),
        func: Box::new(|c: &char| "!@#$%^&*=/+?|<>'.`~:".contains(*c)),
    };

    let infix_parser = EZ(map(
        EZ(concat(&parser, sp(), |expr: Expr, _s| expr)) + EZ(op_parser) + ' ' + &parser,
        |((left, op), right)| Expr::Infix(Box::new(left), op, Box::new(right)),
    ));

    // function call parser
    let op_parser = TakeWhileParser {
        amount: LengthQualifier::Exactly(1),
        func: Box::new(|c: &char| "$ ".contains(*c)),
    };
    let func_parser_1 = EZ(map(
        EZ(concat(&parser, sp(), |expr: Expr, _s| expr)) + EZ(op_parser) + ' ' + &parser,
        |((left, op), right)| Expr::Infix(Box::new(left), op, Box::new(right)),
    ));

    // parenthetical parser
    let paren_parser = EZ(map(
        EZ(SimpleStrParser::from("(")) + ' ' + &parser + ' ' + ")",
        |(_, expr)| expr,
    ));

    // list parser
    let expr_comma = concat(&parser, (' ' + EZ(SimpleStrParser::from(";")) + ' '), fst);
    let list_expr = kleene(parser_ref(expr_comma));
    let opt_last_element = EZ(optional(&parser));
    let list_parser = "[" + (' ' + EZ(concat(list_expr, opt_last_element, |mut xs: Vec<_>, vs| {
        if let Some(x) = vs {
            xs.push(x);
        }
        Expr::List(xs.into_iter().map(Box::new).collect(), ListExprType::List)
    }))) + ' ' + "]"; // finish array implementation



    let full_expr_parser =
        nat_parser /
            string_parser /
            bool_parser /
            infix_parser /
            // func_parser_1 /
            list_parser /
            // tuple /
            paren_parser;

    *parser.borrow().0.borrow_mut() = Box::new(move |content, consume, meta| {
        // use meta.path_repeats() > 1 to allow the smallest amount of explosion
        // use meta.same_paths() to prevent an infix operator from being used multiple times
        if !meta.progress && (meta.same_paths() || meta.max_history_cycles() > 2) {
            Err("Followed the same path more than once".into())
        } else {
            let meta = meta.rotate_paths();
            full_expr_parser.parse(content, consume, meta.no_progress())
        }
    });

    parser
}

#[cfg(test)]
mod test {
    use crate::ez_parse::devlib::get_simple_expr_parser;
    use crate::lang_obj::Expr::List;
    use crate::parse::Parser;
    use crate::Expr::Nat;
    use crate::lang_obj::LONat;
    use crate::lang_obj::ListExprType;

    #[test]
    fn list_test_empty() {
        let p = get_simple_expr_parser();
        let res = p.borrow().parse_all("[]");
        assert_eq!(res, Ok(hashset!{(List(vec![], ListExprType::List), 2)}));


        let res = p.borrow().parse_all("[ ]");
        assert_eq!(res, Ok(hashset!{(List(vec![], ListExprType::List), 3)}));
    }

    #[test]
    fn list_test() {
        let p = get_simple_expr_parser();
        let res = p.borrow().parse_all("[1;[2;3;[4;[5]]]]");
        // println!("possibilities: {:?}", res.as_ref().map(|p| p.len()));
        // println!("{:?}", res);
        assert_eq!(res, Ok(hashset!{
            (List(vec![Nat(LONat { content: 1 }).into(),
                List(vec![Nat(LONat { content: 2 }).into(), Nat(LONat { content: 3 }).into(),
                    List(vec![Nat(LONat { content: 4 }).into(),
                        List(vec![Nat(LONat { content: 5 }).into()], ListExprType::List).into()
                    ], ListExprType::List).into()
                ], ListExprType::List).into()
            ], ListExprType::List).into()
                , 17)}))
    }
}
