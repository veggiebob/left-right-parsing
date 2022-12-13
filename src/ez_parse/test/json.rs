use crate::ez_parse::funcs::UnionResult::{Left, Right};
use crate::ez_parse::funcs::{
    kleene, map, optional, parser_ref, FunctionParser, ParserRef, SimpleStrParser,
};
use crate::ez_parse::ops::EZ;
use crate::lang_obj::{Expr, ParseError};
use crate::parse::{NatParser, StringParser};
use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap};
use std::rc::Rc;

/*

Minimal JSON Parser! (lacking arrays, coming soon, it is trivial I promise)

Demonstrates the use of the library and how EZ can be used to simplify parser
combinations. It is still pretty messy though.

On reflection, one big weakness is the exhaustive types being used.
The variable `parser` at the bottom has a type so long it is definitely NOT
feasible to write out. I'm not sure if this will be a problem in the future,
but it does seem to add baggage.

 */

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
enum JSON {
    Str(String),
    Num(usize),
    Bool(bool),
    Object(BTreeMap<String, JSON>),
    Array(Vec<JSON>),
}

#[test]
fn json_parser() {
    let S = map(parser_ref(StringParser()), |e| match e {
        Expr::Str(lo_string) => lo_string.content,
        x => x.to_string(),
    });
    let N = map(parser_ref(NatParser()), |e| match e {
        Expr::Nat(lo_nat) => JSON::Num(lo_nat.content as usize),
        x => JSON::Num(0),
    });
    let B = map(
        parser_ref(EZ(SimpleStrParser::from("true")) * EZ(SimpleStrParser::from("false"))),
        |u| match u {
            Left(_b) => JSON::Bool(true),
            Right(_b) => JSON::Bool(false),
        },
    );
    let parser: ParserRef<FunctionParser<JSON>> =
        parser_ref(FunctionParser(RefCell::new(Box::new(|_, _, _| {
            Err(ParseError::from("placeholder"))
        }))));
    let O = "{"
        + EZ(kleene(parser_ref(EZ(S.clone()) + ":" + &parser + ",")))
        + EZ(optional(parser_ref(EZ(S.clone()) + ":" + &parser)))
        + "}";
    let O = map(parser_ref(O), |(mut vs, last)| {
        if let Some(x) = last {
            vs.push(x);
        }
        let mut hmap = BTreeMap::new();
        for (k, v) in vs {
            hmap.insert(k, v);
        }
        JSON::Object(hmap)
    });
    let json_S = map(parser_ref(S), JSON::Str);
    // let A = # finish array implementation
    let P = parser_ref(EZ(json_S) / EZ(N) / EZ(B) / EZ(O)); // / A;

    let _p = Rc::clone(&P);
    *parser.borrow().0.borrow_mut() = Box::new(move |content, consume, meta| {
        if meta.same_paths() || meta.max_history_cycles() > 2 {
            Err("Followed the same path more than once".into())
        } else {
            let meta = meta.rotate_paths();
            _p.borrow().parse(content, consume, meta)
        }
    });

    let parser = P.borrow();
    let res = parser.parse_all("{\"key1\":1,\"key2\":{}}");
    println!("{:?}", res);
}
