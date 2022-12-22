use crate::ez_parse::funcs::UnionResult::{Left, Right};
use crate::ez_parse::funcs::{kleene, map, optional, parser_ref, FunctionParser, ParserRef, SimpleStrParser, concat, KleeneParser, CatParser, MappedParser, UnionParser, fst};
use crate::ez_parse::ops::EZ;
use crate::lang_obj::{Expr, ParseError};
use crate::parse::{LengthQualifier, LONatParser, LOStringParser, NatParser, StringParser, TakeWhileParser};
use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::iter::TakeWhile;
use std::rc::Rc;

/*

Minimal JSON Parser!

Demonstrates the use of the library and how EZ can be used to simplify parser
combinations. It is still pretty messy though.

On reflection, one big weakness is the exhaustive types being used.
The variable `parser` at the bottom has a type so long it is definitely NOT
feasible to write out. I'm not sure if this will be a problem in the future,
but it does seem to add baggage.

Additionally, there are still sore spots where EZ doesn't make it much easier
to add parsers together, so we have to revert to the functions. However, I
think those functions are also pretty easy to understand.

 */

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
enum JSON {
    Str(String),
    Num(u64),
    Bool(bool),
    Object(BTreeMap<String, JSON>),
    Array(Vec<JSON>),
}

fn empty_obj() -> JSON {
    JSON::Object(BTreeMap::new())
}

#[test]
fn json_parser() {
    // parse strings
    let S = StringParser();

    // ...JSON string parser
    let json_S = map(S, JSON::Str);

    // parse (natural) numbers
    let N = map(NatParser(), JSON::Num);

    // parse booleans (true | false)
    let B = map(
        EZ(SimpleStrParser::from("true")) * EZ(SimpleStrParser::from("false")),
        |u| match u {
            Left(_b) => JSON::Bool(true),
            Right(_b) => JSON::Bool(false),
        },
    );

    // placeholder for self reference object
    let parser = FunctionParser::<JSON>::placeholder::<JSON>();

    // parse objects (collect data)
    let O = "{" + (' '
        + EZ(kleene(EZ(S.clone()) + ' ' + ":" + ' ' + &parser + ' ' + "," + ' ')))
        + EZ(optional(EZ(S.clone()) + ' ' + ":" + ' ' + &parser)) + ' '
        + "}";

    // ...map this to the correct type; (Vec<JSON>, Option<JSON>) -> JSON
    let O = map(O, |(mut vs, last)| {
        if let Some(x) = last {
            vs.push(x);
        }
        let mut hmap = BTreeMap::new();
        for (k, v) in vs {
            hmap.insert(k, v);
        }
        JSON::Object(hmap)
    });

    // parse arrays
    let expr_comma = concat(&parser, (' ' + EZ(SimpleStrParser::from(",")) + ' '), fst);
    let list_expr = kleene(parser_ref(expr_comma));
    let opt_last_element = ' ' + EZ(optional(&parser));
    let A = "[" + (' ' + EZ(concat(list_expr, opt_last_element, |mut xs: Vec<JSON>, vs| {
        if let Some(x) = vs {
            xs.push(x);
        }
        JSON::Array(xs)
    }))) + ' ' + "]"; // finish array implementation

    // combine all the parsers together
    // "string parser" OR "number parser" OR "boolean parser" OR ...
    let P = parser_ref(EZ(json_S) / EZ(N) / EZ(B) / EZ(O) / EZ(A));

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
    let res: Result<HashSet<(JSON, _)>, _> = parser.parse_all("{ \"key1\" : 1 ,\"key2\": { },\"key3\":[ 1, 2, {} ] }");
    assert_eq!(res, Ok(hashset!{(
        JSON::Object(btreemap!{
            "key1".to_string() => JSON::Num(1),
            "key2".to_string() => empty_obj(),
            "key3".to_string() => JSON::Array(vec![JSON::Num(1), JSON::Num(2), empty_obj()])}),
        47)}));
}
