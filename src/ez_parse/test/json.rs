use crate::ez_parse::funcs::UnionResult::{Left, Right};
use crate::ez_parse::funcs::{kleene, map, optional, parser_ref, FunctionParser, ParserRef, SimpleStrParser, concat};
use crate::ez_parse::ops::EZ;
use crate::lang_obj::{Expr, ParseError};
use crate::parse::{NatParser, StringParser};
use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap};
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
    let parser = FunctionParser::<JSON>::placeholder::<JSON>();
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

    let expr_comma = map(parser_ref(&parser + EZ(SimpleStrParser::from(","))), |(js, _x)| js);
    let list_expr = kleene(parser_ref(expr_comma));
    let opt_last_element = optional(Rc::clone(&parser));
    let A = "[" + EZ(concat(parser_ref(list_expr), parser_ref(opt_last_element), Box::new(|mut xs: Vec<JSON>, vs| {
        if let Some(x) = vs {
            xs.push(x);
        }
        JSON::Array(xs)
    }))) + "]"; // finish array implementation

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
    let res = parser.parse_all("{\"key1\":1,\"key2\":{},\"key3\":[1,2,{}]}");
    println!("{:?}", res);
}
