use crate::interpret::{Interpreter, ProgramRetriever};
use crate::interpret::definitions::{LanguageObject, ProductObject, Term, TupleObject, Type};
use crate::interpret::definitions::Term::Nat;
use crate::lang_obj::{Expr, Identifier, ListExprType, Program, Statement};

#[test]
fn first() {
    let a = Identifier::Unit("a".to_string());
    let b = Identifier::Unit("b".to_string());
    let prgm = Program {
        content: vec![
            Statement::Let(a.clone(), Expr::Nat(3.into())),
            Statement::Let(b.clone(), Expr::Variable(a.clone())),
            Statement::Ret(Expr::Variable(b.clone()))
        ]
    };
    let mut interp = Interpreter::new(prgm, ProgramRetriever {});
    assert_eq!(interp.start().map(|val| val.map(|(_type, term)| term)), Ok(Some(Term::Nat(3))));
}
#[test]
fn f1() {
    // test a very simple program

    // f(x) = x
    // let a = f(1)
    // return a

    let a = Identifier::Unit("a".to_string());
    let f = Identifier::Unit("f".to_string());
    let prgm = Program {
        content: vec![
            // f(x) = x
            Statement::Let(f.clone(), Expr::Function(
                (Box::new(vec![("x".into(), None)]), None),
                vec![],
                Box::new(Expr::Variable("x".into())))
            ),
            // let a = f(1)
            Statement::Let(a.clone(), Expr::Infix(
                Box::new(Expr::Variable(f.clone())),
                " ".to_string(),
                Box::new(Expr::List(vec![Box::new(Expr::Nat(1.into()))], ListExprType::Tuple))
            )),
            // return a
            Statement::Ret(Expr::Variable(a.clone()))
        ]
    };
    let mut interp = Interpreter::new(prgm, ProgramRetriever {});
    assert_eq!(Ok(Some(Nat(1))), interp.start().map(|val| val.map(|(_type, term)| term)));
}

#[test]
fn f2() {
    // test a very simple program

    // fst(x, y) = x
    // let a = f(1, 2)
    // let b = f(2, a)
    // return a, b

    let a = Identifier::Unit("a".to_string());
    let b = Identifier::Unit("b".to_string());
    let f = Identifier::Unit("f".to_string());
    let prgm = Program {
        content: vec![
            // f(x, y) = x
            Statement::Let(f.clone(), Expr::Function(
                (Box::new(vec![("x".into(), None), ("y".into(), None)]), None),
                vec![],
                Box::new(Expr::Variable("x".into())))
            ),
            // let a = f(1, 2)
            Statement::Let(a.clone(), Expr::Infix(
                Box::new(Expr::Variable(f.clone())),
                " ".to_string(),
                Box::new(Expr::List(vec![
                    Box::new(Expr::Nat(1.into())),
                    Box::new(Expr::Nat(2.into()))
                ], ListExprType::Tuple))
            )),
            // let b = f(2, a)
            Statement::Let(b.clone(), Expr::Infix(
                Box::new(Expr::Variable(f.clone())),
                " ".to_string(),
                Box::new(Expr::List(vec![
                    Box::new(Expr::Nat(2.into())),
                    Box::new(Expr::Variable(a.clone()))
                ], ListExprType::Tuple))
            )),
            // return a, b
            Statement::Ret(Expr::Infix(
                Box::new(Expr::Variable(a.clone())),
                ",".to_string(),
                Box::new(Expr::Variable(b.clone()))
            ))
        ]
    };
    let mut interp = Interpreter::new(prgm, ProgramRetriever {});
    assert_eq!(Ok(Some(Term::Object(Box::new(LanguageObject::Product(ProductObject::Tuple(TupleObject(vec![Nat(1), Nat(2)]))))))), interp.start().map(|val| val.map(|(_type, term)| term)));
}

#[test]
fn cond1() {
    // let a = false
    // return a ? 1 : 2
    let a = Identifier::Unit("a".to_string());
    let prgm = Program {
        content: vec![
            Statement::Let(a.clone(), Expr::Bool(false)),
            Statement::Ret(Expr::Conditional(
                Box::new(Expr::Variable(a.clone())),
                Box::new(Expr::Nat(1.into())),
                Box::new(Expr::Nat(2.into()))
            ))
        ]
    };
    let mut interp = Interpreter::new(prgm, ProgramRetriever {});
    assert_eq!(Ok(Some(Nat(2))), interp.start().map(|val| val.map(|(_type, term)| term)));
}

#[test]
fn cond2() {
    // let a = true
    // let f = lambda: a ? 3 : 10
    // return f()
    let a = Identifier::Unit("a".to_string());
    let f = Identifier::Unit("f".to_string());
    let prgm = Program {
        content: vec![
            Statement::Let(a.clone(), Expr::Bool(true)),
            Statement::Let(f.clone(), Expr::Lambda(
                (Box::new(vec![]), None),
                Box::new(Expr::Conditional(
                    Box::new(Expr::Variable(a.clone())),
                    Box::new(Expr::Nat(3.into())),
                    Box::new(Expr::Nat(10.into()))
                ))
            )),
            Statement::Ret(Expr::Infix(
                Box::new(Expr::Variable(f.clone())),
                " ".to_string(),
                Box::new(Expr::List(vec![], ListExprType::Tuple))
            ))
        ]
    };
    let mut interp = Interpreter::new(prgm, ProgramRetriever {});
    assert_eq!(Ok(Some(Nat(3))), interp.start().map(|val| val.map(|(_type, term)| term)));
}

#[test]
fn test_print_1() {
    // not really sure how to actually test this since
    // the goal output is printed to stdout
    //
    let prgm = Program {
        content: vec![
            Statement::Impure(Expr::Infix(
                Box::new(Expr::Variable("print".into())),
                " ".to_string(),
                Box::new(Expr::List(
                    vec![Expr::Str("hello world!".into())]
                        .into_iter().map(Box::new).collect(),
                    ListExprType::Tuple
                ))
            )),
            Statement::Impure(Expr::Infix(
                Box::new(Expr::Variable("eprint".into())),
                " ".to_string(),
                Box::new(Expr::List(
                    vec![Expr::Str("error!".into())]
                        .into_iter().map(Box::new).collect(),
                    ListExprType::Tuple
                ))
            ))
        ]
    };
    let mut interp = Interpreter::new(prgm, ProgramRetriever {});
    assert_eq!(Ok(None), interp.start());
}

#[test]
fn test_input_1() {
    // run configuration should use "src/interpret/test-inputs/test-input-1" as input
    let a = Identifier::Unit("a".to_string());
    let prgm = Program {
        content: vec![
            Statement::Let(a.clone(), Expr::Infix(
                Box::new(Expr::Variable("input".into())),
                " ".into(),
                Box::new(Expr::List(vec![], ListExprType::Tuple))
            )),
            Statement::Ret(Expr::Variable(a))
        ]
    };
    let mut interp = Interpreter::new(prgm, ProgramRetriever {});
    assert_eq!((Type::string(), Term::String("hello world!".to_string())), interp.start().unwrap().unwrap())
}


fn fibonacci (x: u32) -> u32 {
    if x < 2 {
        1
    } else {
        fibonacci(x - 1) + fibonacci(x - 2)
    }
}
#[test]
fn fibonacci_test_1() {

    let f = || Identifier::Unit("fib".to_string());
    let x = || Identifier::Unit("x".to_string());
    let out = || Identifier::Unit("out".to_string());
    let prgm = Program {
        /*
            1. f x | x < 2 => 1
                   | otherwise = f (x-1) + f (x-2)
            2. let out = f 10
            3. print(out)
            4. return out

         */
        content: vec![
            Statement::Let(f(), Expr::Lambda((Box::new(vec![(x(), None)]), None), Box::new(
                Expr::Conditional(
                    // if x < 2
                    Box::new(Expr::Infix(
                        Box::new(Expr::Variable(x())),
                        "<".into(),
                        Box::new(Expr::Nat(2.into()))
                    )),
                    // then 1
                    Box::new(Expr::Nat(1.into())),
                    // else f(x - 1) + f(x - 2)
                    Box::new(Expr::Infix(
                        // f(x - 1)
                        Box::new(Expr::Infix(
                            Expr::Variable(f()).into(),
                            " ".into(),
                            // (x - 1,)
                            Expr::List(vec![Box::new(Expr::Infix(Box::new(Expr::Variable(x())), "-".into(), Expr::Nat(1.into()).into()))], ListExprType::Tuple).into()
                        )),
                        "+".to_string(),
                        // f(x - 2)
                        Box::new(Expr::Infix(
                            Expr::Variable(f()).into(),
                            " ".into(),
                            // (x - 2,)
                            Expr::List(vec![Box::new(Expr::Infix(Box::new(Expr::Variable(x())), "-".into(), Expr::Nat(2.into()).into()))], ListExprType::Tuple).into()
                        ))
                    ))
                )
            ))),
            // let out = f 10
            Statement::Let(
                out(),
                Expr::Infix(
                    Box::new(Expr::Variable(f())),
                    " ".into(),
                    Box::new(Expr::List(vec![Box::new(Expr::Nat(10.into()))], ListExprType::Tuple))
                )
            ),
            Statement::Ret(Expr::Variable(out()))
        ]
    };
    let mut interp = Interpreter::new(prgm, ProgramRetriever {});
    assert_eq!(Ok(Some((Type::nat(), Nat(fibonacci(10).into())))), interp.start())
}