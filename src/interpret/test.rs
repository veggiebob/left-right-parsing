use crate::interpret::{Interpreter, ProgramRetriever};
use crate::interpret::definitions::{LanguageObject, ProductObject, Term, TupleObject};
use crate::interpret::definitions::Term::Nat;
use crate::lang_obj::{Expr, Identifier, Program, Statement};

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
    assert_eq!(interp.start(), Ok(Some(Term::Nat(3))));
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
                Box::new(Expr::List(vec![Box::new(Expr::Nat(1.into()))]))
            )),
            // return a
            Statement::Ret(Expr::Variable(a.clone()))
        ]
    };
    let mut interp = Interpreter::new(prgm, ProgramRetriever {});
    assert_eq!(Ok(Some(Nat(1))), interp.start());
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
                ]))
            )),
            // let b = f(2, a)
            Statement::Let(b.clone(), Expr::Infix(
                Box::new(Expr::Variable(f.clone())),
                " ".to_string(),
                Box::new(Expr::List(vec![
                    Box::new(Expr::Nat(2.into())),
                    Box::new(Expr::Variable(a.clone()))
                ]))
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
    assert_eq!(Ok(Some(Term::Object(Box::new(LanguageObject::Product(ProductObject::Tuple(TupleObject(vec![Nat(1), Nat(2)]))))))), interp.start());
}

#[test]
fn cond1() {
    let a = Identifier::Unit("a".to_string());
    let prgm = Program {
        content: vec![
            Statement::Let(a.clone(), Expr::Bool(true)),
            Statement::Ret(Expr::Conditional(
                Box::new(Expr::Variable(a.clone())),
                Box::new(Expr::Nat(1.into())),
                Box::new(Expr::Nat(2.into()))
            ))
        ]
    };
    let mut interp = Interpreter::new(prgm, ProgramRetriever {});
    assert_eq!(Ok(Some(Nat(2))), interp.start());
}

#[test]
fn cond2() {
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
                Box::new(Expr::List(vec![]))
            ))
        ]
    };
    let mut interp = Interpreter::new(prgm, ProgramRetriever {});
    assert_eq!(Ok(Some(Nat(3))), interp.start());
}