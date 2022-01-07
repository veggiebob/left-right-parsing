use core::convert::From;
use core::option::Option;
use core::option::Option::None;
use crate::parse::Parser;

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub struct LOString {
    pub content: String
}

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub struct LONat {
    pub content: u64
}

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub enum Expr {
    Nat(LONat),
    Str(LOString),
    Infix(Box<Expr>, String, Box<Expr>),
    List(Vec<Box<Expr>>)
}

#[derive(Eq, PartialEq, Debug)]
pub struct ParseError {
    pub location: Option<usize>,
    pub range: Option<(usize, usize)>,
    pub message: String
}

impl From<String> for ParseError {
    fn from(s: String) -> Self {
        ParseError {
            location: None,
            range: None,
            message: s
        }
    }
}

impl From<&str> for ParseError {
    fn from(s: &str) -> Self {
        String::from(s).into()
    }
}

impl From<u64> for LONat {
    fn from(n: u64) -> Self {
        LONat {
            content: n
        }
    }
}

impl From<LONat> for Expr {
    fn from(n: LONat) -> Self {
        Expr::Nat(n)
    }
}
