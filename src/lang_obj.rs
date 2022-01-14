use core::convert::From;
use core::option::Option;
use core::option::Option::None;
use std::collections::HashSet;
use crate::parse::{ParseMetaData, Parser};

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

impl From<String> for LOString {
    fn from(s: String) -> Self {
        LOString {
            content: s
        }
    }
}

impl From<LONat> for Expr {
    fn from(n: LONat) -> Self {
        Expr::Nat(n)
    }
}


type Type = String;

type WhereClause = Box<Vec<Statement>>;

pub struct Program {
    content: Vec<Statement>
}

pub enum Statement {

    /// let expression
    // proposed syntax: let <identifier> = <expression>;
    Let(Identifier, Expr),

    /// function definition
    // (name, arguments, value, where-clause)
    FnDef(String, Vec<(Identifier, Type)>, Expr, WhereClause),

}

/// An identifier is a way of representing the result of an expression or a value
#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum Identifier {

    /// A unit is the most basic way to identify a value, simply a name
    /// However, the more important thing is the characters allowed in an identifier
    // definitely include a-zA-Z
    // using this system of parsing, we should be able to include *MANY* characters
    // that can be used in values
    Unit(String)
}
