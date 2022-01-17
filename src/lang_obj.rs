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

    /// Represents a whole, non-negative integer.
    /// regex: \d+
    Nat(LONat),

    /// Represents a string. Currently does not support escaped strings inside such as `"\""`
    Str(LOString),

    /// An operator between 2 expressions.
    /// pseudo-regex syntax: <expr>\s*<op>\s*<expr>
    /// Note that all operators are right-associative
    Infix(Box<Expr>, String, Box<Expr>),

    /// A list of expressions.
    /// pseudo-regex syntax: \[(<expr><sep>\s*)*<expr>\]
    /// where the separator can be any `char`
    List(Vec<Box<Expr>>),

    /// Represents a function call. The first being the operator, the second being
    /// the operand. There is no separator between the expressions.
    /// Just because it may parse successfully does not mean it's an actual function.
    /// For example, "3(2)" could parse as a valid "function expression" but since
    /// the types won't be known until after parsing has been analyzed, it's impossible
    /// to know if "3" is a function.
    /// Ideally, this will be used for expressions that look like "func_name[arg1, arg2]"
    // this should be implemented in the future, but for now I'll just use an infix
    // operator to symbolize function calls. It's easier that way.
    // Something like "func_name $ [arg1, arg2]"
    Func(Identifier, Box<Expr>),

    /// Any other symbol that would be used to identify a local variable
    Variable(Identifier),

    /// Represents an If-else style conditional
    // syntax:
    // if\s*<condition expr>\s*<true expr>\s*else\s*<false expr>
    Conditional(Box<Expr>, Box<Expr>, Box<Expr>)
}

#[derive(Eq, PartialEq, Debug, Clone)]
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


pub type Type = String;

pub type WhereClause = Box<Vec<Statement>>;

pub struct Program {
    content: Vec<Statement>
}

#[derive(Eq, PartialEq, Hash, Debug, Clone)]
pub enum Statement {

    /// let expression
    // proposed syntax: let <identifier> = <expression>;
    Let(Identifier, Expr),

    /// function definition
    // (name, arguments, body, where-clause)
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
