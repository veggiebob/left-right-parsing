use core::convert::From;
use core::option::Option;
use core::option::Option::None;
use std::collections::HashSet;
use std::fmt::{Display, Formatter};
use crate::interpret::definitions::Term;
use crate::interpret::ImportStatement;
use crate::lang_obj::formatting::Format;
use crate::lang_obj::Identifier::{Temp, Unit};
use crate::parse::{ParseMetaData, Parser};

pub mod formatting;

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub struct LOString {
    pub content: String
}

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub struct LONat {
    pub content: u64
}

/// (vec<(identifier, opt<type_ident>)>, opt<type_ident>)
pub type FunctionSignature = (Box<Vec<(Identifier, Option<TypeIdentifier>)>>, Option<TypeIdentifier>);

pub type FunctionBody = Vec<Box<Statement>>;



#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub enum Expr {

    /// Boolean :)
    Bool(bool),

    /// Represents a whole, non-negative integer.
    /// regex: \d+
    Nat(LONat),

    /// Represents a string. Currently does not support escaped quotes inside such as `"\""`
    Str(LOString),

    /// An operator between 2 expressions.
    /// pseudo-regex syntax: <expr>\s*<op>\s*<expr>
    /// Note that all operators are right-associative
    Infix(Box<Expr>, String, Box<Expr>),

    /// A variable-length list of expressions.
    /// pseudo-regex syntax: \[(<expr><sep>\s*)*<expr>\]
    /// where the separator can be any `char`
    List(Vec<Box<Expr>>, ListExprType),

    /// Any other symbol that would be used to identify a local variable
    Variable(Identifier),

    /// Represents an If-else style conditional
    /// cond, true-expr, false-expr
    // syntax:
    // if\s*<condition expr>\s*<true expr>\s*else\s*<false expr>
    Conditional(Box<Expr>, Box<Expr>, Box<Expr>),

    /// May be impure
    /// name, signature, body, return
    Function(FunctionSignature, FunctionBody, Box<Expr>),

    /// Pure (and should be guaranteed to!)
    /// signature, return
    Lambda(FunctionSignature, Box<Expr>)
}

/// This type exists to signal in what way
/// the list of expressions should be interpreted.
/// There are many contexts in which lists of expressions are used,
/// so we need to signal which kind it is.
#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub enum ListExprType {
    List, // traditionally, [<expr>, <expr>, ...]
    Tuple // traditionally, (<expr>, <expr>, ...)
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

impl From<&str> for LOString {
    fn from(s: &str) -> Self {
        LOString {
            content: s.to_string()
        }
    }
}

impl From<LONat> for Expr {
    fn from(n: LONat) -> Self {
        Expr::Nat(n)
    }
}


// how an identifier for a type is represented
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeIdentifier {
    Name(String),
    Parameter(String),
    Anonymous
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Program {
    pub content: Vec<Statement>
}

#[derive(Eq, PartialEq, Hash, Debug, Clone)]
pub enum Statement {

    /// let statement
    /// proposed syntax: let <identifier> = <expression>
    Let(Identifier, Expr),

    /// assignment exactly as you would expect
    Assignment(Identifier, Expr),

    /// "Run" an expression. This assumes that computing
    /// the expression will mutate some variables.
    /// Example: print(3)
    Impure(Expr),

    /// imports!
    Import(ImportStatement),

    /// return
    /// this is a SPECIAL case because it is *not parsed*
    /// it is, in fact, *only* used by the interpreter to represent
    /// a value that needs to be transferred out of the current StackFrame
    /// and into the one below it, to fit the value of an Expr.
    /// However, it takes an Expr because the actual value cannot be known ahead of time
    Ret(Expr)
}

/// An identifier is a way of representing the result of an expression or a value
#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum Identifier {

    /// A unit is the most basic way to identify a value, simply a name
    /// However, the more important thing is the characters allowed in an identifier
    // definitely include a-zA-Z
    // using this system of parsing, we should be able to include *MANY* characters
    // that can be used in values
    Unit(String),

    /// should be used for temporary interpreter variables
    Temp(u64)
}

impl Identifier {
    fn to_string(&self) -> String {
        match self {
            Unit(x) => x.clone(),
            Temp(x) => format!("_int_{}", x)
        }
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl From<&str> for Identifier {
    fn from(s: &str) -> Self {
        Identifier::Unit(s.to_string())
    }
}

impl From<String> for Identifier {
    fn from(s: String) -> Self {
        Unit(s)
    }
}

impl Statement {
    fn to_string(&self) -> String {
        match self {
            Statement::Let(ident, expr) => format!("let {} = {}", ident.to_string(), expr.to_string()),
            // Statement::FnDef(name, args, expr, wheres) => {
            //     format!("function {} ({}) => {}{}",
            //         name,
            //         args.iter().map(|(ident, ident_type)| {
            //             format!("{}: {}", ident.to_string(), ident_type)
            //         }).collect::<Vec<_>>().join(", "),
            //         expr.to_string(),
            //             if wheres.len() > 0 {
            //                 format!(" where {} {} {}",
            //                 "{",
            //                 wheres.iter().map(ToString::to_string)
            //                     .collect::<Vec<_>>().join("\n"),
            //                 "}")
            //             } else { "".to_string() }
            //     )
            // },
            Statement::Assignment(ident, expr) => format!("{} = {}", ident.to_string(), expr.to_string()),
            Statement::Impure(expr) => format!("{};", expr.to_string()),
            Statement::Ret(expr) => format!("return {}", expr.to_string()),
            _ => "<missing to_string case for statement>".to_string()
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

// impl Display for Expr {
//     fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{}", self.to_string())
//     }
// }

impl ToString for Expr {
    fn to_string(&self) -> String {
        match self {
            Expr::Bool(b) => format!("{}", b),
            Expr::Nat(x) => x.content.to_string(),
            Expr::Str(s) => format!("\"{}\"", s.content.clone()),
            Expr::Infix(left, infix, right) => {
                format!("({} {} {})", left.to_string(), infix.clone(), right.to_string())
            }
            Expr::List(xs, _list_type) => {
                format!("[{}]", xs.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", "))
            }
            Expr::Variable(x) => x.to_string(),
            Expr::Conditional(cond, then, then_else) => {
                format!("if ({}) {} {} {} else {} {} {}",
                        cond.to_string(), "{",
                            then.to_string(),
                        "}", "{",
                            then_else.to_string(),
                        "}"
                )
            }
            Expr::Function(..) => {
                format!("<expression function>")
            },
            Expr::Lambda(..) => {
                format!("<lambda function>")
            }
        }
    }
}

impl ToString for Program {
    fn to_string(&self) -> String {
        format!(
            "Program: {}
{}
{}", "{",
            self.content.iter().map(|x| format!("  {}", x.to_string())).collect::<Vec<_>>().join("\n"),
            "}"
        )
    }
}

impl TypeIdentifier {
    pub fn to_string(&self) -> String {
        match self {
            TypeIdentifier::Name(t) => t.clone(),
            TypeIdentifier::Parameter(s) => s.clone(),
            TypeIdentifier::Anonymous => "«?»".to_string()
        }
    }
}

impl<T: Into<String>> From<T> for TypeIdentifier {
    fn from(s: T) -> Self {
        TypeIdentifier::Name(s.into())
    }
}

impl Display for TypeIdentifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}