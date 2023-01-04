use std::cell::RefCell;
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use crate::lang_obj::{Expr, FunctionBody, FunctionSignature, Identifier, Statement, TypeIdentifier};

/// These are the classes of objects that are stored during runtime
/// a couple arbitrary types will be included for convenience.
/// Their implementations will be written out in Rust.
#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    /// "primitives"
    String(String),
    Nat(u64), // natural unsigned integer
    Float(f64),
    Token(String), // mm hmm heheh?

    /// user types
    Object(Box<LanguageObject>),

    // todo: function types??
    // we will determine whether functions are pure or not LATER
    // and using TYPES
    // which will accompany TERMS
    /// signature, body, return, captured
    Function(FunctionSignature, FunctionBody, Box<Expr>, HashSet<Identifier>),

    // didn't see a great reason to group up pointers

    /// points to an object on the heap
    HeapPointer(HeapID),

    /// points to an object on the stack
    StackPointer(Identifier)
}

pub struct ProgramData {
    pub constants: HashMap<Identifier, Expr>, // should be immutable >:(
    pub types: HashMap<TypeIdentifier, Kind>,
}

type HeapID = usize;

pub struct HeapData {
    pub data: HashMap<HeapID, Term>
}

#[derive(Debug, PartialEq)]
pub struct StackFrame {
    pub data: HashMap<Identifier, Term>,
    pub return_value: Option<Term>
}

pub struct StackData(pub(crate) Vec<StackFrame>);

///////////////////// types and the objects that hold their data //////////////////////////////////

/// the type of type (kind)
pub enum Kind {
    Unit(TypeIdentifier),
    Parametric(TypeIdentifier, Vec<Box<Kind>>),
    Product(ProductType),
    Sum(SumType),
    Enum(EnumType)
}

/// parallel to Kind, but containing data
#[derive(Clone, Debug, PartialEq)]
pub enum LanguageObject {
    Of(Term),
    Product(ProductObject),
    Sum(SumObject),
    Enum(EnumObject),
}

// intersection type (junct)
pub struct ProductType {
    pub name: TypeIdentifier,
    pub data: ProductTypeKind
}

// they do not have names
// also this has been flattened for convenience
// because I didn't want to have a `data` field for it to be the only field
#[derive(Clone, Debug, PartialEq)]
pub enum ProductObject {
    None, // empty tuple
    Tuple(TupleObject),
    Named(NamedProductObject),
    List(ListObject)
}

/// Different ways of representing product types
/// tuples are the more original vector style,
/// while "named products" are more convenient.
/// Both variants are equivalent.
pub enum ProductTypeKind {
    None, // empty tuple??
    Tuple(TupleType),
    Named(NamedProductType),
}

// absence of parallel object here; flattened into ProductObject

// dependent type with tuple length?
/// Yeah! Tuple!
pub struct TupleType {
    pub length: usize,
    pub types: Vec<Box<Kind>>
}

#[derive(Clone, Debug, PartialEq)]
pub struct TupleObject(pub Vec<Term>);

/// Object style
pub struct NamedProductType {
    pub fields: HashMap<Identifier, Box<Kind>>
}

#[derive(Clone, Debug, PartialEq)]
pub struct NamedProductObject(pub HashMap<Identifier, Term>);

#[derive(Clone, Debug, PartialEq)]
pub struct ListObject(pub Vec<Term>);

// union type (disjunct)
pub struct SumType {
    pub name: TypeIdentifier,
    pub options: HashSet<Identifier>
}

#[derive(Clone, Debug, PartialEq)]
pub struct SumObject(pub Identifier);

/// A combination of sum types and product types.
/// Each option has a product constructor associated with it.
pub struct EnumType {
    pub name: TypeIdentifier,
    pub options: HashMap<Identifier, ProductType>
}

#[derive(Clone, Debug, PartialEq)]
pub struct EnumObject(pub Identifier, pub ProductObject);

impl Term {
    pub fn get_type(&self) -> Kind {
        match self {
            Term::String(_) => Kind::Unit("string".to_string()),
            Term::Nat(_) => Kind::Unit("nat".to_string()),
            Term::Float(_) => Kind::Unit("float".to_string()),
            Term::Token(t) => Kind::Unit(t.clone()), // should a token be its own type??
            Term::Object(o) => {
                todo!()
                // match o {
                //     LanguageObject::Of(x) => x.get_type(),
                //     LanguageObject::Enum(en) => EnumType {
                //         name: "hi".to_string(),
                //         options: en.
                //     }
                // }
            },
            Term::HeapPointer(_) => todo!(),
            Term::StackPointer(_) => todo!(),
            Term::Function(..) => todo!()
        }
    }

    pub fn is_list(&self) -> bool {
        if let Term::Object(o) = self {
            if let LanguageObject::Product(ProductObject::List(_)) = **o {
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Term::String(s) => format!("\"{}\"", s),
            Term::Nat(n) => format!("{}", n),
            Term::Float(f) => format!("{}", f),
            Term::Token(token) => format!("Token({})", token),
            Term::Object(obj) => obj.to_string(),
            Term::Function((params, _ret_t), body, ret, captured) => {
                format!(
                    "({}) => <({}\n    return {} )>",
                    params.iter().map(|(name, _type)| name.to_string()).reduce(|s, i| s + ", " + &i).unwrap_or("".to_string()),
                    body.iter().map(|stmt| "    ".to_string() + &stmt.to_string()).reduce(|s, st| s + "\n" + &st).unwrap_or("".into()),
                    ret.to_string()
                )
            }
            Term::HeapPointer(id) => format!("Pointer to {}", id),
            Term::StackPointer(ident) => format!("Pointer to {:?}", ident)
        }
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl ToString for LanguageObject {
    fn to_string(&self) -> String {
        match self {
            LanguageObject::Of(t) => t.to_string(),
            LanguageObject::Product(prod) => {
                match prod {
                    ProductObject::None => "()".to_string(),
                    ProductObject::Tuple(tup) =>
                        format!("({})", remap(&tup.0, ToString::to_string, ", ", "")),
                    ProductObject::Named(p) => format!("{:#?}", p),
                    ProductObject::List(ListObject(list)) =>
                        format!("[{}]", remap(list, ToString::to_string, ", ", ""))
                }
            }
            LanguageObject::Sum(sum) => format!("{:#?}", sum),
            LanguageObject::Enum(en) => format!("{:#?}", en)
        }
    }
}

fn remap<T, F, S>(list: &Vec<T>, map: F, sep: &str, or: &str) -> String
where F: Fn(&T) -> S,
      S: Into<String>
{
    list.iter().map(|x| map(x).into())
        .reduce(|acc, x| acc + sep + &x).unwrap_or(or.into())
}