use std::cell::RefCell;
use std::collections::{HashMap, HashSet, VecDeque};
use std::rc::Rc;
use crate::lang_obj::{Expr, Identifier, Statement, TypeIdentifier};

/// These are the classes of objects that are stored during runtime
/// a couple arbitrary types will be included for convenience.
/// Their implementations will be written out in Rust.
pub enum Term {
    /// "primitives"
    String(String),
    Nat(u64), // natural unsigned integer
    Float(f64),
    Token(String), // mm hmm heheh?

    /// user types
    Object(Box<LanguageObject>),

    // didn't see a great reason to group up pointers

    /// points to an object on the heap
    HeapPointer(HeapID),

    /// points to an object on the stack
    StackPointer(Rc<RefCell<Term>>)
}

pub struct ProgramData {
    pub constants: HashMap<Identifier, Expr>, // should be immutable >:(
    pub types: HashMap<TypeIdentifier, Kind>,
}

type HeapID = usize;

pub struct HeapData {
    pub data: HashMap<HeapID, RefCell<Term>>
}

pub struct StackFrame {
    pub data: HashMap<Identifier, RefCell<Term>>,
    pub return_value: Option<Term>
}

pub struct StackData(pub(crate) Vec<StackFrame>);

///////////////////// types and the objects that hold their data //////////////////////////////////

/// the type of type (kind)
pub enum Kind {
    Unit(TypeIdentifier),
    Product(ProductType),
    Sum(SumType),
    Enum(EnumType)
}

/// parallel to Kind, but containing data
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
pub enum ProductObject {
    None, // empty tuple
    Tuple(TupleObject),
    Named(NamedProductObject)
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

pub struct TupleObject(pub Vec<Term>);

/// Object style
pub struct NamedProductType {
    pub fields: HashMap<Identifier, Box<Kind>>
}

pub struct NamedProductObject(pub HashMap<Identifier, Term>);

// union type (disjunct)
pub struct SumType {
    pub name: TypeIdentifier,
    pub options: HashSet<Identifier>
}

pub struct SumObject(pub Identifier);

/// A combination of sum types and product types.
/// Each option has a product constructor associated with it.
pub struct EnumType {
    pub name: TypeIdentifier,
    pub options: HashMap<Identifier, ProductType>
}

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
            },
            Term::HeapPointer(_) => todo!(),
            Term::StackPointer(_) => todo!()
        }
    }
}