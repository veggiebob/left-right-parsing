use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use crate::lang_obj::{Expr, Identifier, TypeIdentifier};

pub struct ProgramData {
    pub constants: HashMap<Identifier, Expr>, // should be immutable >:(
    pub types: HashMap<TypeIdentifier, Kind>,
}

type HeapID = usize;

pub struct HeapData {
    pub data: HashMap<HeapID, RefCell<Expr>>
}

pub struct StackFrame {
    pub data: HashMap<Identifier, RefCell<Expr>>
}

pub struct StackData(pub(crate) Vec<StackFrame>);

/// the type of type (kind)
pub enum Kind {
    Product(ProductType),
    Sum(SumType),
    Enum(EnumType)
}

// intersection type (junct)
pub struct ProductType {
    pub name: TypeIdentifier,
    pub data: ProductTypeKind
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

// dependent type with tuple length?
/// Yeah! Tuple!
pub struct TupleType {
    pub length: usize,
    pub types: Vec<Box<Kind>>
}

/// Object style
pub struct NamedProductType {
    pub fields: HashMap<Identifier, Box<Kind>>
}

// union type (disjunct)
pub struct SumType {
    pub name: TypeIdentifier,
    pub options: HashSet<Identifier>
}

/// A combination of sum types and product types.
/// Each option has a product constructor associated with it.
pub struct EnumType {
    pub name: TypeIdentifier,
    pub options: HashMap<Identifier, ProductType>
}

pub enum Term {
    String(String),
    Nat(u64),
    Token(String) // mm hmm heheh
}