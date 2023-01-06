use std::borrow::ToOwned;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use crate::interpret::Value;
use crate::lang_obj::{Expr, FunctionBody, FunctionSignature, Identifier, Statement, TypeIdentifier};

/// These are the classes of objects that are stored during runtime
/// a couple arbitrary types will be included for convenience.
/// Their implementations will be written out in Rust.
#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    /// "primitives"
    Bool(bool),
    String(String),
    Nat(u64), // natural unsigned integer
    Float(f64),
    Token(String), // mm hmm heheh?

    /// user types
    Object(Box<LanguageObject>),

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
    pub types: HashMap<TypeIdentifier, Type>,
}

type HeapID = usize;

pub struct HeapData {
    pub data: HashMap<HeapID, Value>
}

#[derive(Debug, PartialEq)]
pub struct StackFrame {
    pub data: HashMap<Identifier, Value>
}

pub struct StackData(pub(crate) Vec<StackFrame>);

///////////////////// types and the objects that hold their data //////////////////////////////////

pub fn function_param_type_id() -> Identifier {
    Identifier::Unit("params".to_owned())
}
pub fn function_return_type_id() -> Identifier {
    Identifier::Unit("return".to_owned())
}
pub fn function_type_param_type_id() -> Identifier {
    Identifier::Unit("type_params".to_owned())
}

/// These are the different KINDS of types
/// an instance, however, of this class, is a TYPE
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Unit(TypeIdentifier),
    /// Parameterized Types
    /// function types: return, params... (in progress)
    ///
    Parametric(TypeIdentifier, Vec<Box<Type>>), // probably shouldn't be used??
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
#[derive(Debug, PartialEq, Eq, Clone)]
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
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ProductTypeKind {
    None, // empty tuple??
    Tuple(TupleType),
    List(ListType),
    Named(NamedProductType),
}

// absence of parallel object here; flattened into ProductObject

// dependent type with tuple length?
/// Yeah! Tuple!
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TupleType {
    pub length: usize,
    pub types: Vec<Box<Type>>
}

#[derive(Clone, Debug, PartialEq)]
pub struct TupleObject(pub Vec<Term>);

/// Object style
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct NamedProductType {
    pub fields: HashMap<Identifier, Box<Type>>
}

#[derive(Clone, Debug, PartialEq)]
pub struct NamedProductObject(pub HashMap<Identifier, Term>);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ListType(pub Box<Type>);

#[derive(Clone, Debug, PartialEq)]
pub struct ListObject(pub Vec<Term>);

// union type (disjunct)
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SumType {
    pub name: TypeIdentifier,
    pub options: HashSet<Identifier>
}

#[derive(Clone, Debug, PartialEq)]
pub struct SumObject(pub Identifier);

/// A combination of sum types and product types.
/// Each option has a product constructor associated with it.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct EnumType {
    pub name: TypeIdentifier,
    pub options: HashMap<Identifier, ProductType>
}

#[derive(Clone, Debug, PartialEq)]
pub struct EnumObject(pub Identifier, pub ProductObject);

impl ProductObject {
    pub fn get_anon_type(&self) -> ProductType {
        ProductType {
            name: TypeIdentifier::Anonymous,
            data: match self {
                ProductObject::None => ProductTypeKind::None,
                ProductObject::Tuple(TupleObject(v)) => ProductTypeKind::Tuple(TupleType {
                    types: v.iter().map(|e| e.get_anon_type()).map(Box::new).collect(),
                    length: v.len()
                }),
                ProductObject::Named(NamedProductObject(fields)) => {
                    ProductTypeKind::Named(NamedProductType {
                        fields: fields.iter()
                            .map(|(ident, term)|
                                (ident.clone(), Box::new(term.get_anon_type())))
                            .collect()
                    })
                }
                ProductObject::List(ListObject(terms)) => {
                    // we assume that the list has been constructed lawfully
                    ProductTypeKind::List(ListType(terms.get(0).map(Term::get_anon_type).unwrap_or(Type::unknown()).into()))
                }
            }
        }
    }
}

impl Term {
    pub fn get_anon_type(&self) -> Type {
        match &self {
            Term::Bool(_) => Type::Unit("bool".into()),
            Term::String(_) => Type::Unit("string".into()),
            Term::Nat(_) => Type::Unit("nat".into()),
            Term::Float(_) => Type::Unit("float".into()),
            Term::Token(t) => Type::Unit(t.into()), // should a token be its own type??
            Term::Object(o) => {
                match o.as_ref() {
                    LanguageObject::Of(x) => x.get_anon_type(),
                    LanguageObject::Product(prod) => Type::Product(prod.get_anon_type()),
                    LanguageObject::Sum(sum) => Type::Sum(SumType {
                        options: hashset!{ sum.0.clone() },
                        name: TypeIdentifier::Anonymous
                    }),
                    LanguageObject::Enum(en) => Type::Enum(EnumType {
                        name: TypeIdentifier::Anonymous,
                        options: hashmap!{ en.0.clone() => en.1.get_anon_type() }
                    })
                }
            },
            Term::HeapPointer(_) => Type::unknown(),
            Term::StackPointer(_) => Type::unknown(),
            Term::Function((params, ret_t), body, ret, cap) => {
                    Type::Product(ProductType {
                        name: "function".into(),
                        data: ProductTypeKind::Named(NamedProductType {
                            fields: hashmap! {
                                function_return_type_id() => Box::new(ret_t.as_ref().map(|t| Type::Unit(t.clone())).unwrap_or(Type::unknown())),
                                function_param_type_id() => Box::new(
                                    params.iter().map(|(_i, t)| t.as_ref().map(|t| t.clone()).map(Type::Unit).unwrap_or(Type::unknown())).collect::<Vec<_>>().into()
                                )
                            }
                        })
                })
            }
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
            Term::Bool(b) => format!("{}", b),
            Term::String(s) => format!("\"{}\"", s),
            Term::Nat(n) => format!("{}", n),
            Term::Float(f) => format!("{}", f),
            Term::Token(token) => format!("Token({})", token),
            Term::Object(obj) => obj.to_string(),
            Term::Function((params, _ret_t), body, ret, captured) => {
                format!(
                    "({}) => {}{}\n    return {} {}",
                    params.iter().map(|(name, _type)| name.to_string()).reduce(|s, i| s + ", " + &i).unwrap_or("".to_string()),
                    "{",
                    body.iter().map(|stmt| "    ".to_string() + &stmt.to_string()).reduce(|s, st| s + "\n" + &st).unwrap_or("".into()),
                    ret.to_string(),
                    "}"
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


impl From<&str> for Type {
    fn from(s: &str) -> Self {
        Type::Unit(TypeIdentifier::Name(s.to_string()))
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.string(true))
    }
}

impl Type {
    pub fn unknown() -> Type {
        Type::Unit(TypeIdentifier::Anonymous)
    }

    pub fn string(&self, inline: bool) -> String {
        match &self {
            Type::Unit(t) => format!("{}", t),
            Type::Parametric(name, types) => format!(
                "{}<{}>",
                name,
                remap(types, ToString::to_string, ", ", "")
            ),
            Type::Product(p) => p.string(inline),
            Type::Sum(sum) => format!(
                "{}({})",
                sum.name,
                remap(&sum.options.iter().collect(), ToString::to_string, " | ", "")
            ),
            Type::Enum(en) => format!(
                "{} {}{}{}{}{}",
                en.name,
                "{", if inline { "" } else { "\n" },
                remap(
                    &en.options.iter().collect(),
                    |(k, v)| format!(
                        "{}{}{}",
                        if inline { "" } else { "   " },
                        k,
                        v.data.string(inline)
                    ),
                    if inline { ",\n" } else { ", " },
                    ""
                ),
                if inline { "" } else { "\n" }, "}"
            )
        }
    }
}

impl ProductType {
    pub fn string(&self, inline: bool) -> String {
        format!(
            "{}{}",
            self.name,
            self.data.string(inline)
        )
    }
}

impl ProductTypeKind {
    pub fn string(&self, inline: bool) -> String {
        match self {
            ProductTypeKind::List(ListType(typ)) => format!("[{}]", typ.string(inline)),
            ProductTypeKind::None => "()".to_string(),
            ProductTypeKind::Tuple(tup) => format!(
                "({})", remap(&tup.types, |x| x.string(true), ", ", "")
            ),
            ProductTypeKind::Named(named) => format!(
                " {}{}{}{}{}",
                "{",
                if inline { "" } else { "\n" },
                remap(
                    &named.fields.iter().collect(),
                    |(k, v)| format!("{}{}: {}",
                                     if inline { "" } else { "   " }, k, v),
                    if inline { ", " } else { ",\n" }, ""
                ),
                if inline { "" } else { "\n" },
                "}",
            )
        }
    }
}

impl From<Vec<Type>> for Type {
    fn from(types: Vec<Type>) -> Self {
        Type::Product(ProductType {
            name: TypeIdentifier::Anonymous,
            data: ProductTypeKind::Tuple(TupleType {
                length: types.len(),
                types: types.into_iter().map(Box::new).collect()
            })
        })
    }
}