use std::fmt::{Display, Formatter};
use crate::{Expr, Statement};

pub trait Format<E> {
    fn format(&self, ast: &E) -> Text<Self> where Self: Sized;
}

pub struct JSON {
    pub pretty: bool
}

struct XML {
    pub pretty: bool
}
struct JavaScript;
struct Haskell;

#[derive(Debug)]
pub struct Text<'a, T: Sized> {
    pub content: String,
    pub gen: &'a T
}

fn quote(content: String) -> String {
    "\"".to_string() + &content + "\""
}

impl Format<Expr> for JSON {
    fn format(&self, ast: &Expr) -> Text<Self> {
        // ignoring pretty printing for now
        let content: String = match ast {
            Expr::Nat(x) => x.content.to_string(),
            Expr::Str(s) => quote(s.content.clone()),
            Expr::Infix(left, infix, right) => {
                format!(
                    "{}\"left\":{},\"infix\":{},\"right\":{}{}",
                    "{",
                    self.format(left.as_ref()),
                    quote(infix.clone()),
                    self.format(right.as_ref()),
                    "}"
                )
            }
            Expr::List(xs) => {
                self.from_vec(xs.iter().map(|x| self.format(x.as_ref())).collect()).content
            },
            Expr::Func(_, _) => {
                todo!("deprecated?")
            },
            Expr::Variable(i) => quote(i.to_string()),
            Expr::Conditional(cond, then, then_else) => {
                format!(
                    "{}\"if\":{},\"then\":{},\"else\":{}{}",
                    "{",
                    self.format(cond.as_ref()),
                    self.format(then.as_ref()),
                    self.format(then_else.as_ref()),
                    "}"
                )
            }
        };
        Text {
            content,
            gen: &self
        }
    }
}

impl Format<Statement> for JSON {
    fn format(&self, ast: &Statement) -> Text<Self> where Self: Sized {
        let content: String = match ast {
            Statement::Let(ident, expr) =>
                format!(
                    "{}\"identifier:\"{},\"expr\":{}{}",
                    "{",
                    quote(ident.to_string()),
                    self.format(expr),
                    "}"
                ),
            Statement::FnDef(name, args, body, wheres) => {
                format!(
                    "{}\"name\":{},\"args\":{},\"expr\":{},\"defs\":{}{}",
                    "{",
                    quote(name.clone()),
                    {
                        let xs = args.into_iter().map(|(ident, it)|
                            format!(
                                "{}\"identifier\":{},\"type\":{}{}",
                                "{",
                                quote(ident.to_string()),
                                quote(it.to_string()),
                                "}"
                            )
                        ).collect::<Vec<_>>();
                        self.from_vec(xs)
                    },
                    self.format(body),
                    self.from_vec(wheres.iter().map(|x| self.format(x)).collect()),
                    "}"
                )
            }
        };
        Text {
            content,
            gen: &self
        }
    }
}

impl JSON {
    pub fn new(pretty: bool) -> JSON {
        JSON {
            pretty
        }
    }
    pub fn from_vec<T: Display>(&self, xs: Vec<T>) -> Text<'_, JSON> {
        // ignoring prettifying again
        if xs.len() == 0 {
            return Text {
                content: "[]".to_string(),
                gen: &self
            }
        }
        let mut res = String::from("[");
        let last_index = xs.len() - 1;
        let mut i = 0;
        for x in xs {
            if i == last_index {
                // if it's the last one
                res += x.to_string().as_str();
            } else {
                res += format!("{},", x).as_str();
            }
            i += 1;
        }
        res += "]";
        Text {
            content: res,
            gen: &self
        }
    }
}

impl<T> Display for Text<'_, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.content)
    }
}