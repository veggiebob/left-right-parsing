use std::fmt::{Display, Formatter};
use std::ops::Add;
use crate::{Expr, Statement};
use crate::lang_obj::{Identifier, LONat, LOString, Program, TypeIdentifier};

pub trait Format<E> {
    fn format(&self, ast: &E) -> Text<Self> where Self: Sized;
}

#[derive(Clone, Copy)]
pub struct JSON {
    pub pretty: bool
}

pub struct XML {
    pub pretty: bool
}

/// The point of this formatter is to make something
/// visually appealing with colors! and style!
#[derive(Clone)]
pub struct HTML {
    pub style: Style
}

struct JavaScript;
struct Haskell;

type Color = (u8, u8, u8);
#[derive(Clone)]
pub struct Style {
    pub color_wheel: Vec<Color> 
}

#[derive(Debug)]
pub struct Text<'a, T: Sized> {
    pub content: String,
    pub gen: &'a T
}

fn quote(content: String) -> String {
    "\"".to_string() + &content + "\""
}

fn html_color(content: &str, color: &str) -> String {
    format!("<span style=\"color:{}\">{}</span>", color, content)
}

impl Format<LOString> for HTML {
    fn format(&self, ast: &LOString) -> Text<Self> where Self: Sized {
        Text {
            content: format!("<span style=\"color:#0c0\">\"{}\"</span>", ast.content),
            gen: &self
        }
    }
}

impl Format<LONat> for HTML {
    fn format(&self, ast: &LONat) -> Text<Self> where Self: Sized {
        Text {
            // color the text light blue
            content: html_color(ast.content.to_string().as_str(), "#aff"),
            gen: &self
        }
    }
}

impl Format<Identifier> for HTML {
    fn format(&self, ast: &Identifier) -> Text<Self> where Self: Sized {
        Text {
            content: html_color(ast.to_string().as_str(), "#ccf"),
            gen: &self
        }
    }
}

impl Format<Expr> for HTML {
    fn format(&self, ast: &Expr) -> Text<Self> where Self: Sized {
        // ignoring style, for now
        let content = format!(
            "{}",
            match ast {
                Expr::Nat(x) => self.format(x),
                Expr::Str(s) => self.format(s),
                Expr::Infix(left, s, right) => {
                    Text {
                        content: format!("({} {} {})", self.format(left.as_ref()), s, self.format(right.as_ref())),
                        gen: self
                    }
                },
                Expr::List(es) => {
                    Text {
                        content:
                            format!(
                                "{}{}{}",
                                html_color("[", "grey"),
                                es.into_iter()
                                    .map(|f| self.format(f.as_ref()))
                                    .map(|t| t.content)
                                    .collect::<Vec<_>>()
                                    .join(", "),
                                html_color("]", "grey")
                            ),
                        gen: self
                    }
                },
                Expr::Variable(ident) => self.format(ident),
                Expr::Conditional(cond, then, then_else) => {
                    Text {
                        content: format!(
                            "{} {} {} {} {} {} {} {} {}",
                            html_color("if", "orange"),
                            self.format(cond.as_ref()),
                            "{",
                            self.format(then.as_ref()),
                            "}",
                            html_color("else", "orange"),
                            "{",
                            self.format(then_else.as_ref()),
                            "}"
                        ),
                        gen: self
                    }
                },
                Expr::Function((args, retT), body, ret) => {
                    // Statement::FnDef(name, args, body, wheres) => {
                    // format!(
                    //     "{} {} = ({}) => {} {} {} {} {}",
                    //     html_color("let", "orange"),
                    //     name,
                    //     args.into_iter()
                    //         .map(|(ident, i_type)| {
                    //             format!("{} {}",
                    //                     html_color(&i_type.unwrap_or("Any".into()), "#ff0"),
                    //                     &self.format(&ident).content)
                    //         })
                    //         .collect::<Vec<_>>()
                    //         .join(", "),
                    //     self.format(body),
                    //     html_color("where", "orange"),
                    //     html_color("{", "grey"),
                    //     wheres.iter()
                    //         .map(|e| self.format(e))
                    //         .map(|t| t.content)
                    //         .collect::<Vec<_>>()
                    //         .join(" "),
                    //     html_color("}", "grey")
                    // )
                    todo!("No impl for Format<Expr::Function> for HTML")
                },
                Expr::Lambda(..) => {
                    todo!("No impl for Format<Expr::Lambda> for HTML")
                }
            }
        );

        Text {
            content,
            gen: &self
        }
    }
}

impl Format<Statement> for HTML {
    fn format(&self, ast: &Statement) -> Text<Self> where Self: Sized {
        Text {
            content: match ast {
                Statement::Let(ident, expr) => {
                    format!(
                        "<span style=\"color:orange\">let</span> {} = {};",
                        self.format(ident),
                        self.format(expr)
                    )
                },
                _ => todo!("missing case for Format<Statement> for HTML")
            },
            gen: &self
        }
    }
}

impl Format<Program> for HTML {
    fn format(&self, ast: &Program) -> Text<Self> where Self: Sized {
        Text {
            content: format!(
                "<div style=\"margin:3px;color:white;background-color:#555;padding:10px;float:left;border-radius:5px\">{}</div>",
                ast.content.iter()
                    .map(|x| self.format(x))
                    .map(|s| format!("&ensp;{}", s))
                    .collect::<Vec<_>>()
                    .join("<br>")
            ),
            gen: self
        }
    }
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
            },
            Expr::Function(..) => {
                todo!("No JSON format for Expr::Function. See old impl of Format<Statement> for JSON")
            },
            Expr::Lambda(..) => {
                todo!("No JSON format for Expr::Lambda. See old impl of Format<Statement> for JSON")
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
                    "{}\"identifier\":{},\"expr\":{}{}",
                    "{",
                    quote(ident.to_string()),
                    self.format(expr),
                    "}"
                ),
            // Statement::FnDef(name, args, body, wheres) => {
            //     format!(
            //         "{}\"name\":{},\"args\":{},\"expr\":{},\"defs\":{}{}",
            //         "{",
            //         quote(name.clone()),
            //         {
            //             let xs = args.into_iter().map(|(ident, it)|
            //                 format!(
            //                     "{}\"identifier\":{},\"type\":{}{}",
            //                     "{",
            //                     quote(ident.to_string()),
            //                     quote(it.to_string()),
            //                     "}"
            //                 )
            //             ).collect::<Vec<_>>();
            //             self.from_vec(xs)
            //         },
            //         self.format(body),
            //         self.from_vec(wheres.iter().map(|x| self.format(x)).collect()),
            //         "}"
            //     )
            // },
            _ => todo!("Missing case for Format<Statement> for JSON")
        };
        Text {
            content,
            gen: &self
        }
    }
}

impl Format<Program> for JSON {
    fn format(&self, ast: &Program) -> Text<Self> where Self: Sized {
        let content: Vec<_> = ast.content.iter().map(|stmt| self.format(stmt)).collect();
        Text {
            content: self.from_vec(content).content,
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

impl<'a, T> Add<Text<'a, T>> for Text<'a, T> {
    type Output = Text<'a, T>;

    fn add(self, rhs: Text<'a, T>) -> Self::Output {
        Text {
            content: self.content + &*rhs.content,
            gen: self.gen
        }
    }
}

impl<'a, T> Add<&'a str> for Text<'a, T> {
    type Output = Text<'a, T>;

    fn add(self, rhs: &str) -> Self::Output {
        Text {
            content: self.content + rhs,
            gen: self.gen
        }
    }
}

// impl<T> Join<String> for Vec<Text<'_, T>> {
//     type Output = Text<'_, T>;

//     fn join(self, sep: String) -> Self::Output {
//         Text {
//             content: self.into_iter().map(|x| x.content.as_str()).collect::<Vec<_>>().join(sep),
//             gen: self.gen
//         }
//     }
// }

// impl<'a, E, T: Format<E>> Add<E> for Text<'a, T> {
//     type Output = Text<'a, T>;
//
//     fn add(self, rhs: E) -> Self::Output {
//         self + self.gen.format(rhs)
//     }
// }
