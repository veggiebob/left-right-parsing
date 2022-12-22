use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::{Rc, Weak};
use crate::funcs::{expect_str, expect_with_used, take, take_while};
use crate::lang_obj::{Expr, Identifier, ParseError, Statement, TypeIdentifier, WhereClause};
use crate::lang_obj::Expr::Variable;
use crate::lang_obj::Identifier::Unit;
use crate::lang_obj::Statement::Lambda;
use crate::parse::{chainable, ExprParser, LengthQualifier, ListParser, ParseMetaData, Parser, ParseResult, TakeWhileParser};

pub struct IdentifierParser {
    /// a-z and A-Z are already allowed. You choose what else can be added
    allowed_characters: HashSet<char>
}

/// exact same as an IdentifierParser, but for expressions
pub struct VariableParser {
    pub id_parser: IdentifierParser,
    pub excluded_keywords: HashSet<String>
}

impl VariableParser {
    pub fn default(id_parser: IdentifierParser) -> VariableParser {
        VariableParser {
            id_parser,
            excluded_keywords: vec![
                "if",
                "else",
                "let"
            ].into_iter().map(ToString::to_string).collect()
        }
    }
}

/// might change in the future?
pub struct TypeParser(pub IdentifierParser);

// quick from just to make it clear
impl From<IdentifierParser> for VariableParser {
    fn from(ip: IdentifierParser) -> Self {
        VariableParser::default(ip)
    }
}
impl From<IdentifierParser> for TypeParser {
    fn from(ip: IdentifierParser) -> Self {
        TypeParser(ip)
    }
}

pub struct StatementParser {
    pub parsers: RefCell<Vec<Weak<GenericStatementParser>>>
}

pub type GenericStatementParser = Box<dyn Parser<Output=Statement>>;

#[macro_export]
macro_rules! box_stmt_parser {
    ($parser:expr) => {
        Box::new($parser) as Box<dyn Parser<Output=Statement>>
    }
}

pub struct LetParser {
    pub id_parser: Rc<IdentifierParser>,
    pub expr_parser: Rc<ExprParser>
}

pub struct FnDefParser {
    pub id_parser: Rc<IdentifierParser>,
    pub statement_parser: Rc<StatementParser>,
    pub type_parser: Rc<TypeParser>,
    pub expr_parser: Rc<ExprParser>,
    pub arg_parser: Rc<ListParser>
}

impl IdentifierParser {
    pub fn new(charset: &str) -> IdentifierParser {
        IdentifierParser {
            allowed_characters: charset.chars().into_iter().collect()
        }
    }
}

impl Parser for IdentifierParser {
    type Output = Identifier;

    fn parse(&self, content: &String, consume: bool, context: ParseMetaData) -> Result<HashSet<(Self::Output, usize)>, ParseError> {
        let ident = take_while(content, |c| c.is_alphabetic() || self.allowed_characters.contains(c)).0;
        if ident.len() == 0 {
            return Err(
                format!("Expected to start with a-z, A-Z, or one of {:?}", self.allowed_characters).into()
            )
        }
        if consume {
            if ident.len() < content.len() {
                Err(
                    format!("<<{}>> did not consume all that it was supposed to.", ident).into()
                )
            } else {
                Ok(
                    hashset![
                        (Identifier::Unit(ident.clone()), content.len())
                    ]
                )
            }
        } else {
            Ok({
                let mut possibilities = hashset![
                    (Identifier::Unit(ident.clone()), ident.len())
                ];
                for i in 1..ident.len() {
                    if let Some((name, _)) = take(&ident, i) {
                        let used = name.len();
                        possibilities.insert((Identifier::Unit(name), used));
                    }
                }
                possibilities
            })
        }
    }
}

impl Parser for VariableParser {
    type Output = Expr;

    fn parse(&self, content: &String, consume: bool, context: ParseMetaData) -> Result<HashSet<(Self::Output, usize)>, ParseError> {
        ParseResult(self.id_parser.parse(content, consume, context))
            // filter out excluded keywords
            .0.and_then(|hs| {
                let rest: HashSet<_> = hs.into_iter().filter_map(
                    |(e, used)| {
                        if let Unit(ident) = &e {
                            if self.excluded_keywords.contains(ident) {
                                None
                            } else {
                                Some((Variable(e), used))
                            }
                        } else {
                            Some((Variable(e), used))
                        }
                    })
                    .collect();
                if rest.len() == 0 {
                    Err("No variables left after excluding keywords".into())
                } else {
                    Ok(rest)
                }
            })
    }
}

impl Parser for LetParser {
    type Output = Statement;

    fn parse(&self, content: &String, consume: bool, context: ParseMetaData) -> Result<HashSet<(Self::Output, usize)>, ParseError> {
        let result = expect_with_used(content, "let", "Expected 'let'".into());
        let result = ParseResult(result) // wrap with the result to do some chaining!
            .parse_some_whitespace(&content, false, context.clone())
            .chain( // parse an identifier
                &content,
                false,
                context.clone().increment_depth(),
                chainable(|_space, next, meta| {
                    self.id_parser.parse(&next, false, context.clone())
                }),
                |_space, ident| ident
            )
            .parse_any_whitespace(&content, false, context.clone())
            .chain( // parse an '='
                &content,
                false,
                context.clone(),
                |_ident, next, meta| {
                    expect_str::<ParseError>(&next, "=",
                                             "Expected an '='".into(),
                    "Expected more characters, specifically an '='".into())
                        .map(|_| vec![((), 1)]).ok()
                },
                |ident, _eq| ident
            )
            .parse_any_whitespace(&content, false, context.clone())
            .chain( // parse the expression! (finally!)
                &content,
                false,
                context.clone(),
                chainable(|_ident, next, meta| {
                    self.expr_parser.parse(&next, consume, meta)
                }),
                |ident, expr| Statement::Let(ident, expr)
            );
        result.0 // unwrap from ParseResult
    }
}

#[derive(Clone, Eq, PartialEq, Debug, Hash)]
struct BaseFnDef {
    pub name: String,
    pub args: Vec<(Identifier, TypeIdentifier)>,
    pub expr: Expr,
    pub where_clause: WhereClause
}
impl From<BaseFnDef> for Statement {
    fn from(stmt: BaseFnDef) -> Self {
        Statement::Lambda(
            stmt.name,
            stmt.args,
            stmt.expr,
            stmt.where_clause
        )
    }
}

impl FnDefParser {
    /// Create an incomplete or base function (no where-clause) definition using
    /// an identifier, arguments, and an expression
    fn create_base_fndef(ident: Identifier, args: Expr, expr: Expr) -> Option<BaseFnDef> {
        if let Unit(name) = ident {
            if let Expr::List(expr_args) = args {
                let mut args = vec![];
                for arg in expr_args {
                    if let Expr::Infix(ident, _, type_name) = *arg {
                        if let Expr::Variable(ident) = *ident {
                            if let Expr::Variable(Unit(type_name)) = *type_name {
                                args.push((ident, type_name));
                            }
                        }
                    }
                }
                Some(
                    BaseFnDef {
                        name,
                        args,
                        expr,
                        where_clause: Box::new(vec![]) // empty where-clause for now
                    }
                )
            } else {
                None // args weren't a list
            }
        } else {
            None // name wasn't a unit string
        }
    }
}

impl Parser for FnDefParser {
    type Output = Statement;

    /*
        Proposed syntax:

        <function-name: identifier>\s*[<identifier1> : <type>, <identifier2> : <type>,...]\s*=>\s*<expr>\s*
            where\s*{
                <statement1>\s*
                <statement2>\s*
                <statement3>\s*
            }
     */
    ///
    fn parse(&self, content: &String, consume: bool, context: ParseMetaData) -> Result<HashSet<(Self::Output, usize)>, ParseError> {
        let ctx = context.clone().increment_depth();

        // first, parse the identifier
        let result = self.id_parser.parse(content, false, ctx.clone());
        let root = ParseResult(result)
            .parse_any_whitespace(&content, false, ctx.clone())
            .chain( // then parse the args
                content,
                false,
                ctx.clone(),
                chainable(|_ident, next, meta| {
                    self.arg_parser.parse(&next, false, meta.increment_depth())
                }),
                |id, args| {
                    (id, args)
                }
            )
            .parse_any_whitespace(&content, false, ctx.clone())
            .chain( // parse a '=>'
                &content,
                false,
                context.clone(),
                |_ident, next, meta| {
                    expect_str::<ParseError>(&next, "=>",
                                             "Expected an \"=>\"".into(),
                                             "Expected more characters, specifically an \"=>\"".into())
                        .map(|_| vec![((), 2)]).ok()
                },
                |x, _| x
            )
            .parse_any_whitespace(&content, false, ctx.clone())
            .chain( // parse an expression
                &content,
                false,
                context.clone().increment_depth(),
                chainable(|_p, next, meta| {
                    self.expr_parser.parse(&next, false, meta)
                }),
                |(ident, args), expr| (ident, args, expr)
            );

        // attempt a 'where' on it
        let with_where = root.clone()
            .parse_any_whitespace(&content, false, ctx.clone())
            .parse_static_text(&content, false, ctx.clone(), "where")
            .parse_any_whitespace(&content, false, ctx.clone())
            .parse_static_text(&content, false, ctx.clone(), "{")
            .parse_any_whitespace(&content, false, ctx.clone());

        let mut final_set = ParseResult(root.0.map(|hs| hs.into_iter().filter_map(
            |((ident, args, expr), used)| {
                if !consume || used == content.len() {
                    if let Some(stmt) = FnDefParser::create_base_fndef(ident, args, expr) {
                        Some((stmt.into(), used))
                    } else {
                        None // unable to create a function definition based on this
                    }
                } else {
                    None // consume problems
                }
            }
        ).collect::<HashSet<_>>())
            .and_then(|hs|
                if hs.len() > 0 {
                    Ok(hs)
                } else {
                    Err("No possibilities".into())
                }
            )
        );

        // if there is a 'where' statement
        if with_where.0.is_ok() {
            // parse some whitespace
            let where_statements = with_where.clone()
                .parse_any_whitespace(&content, false, ctx);

            if let Ok(where_statements) = where_statements.0 {
                //
                let mut where_statements = where_statements.into_iter()
                    .filter_map(|((ident, args, expr), used)| {
                        FnDefParser::create_base_fndef(ident, args, expr)
                            .map(|stmt| (stmt, used))
                    }).collect::<HashSet<_>>();
                // now continue attempting to parse statements until a '}' is reached
                let mut finalized = hashset![];
                while where_statements.len() > 0 {

                    // first, check if we can finish off any
                    let end_statement = ParseResult(Ok(where_statements.clone()))
                        .parse_static_text(&content, false, context.clone(), "}");

                    end_statement.0.map(|hs| {
                        hs.into_iter().for_each(|x| {
                            finalized.insert(x);
                        })
                    });

                    // otherwise, we're gonna have to look for another statement
                    let next_statement = ParseResult(Ok(where_statements.clone()))
                        .chain(
                            &content,
                            false,
                            context.clone(),
                            chainable(|_fndef, next, meta| {
                                self.statement_parser.parse(&next, false, meta.increment_depth())
                            }),
                            |fndef, stmt| {
                                BaseFnDef {
                                    name: fndef.name,
                                    args: fndef.args,
                                    expr: fndef.expr,
                                    where_clause: {
                                        let mut stmts = vec![stmt];
                                        stmts.extend(*fndef.where_clause);
                                        Box::new(stmts)
                                    }
                                }
                            }
                        ).parse_any_whitespace(&content, false, context.clone());
                    where_statements = match next_statement.0 {
                        Ok(stmts) => stmts,
                        Err(e) => hashset![]
                    };
                }
                // filter out statements that don't meet consuming rules
                // (and also map them)
                let finalized = finalized.into_iter().filter_map(|(e, used)| {
                    if !consume || used == content.len() {
                        Some((Statement::from(e), used))
                    } else {
                        None
                    }
                }).collect::<HashSet<_>>();
                if finalized.len() > 0 {
                    let finalized = ParseResult(Ok(finalized));
                    final_set = final_set.merge(&finalized);
                }
            }
        }
        final_set.0
    }
}


impl Parser for StatementParser {
    type Output = Statement;

    fn parse(&self, content: &String, consume: bool, context: ParseMetaData) -> Result<HashSet<(Self::Output, usize)>, ParseError> {
        let mut out = HashSet::new();
        let mut err_messages = "".to_string();
        let mut num_tried = 0;
        for parser in self.parsers.borrow().iter() {
            if let Some(parser) = parser.upgrade() {
                match parser.parse(content, consume, context.clone()) {
                    Ok(parses) => {
                        num_tried += 1;
                        for p in parses {
                            out.insert(p);
                        }
                    },
                    Err(e) => {
                        err_messages += &*("\n".to_string() + e.message.as_str());
                    }
                }
            } else {
                // for some reason the pointer has deteriorated???
            }
        }

        if out.len() > 0 {
            Ok(out)
        } else {
            Err(format!("No valid statements. {} tried. {}", num_tried, err_messages).into())
        }
    }
}