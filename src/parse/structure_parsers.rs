use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::{Rc, Weak};
use crate::funcs::{expect_str, expect_with_used, take, take_while};
use crate::lang_obj::{Expr, Identifier, ParseError, Statement};
use crate::lang_obj::Identifier::Unit;
use crate::parse::{chainable, ExprParser, LengthQualifier, ListParser, ParseMetaData, Parser, ParseResult, TakeWhileParser};

pub struct IdentifierParser {
    /// a-z and A-Z are already allowed. You choose what else can be added
    allowed_characters: HashSet<char>
}

/// exact same as an IdentifierParser, but for expressions
pub struct VariableParser(pub IdentifierParser);

/// might change in the future?
pub struct TypeParser(pub IdentifierParser);

// quick from just to make it clear
impl From<IdentifierParser> for VariableParser {
    fn from(ip: IdentifierParser) -> Self {
        VariableParser(ip)
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
        if consume && ident.len() < content.len() {
            Err(
                format!("<<{}>> did not consume all that it was supposed to.", ident).into()
            )
        } else if ident.len() == 0 {
            Err(
                format!("Expected to start with a-z, A-Z, or one of {:?}", self.allowed_characters).into()
            )
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
        self.0.parse(content, consume, context)
            .map(|hs| hs.into_iter()
                .map(|(ident, used)| (Expr::Variable(ident), used)).collect())
    }
}

impl Parser for LetParser {
    type Output = Statement;

    fn parse(&self, content: &String, consume: bool, context: ParseMetaData) -> Result<HashSet<(Self::Output, usize)>, ParseError> {
        let one_or_more_space = TakeWhileParser::whitespace(LengthQualifier::GEQ(1));
        let maybe_space = TakeWhileParser::whitespace(LengthQualifier::GEQ(0));
        let result = expect_with_used(content, "let", "Expected 'let'".into());
        let result = ParseResult(result) // wrap with the result to do some chaining!
            .parse_some_whitespace(&content, false, context)
            .chain( // parse an identifier
                &content,
                false,
                context.increment_depth(),
                chainable(|_space, next, meta| {
                    self.id_parser.parse(&next, false, context)
                }),
                |_space, ident| ident
            )
            .parse_any_whitespace(&content, false, context)
            .chain( // parse an '='
                &content,
                false,
                context,
                |_ident, next, meta| {
                    expect_str::<ParseError>(&next, "=",
                                             "Expected an '='".into(),
                    "Expected more characters, specifically an '='".into())
                        .map(|_| vec![((), 1)]).ok()
                },
                |ident, _eq| ident
            )
            .parse_any_whitespace(&content, false, context)
            .chain( // parse the expression! (finally!)
                &content,
                false,
                context,
                chainable(|_ident, next, meta| {
                    self.expr_parser.parse(&next, consume, meta)
                }),
                |ident, expr| Statement::Let(ident, expr)
            );
        result.0 // unwrap from ParseResult
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
        let ctx = context.increment_depth();

        // first, parse the identifier
        let result = self.id_parser.parse(content, false, ctx);
        let root = ParseResult(result)
            .parse_any_whitespace(&content, false, ctx)
            .chain( // then parse the args
                content,
                false,
                ctx,
                chainable(|_ident, next, meta| {
                    self.arg_parser.parse(&next, false, meta.increment_depth())
                }),
                |id, args| {
                    (id, args)
                }
            )
            .parse_any_whitespace(&content, false, ctx)
            .chain( // parse a '=>'
                &content,
                false,
                context,
                |_ident, next, meta| {
                    expect_str::<ParseError>(&next, "=>",
                                             "Expected an \"=>\"".into(),
                                             "Expected more characters, specifically an \"=>\"".into())
                        .map(|_| vec![((), 2)]).ok()
                },
                |x, _| x
            )
            .parse_any_whitespace(&content, false, ctx)
            .chain( // parse an expression
                &content,
                false,
                context.increment_depth(),
                chainable(|_p, next, meta| {
                    self.expr_parser.parse(&next, false, meta)
                }),
                |(ident, args), expr| (ident, args, expr)
            );

        let with_where = root.clone()
            .parse_any_whitespace(&content, false, ctx)
            .parse_static_text(&content, false, ctx, "where")
            .parse_any_whitespace(&content, false, ctx)
            .parse_static_text(&content, false, ctx, "{");

        if with_where.0.is_ok() {

            let where_statements = with_where.clone()
                .parse_any_whitespace(&content, false, ctx);
            // meh, finish later
            // test first!
        }

        // temporary solution
        root.0.map(|hs| hs.into_iter().filter_map(
            |((ident, args, expr), used)| {
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
                        if !consume || used == content.len() {
                            Some((
                                Statement::FnDef(
                                    name,
                                    args,
                                    expr,
                                    Box::new(vec![])
                                ),
                                used
                            ))
                        } else {
                            None // didn't consume everything
                        }
                    } else {
                        None // args weren't a list
                    }
                } else {
                    None // wasn't a unit
                }
            }
        ).collect())

    }
}