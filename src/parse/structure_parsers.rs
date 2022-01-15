use std::collections::HashSet;
use std::rc::Rc;
use crate::funcs::{expect_str, expect_with_used, take, take_while};
use crate::lang_obj::{Expr, Identifier, ParseError, Statement};
use crate::parse::{chainable, ExprParser, LengthQualifier, ParseMetaData, Parser, ParseResult, TakeWhileParser};

pub struct IdentifierParser {
    /// a-z and A-Z are already allowed. You choose what else can be added
    allowed_characters: HashSet<char>
}

pub struct LetParser {
    pub id_parser: Rc<IdentifierParser>,
    pub expr_parser: Rc<ExprParser>
}

pub struct FnDefParser {
    // todo
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

impl Parser for LetParser {
    type Output = Statement;

    fn parse(&self, content: &String, consume: bool, context: ParseMetaData) -> Result<HashSet<(Self::Output, usize)>, ParseError> {
        let one_or_more_space = TakeWhileParser::whitespace(LengthQualifier::GEQ(1));
        let maybe_space = TakeWhileParser::whitespace(LengthQualifier::GEQ(0));
        let result = expect_with_used(content, "let", "Expected 'let'".into());
        let result = ParseResult(result) // wrap with the result to do some chaining!
            .chain( // parse some more space
                    &content,
                    false,
                    context,
                    chainable(|_let, next, meta| {
                        one_or_more_space.parse(&next, false, context)
                    }),
                    |_let, _rest| ()
            )
            .chain( // parse an identifier
                &content,
                false,
                context.increment_depth(),
                chainable(|_space, next, meta| {
                    self.id_parser.parse(&next, false, context)
                }),
                |_space, ident| ident
            )
            .chain( // parse some more space
                &content,
                false,
                context,
                chainable(|ident, next, meta| {
                    maybe_space.parse(&next, false, context)
                }),
                |ident, x| ident
            )
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
            .chain( // parse some more space
                &content,
                false,
                context,
                chainable(|ident, next, meta| {
                    maybe_space.parse(&next, false, context)
                }),
                |ident, x| ident
            )
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