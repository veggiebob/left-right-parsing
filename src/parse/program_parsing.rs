use std::collections::HashSet;
use std::rc::Rc;
use std::sync::Arc;
use crate::{chainable, ParseMetaData, Parser, ParseResult, StatementParser};
use crate::lang_obj::{ParseError, Program};

pub struct ProgramParser {
    pub stmt_parser: Arc<Rc<StatementParser>>,
}

impl Parser for ProgramParser {
    type Output = Program;

    // this does not call itself recursively ever, thank goodness
    // and also we don't really ever expect to *not* consume everything
    fn parse(&self, content: &String, consume: bool, context: ParseMetaData) -> Result<HashSet<(Self::Output, usize)>, ParseError> {
        let mut finalized = HashSet::new();
        let mut base = ParseResult(
            self.stmt_parser.parse(content, false, context)
        ).map_inner(|v| vec![v]).parse_any_whitespace(content, false, context);

        if base.len() == 0 {
            return base.map_inner(|v|
                Program {
                    content: v
                }
            ).0;
        } else {
            if let Ok(hs) = &base.0 {
                for (vec, used) in hs {
                    finalized.insert((vec.clone(), *used));
                }
            }
        }

        loop {
            let next = base.clone().chain(
                content,
                false,
                context,
                chainable(|_p, next, meta| {
                    self.stmt_parser.parse(&next, false, meta)
                }),
                |last, current| {
                    let mut stmts = last.clone();
                    stmts.extend(vec![current]);
                    stmts
                }
            ).parse_any_whitespace(content, false, context);
            if next.len() > 0 {
                if let Ok(hs) = &next.0 {
                    for (vec, used) in hs {
                        finalized.insert((vec.clone(), *used));
                    }
                }
            }
            base = next;
            // if no further possibilities are generated, then
            if base.len() == 0 {
                break;
            }
        }

        if finalized.len() > 0 {
            ParseResult(Ok(finalized)).map_inner(
                |v|
                    Program {
                        content: v
                    }
            ).parse_any_whitespace(content, consume, context).0
        } else {
            Err("Program parser: There were no possibilities!".into())
        }
    }
}