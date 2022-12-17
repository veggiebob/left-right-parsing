use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;
use crate::*;

/// Do not change this!
/// (Or if you do, beware of a slew of tests that may fail)
/// Creates a sample language then uses a callback to provide the needed resources.
/// The only reason there isn't a function that returns the parsers is because
/// I couldn't figure out how to get the weak references to stick around outside
/// the scope of the function.
pub fn language_1<F>(mut f: F)
    where
        F: FnMut(ProgramParser, Rc<StatementParser>, Rc<ExprParser>),
    {
    // introduce the various configs for this parser:
    const INFIX_ADDITION_SYMBOL: &str = "+"; // for expressions, like "+" in "3 + 4"
    const INFIX_MULTIPLICATION_SYMBOL: &str = "*"; // for expressions like "*" in "3 * 4"
    const INFIX_SUBTRACTION_SYMBOL: &str = "-"; // for expressions like "-" in "4 - 3"
    const INFIX_FUNCTION_SYMBOL: &str = " "; // for expressions like "$" in "func $ [arg]"
    const INFIX_EQUALITY_SYMBOL: &str = "="; // for checking equality; for expressions like "=" in "x = y"
    const LIST_SEPARATOR_SYMBOL: char = ','; // for list expressions, like ',' in "[1, 2, 3]"
    const IDENTIFIER_ALLOWED_CHARS: &str = "_"; // also, by default, includes a-zA-Z
    const FUNCTION_ARGUMENT_SEPARATOR: char = ','; // seems simple
    const FUNCTION_ARGUMENT_INFIX_SYMBOL: &str = ":"; // like ":" in "func_name[arg1: type1, arg2: type2] => ..."

    ////////////////////////////////////////////////////
    // first, assemble only the finest expression parser
    ////////////////////////////////////////////////////

    // primitive parsers
    let string_parser = Rc::new(box_expr_parser!(LOStringParser()));
    let nat_parser = Rc::new(box_expr_parser!(LONatParser()));
    let var_parser = Rc::new(box_expr_parser!(VariableParser::default(IdentifierParser::new(IDENTIFIER_ALLOWED_CHARS))));

    // start off with a couple simple parsers
    let root_parsers = RefCell::new(vec![
        &string_parser,
        &nat_parser,
        &var_parser
    ].into_iter().map(Rc::downgrade).collect());

    // create the expression parser
    let expr_parser = Rc::new(ExprParser {
        parsers: root_parsers
    });

    // create a couple recursive parsers
    let parenthetical_parser = Rc::new(box_expr_parser!(ParentheticalParser {
            expr_parser: Rc::clone(&expr_parser)
        }));
    let list_parser = Rc::new(box_expr_parser!(ListParser {
            separator: LIST_SEPARATOR_SYMBOL,
            expr_parser: Rc::clone(&expr_parser)
        }));
    let infix_addition = Rc::new(box_expr_parser!(InfixParser {
            expr_parser: Rc::clone(&expr_parser),
            infix: String::from(INFIX_ADDITION_SYMBOL),
        }));
    let infix_multiplication = Rc::new(box_expr_parser!(InfixParser {
            expr_parser: Rc::clone(&expr_parser),
            infix: String::from(INFIX_MULTIPLICATION_SYMBOL)
        }));
    let infix_invocation = Rc::new(box_expr_parser!(InfixParser {
            expr_parser: Rc::clone(&expr_parser),
            infix: String::from(INFIX_FUNCTION_SYMBOL)
        }));
    let infix_subtraction = Rc::new(box_expr_parser!(InfixParser {
            expr_parser: Rc::clone(&expr_parser),
            infix: String::from(INFIX_SUBTRACTION_SYMBOL)
        }));
    let infix_equality = Rc::new(box_expr_parser!(InfixParser {
            expr_parser: Rc::clone(&expr_parser),
            infix: String::from(INFIX_EQUALITY_SYMBOL)
        }));
    let condition_parser = Rc::new(box_expr_parser!(ConditionalParser {
            expr_parser: Rc::clone(&expr_parser)
        }));
    // add the recursive parsers
    expr_parser.parsers.borrow_mut().extend(vec![
        // single-instance
        &parenthetical_parser,
        &condition_parser,

        // configured
        &infix_addition,
        &infix_multiplication,
        &infix_subtraction,
        &infix_invocation,
        &infix_equality,
        &list_parser,
    ].into_iter().map(Rc::downgrade).collect::<Vec<_>>());

    /////////////////////////////////////////////////////
    // now, we append the statement parsers
    /////////////////////////////////////////////////////

    // create the shallow arg parser for function signatures
    let arg_name_parser = Rc::new(box_expr_parser!(VariableParser::default(IdentifierParser::new(IDENTIFIER_ALLOWED_CHARS))));
    let arg_type_parser = Rc::new(box_expr_parser!(VariableParser::default(IdentifierParser::new(IDENTIFIER_ALLOWED_CHARS))));
    let arg_name_type_parser = Rc::new(ExprParser {
        parsers: RefCell::new(vec![
            Rc::downgrade(&arg_name_parser),
            Rc::downgrade(&arg_type_parser)
        ])
    });
    let arg_infix = Rc::new(box_expr_parser!(InfixParser {
            expr_parser: Rc::clone(&arg_name_type_parser),
            infix: String::from(FUNCTION_ARGUMENT_INFIX_SYMBOL)
        }));
    let arg_parser = Rc::new(ExprParser {
        parsers: RefCell::new(vec![
            Rc::downgrade(&arg_infix)
        ])
    });
    let arg_parser = Rc::new(ListParser {
        expr_parser: arg_parser,
        separator: FUNCTION_ARGUMENT_SEPARATOR
    });

    let stmt_parser = Rc::new(StatementParser {
        parsers: RefCell::new(vec![])
    });

    // create function definition parser
    let fn_parser = Rc::new(box_stmt_parser!(FnDefParser {
            id_parser: Rc::new(IdentifierParser::new(IDENTIFIER_ALLOWED_CHARS)),
            statement_parser: Rc::clone(&stmt_parser),
            type_parser: Rc::new(TypeParser(IdentifierParser::new(IDENTIFIER_ALLOWED_CHARS))),
            expr_parser: Rc::clone(&expr_parser),
            arg_parser
        }));

    // create let statement parser
    let let_parser = Rc::new(box_stmt_parser!(LetParser {
            id_parser: Rc::new(IdentifierParser::new(IDENTIFIER_ALLOWED_CHARS)),
            expr_parser: Rc::clone(&expr_parser)
        }));

    stmt_parser.parsers.borrow_mut().extend(vec![
        Rc::downgrade(&fn_parser),
        Rc::downgrade(&let_parser)
    ]);
    let stmt_rc = Rc::clone(&stmt_parser);
    let prgm_parser = ProgramParser {
        stmt_parser: Arc::new(stmt_rc)
    };

    f(prgm_parser, stmt_parser, expr_parser);
}