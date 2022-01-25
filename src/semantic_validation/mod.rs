use std::collections::{HashMap, HashSet};
use crate::lang_obj::{Identifier, Program};
use crate::{Expr, Statement};

pub trait Validator<T> {
    fn validate(&self, structure: &T) -> Result<(), String>;
}

#[derive(Clone)]
pub struct ScopeFrame {

    /// Globals are variables that are always present, but they can still
    /// be shadowed.
    pub globals: HashSet<Identifier>,

    /// Map containing all the local variables, and how many there are.
    /// For example, if a shadow variable is used in a new scope, then the
    /// number is incremented.
    pub locals: HashMap<Identifier, usize>
}

impl ScopeFrame {

    pub fn exists(&self, ident: &Identifier) -> bool {
        self.globals.contains(&ident) || self.locals.contains_key(ident)
    }
    pub fn new_with(&self, new_local_identifiers: HashSet<Identifier>) -> ScopeFrame {
        ScopeFrame {
            globals: self.globals.clone(),
            locals: new_local_identifiers.into_iter().map(|i| (i, 1)).collect()
        }
    }
    pub fn transfer_with(&self, new_local_identifiers: HashSet<Identifier>) -> ScopeFrame {
        let mut n = self.clone();
        for i in new_local_identifiers {
            n.add(i);
        }
        n
    }
    pub fn add_global(&mut self, ident: Identifier) -> Result<(), String> {
        if self.globals.contains(&ident) {
            Err(format!("This frame already contains a global called '{}'!", ident.to_string()))
        } else {
            self.globals.insert(ident);
            Ok(())
        }
    }
    pub fn add(&mut self, ident: Identifier) {
        if let Some(e) = self.locals.get_mut(&ident) {
            *e += 1;
        } else {
            self.locals.insert(ident, 1);
        }
    }
    pub fn remove(&mut self, ident: Identifier) {
        todo!("remove an identifier")
    }
}

pub struct ProgramValidator;

/// evaluates whether or not
/// a statement (function definition or let statement)
/// is valid.
pub struct StatementValidator(pub ScopeFrame);

/// Evaluates whether or not an expression is valid.
pub struct ExpressionValidator(pub ScopeFrame);

impl Validator<Program> for ProgramValidator {
    fn validate(&self, structure: &Program) -> Result<(), String> {
        let mut scope_frame = ScopeFrame {
            globals: hashset![],
            locals: hashmap![]
        };
        for s in structure.content.iter() {
            // this is a special case, mostly we just want to validate
            if let Statement::Let(ident, _expr) = s {
                scope_frame.add_global(ident.clone()).unwrap();
            }
            StatementValidator(scope_frame.new_with(hashset![])).validate(s)?;
        }
        Ok(())
    }
}

impl Validator<Statement> for StatementValidator {
    fn validate(&self, structure: &Statement) -> Result<(), String> {
        if let Statement::Let(ident, expr) = structure {
            let expr_validator = ExpressionValidator(self.0.new_with(hashset![ident.clone()]));
            expr_validator.validate(expr)
        } else if let Statement::FnDef(name, args, expr, wheres) = structure {
            let mut new_f_idents: HashSet<Identifier> = args.iter().map(|(ident, _type)| ident.clone()).collect();
            new_f_idents.extend(vec![Identifier::Unit(name.clone())]); // add the function name (for recursion)

            // start the new scope with function name and argument identifiers
            let mut func_scope = self.0.new_with(new_f_idents);

            // we gonna need the where-idents
            wheres.iter()
                .map(|stmt| {
                    match stmt {
                        Statement::Let(ident, _) => ident.clone(),
                        Statement::FnDef(name, _, _, _) => Identifier::Unit(name.clone())
                    }
                })
                .for_each(|ident| {
                    func_scope.add(ident);
                });

            // first, let the statements be evaluated.
            let stmt_validator = StatementValidator(func_scope.clone());
            for stmt in wheres.iter() {
                stmt_validator.validate(stmt)?;
            }

            // now validate the expression
            let expr_validator = ExpressionValidator(func_scope.clone());
            expr_validator.validate(expr)

        } else {
            Ok(()) // ???
        }
    }
}

impl Validator<Expr> for ExpressionValidator {
    fn validate(&self, structure: &Expr) -> Result<(), String> {
        match structure {
            Expr::Nat(_) => Ok(()),
            Expr::Str(_) => Ok(()),
            Expr::Infix(left, _, right) => {
                self.validate(left)?;
                self.validate(right)
            },
            Expr::List(exprs) => {
                for e in exprs.iter() {
                    self.validate(e)?;
                }
                Ok(())
            },
            Expr::Func(_, _) => todo!(),
            Expr::Variable(ident) => {
                // finally!
                if self.0.exists(ident) {
                    Ok(())
                } else {
                    Err(format!("'{}' does not exist!", ident.to_string()))
                }
            }
            Expr::Conditional(cond, then, then_else) => {
                self.validate(cond)?;
                self.validate(then)?;
                self.validate(then_else)
            }
        }
    }
}