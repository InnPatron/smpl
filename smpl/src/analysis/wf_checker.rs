use log::{error, trace};

use crate::ast::Module;
use crate::span::Span;
use crate::Source;

use super::error::{WfError, WfErrorKind};
use super::StaticModData;

macro_rules! log_info {
    ($($arg:tt)+) => (info!(target: "module_wf", $($arg)+ ))
}

macro_rules! log_trace {
    ($($arg:tt)+) => (trace!(target: "module_wf", $($arg)+ ))
}

macro_rules! log_error {
    ($($arg:tt)+) => (error!(target: "module_wf", $($arg)+ ))
}

macro_rules! log_debug {
    ($($arg:tt)+) => (debug!(target: "module_wf", $($arg)+ ))
}

pub type WfResult<T> = Result<T, WfError>;

///
/// Syntactic well-formedness check on the AST
///     1) A module declares a name
///     2) '_' in expression positions are the immediate children of function calls
///         TODO: also allow in certain operator forms?
///     3) 'while' and 'if' expressions have a default branch
///     4) Checks that literals have registered type
///         NOTE: does NOT check if that type exists or results in a well-typed program
///
pub fn module_wf_check(sdm: &StaticModData, module: &Module) -> WfResult<()> {
    use crate::ast_visitor;

    log_trace!("Module declaration check for '{}'", sdm.source);
    module_decl_check(&sdm.source, module)?;
    log_trace!("Found a module declaration for '{}'", sdm.source);

    log_trace!("Expr WF check for '{}'", sdm.source);
    {
        let mut visitor = self::module_wf::ModuleWfVisitor {
            sdm,
            tracker_stack: vec![],
        };

        ast_visitor::walk_module_for_expr(&mut visitor, module)?;
    }
    log_trace!("All exprs well-formed in '{}'", sdm.source);

    Ok(())
}

fn module_decl_check(source: &Source, module: &Module) -> WfResult<()> {
    if module.mod_decl.is_none() {
        log_error!(
            "Module from source '{}' does not have a module declaration",
            source
        );
        return wf_error!(WfErrorKind::MissingModuleDecl, Span::dummy());
    }

    Ok(())
}

mod module_wf {
    use log::{debug, error, info};

    use crate::ast_node::{AstNode, Spanned};
    use crate::ast_visitor::*;
    use crate::expr_ast::*;

    use super::{StaticModData, WfError, WfErrorKind, WfResult};

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub(super) enum ExprTracker {
        FnCall,
        Other,
    }

    pub(super) struct ModuleWfVisitor<'a> {
        pub(super) sdm: &'a StaticModData<'a>,
        pub(super) tracker_stack: Vec<ExprTracker>,
    }

    impl<'a> Visitor for ModuleWfVisitor<'a> {
        type E = WfError;

        fn visit_lit_expr(
            &mut self,
            node_lit: &AstNode<Literal>,
        ) -> WfResult<()> {
            let lit = node_lit.data();
            let lit_span = node_lit.span();

            let typed_path = match lit {
                Literal::String(ref literal_data) => {
                    self.sdm.lit_sfx_map.get_typed_path(&literal_data.suffix)
                }

                Literal::Int(ref literal_data) => {
                    self.sdm.lit_sfx_map.get_typed_path(&literal_data.suffix)
                }

                Literal::Float(ref literal_data) => {
                    self.sdm.lit_sfx_map.get_typed_path(&literal_data.suffix)
                }

                Literal::Bool(_) => return Ok(()),
            };

            if typed_path.is_some() {
                log_info!(
                    "Literal at '{}' from '{}' had a mapped type",
                    lit_span,
                    &self.sdm.source
                );
                Ok(())
            } else {
                log_error!(
                    "Literal at '{}' from '{}' does not have a mapped type",
                    lit_span,
                    &self.sdm.source
                );
                wf_error!(WfErrorKind::UnknownLiteralKind, lit_span)
            }
        }

        fn visit_if_expr(
            &mut self,
            node_if_expr: &AstNode<If>,
        ) -> WfResult<()> {
            let if_expr = node_if_expr.data();
            let if_span = node_if_expr.span();

            if if_expr.default_branch.is_none() {
                log_error!(
                    "if-expr at {} does not have a default branch",
                    if_span
                );
                return wf_error!(WfErrorKind::IfExprNotTotal, if_span);
            }

            walk_if_expr(self, node_if_expr)
        }

        fn visit_while_expr(
            &mut self,
            node_while_expr: &AstNode<While>,
        ) -> WfResult<()> {
            let while_expr = node_while_expr.data();
            let while_span = node_while_expr.span();

            while while_expr.default_branch.is_none() {
                log_error!(
                    "while-expr at {} does not have a default branch",
                    while_span
                );
                return wf_error!(WfErrorKind::WhileExprNotTotal, while_span);
            }

            walk_while_expr(self, node_while_expr)
        }

        fn visit_expr(&mut self, e: &Expr) -> WfResult<()> {
            match e {
                Expr::FnCall(..) => {
                    self.tracker_stack.push(ExprTracker::FnCall)
                }

                _ => self.tracker_stack.push(ExprTracker::Other),
            }

            let result = walk_expr(self, e);

            self.tracker_stack.pop();

            result
        }

        fn visit_underscore_expr(&mut self, u: &AstNode<()>) -> WfResult<()> {
            match self.tracker_stack.last() {
                Some(ref last) => match last {
                    ExprTracker::FnCall => Ok(()),

                    ExprTracker::Other => {
                        log_error!("WF: underscore expr in bad position (non-empty tracker stack)");
                        log_debug!("{:?}", self.tracker_stack);
                        wf_error!(WfErrorKind::BadUnderscore, u.span())
                    }
                },

                None => {
                    log_error!("WF: underscore expr in bad position (empty tracker stack)");
                    wf_error!(WfErrorKind::BadUnderscore, u.span())
                }
            }
        }
    }
}
