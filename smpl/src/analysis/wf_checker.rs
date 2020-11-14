use log::{error, trace};

use crate::ast::Module;
use crate::Source;

use super::error::WfError;

macro_rules! log_trace {
    ($($arg:tt)+) => (trace!(target: "module_wf", $($arg)+ ))
}

macro_rules! log_error {
    ($($arg:tt)+) => (error!(target: "module_wf", $($arg)+ ))
}

pub type WfResult<T> = Result<T, WfError>;

///
/// Syntactic well-formedness check on the AST
///     1) A module declares a name
///     2) '_' in expression positions are the immediate children of function calls
///     3) 'while' and 'if' expressions have a default branch
///
pub fn module_wf_check(source: &Source, module: &Module) -> WfResult<()> {
    log_trace!("Module WF check for {}", source);
    todo!();
}

mod module_wf {
    use log::{error, trace};

    use crate::ast::*;
    use crate::ast_node::AstNode;
    use crate::expr_ast::*;
    use crate::span::Span;

    use super::super::error::WfErrorKind;
    use super::{Source, WfError, WfResult};

    pub struct ModuleWfVisitor<'a> {
        pub source: &'a Source,
    }

    impl<'a> ModuleWfVisitor<'a> {
        pub fn visit_module(&self, module: &Module) -> WfResult<()> {
            if module.mod_decl.is_none() {
                log_error!("Module from source '{}' does not have a module declaration", self.source);
                return Err(wf_error!(
                    WfErrorKind::MissingModuleDecl,
                    Span::dummy()
                ));
            }

            for decl in module.decls.iter() {
                match decl {
                    Decl::Fn(ref node_fn_decl) => {
                        self.visit_fn(node_fn_decl)?
                    }

                    _ => continue,
                }
            }
            Ok(())
        }

        fn visit_fn(&self, func: &AstNode<FnDecl>) -> WfResult<()> {
            let fn_decl = func.data();
            log_trace!("WF checking function: {}", fn_decl.name.data());

            self.visit_block(&fn_decl.body)?;
            Ok(())
        }

        fn visit_expr(&self, expr: &Expr) -> WfResult<()> {
            todo!();
        }

        fn visit_block(&self, node_block: &AstNode<Block>) -> WfResult<()> {
            for s in node_block.data().stmts.iter() {
                match s {
                    Stmt::ExprStmt(ref expr) => self.visit_expr(expr)?,

                    Stmt::Block(ref node_block) => {
                        self.visit_block(node_block)?
                    }

                    Stmt::If(ref node_if) => self.visit_if(node_if)?,

                    Stmt::While(ref node_while) => {
                        self.visit_while(node_while)?
                    }

                    Stmt::Let(ref node_let) => self.visit_let(node_let)?,

                    Stmt::Return(ref node_return) => {
                        self.visit_return(node_return)?
                    }

                    Stmt::Break(ref node_break) => {
                        self.visit_break(node_break)?
                    }

                    _ => todo!(),
                }
            }

            Ok(())
        }

        fn visit_if(&self, node_if: &AstNode<If>) -> WfResult<()> {
            todo!();
        }

        fn visit_while(&self, node_if: &AstNode<While>) -> WfResult<()> {
            todo!();
        }

        fn visit_let(&self, node_let: &AstNode<LetStmt>) -> WfResult<()> {
            todo!();
        }
    }
}
