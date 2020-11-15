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
///     4) Checks that literals have registered formats
///
pub fn module_wf_check(source: &Source, module: &Module) -> WfResult<()> {
    use crate::span::Span;

    use super::error::WfErrorKind;

    log_trace!("Module WF check for {}", source);

    if module.mod_decl.is_none() {
        log_error!(
            "Module from source '{}' does not have a module declaration",
            source
        );
        return Err(wf_error!(WfErrorKind::MissingModuleDecl, Span::dummy()));
    }

    todo!();
}

mod module_wf {
    use log::{error, trace};

    use crate::ast::*;
    use crate::ast_node::AstNode;
    use crate::ast_visitor::*;
    use crate::expr_ast::*;
    use crate::span::Span;

    use super::super::error::WfErrorKind;
    use super::{Source, WfError, WfResult};

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub(super) enum FnCallTracker {
        FnCall,
        Other,
    }

    pub(super) struct ModuleWfVisitor<'a> {
        pub(super) source: &'a Source,
        pub(super) tracker_stack: Vec<FnCallTracker>,
    }

    impl<'a> Visitor for ModuleWfVisitor<'a> {
        type E = WfError;

        fn visit_underscore_expr(&mut self, _: &AstNode<()>) -> WfResult<()> {
            match self.tracker_stack.last() {
                Some(ref last) => todo!(),

                None => todo!(),
            }
        }
    }
}
