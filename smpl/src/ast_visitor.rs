use log::trace;

use super::ast::*;
use super::ast_node::{AstNode, Spanned};
use super::expr_ast::*;

macro_rules! log_trace {
        ($($arg:tt)+) => (trace!(target: "visitor", $($arg)+ ))
}

type VisitorResult<E> = Result<(), E>;

pub trait Visitor {
    type E;

    fn visit_stmt(&mut self, s: &Stmt) -> VisitorResult<Self::E> {
        walk_stmt(self, s)
    }

    fn visit_if_stmt(&mut self, n: &AstNode<If>) -> VisitorResult<Self::E> {
        walk_if_stmt(self, n)
    }

    fn visit_while_stmt(
        &mut self,
        n: &AstNode<While>,
    ) -> VisitorResult<Self::E> {
        walk_while_stmt(self, n)
    }

    fn visit_let_stmt(
        &mut self,
        n: &AstNode<LetStmt>,
    ) -> VisitorResult<Self::E> {
        walk_let_stmt(self, n)
    }

    fn visit_return_stmt(
        &mut self,
        n: &AstNode<ReturnStmt>,
    ) -> VisitorResult<Self::E> {
        walk_return_stmt(self, n)
    }

    fn visit_break_stmt(
        &mut self,
        n: &AstNode<BreakStmt>,
    ) -> VisitorResult<Self::E> {
        walk_break_stmt(self, n)
    }

    fn visit_extract_stmt(
        &mut self,
        n: &AstNode<ExtractStmt>,
    ) -> VisitorResult<Self::E> {
        walk_extract_stmt(self, n)
    }

    fn visit_continue_stmt(
        &mut self,
        n: &AstNode<ContinueStmt>,
    ) -> VisitorResult<Self::E> {
        walk_continue_stmt(self, n)
    }

    fn visit_block_stmt(
        &mut self,
        n: &AstNode<Block>,
    ) -> VisitorResult<Self::E> {
        walk_block_stmt(self, n)
    }

    fn visit_expr_stmt(&mut self, n: &Expr) -> VisitorResult<Self::E> {
        walk_expr_stmt(self, n)
    }

    fn visit_expr(&mut self, e: &Expr) -> VisitorResult<Self::E> {
        walk_expr(self, e)
    }

    fn visit_if_expr(
        &mut self,
        node_if_expr: &AstNode<If>,
    ) -> VisitorResult<Self::E> {
        walk_if_expr(self, node_if_expr)
    }

    fn visit_while_expr(
        &mut self,
        node_while_expr: &AstNode<While>,
    ) -> VisitorResult<Self::E> {
        walk_while_expr(self, node_while_expr)
    }

    fn visit_bin_expr(
        &mut self,
        node_bin_expr: &AstNode<BinExpr>,
    ) -> VisitorResult<Self::E> {
        walk_bin_expr(self, node_bin_expr)
    }

    fn visit_uni_expr(
        &mut self,
        node_uni_expr: &AstNode<UniExpr>,
    ) -> VisitorResult<Self::E> {
        walk_uni_expr(self, node_uni_expr)
    }

    fn visit_lit_expr(
        &mut self,
        _: &AstNode<Literal>,
    ) -> VisitorResult<Self::E> {
        Ok(())
    }

    fn visit_dot_expr(
        &mut self,
        node_dot_access: &AstNode<DotAccess>,
    ) -> VisitorResult<Self::E> {
        walk_dot_access_expr(self, node_dot_access)
    }

    fn visit_fn_call_expr(
        &mut self,
        node_fn_call: &AstNode<FnCall>,
    ) -> VisitorResult<Self::E> {
        walk_fn_call_expr(self, node_fn_call)
    }

    fn visit_struct_init_expr(
        &mut self,
        node_struct_init: &AstNode<StructInit>,
    ) -> VisitorResult<Self::E> {
        walk_struct_init_expr(self, node_struct_init)
    }

    fn visit_lam_expr(
        &mut self,
        node_lam: &AstNode<Lambda>,
    ) -> VisitorResult<Self::E> {
        walk_lam_expr(self, node_lam)
    }

    fn visit_path_expr(
        &mut self,
        _: &AstNode<TypedPath>,
    ) -> VisitorResult<Self::E> {
        Ok(())
    }

    fn visit_block_expr(
        &mut self,
        node_block: &AstNode<Block>,
    ) -> VisitorResult<Self::E> {
        walk_block_expr(self, node_block)
    }

    fn visit_underscore_expr(
        &mut self,
        _: &AstNode<()>,
    ) -> VisitorResult<Self::E> {
        Ok(())
    }
}

pub fn walk_module_for_expr<V: Visitor>(
    v: &mut V,
    m: &Module,
) -> VisitorResult<V::E> {
    for decl in m.decls.iter() {
        match decl {
            Decl::Fn(ref node_fn_decl) => {
                let fn_decl = node_fn_decl.data();
                log_trace!(
                    "Visitor visiting function: {}",
                    fn_decl.name.data()
                );

                walk_block_stmt(v, &fn_decl.body)?;
            }

            _ => continue,
        }
    }

    Ok(())
}

pub fn walk_block_stmt<V: Visitor + ?Sized>(
    v: &mut V,
    b: &AstNode<Block>,
) -> VisitorResult<V::E> {
    for s in b.data().stmts.iter() {
        v.visit_stmt(s)?;
    }

    Ok(())
}

pub fn walk_if_stmt<V: Visitor + ?Sized>(
    v: &mut V,
    node_if: &AstNode<If>,
) -> VisitorResult<V::E> {
    let if_stmt = node_if.data();

    for branch in if_stmt.branches.iter() {
        v.visit_expr(&branch.condition)?;
        walk_block_stmt(v, &branch.block)?;
    }

    Ok(())
}

pub fn walk_while_stmt<V: Visitor + ?Sized>(
    v: &mut V,
    node_while: &AstNode<While>,
) -> VisitorResult<V::E> {
    let while_stmt = node_while.data();

    v.visit_expr(&while_stmt.condition)?;
    walk_block_stmt(v, &while_stmt.body)?;

    for branch in while_stmt.branches.iter() {
        v.visit_expr(&branch.condition)?;
        walk_block_stmt(v, &branch.block)?;
    }

    Ok(())
}

pub fn walk_let_stmt<V: Visitor + ?Sized>(
    v: &mut V,
    node_let: &AstNode<LetStmt>,
) -> VisitorResult<V::E> {
    let let_stmt = node_let.data();
    v.visit_expr(&let_stmt.init)
}

pub fn walk_return_stmt<V: Visitor + ?Sized>(
    v: &mut V,
    node_return: &AstNode<ReturnStmt>,
) -> VisitorResult<V::E> {
    let return_stmt = node_return.data();
    let return_span = node_return.span();

    match return_stmt.expr {
        Some(ref e) => {
            log_trace!("Return stmt at '{}' has an expr", return_span);
            v.visit_expr(e)
        }

        None => {
            log_trace!(
                "Return stmt at '{}' does NOT have an expr",
                return_span
            );
            Ok(())
        }
    }
}

pub fn walk_break_stmt<V: Visitor + ?Sized>(
    v: &mut V,
    node_break: &AstNode<BreakStmt>,
) -> VisitorResult<V::E> {
    let break_stmt = node_break.data();
    let break_span = node_break.span();

    match break_stmt.expr {
        Some(ref e) => {
            log_trace!("Break stmt at '{}' has an expr", break_span);
            v.visit_expr(e)
        }

        None => {
            log_trace!("Break stmt at '{}' does NOT have an expr", break_span);
            Ok(())
        }
    }
}

pub fn walk_extract_stmt<V: Visitor + ?Sized>(
    v: &mut V,
    node_extract: &AstNode<ExtractStmt>,
) -> VisitorResult<V::E> {
    let extract_stmt = node_extract.data();
    let extract_span = node_extract.span();

    match extract_stmt.expr {
        Some(ref e) => {
            log_trace!("Extract stmt at '{}' has an expr", extract_span);
            v.visit_expr(e)
        }

        None => {
            log_trace!(
                "Extract stmt at '{}' does NOT have an expr",
                extract_span
            );
            Ok(())
        }
    }
}

pub fn walk_continue_stmt<V: Visitor + ?Sized>(
    v: &mut V,
    node_continue: &AstNode<ContinueStmt>,
) -> VisitorResult<V::E> {
    Ok(())
}

pub fn walk_expr_stmt<V: Visitor + ?Sized>(
    v: &mut V,
    expr: &Expr,
) -> VisitorResult<V::E> {
    log_trace!("Visiting expr-stmt at '{}'", expr.span());
    v.visit_expr(expr)
}

pub fn walk_stmt<V: Visitor + ?Sized>(
    v: &mut V,
    s: &Stmt,
) -> VisitorResult<V::E> {
    match s {
        Stmt::If(ref node_if) => v.visit_if_stmt(node_if),

        Stmt::While(ref node_while) => v.visit_while_stmt(node_while),

        Stmt::Let(ref node_let) => v.visit_let_stmt(node_let),

        Stmt::Return(ref node_return) => v.visit_return_stmt(node_return),

        Stmt::Break(ref node_break) => v.visit_break_stmt(node_break),

        Stmt::Extract(ref node_extract) => v.visit_extract_stmt(node_extract),

        Stmt::Continue(ref node_continue) => {
            v.visit_continue_stmt(node_continue)
        }

        Stmt::Block(ref node_block) => walk_block_stmt(v, node_block),

        Stmt::ExprStmt(ref expr) => v.visit_expr_stmt(expr),
    }
}

pub fn walk_expr<V: Visitor + ?Sized>(
    v: &mut V,
    e: &Expr,
) -> VisitorResult<V::E> {
    match e {
        Expr::If(ref node_if_expr) => v.visit_if_expr(node_if_expr),

        Expr::While(ref node_while_expr) => v.visit_while_expr(node_while_expr),

        Expr::Bin(ref node_bin_expr) => v.visit_bin_expr(node_bin_expr),

        Expr::Uni(ref node_uni_expr) => v.visit_uni_expr(node_uni_expr),

        Expr::Literal(ref node_lit_expr) => v.visit_lit_expr(node_lit_expr),

        Expr::DotAccess(ref node_dot_expr) => v.visit_dot_expr(node_dot_expr),

        Expr::FnCall(ref node_fn_call) => v.visit_fn_call_expr(node_fn_call),

        Expr::StructInit(ref node_struct_init) => {
            v.visit_struct_init_expr(node_struct_init)
        }

        Expr::Lambda(ref node_lambda) => v.visit_lam_expr(node_lambda),

        Expr::Path(ref node_path) => v.visit_path_expr(node_path),

        Expr::Block(ref node_block) => v.visit_block_expr(node_block),

        Expr::Underscore(ref node_underscore) => {
            v.visit_underscore_expr(node_underscore)
        }
    }
}

pub fn walk_if_expr<V: Visitor + ?Sized>(
    v: &mut V,
    node_if_expr: &AstNode<If>,
) -> VisitorResult<V::E> {
    let if_expr = node_if_expr.data();
    let if_span = node_if_expr.span();

    for branch in if_expr.branches.iter() {
        v.visit_expr(&branch.condition)?;
        walk_block_stmt(v, &branch.block)?;
    }

    match if_expr.default_branch {
        Some(ref block) => {
            log_trace!("If expr at '{}' has a default branch", if_span);
            walk_block_stmt(v, block)
        }

        None => {
            log_trace!(
                "If expr at '{}' does not have a default branch",
                if_span
            );
            Ok(())
        }
    }
}

pub fn walk_while_expr<V: Visitor + ?Sized>(
    v: &mut V,
    node_while_expr: &AstNode<While>,
) -> VisitorResult<V::E> {
    let while_expr = node_while_expr.data();
    let while_span = node_while_expr.span();

    v.visit_expr(&while_expr.condition)?;
    walk_block_stmt(v, &while_expr.body)?;

    for branch in while_expr.branches.iter() {
        v.visit_expr(&branch.condition)?;
        walk_block_stmt(v, &branch.block)?;
    }

    match while_expr.default_branch {
        Some(ref block) => {
            log_trace!("While expr at '{}' has a default branch", while_span);
            walk_block_stmt(v, block)
        }

        None => {
            log_trace!(
                "While expr at '{}' does not have a default branch",
                while_span
            );
            Ok(())
        }
    }
}

pub fn walk_bin_expr<V: Visitor + ?Sized>(
    v: &mut V,
    node_bin_expr: &AstNode<BinExpr>,
) -> VisitorResult<V::E> {
    let bin_expr = node_bin_expr.data();
    let bin_span = node_bin_expr.span();

    log_trace!(
        "Visiting left of bin expr (op: '{}') at '{}'",
        bin_expr.op.data(),
        bin_span
    );
    v.visit_expr(&bin_expr.left)?;

    log_trace!(
        "Visiting right of bin expr (op: '{}') at '{}'",
        bin_expr.op.data(),
        bin_span
    );
    v.visit_expr(&bin_expr.right)
}

pub fn walk_uni_expr<V: Visitor + ?Sized>(
    v: &mut V,
    node_uni_expr: &AstNode<UniExpr>,
) -> VisitorResult<V::E> {
    let uni_expr = node_uni_expr.data();
    v.visit_expr(&uni_expr.expr)
}

pub fn walk_dot_access_expr<V: Visitor + ?Sized>(
    v: &mut V,
    node_dot_access: &AstNode<DotAccess>,
) -> VisitorResult<V::E> {
    let dot_access = node_dot_access.data();
    v.visit_expr(&dot_access.base)
}

pub fn walk_fn_call_expr<V: Visitor + ?Sized>(
    v: &mut V,
    node_fn_call: &AstNode<FnCall>,
) -> VisitorResult<V::E> {
    let fn_call = node_fn_call.data();
    let fn_call_span = node_fn_call.span();

    log_trace!("Visiting fn call at '{}'", fn_call_span);

    log_trace!("Visiting fn call function at '{}'", fn_call_span);
    v.visit_expr(&fn_call.func)?;

    for (i, arg) in fn_call.args.iter().enumerate() {
        log_trace!(
            "Visiting argument #{} (0-indexed) at '{}'",
            i,
            fn_call_span
        );
        v.visit_expr(arg)?;
    }

    Ok(())
}

pub fn walk_struct_init_expr<V: Visitor + ?Sized>(
    v: &mut V,
    node_struct_init: &AstNode<StructInit>,
) -> VisitorResult<V::E> {
    let struct_init = node_struct_init.data();
    let struct_init_span = node_struct_init.span();

    log_trace!("Visiting struct init expr at '{}'", struct_init_span);

    for (i, (field, init_expr)) in struct_init.field_init.iter().enumerate() {
        log_trace!(
            "Visiting field init '{}'(#{} (0-indexed)) at '{}'",
            field.data(),
            i,
            struct_init_span
        );

        v.visit_expr(init_expr)?;
    }

    Ok(())
}

pub fn walk_lam_expr<V: Visitor + ?Sized>(
    v: &mut V,
    node_lam: &AstNode<Lambda>,
) -> VisitorResult<V::E> {
    let lam = node_lam.data();
    let lam_span = node_lam.span();

    log_trace!("Visiting lambda expr at '{}'", lam_span);
    v.visit_expr(&lam.body)
}

pub fn walk_block_expr<V: Visitor + ?Sized>(
    v: &mut V,
    node_block: &AstNode<Block>,
) -> VisitorResult<V::E> {
    let block = node_block.data();
    let block_span = node_block.span();

    log_trace!("Visiting block expr at '{}'", block_span);

    for s in block.stmts.iter() {
        v.visit_stmt(s)?;
    }

    match block.return_expr {
        Some(ref return_expr) => {
            log_trace!("Block at '{}' has a trailing expr", block_span);
            v.visit_expr(return_expr)
        }

        None => {
            log_trace!(
                "Block at '{}' does not have a trailing expr",
                block_span
            );
            Ok(())
        }
    }
}
