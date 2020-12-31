use log::trace;

use super::ast::*;
use super::ast_node::{AstNode, Spanned};
use super::expr_ast::*;

macro_rules! log_trace {
        ($($arg:tt)+) => (trace!(target: "visitor-mut", $($arg)+ ))
}

pub type VisitorResult<E> = Result<(), E>;

pub trait Visitor {
    type E;

    fn visit_type_ann(
        &mut self,
        type_decl: &mut AstNode<TypeAnn>,
    ) -> VisitorResult<Self::E> {
        walk_type_ann(self, type_decl)
    }

    fn visit_type_param(
        &mut self,
        type_param: &mut AstNode<Name>,
    ) -> VisitorResult<Self::E> {
        Ok(())
    }

    fn visit_type_decl(
        &mut self,
        type_decl: &mut AstNode<TypeDecl>,
    ) -> VisitorResult<Self::E> {
        walk_type_decl(self, type_decl)
    }

    fn visit_struct_field(
        &mut self,
        f: &mut StructField,
        enum_variant: bool,
    ) -> VisitorResult<Self::E> {
        walk_struct_field(self, f)
    }

    fn visit_enum_variant(
        &mut self,
        enum_variant: &mut AstNode<EnumVariant>,
    ) -> VisitorResult<Self::E> {
        walk_enum_variant(self, enum_variant)
    }

    fn visit_enum_decl(
        &mut self,
        enum_decl: &mut AstNode<EnumDecl>,
    ) -> VisitorResult<Self::E> {
        walk_enum_decl(self, enum_decl)
    }

    fn visit_opaque_decl(
        &mut self,
        opaque_decl: &mut AstNode<Opaque>,
    ) -> VisitorResult<Self::E> {
        walk_opaque_decl(self, opaque_decl)
    }

    fn visit_struct_decl(
        &mut self,
        struct_decl: &mut AstNode<Struct>,
    ) -> VisitorResult<Self::E> {
        walk_struct_decl(self, struct_decl)
    }

    fn visit_fn_param(
        &mut self,
        param: &mut AstNode<FnParam>,
        builtin: bool,
    ) -> VisitorResult<Self::E> {
        walk_fn_param(self, param)
    }

    fn visit_builtin_fn_decl(
        &mut self,
        fn_decl: &mut AstNode<BuiltinFnDecl>,
    ) -> VisitorResult<Self::E> {
        walk_builtin_fn_decl(self, fn_decl)
    }

    fn visit_fn_decl(
        &mut self,
        fn_decl: &mut AstNode<FnDecl>,
    ) -> VisitorResult<Self::E> {
        walk_fn_decl(self, fn_decl)
    }

    fn visit_module_inst(
        &mut self,
        inst: &mut AstNode<ModuleInst>,
    ) -> VisitorResult<Self::E> {
        Ok(())
    }

    fn visit_export(
        &mut self,
        e: &mut AstNode<ExportDecl>,
    ) -> VisitorResult<Self::E> {
        walk_export_decl(self, e)
    }

    fn visit_import(
        &mut self,
        i: &mut AstNode<ImportDecl>,
    ) -> VisitorResult<Self::E> {
        walk_import_decl(self, i)
    }

    fn visit_stmt(&mut self, s: &mut Stmt) -> VisitorResult<Self::E> {
        walk_stmt(self, s)
    }

    fn visit_if_stmt(&mut self, n: &mut AstNode<If>) -> VisitorResult<Self::E> {
        walk_if_stmt(self, n)
    }

    fn visit_while_stmt(
        &mut self,
        n: &mut AstNode<While>,
    ) -> VisitorResult<Self::E> {
        walk_while_stmt(self, n)
    }

    fn visit_let_stmt(
        &mut self,
        n: &mut AstNode<LetStmt>,
    ) -> VisitorResult<Self::E> {
        walk_let_stmt(self, n)
    }

    fn visit_return_stmt(
        &mut self,
        n: &mut AstNode<ReturnStmt>,
    ) -> VisitorResult<Self::E> {
        walk_return_stmt(self, n)
    }

    fn visit_break_stmt(
        &mut self,
        n: &mut AstNode<BreakStmt>,
    ) -> VisitorResult<Self::E> {
        walk_break_stmt(self, n)
    }

    fn visit_extract_stmt(
        &mut self,
        n: &mut AstNode<ExtractStmt>,
    ) -> VisitorResult<Self::E> {
        walk_extract_stmt(self, n)
    }

    fn visit_continue_stmt(
        &mut self,
        n: &mut AstNode<ContinueStmt>,
    ) -> VisitorResult<Self::E> {
        walk_continue_stmt(self, n)
    }

    fn visit_block_stmt(
        &mut self,
        n: &mut AstNode<Block>,
    ) -> VisitorResult<Self::E> {
        walk_block_stmt(self, n)
    }

    fn visit_expr_stmt(&mut self, n: &mut Expr) -> VisitorResult<Self::E> {
        walk_expr_stmt(self, n)
    }

    fn visit_expr(&mut self, e: &mut Expr) -> VisitorResult<Self::E> {
        walk_expr(self, e)
    }

    fn visit_if_expr(
        &mut self,
        node_if_expr: &mut AstNode<If>,
    ) -> VisitorResult<Self::E> {
        walk_if_expr(self, node_if_expr)
    }

    fn visit_while_expr(
        &mut self,
        node_while_expr: &mut AstNode<While>,
    ) -> VisitorResult<Self::E> {
        walk_while_expr(self, node_while_expr)
    }

    fn visit_bin_expr(
        &mut self,
        node_bin_expr: &mut AstNode<BinExpr>,
    ) -> VisitorResult<Self::E> {
        walk_bin_expr(self, node_bin_expr)
    }

    fn visit_uni_expr(
        &mut self,
        node_uni_expr: &mut AstNode<UniExpr>,
    ) -> VisitorResult<Self::E> {
        walk_uni_expr(self, node_uni_expr)
    }

    fn visit_lit_expr(
        &mut self,
        _: &mut AstNode<Literal>,
    ) -> VisitorResult<Self::E> {
        Ok(())
    }

    fn visit_dot_expr(
        &mut self,
        node_dot_access: &mut AstNode<DotAccess>,
    ) -> VisitorResult<Self::E> {
        walk_dot_access_expr(self, node_dot_access)
    }

    fn visit_fn_call_expr(
        &mut self,
        node_fn_call: &mut AstNode<FnCall>,
    ) -> VisitorResult<Self::E> {
        walk_fn_call_expr(self, node_fn_call)
    }

    fn visit_typed_path(
        &mut self,
        node_typed_path: &mut AstNode<TypedPath>,
        subexpr: bool,
    ) -> VisitorResult<Self::E> {
        Ok(())
    }

    fn visit_struct_init_expr(
        &mut self,
        node_struct_init: &mut AstNode<StructInit>,
    ) -> VisitorResult<Self::E> {
        walk_struct_init_expr(self, node_struct_init)
    }

    fn visit_lam_expr(
        &mut self,
        node_lam: &mut AstNode<Lambda>,
    ) -> VisitorResult<Self::E> {
        walk_lam_expr(self, node_lam)
    }

    fn visit_path_expr(
        &mut self,
        node_typed_path: &mut AstNode<TypedPath>,
    ) -> VisitorResult<Self::E> {
        walk_path_expr(self, node_typed_path)
    }

    fn visit_block_expr(
        &mut self,
        node_block: &mut AstNode<Block>,
    ) -> VisitorResult<Self::E> {
        walk_block_expr(self, node_block)
    }

    fn visit_underscore_expr(
        &mut self,
        _: &mut AstNode<()>,
    ) -> VisitorResult<Self::E> {
        Ok(())
    }

    fn visit_branch(&mut self, b: &mut Branch) -> VisitorResult<Self::E> {
        walk_expr(self, &mut b.condition)?;
        walk_block_expr(self, &mut b.block)
    }

    fn visit_default_branch(
        &mut self,
        b: &mut AstNode<Block>,
    ) -> VisitorResult<Self::E> {
        walk_block_expr(self, b)
    }
}

pub fn walk_module<V: Visitor>(
    v: &mut V,
    m: &mut Module,
) -> VisitorResult<V::E> {
    for decl in m.decls.iter_mut() {
        match decl {
            Decl::Local(LocalDecl::Fn(ref mut node_local)) => {
                v.visit_fn_decl(node_local)?
            }

            Decl::Local(LocalDecl::BuiltinFn(ref mut node_local)) => {
                v.visit_builtin_fn_decl(node_local)?
            }

            Decl::Local(LocalDecl::Opaque(ref mut node_local)) => {
                v.visit_opaque_decl(node_local)?
            }

            Decl::Local(LocalDecl::Struct(ref mut node_local)) => {
                v.visit_struct_decl(node_local)?
            }

            Decl::Local(LocalDecl::Enum(ref mut node_local)) => {
                v.visit_enum_decl(node_local)?
            }

            Decl::Local(LocalDecl::Type(ref mut node_local)) => {
                v.visit_type_decl(node_local)?
            }

            Decl::Mod => todo!("Dec::Mod"),

            Decl::Import(ref mut decl) => v.visit_import(decl)?,
            Decl::Export(ref mut decl) => v.visit_export(decl)?,
        }
    }

    Ok(())
}

pub fn walk_module_for_expr<V: Visitor>(
    v: &mut V,
    m: &mut Module,
) -> VisitorResult<V::E> {
    for decl in m.decls.iter_mut() {
        match decl {
            Decl::Local(LocalDecl::Fn(ref mut node_fn_decl)) => {
                let fn_decl = node_fn_decl.data_mut();
                log_trace!(
                    "Visitor visiting function: {}",
                    fn_decl.name.data_mut()
                );

                walk_block_stmt(v, &mut fn_decl.body)?;
            }

            _ => continue,
        }
    }

    Ok(())
}

pub fn walk_block_stmt<V: Visitor + ?Sized>(
    v: &mut V,
    b: &mut AstNode<Block>,
) -> VisitorResult<V::E> {
    for i in b.data_mut().imports.iter_mut() {
        v.visit_import(i)?;
    }

    for s in b.data_mut().stmts.iter_mut() {
        v.visit_stmt(s)?;
    }

    Ok(())
}

pub fn walk_if_stmt<V: Visitor + ?Sized>(
    v: &mut V,
    node_if: &mut AstNode<If>,
) -> VisitorResult<V::E> {
    let if_stmt = node_if.data_mut();

    for branch in if_stmt.branches.iter_mut() {
        v.visit_branch(branch)?;
    }

    if let Some(ref mut default_branch) = if_stmt.default_branch {
        v.visit_default_branch(default_branch)?;
    }

    Ok(())
}

pub fn walk_while_stmt<V: Visitor + ?Sized>(
    v: &mut V,
    node_while: &mut AstNode<While>,
) -> VisitorResult<V::E> {
    let while_stmt = node_while.data_mut();

    for branch in while_stmt.branches.iter_mut() {
        v.visit_branch(branch)?;
    }

    if let Some(ref mut default_branch) = while_stmt.default_branch {
        v.visit_default_branch(default_branch)?;
    }

    Ok(())
}

pub fn walk_let_stmt<V: Visitor + ?Sized>(
    v: &mut V,
    node_let: &mut AstNode<LetStmt>,
) -> VisitorResult<V::E> {
    let let_stmt = node_let.data_mut();
    v.visit_expr(&mut let_stmt.init)
}

pub fn walk_return_stmt<V: Visitor + ?Sized>(
    v: &mut V,
    node_return: &mut AstNode<ReturnStmt>,
) -> VisitorResult<V::E> {
    let return_span = node_return.span();
    let return_stmt = node_return.data_mut();

    match return_stmt.expr {
        Some(ref mut e) => {
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
    node_break: &mut AstNode<BreakStmt>,
) -> VisitorResult<V::E> {
    let break_span = node_break.span();
    let break_stmt = node_break.data_mut();

    match break_stmt.expr {
        Some(ref mut e) => {
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
    node_extract: &mut AstNode<ExtractStmt>,
) -> VisitorResult<V::E> {
    let extract_span = node_extract.span();
    let extract_stmt = node_extract.data_mut();

    match extract_stmt.expr {
        Some(ref mut e) => {
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
    _v: &mut V,
    _node_continue: &mut AstNode<ContinueStmt>,
) -> VisitorResult<V::E> {
    Ok(())
}

pub fn walk_expr_stmt<V: Visitor + ?Sized>(
    v: &mut V,
    expr: &mut Expr,
) -> VisitorResult<V::E> {
    log_trace!("Visiting expr-stmt at '{}'", expr.span());
    v.visit_expr(expr)
}

pub fn walk_stmt<V: Visitor + ?Sized>(
    v: &mut V,
    s: &mut Stmt,
) -> VisitorResult<V::E> {
    match s {
        Stmt::If(ref mut node_if) => v.visit_if_stmt(node_if),

        Stmt::While(ref mut node_while) => v.visit_while_stmt(node_while),

        Stmt::Let(ref mut node_let) => v.visit_let_stmt(node_let),

        Stmt::Return(ref mut node_return) => v.visit_return_stmt(node_return),

        Stmt::Break(ref mut node_break) => v.visit_break_stmt(node_break),

        Stmt::Extract(ref mut node_extract) => {
            v.visit_extract_stmt(node_extract)
        }

        Stmt::Continue(ref mut node_continue) => {
            v.visit_continue_stmt(node_continue)
        }

        Stmt::Block(ref mut node_block) => walk_block_stmt(v, node_block),

        Stmt::ExprStmt(ref mut expr) => v.visit_expr_stmt(expr),
    }
}

pub fn walk_expr<V: Visitor + ?Sized>(
    v: &mut V,
    e: &mut Expr,
) -> VisitorResult<V::E> {
    match e {
        Expr::If(ref mut node_if_expr) => v.visit_if_expr(node_if_expr),

        Expr::While(ref mut node_while_expr) => {
            v.visit_while_expr(node_while_expr)
        }

        Expr::Bin(ref mut node_bin_expr) => v.visit_bin_expr(node_bin_expr),

        Expr::Uni(ref mut node_uni_expr) => v.visit_uni_expr(node_uni_expr),

        Expr::Literal(ref mut node_lit_expr) => v.visit_lit_expr(node_lit_expr),

        Expr::DotAccess(ref mut node_dot_expr) => {
            v.visit_dot_expr(node_dot_expr)
        }

        Expr::FnCall(ref mut node_fn_call) => {
            v.visit_fn_call_expr(node_fn_call)
        }

        Expr::StructInit(ref mut node_struct_init) => {
            v.visit_struct_init_expr(node_struct_init)
        }

        Expr::Lambda(ref mut node_lambda) => v.visit_lam_expr(node_lambda),

        Expr::Path(ref mut node_path) => v.visit_path_expr(node_path),

        Expr::Block(ref mut node_block) => v.visit_block_expr(node_block),

        Expr::Underscore(ref mut node_underscore) => {
            v.visit_underscore_expr(node_underscore)
        }
    }
}

pub fn walk_if_expr<V: Visitor + ?Sized>(
    v: &mut V,
    node_if_expr: &mut AstNode<If>,
) -> VisitorResult<V::E> {
    let if_span = node_if_expr.span();
    let if_expr = node_if_expr.data_mut();

    for branch in if_expr.branches.iter_mut() {
        v.visit_branch(branch)?;
    }

    match if_expr.default_branch {
        Some(ref mut block) => {
            log_trace!("If expr at '{}' has a default branch", if_span);
            v.visit_default_branch(block)
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
    node_while_expr: &mut AstNode<While>,
) -> VisitorResult<V::E> {
    let while_span = node_while_expr.span();
    let while_expr = node_while_expr.data_mut();

    for branch in while_expr.branches.iter_mut() {
        v.visit_branch(branch)?;
    }

    match while_expr.default_branch {
        Some(ref mut block) => {
            log_trace!("While expr at '{}' has a default branch", while_span);
            v.visit_default_branch(block)
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
    node_bin_expr: &mut AstNode<BinExpr>,
) -> VisitorResult<V::E> {
    let bin_span = node_bin_expr.span();
    let bin_expr = node_bin_expr.data_mut();

    log_trace!(
        "Visiting left of bin expr (op: '{}') at '{}'",
        bin_expr.op.data_mut(),
        bin_span
    );
    v.visit_expr(&mut bin_expr.left)?;

    log_trace!(
        "Visiting right of bin expr (op: '{}') at '{}'",
        bin_expr.op.data_mut(),
        bin_span
    );
    v.visit_expr(&mut bin_expr.right)
}

pub fn walk_uni_expr<V: Visitor + ?Sized>(
    v: &mut V,
    node_uni_expr: &mut AstNode<UniExpr>,
) -> VisitorResult<V::E> {
    let uni_expr = node_uni_expr.data_mut();
    v.visit_expr(&mut uni_expr.expr)
}

pub fn walk_dot_access_expr<V: Visitor + ?Sized>(
    v: &mut V,
    node_dot_access: &mut AstNode<DotAccess>,
) -> VisitorResult<V::E> {
    let dot_access = node_dot_access.data_mut();
    v.visit_expr(&mut dot_access.base)
}

pub fn walk_fn_call_expr<V: Visitor + ?Sized>(
    v: &mut V,
    node_fn_call: &mut AstNode<FnCall>,
) -> VisitorResult<V::E> {
    let fn_call_span = node_fn_call.span();
    let fn_call = node_fn_call.data_mut();

    log_trace!("Visiting fn call at '{}'", fn_call_span);

    log_trace!("Visiting fn call function at '{}'", fn_call_span);
    v.visit_expr(&mut fn_call.func)?;

    for (i, arg) in fn_call.args.iter_mut().enumerate() {
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
    node_struct_init: &mut AstNode<StructInit>,
) -> VisitorResult<V::E> {
    let struct_init_span = node_struct_init.span();
    let struct_init = node_struct_init.data_mut();

    log_trace!("Visiting struct init expr at '{}'", struct_init_span);

    let _ = struct_init
        .struct_name
        .as_mut()
        .map(|p| v.visit_typed_path(p, false))
        .transpose()?;

    for (i, (field, init_expr)) in struct_init.field_init.iter_mut().enumerate()
    {
        log_trace!(
            "Visiting field init '{}'(#{} (0-indexed)) at '{}'",
            field.data_mut(),
            i,
            struct_init_span
        );

        v.visit_expr(init_expr)?;
    }

    Ok(())
}

pub fn walk_lam_expr<V: Visitor + ?Sized>(
    v: &mut V,
    node_lam: &mut AstNode<Lambda>,
) -> VisitorResult<V::E> {
    let lam_span = node_lam.span();
    let lam = node_lam.data_mut();

    log_trace!("Visiting lambda expr at '{}'", lam_span);
    v.visit_expr(&mut lam.body)
}

pub fn walk_block_expr<V: Visitor + ?Sized>(
    v: &mut V,
    node_block: &mut AstNode<Block>,
) -> VisitorResult<V::E> {
    let block_span = node_block.span();
    let block = node_block.data_mut();

    log_trace!("Visiting block expr at '{}'", block_span);

    for s in block.stmts.iter_mut() {
        v.visit_stmt(s)?;
    }

    match block.return_expr {
        Some(ref mut return_expr) => {
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

fn walk_type_decl<V: Visitor + ?Sized>(
    v: &mut V,
    node_type_decl: &mut AstNode<TypeDecl>,
) -> VisitorResult<V::E> {
    v.visit_type_ann(&mut node_type_decl.data_mut().ann)
}

fn walk_enum_decl<V: Visitor + ?Sized>(
    v: &mut V,
    node_enum_decl: &mut AstNode<EnumDecl>,
) -> VisitorResult<V::E> {
    let enum_decl = node_enum_decl.data_mut();

    for tp in enum_decl.type_params.params.iter_mut() {
        v.visit_type_param(tp)?;
    }

    for variant in enum_decl.variants.iter_mut() {
        v.visit_enum_variant(variant)?;
    }

    Ok(())
}

fn walk_enum_variant<V: Visitor + ?Sized>(
    v: &mut V,
    node_enum_variant: &mut AstNode<EnumVariant>,
) -> VisitorResult<V::E> {
    match node_enum_variant.data_mut() {
        EnumVariant::Struct { ref mut body, .. } => {
            for f in body.iter_mut() {
                v.visit_struct_field(f, true)?;
            }

            Ok(())
        }

        EnumVariant::Unit { .. } => Ok(()),
    }
}

fn walk_struct_field<V: Visitor + ?Sized>(
    v: &mut V,
    node_struct_field: &mut StructField,
) -> VisitorResult<V::E> {
    v.visit_type_ann(&mut node_struct_field.field_type)
}

fn walk_opaque_decl<V: Visitor + ?Sized>(
    v: &mut V,
    node_opaque_decl: &mut AstNode<Opaque>,
) -> VisitorResult<V::E> {
    for tp in node_opaque_decl.data_mut().type_params.params.iter_mut() {
        v.visit_type_param(tp)?;
    }

    Ok(())
}

fn walk_struct_decl<V: Visitor + ?Sized>(
    v: &mut V,
    node_struct_decl: &mut AstNode<Struct>,
) -> VisitorResult<V::E> {
    let struct_decl = node_struct_decl.data_mut();

    for f in struct_decl.body.iter_mut() {
        v.visit_struct_field(f, false)?;
    }

    for tp in struct_decl.type_params.params.iter_mut() {
        v.visit_type_param(tp)?;
    }

    Ok(())
}

fn walk_builtin_fn_decl<V: Visitor + ?Sized>(
    v: &mut V,
    node_fn_decl: &mut AstNode<BuiltinFnDecl>,
) -> VisitorResult<V::E> {
    let fn_decl = node_fn_decl.data_mut();

    if let BuiltinFnParams::Checked(ref mut params) = fn_decl.params {
        for p in params {
            v.visit_fn_param(p, true)?;
        }
    }

    let _ = fn_decl
        .return_type
        .as_mut()
        .map(|ann| v.visit_type_ann(ann))
        .transpose()?;

    for tp in fn_decl.type_params.params.iter_mut() {
        v.visit_type_param(tp)?;
    }

    Ok(())
}

fn walk_fn_param<V: Visitor + ?Sized>(
    v: &mut V,
    node_fn_param: &mut AstNode<FnParam>,
) -> VisitorResult<V::E> {
    v.visit_type_ann(&mut node_fn_param.data_mut().ann)
}

fn walk_fn_decl<V: Visitor + ?Sized>(
    v: &mut V,
    node_fn_decl: &mut AstNode<FnDecl>,
) -> VisitorResult<V::E> {
    let fn_decl = node_fn_decl.data_mut();

    for p in fn_decl.params.iter_mut() {
        v.visit_fn_param(p, false)?;
    }

    let _ = fn_decl
        .return_type
        .as_mut()
        .map(|ann| v.visit_type_ann(ann))
        .transpose()?;

    for tp in fn_decl.type_params.params.iter_mut() {
        v.visit_type_param(tp)?;
    }

    v.visit_block_stmt(&mut fn_decl.body)
}

fn walk_export_decl<V: Visitor + ?Sized>(
    v: &mut V,
    node_export_decl: &mut AstNode<ExportDecl>,
) -> VisitorResult<V::E> {
    match node_export_decl.data_mut() {
        ExportDecl::ExportItems {
            ref mut from_module,
            ref mut items,
        } => from_module
            .as_mut()
            .map(|inst| v.visit_module_inst(inst))
            .transpose()
            .map(|_| ()),

        ExportDecl::ExportAll {
            ref mut from_module,
            ref mut except,
        } => from_module
            .as_mut()
            .map(|inst| v.visit_module_inst(inst))
            .transpose()
            .map(|_| ()),
    }
}

fn walk_import_decl<V: Visitor + ?Sized>(
    v: &mut V,
    node_import_decl: &mut AstNode<ImportDecl>,
) -> VisitorResult<V::E> {
    match node_import_decl.data_mut() {
        ImportDecl::ImportModule { ref mut module, .. } => {
            v.visit_module_inst(module)
        }

        ImportDecl::ImportItems {
            ref mut module,
            ref mut items,
        } => v.visit_module_inst(module),

        ImportDecl::ImportAll {
            ref mut module,
            ref mut except,
        } => v.visit_module_inst(module),
    }
}

fn walk_path_expr<V: Visitor + ?Sized>(
    v: &mut V,
    node_typed_path: &mut AstNode<TypedPath>,
) -> VisitorResult<V::E> {
    v.visit_typed_path(node_typed_path, true)
}

fn walk_type_ann<V: Visitor + ?Sized>(
    v: &mut V,
    node_type_ann: &mut AstNode<TypeAnn>,
) -> VisitorResult<V::E> {
    match node_type_ann.data_mut() {
        TypeAnn::Path(ref mut node_tp) => v.visit_typed_path(node_tp, false),

        TypeAnn::FnType(ref mut node_fn_type) => {
            let fn_type = node_fn_type.data_mut();
            for p in fn_type.params.iter_mut() {
                v.visit_type_ann(p)?;
            }

            fn_type
                .return_type
                .as_mut()
                .map(|t| v.visit_type_ann(t))
                .transpose()
                .map(|_| ())
        }
    }
}
