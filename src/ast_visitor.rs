use ast::*;
use semantic_ck;



impl Program {
    pub fn visit(&mut self, 
                 checker: &mut semantic_ck::SemanticChecker) -> Result<(), semantic_ck::Err> {
        for decl_stmt in self.0.iter_mut() {
            decl_stmt.visit(checker)?;
        }
        Ok(())
    }
}

impl DeclStmt {
    pub fn visit(&mut self, 
                 checker: &mut semantic_ck::SemanticChecker) -> Result<(), semantic_ck::Err> {
        match *self {
            DeclStmt::Struct(ref mut struct_def) => { checker.accept_struct_def(struct_def)?; },
            DeclStmt::Function(ref mut fn_def) => { checker.accept_fn_def(fn_def)?; },
        }

        Ok(())
    }
}

impl Stmt {
    pub fn visit(&mut self, 
                 expector: &mut semantic_ck::StmtCk) -> Result<(), semantic_ck::Err> {
        match *self {
            Stmt::ExprStmt(ref mut expr_stmt) => { expector.accept_expr_stmt(expr_stmt)?; },
            Stmt::Expr(ref expr) => unimplemented!(),
        }

        Ok(())
    }
}
