use ast::*;
use semantic_ck;

pub trait Visitable {
    fn visit(&mut self, 
                 checker: &mut semantic_ck::SemanticChecker) -> Result<(), semantic_ck::Err>;
}

impl Visitable for Program {
    fn visit(&mut self, 
                 checker: &mut semantic_ck::SemanticChecker) -> Result<(), semantic_ck::Err> {
        for decl_stmt in self.0.iter_mut() {
            decl_stmt.visit(checker)?;
        }
        Ok(())
    }
}

impl Visitable for DeclStmt {
    fn visit(&mut self, 
                 checker: &mut semantic_ck::SemanticChecker) -> Result<(), semantic_ck::Err> {
        match *self {
            DeclStmt::Struct(ref mut struct_def) => unimplemented!(),
            DeclStmt::Function(ref mut fn_def) => unimplemented!(),
        }

        Ok(())
    }
}

impl Visitable for Struct {
    fn visit(&mut self, 
                 checker: &mut semantic_ck::SemanticChecker) -> Result<(), semantic_ck::Err> {
        checker.accept_struct_def(self)
    }
}

impl Visitable for Function {
    fn visit(&mut self, 
                 checker: &mut semantic_ck::SemanticChecker) -> Result<(), semantic_ck::Err> {
        checker.accept_fn_def(self)
    }
}
