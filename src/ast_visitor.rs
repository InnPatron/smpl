use ast::*;
use semantic_ck;

pub trait Visitable {
    fn visit(&mut self, 
                 checker: &mut semantic_ck::SemanticChecker) -> Result<(), semantic_ck::Err>;
}
