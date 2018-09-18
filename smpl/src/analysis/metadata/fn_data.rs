use ast::Ident;
use analysis::semantic_data::VarId;

#[derive(Clone, Debug)]
pub struct FunctionParameter {
    id: VarId,
    name: Ident,
}

impl FunctionParameter {
    pub fn new(name: Ident, id: VarId) -> FunctionParameter {
        FunctionParameter { id: id, name: name }
    }

    pub fn var_id(&self) -> VarId {
        self.id
    }

    pub fn name(&self) -> &Ident {
        &self.name
    }
}