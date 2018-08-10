use analysis::semantic_data::{FieldId, FnId, TypeId, VarId};

#[derive(Clone, Debug, PartialEq)]
pub struct FnLayout {
    locals: Vec<(VarId, TypeId)>,
    params: Vec<(VarId, TypeId)>,

    ret_ty: TypeId,
}

impl FnLayout {
    pub fn new(
        locals: Vec<(VarId, TypeId)>,
        params: Vec<(VarId, TypeId)>,
        ret_ty: TypeId,
    ) -> FnLayout {
        FnLayout {
            locals: locals,
            params: params,
            ret_ty: ret_ty,
        }
    }

    pub fn locals(&self) -> &[(VarId, TypeId)] {
        &self.locals
    }

    pub fn params(&self) -> &[(VarId, TypeId)] {
        &self.params
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FieldOrdering {
    id: TypeId,
    order: Vec<FieldId>,
}

impl FieldOrdering {
    pub fn new(id: TypeId, order: Vec<FieldId>) -> FieldOrdering {
        FieldOrdering {
            id: id,
            order: order,
        }
    }

    pub fn id(&self) -> TypeId {
        self.id
    }

    pub fn order(&self) -> &[FieldId] {
        &self.order
    }
}
