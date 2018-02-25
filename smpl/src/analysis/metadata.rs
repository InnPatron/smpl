use std::collections::HashMap;

use ast::Ident;
use err::Err;
use analysis::semantic_data::{VarId, TypeId, ModuleId, FnId, Universe};

#[derive(Clone, Debug)]
pub struct Metadata {
    fn_meta: HashMap<FnId, FnMetadata>,
    field_ordering: HashMap<TypeId, FieldOrdering>,
    main: Option<(FnId, ModuleId)>
}

impl Metadata {

    pub fn new() -> Metadata {
        Metadata {
            fn_meta: HashMap::new(),
            field_ordering: HashMap::new(),
            main: None,
        }
    }

    pub fn insert_field_ordering(&mut self, id: TypeId, data: FieldOrdering) {
        if self.field_ordering.insert(id, data).is_some() {
            panic!("Overwriting field ordering for struct {}", id);
        }
    }

    pub fn insert_fn_data(&mut self, id: FnId, data: FnMetadata) {
        if self.fn_meta.insert(id, data).is_some() {
            panic!("Overwriting for fn {}", id);
        }
    }

    pub fn find_main(&mut self, universe: &Universe) -> Result<(), Err> {
        use ast::Path;

        for (_, mod_id) in universe.all_modules().into_iter() {
            let module = universe.get_module(*mod_id);
            if let Ok(id) = module.module_scope().get_fn(&path!("main")) {
                if self.main.is_none() {
                    self.main = Some((id, *mod_id))
                } else {
                    return Err(Err::MultipleMainFns);
                }
            }
        }

        Ok(())
    }

    pub fn main(&self) -> Option<(FnId, ModuleId)> {
        self.main
    }

    pub fn field_ordering(&self, id: TypeId) -> &FieldOrdering {
        self.field_ordering.get(&id).unwrap()
    }

    pub fn fn_data(&self, id: FnId) -> &FnMetadata {
        self.fn_meta.get(&id).unwrap()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnMetadata {
    locals: Vec<(VarId, TypeId)>,
    params: Vec<(VarId, TypeId)>,

    ret_ty: TypeId,
}

impl FnMetadata {
    pub fn new(locals: Vec<(VarId, TypeId)>, params: Vec<(VarId, TypeId)>, ret_ty: TypeId) -> FnMetadata {
        FnMetadata {
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
    order: Vec<Ident>,
}

impl FieldOrdering {
    pub fn new(id: TypeId, order: Vec<Ident>) -> FieldOrdering {
        FieldOrdering {
            id: id,
            order: order,
        }
    }

    pub fn id(&self) -> TypeId {
        self.id
    }

    pub fn order(&self) -> &[Ident] {
        &self.order
    }
}
