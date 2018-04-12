use std::collections::HashMap;
use std::collections::hash_map::Iter;

use ast::Ident;
use err::Err;
use analysis::semantic_data::{VarId, TypeId, ModuleId, FnId, FieldId, Universe, Program};

#[derive(Clone, Debug)]
pub struct Metadata {
    fn_layout: HashMap<FnId, FnLayout>,
    field_ordering: HashMap<TypeId, FieldOrdering>,
    array_types: HashMap<ModuleId, Vec<TypeId>>,
    main: Option<(FnId, ModuleId)>,
}

impl Metadata {

    pub fn new() -> Metadata {
        Metadata {
            fn_layout: HashMap::new(),
            field_ordering: HashMap::new(),
            array_types: HashMap::new(),
            main: None,
        }
    }

    pub fn insert_field_ordering(&mut self, id: TypeId, data: FieldOrdering) {
        if self.field_ordering.insert(id, data).is_some() {
            panic!("Overwriting field ordering for struct {}", id);
        }
    }

    pub fn insert_fn_layout(&mut self, id: FnId, data: FnLayout) {
        if self.fn_layout.insert(id, data).is_some() {
            panic!("Overwriting for fn {}", id);
        }
    }

    pub fn insert_array_type(&mut self, mod_id: ModuleId, type_id: TypeId) {
        if self.array_types.contains_key(&mod_id) {
            let mut v = self.array_types.get_mut(&mod_id).unwrap();
            v.push(type_id);
        } else {
            self.array_types.insert(mod_id, vec![type_id]);
        }
    }

    pub fn find_main(program: &mut Program) -> Result<(), Err> {
        use ast::TypePath;

        let (u, m, f) = program.analysis_context();
        let universe = u;

        for (_, mod_id) in universe.all_modules().into_iter() {
            let module = universe.get_module(*mod_id);
            if let Ok(id) = module.module_scope().get_fn(&type_path!("main")) {
                if m.main.is_none() {
                    m.main = Some((id, *mod_id))
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

    pub fn array_types(&self, id: ModuleId) -> Option<&[TypeId]> {
        self.array_types.get(&id).map(|v| v.as_slice())
    }

    pub fn field_ordering(&self, id: TypeId) -> &FieldOrdering {
        self.field_ordering.get(&id).unwrap()
    }

    pub fn fn_layout(&self, id: FnId) -> &FnLayout {
        self.fn_layout.get(&id).unwrap()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnLayout {
    locals: Vec<(VarId, TypeId)>,
    params: Vec<(VarId, TypeId)>,

    ret_ty: TypeId,
}

impl FnLayout {
    pub fn new(locals: Vec<(VarId, TypeId)>, params: Vec<(VarId, TypeId)>, ret_ty: TypeId) -> FnLayout {
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
