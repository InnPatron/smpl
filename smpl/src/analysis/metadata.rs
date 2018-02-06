use std::collections::HashMap;

use ast::Ident;
use err::Err;
use analysis::semantic_data::{TypeId, ModuleId, FnId, Universe};

#[derive(Clone, Debug)]
pub struct Metadata {
    fn_meta: HashMap<FnId, FnMetadata>,
    struct_meta: HashMap<TypeId, StructMetadata>,
    main: Option<(FnId, ModuleId)>
}

impl Metadata {

    pub fn new() -> Metadata {
        Metadata {
            fn_meta: HashMap::new(),
            struct_meta: HashMap::new(),
            main: None,
        }
    }

    pub fn insert_struct_data(&mut self, id: TypeId, data: StructMetadata) {
        if self.struct_meta.insert(id, data).is_some() {
            panic!("Overwriting for struct {}", id);
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
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnMetadata {
    local_vars: Vec<TypeId>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructMetadata {
    id: TypeId,
    order: Vec<Ident>,
}

impl StructMetadata {
    pub fn new(id: TypeId, order: Vec<Ident>) -> StructMetadata {
        StructMetadata {
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
