use std::collections::{HashMap, HashSet};

use ast::Ident;
use err::Err;
use analysis::semantic_data::{FieldId, FnId, ModuleId, Program, TypeId, VarId};

#[derive(Clone, Debug)]
pub struct Metadata {
    fn_param_ids: HashMap<FnId, Vec<FunctionParameter>>,
    fn_layout: HashMap<FnId, FnLayout>,
    field_ordering: HashMap<TypeId, FieldOrdering>,
    array_types: HashMap<ModuleId, Vec<TypeId>>,
    main: Option<(FnId, ModuleId)>,

    fn_map: HashMap<(ModuleId, Ident), FnId>,
    builtin: HashSet<FnId>,

    unchecked_builtins_params: HashSet<FnId>,
}

impl Metadata {
    pub fn new() -> Metadata {
        Metadata {
            fn_param_ids: HashMap::new(),
            fn_layout: HashMap::new(),
            field_ordering: HashMap::new(),
            array_types: HashMap::new(),
            main: None,
            fn_map: HashMap::new(),
            builtin: HashSet::new(),
            unchecked_builtins_params: HashSet::new(),
        }
    }

    pub fn insert_builtin(&mut self, id: FnId) {
        self.builtin.insert(id);
    }

    pub fn is_builtin(&self, id: FnId) -> bool {
        self.builtin.contains(&id)
    }

    pub fn insert_unchecked_builtin_params(&mut self, id: FnId) {
        self.insert_builtin(id);
        self.unchecked_builtins_params.insert(id);
    }

    pub fn is_builtin_params_unchecked(&self, id: FnId) -> bool {
        self.unchecked_builtins_params.contains(&id)
    }

    pub fn insert_module_fn(&mut self, mod_id: ModuleId, name: Ident, fn_id: FnId) {
        self.fn_map.insert((mod_id, name), fn_id);
    }

    pub fn module_fn(&self, mod_id: ModuleId, name: Ident) -> Option<FnId> {
        self.fn_map.get(&(mod_id, name)).map(|id| id.clone())
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
            let v = self.array_types.get_mut(&mod_id).unwrap();
            v.push(type_id);
        } else {
            self.array_types.insert(mod_id, vec![type_id]);
        }
    }

    pub fn insert_function_param_ids(&mut self, fn_id: FnId, params: Vec<FunctionParameter>) {
        if self.fn_param_ids.insert(fn_id, params).is_some() {
            panic!();
        }
    }

    pub fn function_param_ids(&self, fn_id: FnId) -> &[FunctionParameter] {
        self.fn_param_ids.get(&fn_id).unwrap().as_slice()
    }

    pub fn find_main(program: &mut Program) -> Result<(), Err> {
        use ast::{AstNode, ModulePath};
        use span::Span;

        let (u, m, _f) = program.analysis_context();
        let universe = u;

        for (_, mod_id) in universe.all_modules().into_iter() {
            let module = universe.get_module(*mod_id);
            if let Ok(id) = module.module_scope().get_fn(&ModulePath(vec![
                AstNode::new(ident!["main"], Span::new(0, 0)),
            ])) {
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
