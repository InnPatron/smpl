/*!
  Contains data structures for metadata collected during static analysis.
*/

macro_rules! map_unique_set {
    ($map: expr, $key: expr, $val: expr, $msg: expr) => {{
        if $map.insert($key, $val).is_some() {
            panic!($msg);
        }
    }};
}

mod attribute_keys;
mod fn_data;
mod layout;
mod modules;

pub use self::fn_data::*;
pub use self::layout::*;
pub use self::modules::*;

use std::collections::{HashMap, HashSet};

use super::error::AnalysisError;
use crate::analysis::semantic_data::{FnId, ModuleId, Program, TypeId};
use crate::ast::{Annotation, Ident};

///
/// Primary container for metadata collected during static analysis.
///
#[derive(Clone, Debug)]
pub struct Metadata {
    module_meta: ModuleMetadata,
    fn_param_ids: HashMap<FnId, Vec<FunctionParameter>>,
    fn_layout: HashMap<FnId, FnLayout>,
    field_ordering: HashMap<TypeId, FieldOrdering>,
    array_types: HashMap<ModuleId, Vec<TypeId>>,
    main: Option<(FnId, ModuleId)>,

    fn_map: HashMap<(ModuleId, Ident), FnId>,
    builtin: HashSet<FnId>,

    unchecked_builtins_params: HashSet<FnId>,

    struct_annotations: HashMap<TypeId, HashMap<String, Option<String>>>,
    fn_annotations: HashMap<FnId, HashMap<String, Option<String>>>,
}

impl Metadata {
    pub(super) fn new() -> Metadata {
        Metadata {
            module_meta: ModuleMetadata::new(),
            fn_param_ids: HashMap::new(),
            fn_layout: HashMap::new(),
            field_ordering: HashMap::new(),
            array_types: HashMap::new(),
            main: None,
            fn_map: HashMap::new(),
            builtin: HashSet::new(),
            unchecked_builtins_params: HashSet::new(),
            struct_annotations: HashMap::new(),
            fn_annotations: HashMap::new(),
        }
    }

    pub(super) fn mod_metadata_mut(&mut self) -> &mut ModuleMetadata {
        &mut self.module_meta
    }

    pub fn mod_metadata(&self) -> &ModuleMetadata {
        &self.module_meta
    }

    pub(super) fn insert_builtin(&mut self, id: FnId) {
        self.builtin.insert(id);
    }

    pub fn is_builtin(&self, id: FnId) -> bool {
        self.builtin.contains(&id)
    }

    pub(super) fn insert_unchecked_builtin_params(&mut self, id: FnId) {
        self.insert_builtin(id);
        self.unchecked_builtins_params.insert(id);
    }

    pub fn is_builtin_params_unchecked(&self, id: FnId) -> bool {
        self.unchecked_builtins_params.contains(&id)
    }

    pub(super) fn insert_module_fn(
        &mut self,
        mod_id: ModuleId,
        name: Ident,
        fn_id: FnId,
    ) {
        self.fn_map.insert((mod_id, name), fn_id);
    }

    pub fn module_fn<T: Into<Ident>>(
        &self,
        mod_id: ModuleId,
        name: T,
    ) -> Option<FnId> {
        self.fn_map.get(&(mod_id, name.into())).map(|id| id.clone())
    }

    pub(super) fn insert_field_ordering(
        &mut self,
        id: TypeId,
        data: FieldOrdering,
    ) {
        map_unique_set!(
            self.field_ordering,
            id,
            data,
            format!("Overwriting field ordering for struct {}", id)
        );
    }

    pub fn field_ordering(&self, id: TypeId) -> &FieldOrdering {
        self.field_ordering.get(&id).unwrap()
    }

    pub(super) fn insert_fn_layout(&mut self, id: FnId, data: FnLayout) {
        map_unique_set!(
            self.fn_layout,
            id,
            data,
            format!("Overwriting for fn {}", id)
        );
    }

    pub fn fn_layout(&self, id: FnId) -> &FnLayout {
        self.fn_layout.get(&id).unwrap()
    }

    pub(super) fn insert_array_type(
        &mut self,
        mod_id: ModuleId,
        type_id: TypeId,
    ) {
        if self.array_types.contains_key(&mod_id) {
            let v = self.array_types.get_mut(&mod_id).unwrap();
            v.push(type_id);
        } else {
            self.array_types.insert(mod_id, vec![type_id]);
        }
    }

    pub fn array_types(&self, id: ModuleId) -> Option<&[TypeId]> {
        self.array_types.get(&id).map(|v| v.as_slice())
    }

    pub(super) fn insert_function_param_ids(
        &mut self,
        fn_id: FnId,
        params: Vec<FunctionParameter>,
    ) {
        map_unique_set!(
            self.fn_param_ids,
            fn_id,
            params,
            format!("Overriding function param ids for {:?}", fn_id)
        );
    }

    pub fn function_param_ids(&self, fn_id: FnId) -> &[FunctionParameter] {
        self.fn_param_ids.get(&fn_id).unwrap().as_slice()
    }

    pub(super) fn find_main(
        program: &mut Program,
    ) -> Result<(), AnalysisError> {
        use crate::ast::{AstNode, ModulePath};
        use crate::span::Span;

        let (u, m, _f) = program.analysis_context();
        let universe = u;

        for (_, mod_id) in universe.all_modules().into_iter() {
            let module = universe.get_module(*mod_id);
            if let Ok(id) =
                module.module_scope().get_fn(&ModulePath(vec![AstNode::new(
                    ident!["main"],
                    Span::dummy(),
                )]))
            {
                if m.main.is_none() {
                    m.main = Some((id, *mod_id))
                } else {
                    return Err(AnalysisError::MultipleMainFns);
                }
            }
        }

        Ok(())
    }

    pub fn main(&self) -> Option<(FnId, ModuleId)> {
        self.main
    }

    pub(super) fn set_struct_annotations(
        &mut self,
        type_id: TypeId,
        annotations: &[Annotation],
    ) {
        self.struct_annotations.insert(
            type_id,
            annotations
                .into_iter()
                .fold(HashMap::new(), |mut acc, anno| {
                    for &(ref k, ref v) in anno.keys.iter() {
                        acc.insert(k.to_string(), v.clone());
                    }

                    acc
                }),
        );
    }

    pub fn get_struct_annotations(
        &self,
        type_id: TypeId,
    ) -> Option<&HashMap<String, Option<String>>> {
        self.struct_annotations.get(&type_id)
    }

    pub(super) fn set_fn_annotations(
        &mut self,
        fn_id: FnId,
        annotations: &[Annotation],
    ) {
        self.fn_annotations.insert(
            fn_id,
            annotations
                .into_iter()
                .fold(HashMap::new(), |mut acc, anno| {
                    for &(ref k, ref v) in anno.keys.iter() {
                        acc.insert(k.to_string(), v.clone());
                    }

                    acc
                }),
        );
    }

    pub fn get_fn_annotations(
        &self,
        fn_id: FnId,
    ) -> Option<&HashMap<String, Option<String>>> {
        self.fn_annotations.get(&fn_id)
    }

    pub fn is_opaque(&self, type_id: TypeId) -> bool {
        self.get_struct_annotations(type_id)
            .map_or(false, |map| map.contains_key(attribute_keys::OPAQUE))
    }
}
