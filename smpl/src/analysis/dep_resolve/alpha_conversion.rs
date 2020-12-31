use log::trace;

use std::collections::{HashMap, HashSet};

use crate::ast::{Module, ModuleInst, ModulePath, Name, TypedPath};
use crate::ast_mut_visitor::{self as amv, VisitorResult};
use crate::ast_node::AstNode;

use super::super::GlobalData;
use super::{DepResult, Error};

macro_rules! log_trace {
        ($($arg:tt)+) => (trace!(target: "alpha-converter", $($arg)+ ))
}

pub struct AlphaConverter {
    // Map of original name (Name::Ident) to Name::Atom
    params: HashMap<Name, Name>,
}

impl AlphaConverter {
    /// Modifies the module AST by alpha-converting module parameters and their usage to use
    ///   a unique name `Name::Atom`
    ///
    /// Returns the set of `Name::Atom` produced by alpha conversion
    ///
    /// Assumes that all local items are unique
    ///   i.e. that `self::param_name` does not clash with any other local items
    pub fn convert(
        global_data: &GlobalData,
        module: &mut Module,
    ) -> DepResult<HashSet<Name>> {
        let params = module
            .mod_decl
            .as_ref()
            .map(|node_decl| {
                node_decl.data().mod_params.iter().map(|node_mod_param| {
                    node_mod_param.data().name.data().clone()
                })
            })
            .expect("No module decl: not WF");

        let mut converter = AlphaConverter::new(global_data, params);
        if converter.params.len() > 0 {
            amv::walk_module(&mut converter, module)?;
        }

        let mut set = HashSet::new();
        for v in converter.params.values() {
            if set.insert(v.clone()) {
                panic!("Duplicate atoms: {:?}", v);
            }
        }

        Ok(set)
    }

    fn new<I: Iterator<Item = Name>>(
        global_data: &GlobalData,
        input_params: I,
    ) -> AlphaConverter {
        let mut params = HashMap::new();

        for p in input_params {
            let atom = match p.clone() {
                Name::Ident(id) => {
                    Name::Atom(id, global_data.inc_atom_counter())
                }

                p => {
                    panic!("Parameter not refered as a `Name::Ident`: {:?}", p)
                }
            };

            params.insert(p, atom);
        }

        AlphaConverter { params }
    }

    fn convert_path(&self, base: &mut ModulePath) {
        if base.len() > 2 {
            if *base.0[0].data() == Name::Ident("self".to_string().into()) {
                if let Some(mut replacement) =
                    self.params.get(base.0[1].data()).cloned()
                {
                    let mut to_swap = base.0[0].data_mut();

                    match to_swap {
                        Name::Ident(..) => {
                            log_trace!(
                                "Replaced {} with {}",
                                to_swap,
                                replacement
                            );
                            std::mem::swap(&mut replacement, to_swap);
                        }

                        _ => {
                            panic!("Parameter not represented by a Name::Ident: {:?}",
                                to_swap);
                        }
                    }
                }
            }
        }
    }
}

impl amv::Visitor for AlphaConverter {
    type E = Error;

    fn visit_typed_path(
        &mut self,
        node_typed_path: &mut AstNode<TypedPath>,
        _subexpr: bool,
    ) -> VisitorResult<Self::E> {
        let typed_path = node_typed_path.data_mut();
        let base = typed_path.base.data_mut();

        self.convert_path(base);
        amv::walk_typed_path(self, node_typed_path)
    }

    fn visit_module_inst(
        &mut self,
        node_inst: &mut AstNode<ModuleInst>,
    ) -> VisitorResult<Self::E> {
        let inst = node_inst.data_mut();
        self.convert_path(inst.module.data_mut());

        for inst_arg in inst.args.iter_mut() {
            self.visit_module_inst(inst_arg)?;
        }

        Ok(())
    }
}
