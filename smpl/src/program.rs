use std::cell::RefCell;
use std::rc::Rc;

use crate::analysis::error::AnalysisError;
use crate::analysis::{
    check_program, AnonymousFunction, FnId, Function,
    Program as AnalyzedProgram, TypingContext, CFG,
    ModuleId, Module as AnalyzedModule,
};
use crate::module::ParsedModule;

pub struct Program {
    program: AnalyzedProgram,
}

impl Program {
    pub fn create(
        modules: Vec<ParsedModule>,
    ) -> Result<Program, AnalysisError> {
        Ok(Program {
            program: check_program(modules)?,
        })
    }

    pub fn metadata(&self) -> &crate::metadata::Metadata {
        &self.program.metadata()
    }

    pub fn compilable_modules(&self) -> impl Iterator<Item = CompilableModule> {

        self.program
            .universe()
            .all_modules()
            .map(move |(_, mod_id)| {
                let module = self.program.universe().get_module(mod_id);

                CompilableModule {
                    program: &self.program,
                    module: module,
                }
            })
    }

    pub fn get_module(&self, module_id: ModuleId) -> CompilableModule {
        let module = self.program
            .universe()
            .get_module(module_id);

        CompilableModule {
            program: &self.program,
            module: module
        }
    }
}

pub struct CompilableModule<'a> {
    program: &'a AnalyzedProgram,
    module: &'a AnalyzedModule,
}

impl<'a> CompilableModule<'a> {

    pub fn id(&self) -> ModuleId {
        self.module.module_id()
    }

    pub fn source(&self) -> &crate::module::ModuleSource {
        self.module.source()
    }

    pub fn get_fn(&self, fn_id: FnId) -> Option<CompilableFn> {
        self.module.owned_fns
            .get(&fn_id)
            .map(move |fn_id| {
                let func = self.program.universe().get_fn(fn_id.clone());
                CompilableModule::to_compilable_fn(fn_id.clone(), func)
            })
    }

    pub fn compilable_fns(&self) -> 
        impl Iterator<Item = CompilableFn> {

        self.module
            .owned_fns()
            .map(move |fn_id| {
                let func = self.program.universe().get_fn(fn_id.clone());
                CompilableModule::to_compilable_fn(fn_id.clone(), func)
            })
    }

    fn to_compilable_fn(f_id: FnId, f: &Function) -> CompilableFn {
        match f {
            Function::SMPL(f) => {
                let c_fn = CompilableFn {
                    fn_id: f_id.clone(),
                    cfg: f.cfg(),
                    typing_context: f.analysis_context().typing_context(),
                };
                c_fn
            },

            Function::Anonymous(AnonymousFunction::Reserved(_)) => {
                panic!("All anonymous functions should be resolved after analysis");
            }

            Function::Anonymous(AnonymousFunction::Resolved {
                ref cfg,
                ref analysis_context,
                ..
            }) => {
                let c_fn = CompilableFn {
                    fn_id: f_id.clone(),
                    cfg: cfg.clone(),
                    typing_context: analysis_context.typing_context()
                };

                c_fn
            },

            _ => unreachable!(),
        }
    }
}

pub struct CompilableFn<'a> {
    fn_id: FnId,
    cfg: Rc<RefCell<CFG>>,
    typing_context: &'a TypingContext,
}

impl<'a> CompilableFn<'a> {

    pub fn fn_id(&self) -> FnId {
        self.fn_id.clone()
    }

    pub(crate) fn cfg(&self) -> &Rc<RefCell<CFG>> {
        &self.cfg
    }

    pub(crate) fn typing_context(&self) -> &TypingContext {
        self.typing_context
    }
}
