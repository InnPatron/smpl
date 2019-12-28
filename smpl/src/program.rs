use std::cell::RefCell;
use std::rc::Rc;

use crate::analysis::error::AnalysisError;
use crate::analysis::{
    check_program, AnonymousFn, FnId, Function,
    Program as AnalyzedProgram, TypingContext, CFG,
    ModuleId, Module as AnalyzedModule,
};
use crate::module::{UnparsedModule, ParsedModule};
use crate::error::Error;

///
/// Represents a self-contained SMPL program made of fully parsed and analyzed SMPL modules.
///
pub struct Program {
    program: AnalyzedProgram,
}

impl Program {

    ///
    /// Create a new `Program` from an iterator of unparsed SMPL modules.
    ///
    pub fn from_unparsed<'a, I>(modules: I) -> Result<Program, Error>
    where I: Iterator<Item=UnparsedModule<'a>> {
        use crate::parser::parse_module;

        let parsed = modules
            .into_iter()
            .map(|p| parse_module(p))
            .collect::<Result<Vec<_>, _>>()?;

        Program::from_parsed(parsed.into_iter())
            .map_err(|e| e.into())
    }

    ///
    /// Create a new `Program` from an iterator of pre-parsed SMPL modules.
    ///
    pub fn from_parsed<I>(
        modules: I,
    ) -> Result<Program, AnalysisError>
    where I: Iterator<Item=ParsedModule> {
        Ok(Program {
            program: check_program(modules.collect())?,
        })
    }

    ///
    /// Access to this `Program`'s metadata collected during static analysis.
    ///
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

///
/// Represents a fully analyzed SMPL module that can be compiled.
///
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
            .map_or(None, |fn_id| {
                let func = self.program.universe().get_fn(fn_id.clone());

                match func {
                    Function::Builtin(_) => None,

                    f => Some(CompilableModule::to_compilable_fn(fn_id.clone(), func))
                }
            })
    }

    pub fn compilable_fns(&self) ->
        impl Iterator<Item = CompilableFn> {

        self.module
            .owned_fns()
            .filter(move |fn_id| {
                match self.program.universe().get_fn(fn_id.clone()) {
                    Function::Builtin(_) => false,

                    _ => true
                }
            })
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
                    // TODO: Temporary measure
                    //  Change this to something else?
                    cfg: Rc::new(RefCell::new(f.cfg().clone())),
                    typing_context: f.analysis_context().typing_context(),
                };
                c_fn
            },

            Function::Anonymous( AnonymousFn {
                ref cfg,
                ref analysis_context,
                ..
            }) => {
                let c_fn = CompilableFn {
                    fn_id: f_id.clone(),
                    cfg: Rc::new(RefCell::new(cfg.clone())),
                    typing_context: analysis_context.typing_context()
                };

                c_fn
            },

            _ => unreachable!(),
        }
    }
}

///
/// Represents a fully analyzed SMPL function that can be compiled.
///
/// Does **NOT** include builtin functions.
///
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
