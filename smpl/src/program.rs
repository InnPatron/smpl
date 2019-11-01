use std::cell::RefCell;
use std::rc::Rc;

use crate::analysis::error::AnalysisError;
use crate::analysis::{
    check_program, AnonymousFunction, FnId, Function,
    Program as AnalyzedProgram, TypingContext, CFG,
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

    pub fn compilable_fns<'a>(
        &'a self,
    ) -> impl Iterator<Item = (FnId, CompilableFn)> + 'a {
        self.program
            .all_fns()
            .filter(|(_, f)| {
                match f {
                    Function::SMPL(_) => true,
                    Function::Anonymous(_) => true,
                    _ => false
                }
            })
            .map(|(f_id, f)| {
                match f {
                    Function::SMPL(f) => {
                        let c_fn = CompilableFn {
                            cfg: f.cfg(),
                            typing_context: f.analysis_context().typing_context(),
                        };
                        (f_id, c_fn)
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
                            cfg: cfg.clone(),
                            typing_context: analysis_context.typing_context()
                        };

                        (f_id, c_fn)
                    },

                    _ => unreachable!(),
                }
            })
    }
}

pub struct CompilableFn<'a> {
    cfg: Rc<RefCell<CFG>>,
    typing_context: &'a TypingContext,
}

impl<'a> CompilableFn<'a> {
    pub(crate) fn cfg(&self) -> &Rc<RefCell<CFG>> {
        &self.cfg
    }

    pub(crate) fn typing_context(&self) -> &TypingContext {
        self.typing_context
    }
}
