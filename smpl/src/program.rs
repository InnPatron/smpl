use std::cell::RefCell;
use std::rc::Rc;

use crate::analysis::error::AnalysisError;
use crate::analysis::{
    check_program, AnonymousFunction, FnId, Function,
    Program as AnalyzedProgram, TypingContext, CFG,
};
use crate::module::{UnparsedModule, ParsedModule};
use crate::error::Error;

///
/// Represents a collection of fully parsed and analyzed SMPL modules.
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
