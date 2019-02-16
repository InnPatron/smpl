use crate::analysis::ModuleId;
use crate::code_gen::interpreter::BuiltinFn;
use crate::module::ParsedModule;

pub struct VmModule {
    pub parsed: ParsedModule,
    pub builtins: Vec<(String, BuiltinFn)>,
}

impl VmModule {
    pub fn new(m: ParsedModule) -> VmModule {
        VmModule::with_builtins(m, Vec::new())
    }

    pub fn with_builtins(m: ParsedModule, v: Vec<(String, BuiltinFn)>) -> VmModule {
        VmModule {
            parsed: m,
            builtins: v,
        }
    }

    pub fn add_builtin(mut self, fn_name: &str, builtin: BuiltinFn) -> VmModule {
        self.builtins.push((fn_name.to_string(), builtin));
        self
    }

    pub fn id(&self) -> ModuleId {
        self.parsed.id()
    }
}
