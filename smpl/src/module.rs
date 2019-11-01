use std::path::PathBuf;

use crate::analysis::ModuleId;
use crate::ast::Module;

#[derive(Debug, Clone)]
pub enum ModuleSource {
    Anonymous(Option<String>),
    File(PathBuf),
}

impl std::fmt::Display for ModuleSource {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ModuleSource::Anonymous(Some(ref a)) => write!(f, "{}", a),
            ModuleSource::Anonymous(None) => write!(f, "{}", "anonymous"),
            ModuleSource::File(ref buff) => write!(f, "{}", buff.display()),
        }
    }
}

pub struct UnparsedModule<'a> {
    pub source: ModuleSource,
    pub module: &'a str,
}

impl<'a> UnparsedModule<'a> {
    pub fn file(path: PathBuf, data: &str) -> UnparsedModule {
        UnparsedModule {
            source: ModuleSource::File(path),
            module: data,
        }
    }

    pub fn anonymous(data: &str) -> UnparsedModule {
        UnparsedModule {
            source: ModuleSource::Anonymous(None),
            module: data,
        }
    }

    pub fn anonymous_hint(hint: String, data: &str) -> UnparsedModule {
        UnparsedModule {
            source: ModuleSource::Anonymous(Some(hint)),
            module: data,
        }
    }
}

pub struct ParsedModule {
    pub source: ModuleSource,
    pub module: Module,
    pub id: ModuleId,
}

impl ParsedModule {
    pub fn new(data: Module, source: ModuleSource) -> ParsedModule {
        ParsedModule {
            source: source,
            module: data,
            id: ModuleId::new(),
        }
    }

    pub fn id(&self) -> ModuleId {
        self.id
    }
}
