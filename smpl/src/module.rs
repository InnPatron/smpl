use std::path::PathBuf;

use crate::analysis::ModuleId;
use crate::ast::Module;

///
/// Represents where a SMPL module came from. Used to help gather source location info.
///
/// * `ModuleSource::File` should be used whenever checking SMPL code from a file.
///
/// * Otherwise, use `ModuleSource::Anonymous`
///
///   * `ModuleSource::Anonymous` can carry a hint (`Some(string)`) that will appear
///   within source location info
///
///   * Without a hint, source location info will contain an `anonymous`
///
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

///
/// Represents an unparsed SMPL module.
///
pub struct UnparsedModule<'a> {
    pub(crate) source: ModuleSource,
    pub(crate) module: &'a str,
}

impl<'a> UnparsedModule<'a> {

    ///
    /// Symbolizes an unparsed SMPL module from a file.
    ///
    /// # Arguments
    ///
    /// * `path` - Path to the file
    /// * `data` - ENTIRE content of the file 
    ///
    pub fn file(path: PathBuf, data: &str) -> UnparsedModule {
        UnparsedModule {
            source: ModuleSource::File(path),
            module: data,
        }
    }

    ///
    /// Symbolizes an unparsed SMPL module from an unnamed source.
    ///
    /// # Arguments
    ///
    /// * `data` - ENTIRE SMPL module
    ///
    pub fn anonymous(data: &str) -> UnparsedModule {
        UnparsedModule {
            source: ModuleSource::Anonymous(None),
            module: data,
        }
    }

    ///
    /// Symbolizes an unparsed SMPL module from an hintable unnamed source.
    ///
    /// # Arguments
    ///
    /// * `hint` - Hint to where the SMPL code came from
    /// * `data` - ENTIRE SMPL module
    ///
    pub fn anonymous_hint(hint: String, data: &str) -> UnparsedModule {
        UnparsedModule {
            source: ModuleSource::Anonymous(Some(hint)),
            module: data,
        }
    }

    ///
    /// Returns a reference to a SMPL module's source data
    ///
    pub fn source(&self) -> &ModuleSource {
        &self.source
    }
}

///
/// Represents a parsed SMPL module that is syntactically correct.
///
/// Created by calling `smpl::parser::parse_module`.
/// 
pub struct ParsedModule {
    pub(crate) source: ModuleSource,
    pub(crate) module: Module,
    pub(crate) id: ModuleId,
}

impl ParsedModule {
    pub(crate) fn new(data: Module, source: ModuleSource) -> ParsedModule {
        ParsedModule {
            source: source,
            module: data,
            id: ModuleId::new(),
        }
    }

    pub fn id(&self) -> ModuleId {
        self.id
    }

    ///
    /// Returns a reference to a SMPL module's source data
    ///
    pub fn source(&self) -> &ModuleSource {
        &self.source
    }
}
