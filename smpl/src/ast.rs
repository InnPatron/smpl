use std::fmt::{self, Debug};

use crate::ast_node::AstNode;
use crate::expr_ast::Block;

#[derive(Debug, Clone)]
pub struct Module {
    pub mod_name: Option<AstNode<Ident>>,
    pub decls: Vec<Decl>,
}

#[derive(Debug, Clone)]
pub enum Decl {
    Import(AstNode<ImportDecl>),
    Export(AstNode<ExportDecl>),
    Opaque(AstNode<Opaque>),
    Struct(AstNode<Struct>),
    Function(AstNode<Function>),
    BuiltinFunction(AstNode<BuiltinFunction>),
}

#[derive(Debug, Clone)]
pub enum ExportDecl {
    ExportItems {
        from_module: Option<AstNode<Ident>>,
        items: Vec<AstNode<ExportItem>>
    },
    ExportAll  {
        from_module: Option<AstNode<Ident>>,
        except: Vec<AstNode<ExportItem>>,
    },
}

#[derive(Debug, Clone)]
pub struct ExportItem(pub ModuleItemData);

#[derive(Debug, Clone)]
pub enum ImportDecl {
    ImportItems {
        module: AstNode<Ident>,
        items: Vec<AstNode<ImportItem>>
    },
    ImportModule {
        module: AstNode<Ident>,
        alias: Option<AstNode<Ident>>,
    },
    ImportAll {
        module: AstNode<Ident>,
        except: Vec<AstNode<ImportItem>>,
    },
}

#[derive(Debug, Clone)]
pub struct ImportItem(pub ModuleItemData);

#[derive(Debug, Clone)]
pub struct ModuleItemData {
    pub original_name: AstNode<Ident>,
    pub name_override: Option<AstNode<Ident>>,
}

#[derive(Debug, Clone)]
pub struct BuiltinFunction {
    pub name: AstNode<Name>,
    pub params: BuiltinFnParams,
    pub return_type: Option<AstNode<TypeAnn>>,
    pub annotations: Vec<Annotation>,
    pub type_params: Option<TypeParams>,
    pub where_clause: Option<WhereClause>,
}

#[derive(Debug, Clone)]
pub enum BuiltinFnParams {
    Unchecked,
    Checked(Vec<AstNode<FnParameter>>),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: AstNode<Name>,
    pub params: Vec<AstNode<FnParameter>>,
    pub return_type: Option<AstNode<TypeAnn>>,
    pub body: AstNode<Block>,
    pub annotations: Vec<Annotation>,
    pub type_params: Option<TypeParams>,
    pub where_clause: Option<WhereClause>,
}

#[derive(Debug, Clone)]
pub struct FnParameter {
    pub name: AstNode<Name>,
    pub param_type: AstNode<TypeAnn>,
}

#[derive(Debug, Clone)]
pub struct Opaque {
    pub name: AstNode<Name>,
    pub annotations: Vec<Annotation>,
    pub type_params: Option<TypeParams>,
    pub where_clause: Option<WhereClause>,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: AstNode<Name>,
    pub body: Vec<StructField>,
    pub annotations: Vec<Annotation>,
    pub type_params: Option<TypeParams>,
    pub where_clause: Option<WhereClause>,
}

#[derive(Debug, Clone)]
pub struct WhereClause;

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: AstNode<Ident>,
    pub field_type: AstNode<TypeAnn>,
}

#[derive(Clone, Debug)]
pub enum TypeAnn {
    Path(AstNode<TypedPath>),
    Array(Box<AstNode<TypeAnn>>, u64),
    FnType(
        Option<TypeParams>,
        Vec<AstNode<TypeAnn>>,
        Option<Box<AstNode<TypeAnn>>>,
    ),
}

#[derive(Debug, Clone)]
pub struct TypeParams {
    pub params: Vec<AstNode<Name>>,
}

#[derive(Clone, Debug)]
pub struct TypedPath {
    pub base: AstNode<ModulePath>,
    pub args: Vec<AstNode<TypeAnn>>,
}

impl TypedPath {
    pub fn nil_arity(base: AstNode<ModulePath>) -> Self {
        TypedPath {
            base,
            args: Vec::with_capacity(0),
        }
    }

    pub fn n_arity(base: AstNode<ModulePath>, args: Vec<AstNode<TypeAnn>>) -> Self {
        TypedPath {
            base,
            args,
        }
    }
}

#[derive(Clone, Debug)]
pub struct ModulePath(pub Vec<AstNode<Name>>);

#[derive(Debug, Clone)]
pub struct Annotation {
    pub keys: Vec<(Ident, Option<String>)>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Ident {
    Quoted(String),
    Unquoted(String)
}

impl<T> From<T> for Ident where T: Into<String> {
    fn from(s: T) -> Ident {
        Ident::Quoted(s.into())
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Ident::Quoted(ref s) => write!(f, "`{}`", s),
            Ident::Unquoted(ref s) => write!(f, "{}", s),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Name {
    Name(Ident),
    Atom(Ident, u64),
    Compiler(u64),
    ModuleItem {
        module: Ident,
        item: Ident,
    },
}

impl Name {
    pub fn into_ident(self) -> Ident {
        if let Name::Name(ident) = self {
            return ident;
        }

        panic!("Attempting convert a non `Name::Name(..)` into an `Ident`");
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Name::Name(ref s) => write!(f, "{}", s),
            Name::Atom(ref s, ref id) => write!(f, "{}{}", s, id),
            Name::Compiler(ref id) => write!(f, "$id{}", id),
            Name::ModuleItem {
                ref module,
                ref item
            } => write!(f, "{}::{}", module, item),
        }
    }
}

impl<T> From<T> for Name where T: Into<Ident> {
    fn from(i: T) -> Self {
        Name::Name(i.into())
    }
}
