use std::fmt::{self, Debug};

use crate::ast_node::AstNode;
use crate::expr_ast::Block;

#[derive(Debug, Clone)]
pub enum CodeUnit {
    Module(Module),
    ModSig(ModSig),
}

// TODO: ModuleInterface
#[derive(Debug, Clone)]
pub struct ModSig {
    pub name: AstNode<Ident>,
    pub members: Vec<AstNode<SigMember>>,
}

#[derive(Debug, Clone)]
pub enum SigMember {
    Type(AstNode<Ident>),
    Value {
        name: AstNode<Ident>,
        ann: AstNode<TypeAnn>,
    },
}

#[derive(Debug, Clone)]
pub struct Module {
    pub mod_decl: Option<AstNode<ModDecl>>,
    pub decls: Vec<Decl>,
}

#[derive(Debug, Clone)]
pub struct ModDecl {
    pub mod_name: AstNode<Ident>,
    pub mod_params: Vec<AstNode<ModParam>>,
    pub declared_sigs: Vec<AstNode<Name>>,
}

#[derive(Debug, Clone)]
pub struct ModParam {
    pub name: AstNode<Name>,
    pub sigs: Vec<AstNode<Name>>,
}

#[derive(Debug, Clone)]
pub enum Decl {
    Import(AstNode<ImportDecl>),
    Export(AstNode<ExportDecl>),
    Opaque(AstNode<Opaque>),
    Struct(AstNode<Struct>),
    Enum(AstNode<EnumDecl>),
    Fn(AstNode<FnDecl>),
    BuiltinFn(AstNode<BuiltinFnDecl>),
    Type(AstNode<TypeDecl>),
}

#[derive(Debug, Clone)]
pub struct EnumDecl {
    pub name: AstNode<Ident>,
    pub type_params: TypeParams,
    pub annotations: Vec<Annotation>,
    pub variants: Vec<AstNode<EnumVariant>>,
}

#[derive(Debug, Clone)]
pub enum EnumVariant {
    Struct {
        name: AstNode<Ident>,
        body: Vec<StructField>,
        annotations: Vec<Annotation>,
    },
    Unit {
        name: AstNode<Ident>,
        annotations: Vec<Annotation>,
    },
}

#[derive(Debug, Clone)]
pub struct TypeDecl {
    pub name: AstNode<Name>,
    pub ann: AstNode<TypeAnn>,
}

#[derive(Debug, Clone)]
pub enum ExportDecl {
    ExportItems {
        from_module: Option<AstNode<ModulePath>>,
        items: Vec<AstNode<ExportItem>>,
    },
    ExportAll {
        from_module: Option<AstNode<ModulePath>>,
        except: Vec<AstNode<ExportItem>>,
    },
}

pub type ExportItem = ModuleItemData;

#[derive(Debug, Clone)]
pub enum ImportDecl {
    ImportItems {
        module: AstNode<ModulePath>,
        items: Vec<AstNode<ImportItem>>,
    },
    ImportModule {
        module: AstNode<ModulePath>,
        alias: Option<AstNode<Ident>>,
    },
    ImportAll {
        module: AstNode<ModulePath>,
        except: Vec<AstNode<ImportItem>>,
    },
}

pub type ImportItem = ModuleItemData;

#[derive(Debug, Clone)]
pub struct ModuleItemData {
    pub original_name: AstNode<Ident>,
    pub name_override: Option<AstNode<Ident>>,
}

#[derive(Debug, Clone)]
pub struct BuiltinFnDecl {
    pub name: AstNode<Name>,
    pub params: BuiltinFnParams,
    pub return_type: Option<AstNode<TypeAnn>>,
    pub annotations: Vec<Annotation>,
    pub type_params: TypeParams,
    pub where_clause: Option<WhereClause>,
}

#[derive(Debug, Clone)]
pub enum BuiltinFnParams {
    Unchecked,
    Checked(Vec<AstNode<FnParam>>),
}

#[derive(Debug, Clone)]
pub struct FnDecl {
    pub name: AstNode<Name>,
    pub params: Vec<AstNode<FnParam>>,
    pub return_type: Option<AstNode<TypeAnn>>,
    pub body: AstNode<Block>,
    pub annotations: Vec<Annotation>,
    pub type_params: TypeParams,
    pub where_clause: Option<WhereClause>,
}

#[derive(Debug, Clone)]
pub struct FnParam {
    pub name: AstNode<Name>,
    pub ann: AstNode<TypeAnn>,
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
    pub type_params: TypeParams,
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
    FnType(AstNode<FnType>),
}

#[derive(Clone, Debug)]
pub struct FnType {
    pub params: Vec<AstNode<TypeAnn>>,
    pub return_type: Option<Box<AstNode<TypeAnn>>>,
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

    pub fn n_arity(
        base: AstNode<ModulePath>,
        args: Vec<AstNode<TypeAnn>>,
    ) -> Self {
        TypedPath { base, args }
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
    Unquoted(String),
}

impl<T> From<T> for Ident
where
    T: Into<String>,
{
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
    Ident(Ident),
    Atom(Ident, u64),
    Compiler(u64),
    ModuleItem { module: Ident, item: Ident },
}

impl Name {
    pub fn into_ident(self) -> Ident {
        if let Name::Ident(ident) = self {
            return ident;
        }

        panic!("Attempting convert a non `Name::Ident(..)` into an `Ident`");
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Name::Ident(ref s) => write!(f, "{}", s),
            Name::Atom(ref s, ref id) => write!(f, "{}{}", s, id),
            Name::Compiler(ref id) => write!(f, "$id{}", id),
            Name::ModuleItem {
                ref module,
                ref item,
            } => write!(f, "{}::{}", module, item),
        }
    }
}

impl From<Ident> for Name {
    fn from(i: Ident) -> Self {
        Name::Ident(i)
    }
}
