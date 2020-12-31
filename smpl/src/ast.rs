use std::collections::HashMap;
use std::fmt::{self, Debug};

use crate::ast_node::AstNode;
use crate::expr_ast::{Block, Expr};
use crate::span::Span;

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
    pub imports: Vec<AstNode<ImportDecl>>,
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

impl Module {
    pub fn name(&self) -> Option<&AstNode<Ident>> {
        self.mod_decl.as_ref().map(|d| &d.data().mod_name)
    }
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
    pub sigs: Vec<AstNode<SigConstraint>>,
}

#[derive(Debug, Clone)]
pub struct SigConstraint {
    pub name: AstNode<Name>,
    pub constraints: HashMap<Name, Expr>,
}

#[derive(Debug, Clone)]
pub enum Decl {
    Import(AstNode<ImportDecl>),
    Export(AstNode<ExportDecl>),
    Local(LocalDecl),
    Mod,
}

#[derive(Debug, Clone)]
pub enum LocalDecl {
    Opaque(AstNode<Opaque>),
    Struct(AstNode<Struct>),
    Enum(AstNode<EnumDecl>),
    Fn(AstNode<FnDecl>),
    BuiltinFn(AstNode<BuiltinFnDecl>),
    Type(AstNode<TypeDecl>),
}

impl LocalDecl {
    pub fn name(&self) -> &AstNode<Name> {
        match self {
            LocalDecl::Opaque(ref n) => &n.data().name,
            LocalDecl::Struct(ref n) => &n.data().name,
            LocalDecl::Enum(ref n) => &n.data().name,
            LocalDecl::Fn(ref n) => &n.data().name,
            LocalDecl::BuiltinFn(ref n) => &n.data().name,
            LocalDecl::Type(ref n) => &n.data().name,
        }
    }

    pub fn set_mod(&mut self, module: Ident) {
        match self {
            LocalDecl::Opaque(ref mut n) => {
                n.data_mut().name.data_mut().to_module_item(module)
            }
            LocalDecl::Struct(ref mut n) => {
                n.data_mut().name.data_mut().to_module_item(module)
            }
            LocalDecl::Enum(ref mut n) => {
                n.data_mut().name.data_mut().to_module_item(module)
            }
            LocalDecl::Fn(ref mut n) => {
                n.data_mut().name.data_mut().to_module_item(module)
            }
            LocalDecl::BuiltinFn(ref mut n) => {
                n.data_mut().name.data_mut().to_module_item(module)
            }
            LocalDecl::Type(ref mut n) => {
                n.data_mut().name.data_mut().to_module_item(module)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct EnumDecl {
    pub name: AstNode<Name>,
    pub type_params: TypeParams,
    pub annotations: Vec<Annotation>,
    pub variants: Vec<AstNode<EnumVariant>>,
}

#[derive(Debug, Clone)]
pub enum EnumVariant {
    Struct {
        name: AstNode<Name>,
        body: Vec<StructField>,
        annotations: Vec<Annotation>,
    },
    Unit {
        name: AstNode<Name>,
        annotations: Vec<Annotation>,
    },
}

#[derive(Debug, Clone)]
pub struct TypeDecl {
    pub name: AstNode<Name>,
    pub ann: AstNode<TypeAnn>,
}

#[derive(Debug, Clone)]
pub struct ModuleInst {
    pub module: AstNode<ModulePath>,
    pub args: Vec<AstNode<ModuleInst>>,
}

#[derive(Debug, Clone)]
pub enum ExportDecl {
    ExportItems {
        from_module: Option<AstNode<ModuleInst>>,
        items: Vec<AstNode<ExportItem>>,
    },
    ExportAll {
        from_module: Option<AstNode<ModuleInst>>,
        except: Vec<AstNode<ExportItem>>,
    },
}

pub type ExportItem = ModuleItemData;

#[derive(Debug, Clone)]
pub enum ImportDecl {
    ImportItems {
        module: AstNode<ModuleInst>,
        items: Vec<AstNode<ImportItem>>,
    },
    ImportModule {
        module: AstNode<ModuleInst>,
        alias: Option<AstNode<Ident>>,
    },
    ImportAll {
        module: AstNode<ModuleInst>,
        except: Vec<AstNode<ImportItem>>,
    },
}

pub type ImportItem = ModuleItemData;

#[derive(Debug, Clone)]
pub struct ModuleItemData {
    pub original_name: AstNode<Name>,
    pub name_override: Option<AstNode<Name>>,
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
    pub type_params: TypeParams,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModulePath(pub Vec<AstNode<Name>>);

impl ModulePath {
    pub fn mod_param_path<N: Into<Name>>(n: N, span: Span) -> Self {
        ModulePath(vec![
            AstNode::new(
                Ident::Unquoted("self".to_string()).into(),
                span.clone(),
            ),
            AstNode::new(n.into(), span),
        ])
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }
}

impl fmt::Display for ModulePath {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut iter = self.0.iter();
        let base = iter.next().expect("Module path length == 0");
        write!(f, "{}", base.data())?;

        for node in iter {
            write!(f, "::{}", node.data())?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Annotation {
    pub keys: Vec<(Ident, Option<String>)>,
}

#[derive(Clone, Debug, Eq)]
pub enum Ident {
    Quoted(String),
    Unquoted(String),
}

// Implemented such that Quoted("a") == Unquoted("a")
impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        let (l, r) = match (self, other) {
            (Self::Quoted(ref l), Self::Quoted(ref r)) => (l, r),
            (Self::Quoted(ref l), Self::Unquoted(ref r)) => (l, r),
            (Self::Unquoted(ref l), Self::Quoted(ref r)) => (l, r),
            (Self::Unquoted(ref l), Self::Unquoted(ref r)) => (l, r),
        };

        l == r
    }
}

impl std::hash::Hash for Ident {
    fn hash<H: std::hash::Hasher>(&self, h: &mut H) {
        match self {
            Self::Quoted(ref s) => s.hash(h),
            Self::Unquoted(ref s) => s.hash(h),
        }
    }
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

    pub fn to_module_item(&mut self, module: Ident) {
        let mut tmp = Name::ModuleItem {
            module,
            item: "".into(),
        };

        std::mem::swap(self, &mut tmp);

        match self {
            Name::ModuleItem { ref mut item, .. } => {
                *item = tmp.into_ident();
            }

            _ => unreachable!(),
        }
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

impl From<AstNode<Ident>> for AstNode<Name> {
    fn from(i: AstNode<Ident>) -> Self {
        let (data, span) = i.into_data();
        AstNode::new(data.into(), span)
    }
}
