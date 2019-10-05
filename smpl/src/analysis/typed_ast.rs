use std::collections::HashMap;
use std::iter::Iterator;
use std::slice::Iter;

use crate::span::Span;

use crate::ast;
use crate::ast::AstNode;
pub use crate::ast::BinOp;
pub use crate::ast::Literal;
pub use crate::ast::UniOp;

use super::expr_flow;
use super::semantic_data::*;
use super::type_cons::*;

#[derive(Debug, Clone)]
pub struct Typed<T>
where
    T: ::std::fmt::Debug + Clone,
{
    data: T,
    data_type: Option<AbstractType>,
}

impl<T> Typed<T>
where
    T: ::std::fmt::Debug + Clone,
{
    pub fn data(&self) -> &T {
        &self.data
    }

    pub fn data_mut(&mut self) -> &mut T {
        &mut self.data
    }

    pub fn untyped(data: T) -> Typed<T> {
        Typed {
            data: data,
            data_type: None,
        }
    }

    pub fn typed(data: T, t: AbstractType) -> Typed<T> {
        Typed {
            data: data,
            data_type: Some(t),
        }
    }

    pub fn set_type(&mut self, t: AbstractType) {
        // TODO: Handle type override
        if self.data_type.is_some() {
            panic!("Attempting to overwrite the type of this node ({:?})", self);
        } else {
            self.data_type = Some(t);
        }
    }

    pub fn get_type(&self) -> Option<AbstractType> {
        self.data_type.clone()
    }
}

#[derive(Debug, Clone)]
pub struct Assignment {
    field_access: FieldAccess,
    access_span: Span,
    value: self::Expr,
}

impl Assignment {
    pub fn new(universe: &Universe, assignment: ast::Assignment) -> Assignment {
        let (name, name_span) = assignment.name.to_data();
        let field_access = FieldAccess::new(universe, name);
        Assignment {
            field_access: field_access,
            value: expr_flow::flatten(universe, assignment.value),
            access_span: name_span,
        }
    }

    pub fn access_span(&self) -> Span {
        self.access_span
    }

    pub fn assignee(&self) -> &FieldAccess {
        &self.field_access
    }

    pub fn assignee_mut(&mut self) -> &mut FieldAccess {
        &mut self.field_access
    }

    pub fn value(&self) -> &self::Expr {
        &self.value
    }

    pub fn value_mut(&mut self) -> &mut self::Expr {
        &mut self.value
    }
}

#[derive(Debug, Clone)]
pub struct LocalVarDecl {
    type_ann: Option<ast::AstNode<ast::TypeAnnotation>>,
    var_name: ast::AstNode<ast::Ident>,
    var_init: self::Expr,
    var_type: Option<AbstractType>,
    var_id: VarId,
    span: Span,
}

impl LocalVarDecl {
    pub fn new(universe: &Universe, decl: ast::LocalVarDecl, stmt_span: Span) -> LocalVarDecl {
        LocalVarDecl {
            type_ann: decl.var_type,
            var_name: decl.var_name,
            var_init: expr_flow::flatten(universe, decl.var_init),
            var_type: None,
            var_id: universe.new_var_id(),
            span: stmt_span,
        }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn type_annotation(&self) -> Option<&ast::TypeAnnotation> {
        self.type_ann.as_ref().map(|node| node.data())
    }

    pub fn var_name(&self) -> &ast::Ident {
        self.var_name.data()
    }

    pub fn set_type(&mut self, app: AbstractType) {
        if self.var_type.is_some() {
            panic!("Attempting to override type for local variable declarration");
        } else {
            self.var_type = Some(app);
        }
    }

    pub fn var_type(&self) -> Option<AbstractType> {
        self.var_type.clone()
    }

    pub fn var_id(&self) -> VarId {
        self.var_id
    }

    pub fn init_expr(&self) -> &self::Expr {
        &self.var_init
    }

    pub fn init_expr_mut(&mut self) -> &mut self::Expr {
        &mut self.var_init
    }
}

#[derive(Debug, Clone)]
pub struct Expr {
    map: HashMap<TmpId, Tmp>,
    execution_order: Vec<TmpId>,
    span: Option<Span>,
}

impl Expr {
    pub fn new() -> Expr {
        Expr {
            map: HashMap::new(),
            execution_order: Vec::new(),
            span: None,
        }
    }

    pub fn get_tmp(&self, id: TmpId) -> &Tmp {
        self.map
            .get(&id)
            .expect("Given ID should always be valid if taken from the correct Expr")
    }

    pub fn get_tmp_mut(&mut self, id: TmpId) -> &mut Tmp {
        self.map
            .get_mut(&id)
            .expect("Given ID should always be valid if taken from the correct Expr")
    }

    pub fn last(&self) -> TmpId {
        self.execution_order.last().unwrap().clone()
    }

    pub fn execution_order(&self) -> impl Iterator<Item=TmpId> {
        self.execution_order
            .clone()
            .into_iter()
    }

    pub fn order_length(&self) -> usize {
        self.execution_order.len()
    }

    pub fn tmp_by_index(&self, tmp_index: usize) -> TmpId {
        self.execution_order
            .get(tmp_index)
            .expect(&format!("Invalid temporary index {}", tmp_index))
            .clone()
    }

    pub fn set_span(&mut self, span: Span) {
        if self.span.is_some() {
            panic!();
        } else {
            self.span = Some(span);
        }
    }

    pub fn span(&self) -> Span {
        self.span.clone().unwrap()
    }

    pub fn map_tmp(&mut self, universe: &Universe, val: Value, span: Span) -> TmpId {
        let tmp = Tmp {
            id: universe.new_tmp_id(),
            value: Typed {
                data: val,
                data_type: None,
            },
            span: span,
        };
        let id = tmp.id;

        if self.map.insert(id, tmp).is_some() {
            panic!("Attempting to override {}", id);
        }

        self.execution_order.push(id);

        id
    }
}

#[derive(Debug, Clone)]
pub struct Tmp {
    id: TmpId,
    value: Typed<Value>,
    span: Span,
}

impl Tmp {
    pub fn id(&self) -> TmpId {
        self.id
    }

    pub fn value_mut(&mut self) -> &mut Typed<Value> {
        &mut self.value
    }

    pub fn value(&self) -> &Typed<Value> {
        &self.value
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Literal(ast::Literal),
    Binding(self::Binding),
    FieldAccess(self::FieldAccess),
    FnCall(self::FnCall),
    BinExpr(ast::BinOp, Typed<TmpId>, Typed<TmpId>),
    UniExpr(ast::UniOp, Typed<TmpId>),
    StructInit(StructInit),
    AnonStructInit(AnonStructInit),
    ArrayInit(self::ArrayInit),
    Indexing(Indexing),
    ModAccess(self::ModAccess),
    AnonymousFn(self::AnonymousFn),
    TypeInst(self::TypeInst),
}

// Can currently only type instantiate on static functions
#[derive(Debug, Clone)]
pub struct TypeInst {
    path: ast::ModulePath,
    args: Vec<ast::TypeAnnotation>,
    fn_id: Option<FnId>,
}

impl TypeInst {
    pub fn new(path: ast::ModulePath, args: Vec<ast::TypeAnnotation>) -> TypeInst {
        TypeInst {
            path: path,
            args: args,
            fn_id: None,
        }
    }

    pub fn path(&self) -> &ast::ModulePath {
        &self.path
    }

    pub fn args(&self) -> &[ast::TypeAnnotation] {
        &self.args
    }

    pub fn set_id(&mut self, id: FnId) {
        if self.fn_id.is_some() {
            panic!(
                "Attempting to overwrite fn-id of the type-inst {:?}",
                self.path
            );
        } else {
            self.fn_id = Some(id);
        }
    }

    pub fn get_id(&self) -> Option<FnId> {
        self.fn_id.clone()
    }
}

#[derive(Debug, Clone)]
pub struct ModAccess {
    path: ast::ModulePath,
    id: Option<FnId>,
}

impl ModAccess {
    pub fn new(path: ast::ModulePath) -> ModAccess {
        ModAccess {
            path: path,
            id: None,
        }
    }

    pub fn path(&self) -> &ast::ModulePath {
        &self.path
    }

    pub fn set_fn_id(&mut self, id: FnId) {
        if self.id.is_some() {
            panic!();
        }

        self.id = Some(id);
    }

    pub fn fn_id(&self) -> Option<FnId> {
        self.id.clone()
    }
}

#[derive(Debug, Clone)]
pub struct Indexing {
    pub array: Typed<TmpId>,
    pub indexer: Typed<TmpId>,
}

#[derive(Debug, Clone)]
pub enum ArrayInit {
    List(Vec<Typed<TmpId>>),
    Value(Typed<TmpId>, u64),
}

#[derive(Debug, Clone)]
pub struct StructInit {
    struct_type_name: ast::TypedPath,
    field_init: Vec<(ast::Ident, Typed<TmpId>)>,
    struct_type: Option<AbstractType>,
    mapped_field_init: Option<Vec<(FieldId, Typed<TmpId>)>>,
}

impl StructInit {
    pub fn new(
        struct_type_name: ast::TypedPath,
        field_init: Vec<(ast::Ident, Typed<TmpId>)>,
    ) -> StructInit {
        StructInit {
            struct_type_name: struct_type_name,
            struct_type: None,
            field_init: field_init,
            mapped_field_init: None,
        }
    }

    pub fn type_name(&self) -> &ast::ModulePath {
        self.struct_type_name.module_path()
    }

    pub fn type_args(&self) -> Option<&[ast::TypeAnnotation]> {
        self.struct_type_name.annotations()
    }

    pub fn set_struct_type(&mut self, app: AbstractType) {
        if self.struct_type.is_some() {
            panic!("Attempting to overwrite struct type of struct init",);
        } else {
            self.struct_type = Some(app);
        }
    }

    pub fn raw_field_init(&self) -> &[(ast::Ident, Typed<TmpId>)] {
        self.field_init.as_slice()
    }

    pub fn field_init(&self) -> Option<Vec<(FieldId, Typed<TmpId>)>> {
        self.mapped_field_init.clone()
    }

    pub fn init_order<'a>(&'a self) -> impl Iterator<Item = &'a ast::Ident> {
        self.field_init
            .iter()
            .map(|(ref ident, _)| ident)
    } 

    pub fn struct_type(&self) -> Option<AbstractType> {
        self.struct_type.as_ref().map(|t| t.clone())
    }
}

#[derive(Debug, Clone)]
pub struct AnonStructInit {
    field_init: Vec<(ast::Ident, TmpId)>,
    struct_type: Option<AbstractType>,
    mapped_field_init: Option<Vec<(FieldId, Typed<TmpId>)>>,
}

impl AnonStructInit {
    pub fn new(
        field_init: Vec<(ast::Ident, TmpId)>,
    ) -> AnonStructInit {
        AnonStructInit {
            struct_type: None,
            field_init: field_init,
            mapped_field_init: None,
        }
    }

    fn set_struct_type(&mut self, app: AbstractType) {
        if self.struct_type.is_some() {
            panic!("Attempting to overwrite struct type of struct init",);
        } else {
            self.struct_type = Some(app);
        }
    }

    pub fn raw_field_init(&self) -> &[(ast::Ident, TmpId)] {
        self.field_init.as_slice()
    }

    pub fn field_init(&self) -> Option<Vec<(FieldId, Typed<TmpId>)>> {
        self.mapped_field_init.clone()
    }

    pub fn init_order<'a>(&'a self) -> impl Iterator<Item = &'a ast::Ident> {
        self.field_init
            .iter()
            .map(|(ref ident, _)| ident)
    }

    pub fn struct_type(&self) -> Option<AbstractType> {
        self.struct_type.as_ref().map(|t| t.clone())
    }
}

#[derive(Debug, Clone)]
pub struct FieldAccess {
    raw_path: ast::Path,
    path: self::Path,
    field_type: Option<AbstractType>,
}

impl FieldAccess {
    pub fn new(universe: &Universe, path: ast::Path) -> FieldAccess {
        FieldAccess {
            raw_path: path.clone(),
            path: self::Path::new(universe, path),
            field_type: None,
        }
    }

    pub fn raw_path(&self) -> &ast::Path {
        &self.raw_path
    }

    pub fn path(&self) -> &self::Path {
        &self.path
    }

    pub fn path_mut(&mut self) -> &mut self::Path {
        &mut self.path
    }

    pub fn set_field_type(&mut self, app: AbstractType) {
        if self.field_type.is_some() {
            panic!("Attempting to override type of a field access",);
        } else {
            self.field_type = Some(app);
        }
    }

    pub fn field_type(&self) -> Option<AbstractType> {
        self.field_type.clone()
    }
}

#[derive(Debug, Clone)]
pub struct Binding {
    ident: ast::AstNode<ast::Ident>,
    binding_id: Option<BindingId>,
}

impl Binding {
    pub fn new(ident: ast::AstNode<ast::Ident>) -> Binding {
        Binding {
            ident: ident,
            binding_id: None,
        }
    }

    pub fn ident(&self) -> &AstNode<ast::Ident> {
        &self.ident
    }

    pub fn set_id<T>(&mut self, id: T)
    where
        T: Into<BindingId> + ::std::fmt::Debug,
    {
        if self.binding_id.is_some() {
            panic!(
                "Attempting to overwrite {:?} of the Ident {:?} with {:?}",
                self.binding_id.unwrap(),
                self.ident,
                id
            );
        } else {
            self.binding_id = Some(id.into());
        }
    }

    pub fn get_id(&self) -> Option<BindingId> {
        self.binding_id.clone()
    }
}

#[derive(Debug, Clone)]
pub struct FnCall {
    fn_value: TmpId,
    args: Option<Vec<Typed<TmpId>>>,
}

impl FnCall {
    pub fn new(fn_value: TmpId, args: Option<Vec<Typed<TmpId>>>) -> FnCall {
        FnCall {
            fn_value: fn_value,
            args: args,
        }
    }

    pub fn fn_value(&self) -> TmpId {
        self.fn_value
    }

    pub fn args(&self) -> Option<&Vec<Typed<TmpId>>> {
        self.args.as_ref()
    }

    pub fn args_mut(&mut self) -> &mut Option<Vec<Typed<TmpId>>> {
        &mut self.args
    }
}

#[derive(Debug, Clone)]
pub struct Path {
    root_name: ast::AstNode<ast::Ident>,
    root_indexing: Option<Expr>,
    root_var: Option<Typed<VarId>>,
    path: Vec<self::PathSegment>,
}

impl self::Path {
    fn new(universe: &Universe, path: ast::Path) -> self::Path {
        let mut path_iter = path.0.into_iter();
        let root = path_iter.next().unwrap();

        let (name, indexing) = match root {
            ast::PathSegment::Ident(i) => (i, None),
            ast::PathSegment::Indexing(i, e) => {
                (i, Some(expr_flow::flatten(universe, *e)))
            }
        };

        let path = path_iter
            .map(|ps| match ps {
                ast::PathSegment::Ident(i) => self::PathSegment::Ident(Field::new(i)),
                ast::PathSegment::Indexing(i, e) => self::PathSegment::Indexing(
                    Field::new(i),
                    expr_flow::flatten(universe, *e),
                ),
            })
            .collect();

        self::Path {
            root_name: name,
            root_indexing: indexing,
            root_var: None,
            path: path,
        }
    }

    pub fn root_name(&self) -> &AstNode<ast::Ident> {
        &self.root_name
    }

    pub fn root_indexing_expr(&self) -> Option<&Expr> {
        self.root_indexing.as_ref()
    }

    pub fn root_var_id(&self) -> VarId {
        match self.root_var {
            Some(ref typed_var_id) => *typed_var_id.data(),
            None => panic!("No root var"),
        }
    }

    pub fn root_var_type(&self) -> AbstractType {
        match self.root_var {
            Some(ref typed_var_id) => typed_var_id.get_type().unwrap().clone(),
            None => panic!("No root var"),
        }
    }

    pub fn set_root_var(&mut self, id: VarId) {
        if self.root_var.is_some() {
            panic!("Attempting to overwrite root VarId");
        }

        self.root_var = Some(Typed::untyped(id));
    }

    pub fn set_root_var_type(&mut self, ty: AbstractType) {
        match self.root_var {
            Some(ref mut t) => t.set_type(ty),
            None => panic!("No root var"),
        }
    }

    pub fn path(&self) -> &[self::PathSegment] {
        &self.path
    }
}

#[derive(Debug, Clone)]
pub enum PathSegment {
    Ident(Field),
    Indexing(Field, Expr),
}

#[derive(Debug, Clone)]
pub struct Field {
    name: ast::AstNode<ast::Ident>,
    field_id: Option<Typed<FieldId>>,
}

impl Field {
    pub fn new(name: ast::AstNode<ast::Ident>) -> Field {
        Field {
            name: name,
            field_id: None,
        }
    }

    pub fn name(&self) -> &ast::Ident {
        self.name.data()
    }

    pub fn field_id(&self) -> FieldId {
        match self.field_id {
            Some(ref typed_field_id) => *typed_field_id.data(),
            None => panic!("No field"),
        }
    }

    pub fn field_type(&self) -> AbstractType {
        match self.field_id {
            Some(ref typed_field_id) => typed_field_id.get_type().unwrap().clone(),
            None => panic!("No field"),
        }
    }

    pub fn set_field_id(&mut self, id: FieldId) {
        if self.field_id.is_some() {
            panic!("Attempting to override field id.");
        }

        self.field_id = Some(Typed::untyped(id));
    }

    pub fn set_field_type(&mut self, app: AbstractType) {
        match self.field_id {
            Some(ref mut t) => t.set_type(app),
            None => panic!("No field"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct AnonymousFn {
    a_fn: ast::AnonymousFn,
    fn_id: FnId,
}

impl AnonymousFn {
    pub fn new(universe: &Universe, a_fn: ast::AnonymousFn) -> AnonymousFn {
        AnonymousFn {
            a_fn: a_fn,
            fn_id: universe.new_fn_id(),
        }
    }

    pub fn a_fn(&self) -> &ast::AnonymousFn {
        &self.a_fn
    }

    pub fn fn_id(&self) -> FnId {
        self.fn_id
    }
}
