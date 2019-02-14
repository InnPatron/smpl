use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::slice::Iter;
use std::iter::Iterator;

use crate::span::Span;

pub use crate::ast::BinOp;
pub use crate::ast::UniOp;
pub use crate::ast::Literal;
use crate::ast;

use super::type_cons::*;
use super::semantic_data::*;
use super::expr_flow;

#[derive(Debug, Clone)]
pub struct Typed<T>
where
    T: ::std::fmt::Debug + Clone,
{
    data: T,
    data_type: RefCell<Option<Type>>,
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
            data_type: RefCell::new(None),
        }
    }

    pub fn typed(data: T, t: Type) -> Typed<T> {
        Typed {
            data: data,
            data_type: RefCell::new(Some(t)),
        }
    }

    pub fn set_type(&self, t: Type) {
        // TODO: Handle type override
        let mut borrow = self.data_type.borrow_mut();
        if borrow.is_some() {
            panic!("Attempting to overwrite the type of this node ({:?})", self);
        } else {
            *borrow = Some(t);
        }
    }

    pub fn get_type(&self) -> Option<Type> {
        self.data_type.borrow().clone()
    }
}

#[derive(Debug, Clone)]
pub struct Assignment {
    field_access: FieldAccess,
    access_span: Span,
    access: self::Expr,
    value: self::Expr,
}

impl Assignment {
    pub fn new(universe: &Universe, assignment: ast::Assignment) -> Assignment {
        let (name, name_span) = assignment.name.to_data();
        let mut access = Expr::new();
        let field_access = FieldAccess::new(universe, &mut access, name);
        Assignment {
            field_access: field_access,
            access: access,
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

    pub fn value(&self) -> &self::Expr {
        &self.value
    }

    pub fn access(&self) -> &self::Expr {
        &self.access
    }
}

#[derive(Debug, Clone)]
pub struct LocalVarDecl {
    type_ann: Option<ast::AstNode<ast::TypeAnnotation>>,
    var_name: ast::AstNode<ast::Ident>,
    var_init: self::Expr,
    var_type: RefCell<Option<Type>>,
    var_id: VarId,
    span: Span,
}

impl LocalVarDecl {
    pub fn new(universe: &Universe, decl: ast::LocalVarDecl, stmt_span: Span) -> LocalVarDecl {
        LocalVarDecl {
            type_ann: decl.var_type,
            var_name: decl.var_name,
            var_init: expr_flow::flatten(universe, decl.var_init),
            var_type: RefCell::new(None),
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

    pub fn set_type(&self, app: Type) {
        let mut borrow = self.var_type.borrow_mut();
        if borrow.is_some() {
            panic!(
                "Attempting to override type for local variable declarration"
            );
        } else {
            *borrow = Some(app);
        }
    }

    pub fn var_type(&self) -> Option<Type> {
        self.var_type.borrow().clone()
    }

    pub fn var_id(&self) -> VarId {
        self.var_id
    }

    pub fn init_expr(&self) -> &self::Expr {
        &self.var_init
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

    pub fn execution_order(&self) -> Iter<TmpId> {
        self.execution_order.iter()
    }

    pub fn order_length(&self) -> usize {
        self.execution_order.len()
    }

    pub fn tmp_by_index(&self, tmp_index: usize) -> TmpId {
        self.execution_order.get(tmp_index).expect(&format!("Invalid temporary index {}", tmp_index)).clone()
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
                data_type: RefCell::new(None),
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
    fn_id: Cell<Option<FnId>>,
}

impl TypeInst {
    pub fn new(path: ast::ModulePath, args: Vec<ast::TypeAnnotation>) -> TypeInst {
        TypeInst {
            path: path,
            args: args,
            fn_id: Cell::new(None),
        }
    }

    pub fn path(&self) -> &ast::ModulePath {
        &self.path
    }

    pub fn args(&self) -> &[ast::TypeAnnotation] {
        &self.args
    }

    pub fn set_id(&self, id: FnId) {
        if self.fn_id.get().is_some() {
            panic!(
                "Attempting to overwrite fn-id of the type-inst {:?}",
                self.path
            );
        } else {
            self.fn_id.set(Some(id));
        }
    }

    pub fn get_id(&self) -> Option<FnId> {
        self.fn_id.get()
    } 
}

#[derive(Debug, Clone)]
pub struct ModAccess {
    path: ast::ModulePath,
    id: Cell<Option<FnId>>,
}

impl ModAccess {
    pub fn new(path: ast::ModulePath) -> ModAccess {
        ModAccess {
            path: path,
            id: Cell::new(None),
        }
    }

    pub fn path(&self) -> &ast::ModulePath {
        &self.path
    }

    pub fn set_fn_id(&self, id: FnId) {
        if self.id.get().is_some() {
            panic!();
        }

        self.id.set(Some(id))
    }

    pub fn fn_id(&self) -> Option<FnId> {
        self.id.get().map(|i| i.clone())
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
    field_init: Option<Vec<(ast::Ident, Typed<TmpId>)>>,
    struct_type: RefCell<Option<Type>>,
    mapped_field_init: RefCell<Option<Vec<(FieldId, Typed<TmpId>)>>>,
}

impl StructInit {
    pub fn new(
        struct_type_name: ast::TypedPath,
        field_init: Option<Vec<(ast::Ident, Typed<TmpId>)>>,
    ) -> StructInit {
        StructInit {
            struct_type_name: struct_type_name,
            struct_type: RefCell::new(None),
            field_init: field_init,
            mapped_field_init: RefCell::new(None),
        }
    }

    pub fn type_name(&self) -> &ast::ModulePath {
        self.struct_type_name.module_path()
    }

    pub fn type_args(&self) -> Option<&[ast::TypeAnnotation]> {
        self.struct_type_name.annotations()
    }

    pub fn set_struct_type(&self, app: Type) {
        let mut borrow = self.struct_type.borrow_mut();
        if borrow.is_some() {
            panic!(
                "Attempting to overwrite struct type of struct init",
            );
        } else {
            *borrow = Some(app);
        }
    }

    pub fn field_init(&self) -> Option<Vec<(FieldId, Typed<TmpId>)>> {
        self.mapped_field_init.borrow().clone()
    }

    pub fn init_order<'a>(&'a self) -> Option<impl Iterator<Item = &'a ast::Ident>> {
        match self.field_init {
            Some(ref vec) => Some(vec.iter().map(|(ref ident, _)| ident)),

            None => None,
        }
    }

    pub fn set_field_init(&self, universe: &Universe) -> Result<(), Vec<ast::Ident>> {
        let struct_type = self.struct_type.borrow();
        let struct_type = struct_type.as_ref().unwrap();

        let field_map = match struct_type {
            Type::Record { 
                    field_map: field_map,
                    ..
            } => field_map,

            _ => unimplemented!(),
        };

        match self.field_init {
            Some(ref map) => {
                let mut result = Vec::new();
                let mut unknown_fields = Vec::new();
                for &(ref ident, ref tmp) in map.iter() {
                    match field_map.get(ident) {
                        Some(field_id) => {
                            result.push((field_id.clone(), tmp.clone()));
                        },

                        None => {
                            unknown_fields.push(ident.clone());
                        }
                    }
                }

                if unknown_fields.len() > 0 {
                    Err(unknown_fields)
                } else {
                    *self.mapped_field_init.borrow_mut() = Some(result);
                    Ok(())
                }
            }

            None => Ok(()),
        }
    }

    pub fn struct_type(&self) -> Option<Type> {
        let borrow = self.struct_type.borrow();
        borrow.clone()
    }
}

#[derive(Debug, Clone)]
pub struct FieldAccess {
    raw_path: ast::Path,
    path: self::Path,
    field_type: RefCell<Option<Type>>,
}

impl FieldAccess {
    pub fn new(universe: &Universe, expr: &mut Expr, path: ast::Path) -> FieldAccess {
        FieldAccess {
            raw_path: path.clone(),
            path: self::Path::new(universe, expr, path),
            field_type: RefCell::new(None),
        }
    }

    pub fn raw_path(&self) -> &ast::Path {
        &self.raw_path
    }

    pub fn path(&self) -> &self::Path {
        &self.path
    }

    pub fn set_field_type(&self, app: Type) {
        let mut borrow = self.field_type.borrow_mut();

        if borrow.is_some() {
            panic!(
                "Attempting to override type of a field access",
            );
        } else {
            *borrow = Some(app);
        }
    }

    pub fn field_type(&self) -> Option<Type> {
        self.field_type.borrow().clone()
    }
}

#[derive(Debug, Clone)]
pub struct Binding {
    ident: ast::AstNode<ast::Ident>,
    binding_id: Cell<Option<BindingId>>,
}

impl Binding {
    pub fn new(ident: ast::AstNode<ast::Ident>) -> Binding {
        Binding {
            ident: ident,
            binding_id: Cell::new(None),
        }
    }

    pub fn ident(&self) -> &ast::Ident {
        self.ident.data()
    }

    pub fn set_id<T>(&self, id: T)
    where
        T: Into<BindingId> + ::std::fmt::Debug,
    {
        if self.binding_id.get().is_some() {
            panic!(
                "Attempting to overwrite {:?} of the Ident {:?} with {:?}",
                self.binding_id.get().unwrap(),
                self.ident,
                id
            );
        } else {
            self.binding_id.set(Some(id.into()));
        }
    }

    pub fn get_id(&self) -> Option<BindingId> {
        self.binding_id.get()
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
    root_indexing: Option<TmpId>,
    root_var: RefCell<Option<Typed<VarId>>>,
    path: Vec<self::PathSegment>,
}

impl self::Path {
    fn new(universe: &Universe, expr: &mut self::Expr, path: ast::Path) -> self::Path {
        let mut path_iter = path.0.into_iter();
        let root = path_iter.next().unwrap();

        let (name, indexing) = match root {
            ast::PathSegment::Ident(i) => (i, None),
            ast::PathSegment::Indexing(i, e) => (i, Some(expr_flow::flatten_expr(universe, expr, *e).0)),
        };

        let path = path_iter
            .map(|ps| match ps {
                ast::PathSegment::Ident(i) => self::PathSegment::Ident(Field::new(i)),
                ast::PathSegment::Indexing(i, e) => {
                    self::PathSegment::Indexing(Field::new(i), expr_flow::flatten_expr(universe, expr, *e).0)
                }
            })
            .collect();

        self::Path {
            root_name: name,
            root_indexing: indexing,
            root_var: RefCell::new(None),
            path: path,
        }
    }

    pub fn root_name(&self) -> &ast::Ident {
        self.root_name.data()
    }

    pub fn root_indexing_expr(&self) -> Option<TmpId> {
        self.root_indexing.clone()
    }

    pub fn root_var_id(&self) -> VarId {
        let r = self.root_var.borrow();

        match *r {
            Some(ref typed_var_id) => *typed_var_id.data(),
            None => panic!("No root var"),
        }
    }

    pub fn root_var_type(&self) -> Type {
        let r = self.root_var.borrow();

        match *r {
            Some(ref typed_var_id) => typed_var_id.get_type().unwrap().clone(),
            None => panic!("No root var"),
        }
    }

    pub fn set_root_var(&self, id: VarId) {
        let mut r = self.root_var.borrow_mut();

        if r.is_some() {
            panic!("Attempting to overwrite root VarId");
        }

        *r = Some(Typed::untyped(id));
    }

    pub fn set_root_var_type(&self, ty: Type) {
        let r = self.root_var.borrow_mut();

        match *r {
            Some(ref t) => t.set_type(ty),
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
    Indexing(Field, TmpId),
}

#[derive(Debug, Clone)]
pub struct Field {
    name: ast::AstNode<ast::Ident>,
    field_id: RefCell<Option<Typed<FieldId>>>,
}

impl Field {
    pub fn new(name: ast::AstNode<ast::Ident>) -> Field {
        Field {
            name: name,
            field_id: RefCell::new(None),
        }
    }

    pub fn name(&self) -> &ast::Ident {
        self.name.data()
    }

    pub fn field_id(&self) -> FieldId {
        let f = self.field_id.borrow();

        match *f {
            Some(ref typed_field_id) => *typed_field_id.data(),
            None => panic!("No field"),
        }
    }

    pub fn field_type(&self) -> Type {
        let f = self.field_id.borrow();

        match *f {
            Some(ref typed_field_id) => typed_field_id.get_type().unwrap().clone(),
            None => panic!("No field"),
        }
    }

    pub fn set_field_id(&self, id: FieldId) {
        let mut f = self.field_id.borrow_mut();

        if f.is_some() {
            panic!("Attempting to override field id.");
        }

        *f = Some(Typed::untyped(id));
    }

    pub fn set_field_type(&self, app: Type) {
        let f = self.field_id.borrow_mut();

        match *f {
            Some(ref t) => t.set_type(app),
            None => panic!("No field"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct AnonymousFn {
    a_fn: ast::AnonymousFn,
    fn_id: Cell<Option<FnId>>,
}

impl AnonymousFn {
    pub fn new(a_fn: ast::AnonymousFn) -> AnonymousFn {
        AnonymousFn {
            a_fn: a_fn,
            fn_id: Cell::new(None),
        }
    }

    pub fn a_fn(&self) -> &ast::AnonymousFn {
        &self.a_fn
    }

    pub fn fn_id(&self) -> FnId {
        self.fn_id.get().unwrap()
    }

    pub fn set_fn_id(&self, fn_id: FnId) {
        if self.fn_id.get().is_some() {
            panic!("Attempting to overwrite an anonymous function's FnId");
        }

        self.fn_id.set(Some(fn_id));
    }
}
