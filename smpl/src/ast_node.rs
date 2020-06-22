use std::fmt;

use crate::span::Span;
use crate::analysis::abstract_type::AbstractType;
use crate::typable_ast::Typed;

pub type EmptyAstNode = AstNode<()>;

pub trait Spanned {
    fn span(&self) -> Span;
}

pub struct AstNode<T> {
    node: T,
    span: Span,
}

impl<T> AstNode<T> {
    pub fn new(node: T, span: Span) -> Self {
        AstNode {
            node,
            span,
        }
    }

    pub fn split(self) -> (T, Span) {
        (self.node, self.span)
    }

    pub fn node(&self) -> &T {
        &self.node
    }

    pub fn node_mut(&mut self) -> &mut T {
        &mut self.node
    }
}

impl<T> Spanned for AstNode<T> {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl<T> PartialEq for AstNode<T>
where
    T: PartialEq,
{
    fn eq(&self, other: &AstNode<T>) -> bool {
        self.node == other.node
    }
}

impl<T> Eq for AstNode<T> where T: Eq {}

impl<T> ::std::hash::Hash for AstNode<T>
where
    T: ::std::hash::Hash,
{
    fn hash<H: ::std::hash::Hasher>(&self, state: &mut H) {
        self.node.hash(state);
    }
}

impl<T> Clone for AstNode<T>
where
    T: Clone,
{
    fn clone(&self) -> AstNode<T> {
        AstNode::new(self.node.clone(), self.span.clone())
    }
}

impl<T> ::std::fmt::Debug for AstNode<T>
where
    T: ::std::fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        ::std::fmt::Debug::fmt(&self.node, f)
    }
}

impl<T> Typed for AstNode<T> where T: Typed {
    fn typ(&self) -> &AbstractType {
        self.node.typ()
    }

    fn set_type(&mut self, t: AbstractType) {
        self.node.set_type(t);
    }
}
