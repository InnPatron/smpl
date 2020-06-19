use std::fmt;

use crate::span::Span;

pub type EmptyAstNode = AstNode<()>;

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

    pub fn span(&self) -> Span {
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
