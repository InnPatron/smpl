use crate::span::Span;

pub trait Spanned {
    fn span(&self) -> Span;
}

pub struct AstNode<T> {
    span: Span,
    data: T,
}

impl<T> Copy for AstNode<T> where T: Copy {}

impl<T> Eq for AstNode<T> where T: Eq {}

impl<T> Clone for AstNode<T>
where
    T: Clone,
{
    fn clone(&self) -> Self {
        AstNode {
            span: self.span.clone(),
            data: self.data.clone(),
        }
    }
}

impl<T> PartialEq for AstNode<T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data
    }
}

impl<T> std::hash::Hash for AstNode<T>
where
    T: std::hash::Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.data.hash(state)
    }
}

impl<T> std::fmt::Debug for AstNode<T>
where
    T: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.data.fmt(f)
    }
}

impl<T> AstNode<T> {
    pub fn new(data: T, span: Span) -> Self {
        AstNode { span, data }
    }

    pub fn data(&self) -> &T {
        &self.data
    }

    pub fn data_mut(&mut self) -> &mut T {
        &mut self.data
    }

    pub fn into_data(self) -> (T, Span) {
        (self.data, self.span)
    }
}

impl<T> Spanned for AstNode<T> {
    fn span(&self) -> Span {
        self.span
    }
}
