use crate::span::Span;

pub trait Spanned {
    fn span(&self) -> Span;
}

pub struct AstNode<T> {
    span: Span,
    data: T,
}

impl<T> Copy for AstNode<T> where T: Copy {}

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

    pub fn into_data(self) -> (T, Span) {
        (self.data, self.span)
    }
}

impl<T> Spanned for AstNode<T> {
    fn span(&self) -> Span {
        self.span
    }
}
