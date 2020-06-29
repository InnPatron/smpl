macro_rules! dummy_node {
    ($content: expr) => {
        AstNode::new($content, Span::dummy())
    };

    (UNTYPED => $content: expr) => {
        Typable::untyped(AstNode::new($content, Span::dummy()))
    }
}


macro_rules! module {
    ($name: expr => $content: expr) => {
        Module {
            ident: Some(dummy_node!(Ident($name.to_string()))),
            top_levels: $content,
        }
    }
}

macro_rules! decl {
    (USE => $module: expr) => {
        DeclStmt::Use(UseDecl(dummy_node!($module)))
    };

    (FN => $fn: expr) => {
        DeclStmt::Function(dummy_node!($fn))
    };

    (STRUCT => $st: expr) => {
        DeclStmt::Struct(dummy_node!($st))
    }
}

macro_rules! fn_param {
    ($name: ident => $type: expr) => {{
        dummy_node!(UNTYPED => FnParameter {
            name: dummy_node!(ident!(stringify!($name))),
            param_type: dummy_node!($type)
        })
    }}
}

macro_rules! struct_field {
    ($name: ident => $type: expr) => {{
        dummy_node!(UNTYPED => FnParameter {
            name: dummy_node!(ident!(stringify!($name))),
            param_type: dummy_node!($type)
        })
    }}
}

macro_rules! block {
    (EMPTY) => {
        Block(vec![])
    };
}

macro_rules! boolean {
    ($val: expr) => {{
        Literal::Bool($val)
    }};

    ($val: expr => Expr) => {{
        Expr::Literal(dummy_node!(boolean!($val)))
    }};

    ($val: expr => BoxExpr) => {{
        Box::new(boolean!($val => Expr))
    }}
}

macro_rules! string {
    ($val: expr) => {{
        Literal::String($val.to_string())
    }};

    ($val: expr => Expr) => {{
        Expr::Literal(dummy_node!(string!($val)))
    }};

    ($val: expr => BoxExpr) => {{
        Box::new(string!($val => Expr))
    }}
}

macro_rules! int {
    ($int: expr) => {{
        Literal::Int($int)
    }};

    ($int: expr => Expr) => {{
        Expr::Literal(dummy_node!(int!($int)))
    }};

    ($int: expr => BoxExpr) => {{
        Box::new(int!($int => Expr))
    }}
}

macro_rules! ident {
    ($ident: expr) => {{
        Ident($ident.to_string())
    }};

    ($ident: expr => Expr) => {{
        Expr::Ident(dummy_node!(ident!($ident)))
    }};

    ($ident: expr => BoxExpr) => {{
        Box::new(ident!($ident => Expr))
    }}
}

macro_rules! module_path {
    ($root: ident  $(:: $segment: ident)*) => {{
        let mut v = vec![dummy_node!(ident!(stringify!($root)))];

        $(v.push(dummy_node!(dummy_node!(ident!(stringify!($segment)))));)*

        ModulePath(v)
    }}
}

macro_rules! type_ann {
    (PATH => $root: ident $(:: $segment: ident)*) => {{
        let path = module_path!($root $(:: $segment)*);

        TypeAnn::ModulePath(dummy_node!(UNTYPED => path))
    }}
}
