#[macro_use]
use std::collections::HashMap;
use std::str::FromStr;
use std::borrow::BorrowMut;
use std::fmt::Debug;

use ascii::*;

use smpl_type::*;
use ast::*;

#[derive(Debug, Clone)]
pub enum Err {

}

#[derive(Clone, Debug)]
pub struct SemanticData {
    pub type_map: HashMap<Path, SmplType>,
    pub binding_map: HashMap<Ident, SmplType>,
    pub is_loop: bool,
    pub return_type: Option<SmplType>,
}

impl SemanticData {
    fn new() -> SemanticData {
        let mut type_map = HashMap::new();
        type_map.insert(path!("int"), SmplType::Int);
        type_map.insert(path!("float"), SmplType::Float);
        type_map.insert(path!("String"), SmplType::String);
        type_map.insert(path!("bool"), SmplType::Bool);
        type_map.insert(path!("unit"), SmplType::Unit);

        let mut binding_map = HashMap::new();

        SemanticData {
            type_map: type_map,
            binding_map: binding_map,
            is_loop: false,
            return_type: None,
        }
    }

    fn map_type<T: Into<Path>>(&mut self, name: T, def: SmplType) -> Option<SmplType> {
        self.type_map.insert(name.into(), def)
    }

    fn bind(&mut self, name: Ident, binding_type: SmplType) -> Option<SmplType> {
        self.binding_map.insert(name, binding_type)
    }

    pub fn accept_struct_def(&mut self, struct_def: &Struct) -> Result<(), Err> {
        let def = self.gen_struct_type(struct_def)?;
        match self.map_type(struct_def.name.clone(), SmplType::Struct(def)) {
            Some(_) => unimplemented!("TODO: Handle type override"),
            None => Ok(())
        }
    }

    fn gen_struct_type(&self, struct_def: &Struct) -> ::std::result::Result<StructType, Err> {
        let name = struct_def.name.clone();
        let body = &struct_def.body;

        let mut struct_fields = HashMap::new();

        let body = match body.0 {
            Some(ref b) => b,
            None => {
                return Ok(StructType {
                    name: name,
                    fields: struct_fields,
                });
            }
        };

        for field in body.iter() {
            // TODO: assumes 1 to 1 matching until modules are maybe added
            let type_name = &field.field_type;
            let field_name = &field.name;
            
            match self.type_map.get(type_name) {
                Some(t) => { 
                    let previous = struct_fields.insert(field_name.clone(), t.clone());
                    if previous.is_some() {
                        unimplemented!("Found field with duplicate names");
                    }
                },
                None => unimplemented!("Could not find field type for {}.{}", type_name, field_name),
            }
        }

        Ok(StructType {
                    name: name,
                    fields: struct_fields,
        })
    }

    pub fn accept_fn_def(&mut self, fn_def: &mut Function) -> Result<(), Err> {
        let fn_type = self.gen_fn_type(fn_def)?;
        let arg_types = fn_type.args.clone();
        let return_type = fn_type.return_type.clone();
        match self.bind(fn_def.name.clone(), SmplType::Function(fn_type)) {
            Some(_) => unimplemented!("TODO: Handle binding override"),
            None => (), 
        }
        
        let mut fn_checker = FunctionChecker::new(self, &*return_type);

        if let Some(ref args) = fn_def.args {   
            // add bindings for args
            for (arg, arg_type) in args.iter().zip(arg_types.iter()) {
                fn_checker.semantic_data_mut().bind(arg.name.clone(), arg_type.clone());
            }
        }

        for stmt in fn_def.body.data.0.iter_mut() {
            fn_checker.accept_stmt(stmt)?;
        }
        
        Ok(())
    }

    fn gen_fn_type(&self, fn_def: &Function) -> Result<FunctionType, Err> {

        let return_type = {
            match fn_def.return_type {
                Some(ref path) => {
                    // TODO: assumes 1 to 1 matching until modules are maybe added
                    let type_name = path;
                    match self.type_map.get(type_name) {
                        Some(t) => t.clone(),
                        None => unimplemented!("could not find field type for {}.{}", type_name, fn_def.name),
                    }
                }

                None => SmplType::Unit,
            }
        };

        let arg_types = {
            match fn_def.args {
                Some(ref arg_list) => {
                    let mut arg_types = Vec::new();
                    for arg in arg_list.iter() {
                        let type_name = &arg.arg_type;
                        match self.type_map.get(type_name) {
                            Some(t) => arg_types.push(t.clone()),
                            None => unimplemented!("could not find field type for {}.{}", type_name, fn_def.name),
                        }
                    }

                    arg_types
                },

                None => Vec::new(),
            }
        };

        Ok(FunctionType {
            args: arg_types,
            return_type: Box::new(return_type),
        })      
    }

    fn typify_expr<T: BorrowMut<Expr> + PartialEq + Clone + Debug>(&self, expr: &mut AstNode<T>) -> Result<(), Err> {
        use ast::Expr::*;
        match *expr.data.borrow_mut() {
            Literal(ref mut node_l) => {
                self.typify_literal(node_l)?;
                expr.d_type = node_l.d_type.clone();
            }

            Ident(ref mut node_id) => {
                match self.binding_map.get(&node_id.data) {
                    Some(smpl_type) => node_id.d_type = Some(smpl_type.clone()),
                    None => unimplemented!("Binding does not exist"),
                }
                expr.d_type = node_id.d_type.clone();
            },

            Bin(ref mut node_bin) => {
                self.typify_expr(&mut node_bin.data.lhs)?;
                self.typify_expr(&mut node_bin.data.rhs)?;
                if node_bin.data.lhs.d_type == node_bin.data.rhs.d_type {
                    node_bin.d_type = node_bin.data.lhs.d_type.clone();
                } else {
                    unimplemented!("lhs and rhs must be the same type");
                }
                expr.d_type = node_bin.d_type.clone()
            },

            FnCall(ref mut fn_call) => {
                let smpl_type = self.binding_map.get(&fn_call.data.name)
                                                .ok_or(unimplemented!("Did not find fn in binding map"))?;
                if let &SmplType::Function(ref fn_type) = smpl_type {

                    // Verify arg types matches types in fn call
                    match fn_call.data.args {
                        Some(ref mut call_args) => {
                            if call_args.len() != fn_type.args.len() {
                                unimplemented!("Arg lengths do not match.");
                            }

                            for (ref mut call_arg, fn_type_arg) in call_args.iter_mut()
                                                                            .zip(fn_type.args.iter()) {
                                self.typify_expr(call_arg)?;
                                if call_arg.d_type.as_ref() != Some(fn_type_arg) {
                                    unimplemented!("Arg types do not match");
                                }
                            }
                        },

                        None => {
                            if fn_type.args.len() != 0 {
                                unimplemented!("Arg lengths do not match.");
                            }
                        }
                    }

                    fn_call.d_type = Some(*fn_type.return_type.clone());
                    expr.d_type = fn_call.d_type.clone();
                } else {
                    unimplemented!("Binding does not map to a function type");
                }
            },

            Uni(ref mut uni_expr) => {
                self.typify_expr(&mut uni_expr.data.expr)?;
                unimplemented!("Handle uni op potential type transformation");
            },
        }

        Ok(())
    }

    fn typify_literal(&self, literal: &mut AstNode<Literal>) -> Result<(), Err> {
        use ast::Literal::*;
        if literal.d_type.is_some() {
            return Ok(());
        }

        match literal.data {
            String(_) => literal.d_type = Some(SmplType::String),
            Bool(_) => literal.d_type = Some(SmplType::Bool),
            Number(ref num) => {
                let int_result = num.parse::<i64>();
                let float_result = num.parse::<f64>();

                match (int_result.is_ok(), float_result.is_ok()) {
                    (true, true) => literal.d_type = Some(SmplType::Int),
                    (true, false) => literal.d_type = Some(SmplType::Int),
                    (false, true) => literal.d_type = Some(SmplType::Float),
                    (false, false) => unimplemented!("Should not have parsed"),
                }
            },
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct FunctionChecker {
    semantic_data: SemanticData,
}

impl FunctionChecker {
    fn new(parent_data: &SemanticData, return_type: &SmplType) -> FunctionChecker {
        let mut data = parent_data.clone();
        data.is_loop = false;
        data.return_type = Some(return_type.clone());
        FunctionChecker {
            semantic_data: data,
        }
    }
}

impl StmtCk for FunctionChecker { 
    fn semantic_data(&self) -> &SemanticData {
        &self.semantic_data
    }

    fn semantic_data_mut(&mut self) -> &mut SemanticData {
        &mut self.semantic_data
    }
}

#[derive(Debug)]
pub struct LoopChecker {
    semantic_data: SemanticData,
}

impl LoopChecker {
    fn new(parent_data: &SemanticData) -> LoopChecker {
        let mut data = parent_data.clone();
        data.is_loop = true;
        LoopChecker {
           semantic_data: data, 
        }
    }
}

impl StmtCk for LoopChecker {
    fn semantic_data(&self) -> &SemanticData {
        &self.semantic_data
    }

    fn semantic_data_mut(&mut self) -> &mut SemanticData {
        &mut self.semantic_data
    }
}

pub trait StmtCk: Debug {

    fn accept_stmt(&mut self, stmt: &mut Stmt) -> Result<(), Err> {
        match *stmt {
            Stmt::ExprStmt(ref mut expr_stmt) => self.accept_expr_stmt(expr_stmt),
            Stmt::Expr(ref mut expr) => self.semantic_data().typify_expr(expr),
        }
    }

    fn semantic_data(&self) -> &SemanticData;
    fn semantic_data_mut(&mut self) -> &mut SemanticData;

    fn accept_expr_stmt(&mut self, expr_stmt: &mut ExprStmt) -> Result<(), Err> {
        match *expr_stmt {
            ExprStmt::LocalVarDecl(ref mut decl) => {
                /*
                 * 1) Get supposed type of new variable.
                 * 2) Typify init expr
                 * 3) Check if init expr type == variable type
                 * 4) Insert binding
                 */
                
                {
                    self.semantic_data().typify_expr(&mut decl.var_init)?;
                }

                let v_type = {
                    let v_type = {
                        match self.semantic_data().type_map.get(&decl.var_type) {
                            Some(t) => t,
                            None => unimplemented!("Failed to find [{:?}]", decl.var_type),
                            //TODO: Found out why Option::ok_or makes test_full_fn_type_check fail
                        }
                    };

                    if decl.var_init.d_type.as_ref() != Some(v_type) {
                       unimplemented!("LHS and RHS types do not match"); 
                    }
                    v_type.clone()
                };

                // Ignore any name overrides (ALLOW shadowing).
                self.semantic_data_mut().bind(decl.var_name.clone(), v_type);
            },

            ExprStmt::Assignment(ref mut asgmnt) => {
                /*
                 * 1) Retrieve type of binding.
                 * 2) Typify rhs expr
                 * 3) Check if lhs type == rhs type
                 */
                let destination_type = {
                    unimplemented!("Walk path; going from binding through struct defs");
                };

                self.semantic_data().typify_expr(&mut asgmnt.value)?;
                if Some(destination_type) != asgmnt.value.d_type.as_ref() {
                    unimplemented!("LHS and RHS types do not match");
                }
            },

            ExprStmt::If(ref mut if_stmt) => {
                /*
                 * 1) Make sure conditional expr is a boolean type
                 * 2) Generate new scope & typify block
                 */

                self.semantic_data().typify_expr(&mut if_stmt.conditional)?;
                if if_stmt.conditional.d_type != Some(SmplType::Bool) {
                    unimplemented!("Condition must evaluate to a boolean.");
                }

                let mut scoped_semantic_data = self.semantic_data().clone();
                let mut scoped_expector = LoopChecker::new(&mut scoped_semantic_data);
                for stmt in if_stmt.block.0.iter_mut() {
                    scoped_expector.accept_stmt(stmt)?;
                }
            },

            ExprStmt::While(ref mut while_stmt) => {
                /*
                 * 1) Make sure conditional expr is a boolean type
                 * 2) Generate new scope & typify block
                 */

                self.semantic_data().typify_expr(&mut while_stmt.conditional)?;
                if while_stmt.conditional.d_type != Some(SmplType::Bool) {
                    unimplemented!("Condition must evaluate to a boolean.");
                }

                let mut scoped_expector = LoopChecker::new(self.semantic_data());
                for stmt in while_stmt.block.0.iter_mut() {
                    scoped_expector.accept_stmt(stmt)?;
                }
            },

            ExprStmt::Continue => {
                if self.semantic_data().is_loop == false {
                    unimplemented!("Continue should only appear in loop");
                }
            },

            ExprStmt::Break => {
                if self.semantic_data().is_loop == false {
                    unimplemented!("Break should only appear in loop");
                }
            },

            ExprStmt::Return(ref mut expr) => {
                self.semantic_data().typify_expr(expr)?;
                if let Some(ref return_type) = self.semantic_data().return_type {
                    if expr.d_type.as_ref() != Some(return_type) {
                        unimplemented!("Return type [{:?}] does not match expr type [{:?}]", 
                                       expr.d_type,
                                       self.semantic_data().return_type.as_ref());
                    }
                } else {
                    panic!("Should have a return type");
                }
            },
        }

        Ok(())
    }
}

#[cfg(test)]
mod semantic_tests {
    use std::collections::HashMap;
    use super::*;
    use parser::*;

    #[test]
    fn test_typify_expr() {
        {
            let input = "123 - 532 / 2";
            let mut expr = AstNode::untyped(parse_Expr(input).unwrap());
            let sck = SemanticData::new();
            sck.typify_expr(&mut expr).unwrap();
            assert_eq!(expr.d_type, Some(SmplType::Int));
        }

        {
            let input = "true && (false == false)";
            let mut expr = AstNode::untyped(parse_Expr(input).unwrap());
            let sck = SemanticData::new();
            sck.typify_expr(&mut expr).unwrap();
            assert_eq!(expr.d_type, Some(SmplType::Bool));
        }
    }

    #[test]
    fn test_struct_type() {
        {
            let input = "struct Test { foo: int, bar: float }";
            let struct_def = parse_StructDecl(input).unwrap();
            let sck = SemanticData::new();
            
            let s_type = sck.gen_struct_type(&struct_def).unwrap();
            assert_eq!(s_type, StructType {
                name: ident!("Test"),
                fields: {
                    let mut map = HashMap::new();
                    map.insert(ident!("foo"), SmplType::Int);
                    map.insert(ident!("bar"), SmplType::Float);
                    map
                }
            });
            
        }
    }

    #[test]
    fn test_fn_type() {
        {
            let input = "fn test() {}";
            let fn_def = parse_FnDecl(input).unwrap();
            let sck = SemanticData::new();
            
            let fn_type = sck.gen_fn_type(&fn_def).unwrap();

            assert_eq!(fn_type, FunctionType {
                args: vec![],
                return_type: Box::new(SmplType::Unit)
            });
        }

        {
            let input = "fn test(int arg1, bool arg2) {}";
            let fn_def = parse_FnDecl(input).unwrap();
            let sck = SemanticData::new();
            
            let fn_type = sck.gen_fn_type(&fn_def).unwrap();
            assert_eq!(fn_type, FunctionType {
                args: vec![
                           SmplType::Int,
                           SmplType::Bool,
                ],

                return_type: Box::new(SmplType::Unit),
            });
        }

        {
            let input = "fn test(int arg1, bool arg2, float arg3) -> String {}";
            let fn_def = parse_FnDecl(input).unwrap();
            let sck = SemanticData::new();
            
            let fn_type = sck.gen_fn_type(&fn_def).unwrap();
            assert_eq!(fn_type, FunctionType {
                args: vec![
                           SmplType::Int,
                           SmplType::Bool,
                           SmplType::Float,
                ],

                return_type: Box::new(SmplType::String),
            });
        }
    }

    #[test]
    fn test_full_fn_type_check() {
        {
            let input =
"fn test(int arg) -> int {
    return arg;
}";
            let mut fn_def = parse_FnDecl(input).unwrap();
            let mut sck = SemanticData::new();
            sck.accept_fn_def(&mut fn_def).unwrap();
        }

        {
            let input =
"fn test(int arg) {
    int a = 2;
}";
            let mut fn_def = parse_FnDecl(input).unwrap();
            let mut sck = SemanticData::new();

            sck.accept_fn_def(&mut fn_def).unwrap();
        }

        {
            let input =
"fn test(int arg) -> int {
    int a = 2;
    return a;
}";
            let mut fn_def = parse_FnDecl(input).unwrap();
            let mut sck = SemanticData::new();

            sck.accept_fn_def(&mut fn_def).unwrap(); 
        }
    }
}
