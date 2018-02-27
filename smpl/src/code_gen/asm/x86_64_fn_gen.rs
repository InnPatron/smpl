use std::collections::HashMap;

use petgraph::graph::NodeIndex;

use analysis::{Traverser, Passenger};
use analysis::{CFG, Node, Expr, LocalVarDecl, Assignment, FnId, VarId, TypeId};
use analysis::metadata::Metadata;

use super::fn_id;
use super::x86_64_gen::*;

pub struct x86_64Fn {
    output: String,
    param_total: usize,
}

pub struct x86_64FnGenerator<'a, 'b> {
    id: FnId,
    output: String,
    shift: u32,
    cfg: &'a CFG,
    context: &'b Context,

    param_map: HashMap<VarId, usize>,   // Above RBP
    local_map: HashMap<VarId, usize>,   // Below RBP
}

impl<'a, 'b> x86_64FnGenerator<'a, 'b> {

    pub fn generate(id: FnId, meta: &Metadata, cfg: &CFG, context: &Context) -> x86_64Fn {
        let mut fn_gen = x86_64FnGenerator {
            id: id,
            output: String::new(),
            shift: 0,
            cfg: cfg,
            context: context,
            param_map: HashMap::new(),
            local_map: HashMap::new(),
        };

        let layout = meta.fn_layout(id);

        let mut param_total = 0;

        // Stack grows down (high -> low)
        
        // [RBP + 0] is the old RBP
        // [RBP + 8] is the return address
        // Paramaters start at [RBP + 16]
        // Data read/written low -> high (so old RBP is [RBP + 0] to execlusive [RBP + 8])
        let mut param_tracker = 2 * POINTER_SIZE;
        for &(var_id, type_id) in layout.params().into_iter().rev() {
            let layout = fn_gen.context.get_layout(type_id);
            param_total += layout.total_size();

            fn_gen.param_map.insert(var_id, param_tracker);
            param_tracker += layout.total_size();
        }

        // [RBP + 0] is the old RBP
        // Data read/written low -> high
        // First parameter is thus [RBP - Size] to exclusive [RBP + 0]
        let mut var_tracker = 0;
        for &(var_id, type_id) in layout.locals() {
            let layout = fn_gen.context.get_layout(type_id);
            let param_size = layout.total_size();

            var_tracker += layout.total_size();
            fn_gen.local_map.insert(var_id, var_tracker);
        }

        {
            let traverser = Traverser::new(cfg, &mut fn_gen);
            traverser.traverse();
        }

        x86_64Fn {
            output: fn_gen.output,
            param_total: param_total
        }
    }

    fn locate_stack_data(&self, id: VarId) -> StackData {
        if self.param_map.contains_key(&id) && self.local_map.contains_key(&id) {
            panic!("{} was found in both the parameter and local stack mappings", id);
        }

        if self.param_map.contains_key(&id) {
            StackData::Param(*self.param_map.get(&id).unwrap())
        } else {
            StackData::Local(*self.local_map.get(&id).unwrap())
        }
            
    }

    fn shift_left(&mut self) {
        if self.shift > 0 {
            self.shift -= 1;
        }
    }

    fn shift_right(&mut self) {
        if self.shift < <u32>::max_value() {
            self.shift += 1;
        }
    }

    fn padding(&mut self) {
        for _ in 0..self.shift {
            self.output.push('\t');
        }
    }

    fn emit_line(&mut self, str: &str) {
        self.padding();
        self.output.push_str(str);
        self.output.push('\n');
    }

    fn emit(&mut self, str: &str) {
        self.padding();
        self.output.push_str(str);
    }
}

impl<'a, 'b> Passenger<()> for x86_64FnGenerator<'a, 'b> {
    fn start(&mut self, id: NodeIndex) -> Result<(), ()> {
        let id = self.id;
        self.emit_line(&format!("{}:", fn_id(id)));
        self.shift_right();

        // Save the stack base pointer
        self.emit_line("push rbp");

        // New stack base pointer
        self.emit_line("mov rbp, rsp");
        Ok(())
    }

    fn end(&mut self, id: NodeIndex) -> Result<(), ()> {
        // Get the previous stack base pointer
        self.emit_line("pop rbp");

        self.emit_line("ret");
        Ok(())
    }

    fn branch_merge(&mut self, id: NodeIndex) -> Result<(), ()> {
        unimplemented!();
    }

    fn loop_head(&mut self, id: NodeIndex) -> Result<(), ()> {
        unimplemented!();
    }

    fn loop_foot(&mut self, id: NodeIndex) -> Result<(), ()> {
        unimplemented!();
    }

    fn cont(&mut self, id: NodeIndex) -> Result<(), ()> {
        unimplemented!();
    }

    fn br(&mut self, id: NodeIndex) -> Result<(), ()> {
        unimplemented!();
    }

    fn enter_scope(&mut self, id: NodeIndex) -> Result<(), ()> {
        // Do nothing
        Ok(())
    }

    fn exit_scope(&mut self, id: NodeIndex) -> Result<(), ()> {
        // Do nothing
        Ok(())
    }

    fn local_var_decl(&mut self, id: NodeIndex, decl: &LocalVarDecl) -> Result<(), ()> {
        unimplemented!()
    }

    fn assignment(&mut self, id: NodeIndex, assign: &Assignment) -> Result<(), ()> {
        unimplemented!()
    }

    fn expr(&mut self, id: NodeIndex, expr: &Expr) -> Result<(), ()> {
        unimplemented!()
    }

    fn ret(&mut self, id: NodeIndex, expr: Option<&Expr>) -> Result<(), ()> {
        unimplemented!()
    }

    fn loop_condition(&mut self, id: NodeIndex, e: &Expr) -> Result<(), ()> {
        unimplemented!()
    }

    fn loop_start_true_path(&mut self, id: NodeIndex) -> Result<(), ()> {
        // Do nothing
        Ok(())
    }

    fn loop_end_true_path(&mut self, id: NodeIndex) -> Result<(), ()> {
        // Do nothing
        Ok(())
    }

    fn branch_condition(&mut self, id: NodeIndex, e: &Expr) -> Result<(), ()> {
        unimplemented!()
    }

    fn branch_start_true_path(&mut self, id: NodeIndex) -> Result<(), ()> {
        // Do nothing
        Ok(())
    }

    fn branch_start_false_path(&mut self, id: NodeIndex) -> Result<(), ()> {
        // Do nothing
        Ok(())
    }

    fn branch_end_true_path(&mut self, id: NodeIndex) -> Result<(), ()> {
        // Do nothing
        Ok(())
    }

    fn branch_end_false_path(&mut self, id: NodeIndex) -> Result<(), ()> {
        // Do nothing
        Ok(())
    }
}

enum StackData {
    Local(usize),   // Below RBP
    Param(usize),   // Above RBP
}
