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

    prologue: String,
    body: String,
    epilogue: String,

    prologue_shift: u32,
    body_shift: u32,
    epilogue_shift: u32,

    cfg: &'a CFG,
    context: &'b Context,

    param_map: HashMap<VarId, usize>,   // Above RBP
    local_map: HashMap<VarId, usize>,   // Below RBP

    param_total: usize,
    local_total: usize,
    param_tracker: usize,
    local_tracker: usize,
}

impl<'a, 'b> x86_64FnGenerator<'a, 'b> {

    pub fn generate(id: FnId, meta: &Metadata, cfg: &CFG, context: &Context) -> x86_64Fn {
        let mut fn_gen = x86_64FnGenerator {
            id: id,
            prologue: String::new(),
            body: String::new(),
            epilogue: String::new(),

            prologue_shift: 0,
            body_shift: 0,
            epilogue_shift: 0,

            cfg: cfg,
            context: context,
            param_map: HashMap::new(),
            local_map: HashMap::new(),

            param_total: 0,
            local_total: 0,
            param_tracker: 0,
            local_tracker: 0,
        };

        let layout = meta.fn_layout(id);

        // Stack grows down (high -> low)
        
        // [RBP + 0] is the old RBP
        // [RBP + 8] is the return address
        // Paramaters start at [RBP + 16]
        // Data read/written low -> high (so old RBP is [RBP + 0] to execlusive [RBP + 8])
        let mut param_tracker = 2 * POINTER_SIZE;
        for &(var_id, type_id) in layout.params().into_iter().rev() {
            let layout = fn_gen.context.get_layout(type_id);
            fn_gen.allocate_param(var_id, layout.total_size());
        }

        
        for &(var_id, type_id) in layout.locals() {
            let layout = fn_gen.context.get_layout(type_id);
            fn_gen.allocate_local(var_id, layout.total_size());
        }

        {
            let traverser = Traverser::new(cfg, &mut fn_gen);
            traverser.traverse();
        }


        fn_gen.emit_prologue();
        fn_gen.emit_epilogue();


        let mut output = String::new();
        output.push_str(&fn_gen.prologue);
        output.push_str(&fn_gen.body);
        output.push_str(&fn_gen.epilogue);

        x86_64Fn {
            output: output,
            param_total: fn_gen.param_total,
        }
    }

    fn emit_prologue(&mut self) {
        let id = self.id;
        self.emit_line(&format!("{}:", fn_id(id)), EmitLoc::Prologue);
        self.shift_right(EmitLoc::Prologue);

        // Save the stack base pointer
        self.emit_line("push rbp", EmitLoc::Prologue);

        // New stack base pointer
        self.emit_line("mov rbp, rsp", EmitLoc::Prologue);
    }

    fn emit_epilogue(&mut self) {
        // Get the previous stack base pointer
        self.emit_line("pop rbp", EmitLoc::Epilogue);

        self.emit_line("ret", EmitLoc::Epilogue);
    }

    fn allocate_param(&mut self, id: VarId, size: usize) {
        self.param_total += size;
        self.param_map.insert(id, self.param_tracker);
        self.param_tracker += size;
    }

    fn allocate_local(&mut self, id: VarId, size: usize) {
        // [RBP + 0] is the old RBP
        // Data read/written low -> high
        // First parameter is thus [RBP - Size] to exclusive [RBP + 0]

        self.local_total += size;
        self.local_tracker += size;
        self.local_map.insert(id, self.local_tracker);
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

    fn shift_left(&mut self, loc: EmitLoc) {
        use self::EmitLoc::*;
        match loc {
            Prologue => {
                if self.prologue_shift > 0 {
                    self.prologue_shift -= 1;
                }
            }

            Body => {
                if self.body_shift > 0 {
                    self.body_shift -= 1;
                }
            }

            Epilogue => {
                if self.epilogue_shift > 0 {
                    self.epilogue_shift -= 1;
                }
            }
        }
    }

    fn shift_right(&mut self, loc: EmitLoc) {
        use self::EmitLoc::*;
        match loc {
            Prologue => {
                if self.prologue_shift < u32::max_value() {
                    self.prologue_shift += 1;
                }
            }

            Body => {
                if self.body_shift < u32::max_value(){
                    self.body_shift += 1;
                }
            }

            Epilogue => {
                if self.epilogue_shift < u32::max_value() {
                    self.epilogue_shift += 1;
                }
            }
        }
    }

    fn padding(&mut self, loc: EmitLoc) {
        use self::EmitLoc::*;
        match loc {
            Prologue => {
                for _ in 0..self.prologue_shift {
                    self.prologue.push('\t');
                }
            }

            Body => {
                for _ in 0..self.body_shift {
                    self.body.push('\t');
                }
            }

            Epilogue => {
                for _ in 0..self.epilogue_shift {
                    self.epilogue.push('\t');
                }
            }
        }
    }

    fn emit_line(&mut self, str: &str, loc: EmitLoc) {
        self.padding(loc);

        use self::EmitLoc::*;
        match loc {
            Prologue => {
                self.prologue.push_str(str);
                self.prologue.push('\n');
            }

            Body => {
                self.body.push_str(str);
                self.body.push('\n');
            }

            Epilogue => {
                self.epilogue.push_str(str);
                self.epilogue.push('\n');
            }
        }
    }

    fn emit(&mut self, str: &str, loc: EmitLoc) {
        self.padding(loc);

        use self::EmitLoc::*;
        match loc {
            Prologue => {
                self.prologue.push_str(str);
            }

            Body => {
                self.body.push_str(str);
            }

            Epilogue => {
                self.epilogue.push_str(str);
            }
        }
    }
}

impl<'a, 'b> Passenger<()> for x86_64FnGenerator<'a, 'b> {
    fn start(&mut self, id: NodeIndex) -> Result<(), ()> {
        // Do nothing
        Ok(())
    }

    fn end(&mut self, id: NodeIndex) -> Result<(), ()> {
        // Do nothing
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

#[derive(Clone, Copy)]
enum EmitLoc {
    Prologue,
    Body,
    Epilogue,
}
