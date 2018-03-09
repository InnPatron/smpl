use std::collections::HashMap;

use petgraph::graph::NodeIndex;

use analysis::*;
use analysis::metadata::Metadata;

use super::*;
use super::nasm_const::*;
use super::x86_64_gen::*;
use code_gen::StringEmitter;

pub struct x86_64Fn {
    output: String,
    param_total: usize,
}

pub struct x86_64FnGenerator<'a, 'b> {
    id: FnId,

    prologue: StringEmitter,
    body: StringEmitter,
    epilogue: StringEmitter,

    cfg: &'a CFG,
    context: &'b Context,

    param_map: HashMap<DataId, usize>,   // Above RBP
    local_map: HashMap<DataId, usize>,   // Below RBP
    register_map: HashMap<DataId, Register>,

    register_allocator: RegisterAllocator,

    param_total: usize,
    local_total: usize,
    param_tracker: usize,
    local_tracker: usize,
}

impl<'a, 'b> x86_64FnGenerator<'a, 'b> {

    pub fn generate(id: FnId, meta: &Metadata, cfg: &CFG, context: &Context) -> x86_64Fn {
        let mut fn_gen = x86_64FnGenerator {
            id: id,
            prologue: StringEmitter::new(),
            body: StringEmitter::new(),
            epilogue: StringEmitter::new(),

            cfg: cfg,
            context: context,
            param_map: HashMap::new(),
            local_map: HashMap::new(),
            register_map: HashMap::new(),

            
            register_allocator: RegisterAllocator::new(),

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
        output.push_str(&fn_gen.prologue.output());
        output.push_str(&fn_gen.body.output());
        output.push_str(&fn_gen.epilogue.output());

        x86_64Fn {
            output: output,
            param_total: fn_gen.param_total,
        }
    }

    fn emit_prologue(&mut self) {

        let local_total = self.local_total;
        let id = self.id;

        self.prologue.emit_line(&format!("{}:", fn_id(id)));
        self.prologue.shift_right();
        // Save the stack base pointer
        self.prologue.emit_line("push rbp");

        // New stack base pointer
        self.prologue.emit_line("mov rbp, rsp");

        // Allocate stack space for local variables
        self.prologue.emit_line(&format!("sub rsp, {}", local_total));
    }

    fn emit_epilogue(&mut self) {
        // Clear local variables by resetting stack pointer back to base
        self.epilogue.emit_line("mov rsp, rsb");

        // Get the previous stack base pointer
        self.epilogue.emit_line("pop rbp");

        self.epilogue.emit_line("ret");
    }

    fn allocate_param<T: Into<DataId>>(&mut self, id: T, size: usize) -> DataLocation<Register> {
        self.param_total += size;

        let loc = DataLocation::Local(self.local_tracker);

        self.param_map.insert(id.into(), self.param_tracker);
        self.param_tracker += size;

        loc
    }

    fn allocate_local<T: Into<DataId>>(&mut self, id: T, size: usize) -> DataLocation<Register> {
        // [RBP + 0] is the old RBP
        // Data read/written low -> high
        // First parameter is thus [RBP - Size] to exclusive [RBP + 0]

        self.local_total += size;
        self.local_tracker += size;
        self.local_map.insert(id.into(), self.local_tracker);

        DataLocation::Local(self.local_tracker)
    }

    fn allocate_tmp<T: Into<DataId> + Copy>(&mut self, id: T, size: usize) -> DataLocation<Register> {

        if size <= REGISTER_SIZE {
            if let Some(r) = self.register_allocator.alloc() {
                self.register_map.insert(id.into(), r);

                return DataLocation::Register(r);
            }
        }

        self.allocate_local(id, size)
    }

    fn remap_register<T: Into<DataId> + Copy>(&mut self, old: T, new: T) {
        let register = self.register_map.remove(&old.into()).unwrap();
        self.register_map.insert(new.into(), register);
    }

    fn locate_data<T: Into<DataId>>(&self, id: T) -> DataLocation<Register> {
        let id = id.into();
        if self.param_map.contains_key(&id) && self.local_map.contains_key(&id) {
            panic!("{} was found in both the parameter and local stack mappings", id);
        }

        if self.param_map.contains_key(&id) {
            DataLocation::Param(*self.param_map.get(&id).unwrap())
        } else if self.param_map.contains_key(&id) {
            DataLocation::Local(*self.local_map.get(&id).unwrap())
        } else {
            DataLocation::Register(*self.register_map.get(&id).unwrap())
        }
    }

    fn emit_expr(&mut self, e: &Expr) -> TmpId {
        unimplemented!()
    }

    fn emit_tmp(&mut self, tmp: &Tmp, result_loc: DataLocation<Register>) {
        let tmp_to_assign = tmp.id();
        let value = tmp.value();

        match *value.data() {
            Value::Literal(ref lit) => match *lit {
                Literal::String(_) => unimplemented!("Strings not supported"),
                Literal::Int(int) => mov!(self, result_loc, int),
                Literal::Float(float) => mov!(self, result_loc, float),
                Literal::Bool(boolean) => {
                    let value = if boolean {
                        TRUE
                    } else {
                        FALSE
                    };

                    mov!(self, result_loc, value);
                }
            }
            _ => unimplemented!(),
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

struct RegisterAllocator {
    registers: Vec<Register>,
}

impl RegisterAllocator {
    fn new() -> RegisterAllocator {
        RegisterAllocator {
            registers: vec![
                            Register::R8,
                            Register::R9,
                            Register::R10,
                            Register::R11,
                            Register::R12,
                            Register::R13,
                            Register::R14,
                            Register::R15,
            ]
        }
    }

    fn alloc(&mut self) -> Option<Register> {
        self.registers.pop()
    }

    fn dealloc(&mut self, r: Register) {
        if self.registers.contains(&r) {
            panic!("Attempting to deallocate register {} when it is already available", r);
        }

        self.registers.push(r);
    }
}
