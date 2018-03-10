/*
 * Sources:
 *
 *  1) System V Application Binary Interface
 *  AMD64 Architecture Processor Supplement
 *  Draft Version 0.99.6a
 *
 *  Alias: AMD64_ABI
 *
 *  2) Intel® 64 and IA-32 Architectures
 *  Software Developer’s Manual
 *  Combined Volumes:
 *  1, 2A, 2B, 2C, 2D, 3A, 3B, 3C, 3D and 4
 *
 *  Alias: INTEL_INSTR
 *
 *
 *
 *
 *
 *
 *
 *
 *
 */


use std::collections::{VecDeque, HashMap};

use petgraph::graph::NodeIndex;

use analysis::*;
use analysis::metadata::Metadata;

use ast::BinOp;
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

    data_map: HashMap<DataId, DataLocation<Register>>,

    register_allocator: RegisterAllocator,
    local_allocator: LocalAllocator,
    param_allocator: ParamAllocator,
}

impl<'a, 'b> x86_64FnGenerator<'a, 'b> {

    pub fn generate(id: FnId, meta: &Metadata, cfg: &CFG, context: &Context) -> x86_64Fn {

        // Stack grows down (high -> low)
        // [RBP + 0] is the old RBP
        // [RBP + 8] is the return address
        // Paramaters start at [RBP + 16]
        let param_start = 2 * POINTER_SIZE;

        let mut fn_gen = x86_64FnGenerator {
            id: id,
            prologue: StringEmitter::new(),
            body: StringEmitter::new(),
            epilogue: StringEmitter::new(),

            cfg: cfg,
            context: context,

            data_map: HashMap::new(),
            
            register_allocator: RegisterAllocator::new(),
            local_allocator: LocalAllocator::new(LOCAL_BLOCK_SIZE, MAX_BLOCKS),
            param_allocator: ParamAllocator::new(param_start),
        };

        let layout = meta.fn_layout(id);

        
        for &(var_id, type_id) in layout.params().into_iter().rev() {
            let layout = fn_gen.context.get_layout(type_id);
            let dl = fn_gen.param_allocator
                .alloc(layout.total_size());
            fn_gen.data_map.insert(var_id.into(), dl);
        }
        
        for &(var_id, type_id) in layout.locals() {
            let layout = fn_gen.context.get_layout(type_id);
            let dl = fn_gen.local_allocator
                .alloc(var_id, layout.total_size());

            fn_gen.data_map.insert(var_id.into(), dl);
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
            param_total: fn_gen.param_allocator.param_total,
        }
    }

    fn emit_prologue(&mut self) {

        let local_total = self.local_allocator.deepest_offset * LOCAL_BLOCK_SIZE;
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

    fn allocate_tmp<T: Into<DataId> + Copy>(&mut self, id: T, size: usize) -> DataLocation<Register> {

        if size <= REGISTER_SIZE {
            if let Some(r) = self.register_allocator.alloc() {
                self.data_map.insert(id.into(), r);

                return r;
            }
        }

        self.local_allocator.alloc(id, size)
    }

    fn deallocate_tmp<T: Into<DataId> + Copy>(&mut self, id: T) {
        let id = id.into();
        let dl = self.data_map.remove(&id).unwrap().clone();

        match dl {
            DataLocation::Param(_) => unreachable!("Attempting to deallocate a temp value."),
            DataLocation::Local(_) => self.local_allocator.dealloc(id),
            DataLocation::Register(r) => self.register_allocator.dealloc(r),
        }
    }


    fn locate_data<T: Into<DataId>>(&self, id: T) -> DataLocation<Register> {
        let id = id.into();
        
        self.data_map.get(&id).unwrap().clone()
    }

    fn emit_expr(&mut self, expr: &Expr) {
        // TMPS form a tree
        let root_id = expr.execution_order().rev().next();
        let root = expr.get_tmp(*root_id.unwrap());
        let root_layout = self.context.get_layout(root.value().type_id().unwrap());

        // Should never collide b/c TmpId's are made VERY late.
        // HOPEFULLY
        let throw_away_loc = self.allocate_tmp(DataId::dummy(), root_layout.total_size());
        self.emit_tmp(expr, root, DataLocation::Register(Register::RAX));

        self.deallocate_tmp(DataId::dummy());
    }

    fn emit_assignment(&mut self, var: VarId, expr: &Expr) {
        // TMPS form a tree
        let root_id = expr.execution_order().rev().next();
        let root = expr.get_tmp(*root_id.unwrap());
        let root_layout = self.context.get_layout(root.value().type_id().unwrap());

        let var_dl = self.locate_data(var);
        self.emit_tmp(expr, root, var_dl);
    }

    ///
    /// result_loc: Where caller wants the result
    fn emit_tmp(&mut self, expr: &Expr, tmp: &Tmp, result_loc: DataLocation<Register>) {
        let tmp_to_assign = tmp.id();
        let value = tmp.value();

        match *value.data() {
            Value::Literal(ref lit) => match *lit {
                Literal::String(_) => unimplemented!("Strings not supported"),
                Literal::Int(int) => {
                    self.body.emit_line(&mov!(location!(result_loc), int));
                }
                Literal::Float(float) => {
                    // TODO: Floating points use different instructions and registers
                    // INTEL_INSTR 887 (vol. 2a)
                    self.body.emit_line(&mov!(location!(result_loc), float));
                }
                Literal::Bool(boolean) => {
                    let value = if boolean {
                        TRUE
                    } else {
                        FALSE
                    };
                    self.body.emit_line(&mov!(location!(result_loc), value));
                }
            }

            Value::Variable(ref var) => {
                let id = var.get_id().unwrap();
                let var_loc = self.locate_data(id);
                self.body.emit_line(&mov!(location!(result_loc), location!(var_loc)));

                // TODO: Floating points use different instructions and registers
                // INTEL_INSTR 887 (vol. 2a)
            }

            Value::BinExpr(ref op, ref lhs, ref rhs) => {
                let rhs_type_id = rhs.type_id().unwrap();
                let rhs_layout = self.context.get_layout(rhs_type_id);

                let lhs = expr.get_tmp(*lhs.data());
                let rhs = expr.get_tmp(*rhs.data());
                
                // result_loc should be of the correct size
                let lhs_loc = result_loc;
                let rhs_loc = self.allocate_tmp(rhs.id(), rhs_layout.total_size());

                self.emit_tmp(expr, lhs, lhs_loc);
                self.emit_tmp(expr, rhs, rhs_loc);

                self.emit_bin_expr(op.clone(), lhs_loc, rhs_loc);

                self.deallocate_tmp(rhs.id());

                // TODO: Floating points use different instructions and registers
                // INTEL_INSTR 887 (vol. 2a)
            }

            _ => unimplemented!(),
        }
    }

    fn emit_bin_expr(&mut self, op: BinOp, lhs: DataLocation<Register>, rhs: DataLocation<Register>) {

        // TODO: Floating points use different instructions and registers
        // INTEL_INSTR 887 (vol. 2a)
        let op = bin_op!(op);

        // Op instructions cannot have both operands as memory locations
        if lhs.is_register() || rhs.is_register() {
            // At least one op is NOT a memory location
            self.body.emit_line(&format!("{} {}, {}", op, location!(lhs), location!(rhs)));
        } else {
            // Move lhs to rax to make one operand a register and then
            // Update lhs with rax value
            self.body.emit_line(&mov!("rax", location!(lhs)));
            self.body.emit_line(&format!("{} {}, {}", op, "rax", location!(rhs)));
            self.body.emit_line(&mov!(location!(lhs), "rax"));
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

struct LocalAllocator {
    storage: VecDeque<Block>,
    map: HashMap<DataId, Vec<Block>>,
    deepest_offset: usize,               // Used to track how much TOTAL stack space to allocate
    block_size: usize,
}

impl LocalAllocator {
    fn new(block_size: usize, blocks: usize) -> LocalAllocator {
        let mut s = VecDeque::new();

        // Begin at 1; stack grows DOWN
        // Locals/temps are placed top-down but read down-top
        // First variable will be at RBP-8
        for i in 1..blocks+1 {
            s.push_back(Block::new(i));
        }

        LocalAllocator {
            storage: s,
            map: HashMap::new(),
            deepest_offset: 0,
            block_size: block_size,
        }
    }

    fn dealloc<T: Into<DataId>>(&mut self, id: T) {
        let memory = self.map.remove(&id.into())
            .expect("Attempting to deallocate nonexistant local.");

        // Insert memory blocks to the head in reverse order.
        // Memory block closest to RBP should be closer to head.
        // Storage should probably be a different data type (like a BST),
        // but a Queue is fine b/c most memory will allocated and deallocated
        // from the front.
        for b in memory.into_iter().rev() {
            self.storage.push_front(b);
        }
    }

    fn alloc<T: Into<DataId>>(&mut self, id: T, size: usize) -> DataLocation<Register> {
        let block_min = size / self.block_size;
        let excess = size % self.block_size;

        let mut blocks_required = block_min;
        if excess > 0 {
            blocks_required += 1;
        }

        let mut start = None;
        let mut to_allocate = 0;
        let mut deepest_offset = 0;
        for (current, ref current_block) in self.storage.iter().enumerate() {
            if start.is_none() {
                start = Some(current);
                to_allocate = 1;
                deepest_offset = current_block.offset;
            } else if start.is_some() {
                if to_allocate == blocks_required {
                    break;
                }

                let previous_block = self.storage.get(current - 1).unwrap();
                if previous_block.offset + 1 == current_block.offset {
                    to_allocate += 1;
                } else {
                    start = Some(current);
                    to_allocate = 1;
                }

                deepest_offset = current_block.offset;
            }
        }

        if to_allocate != blocks_required {
            panic!("Unable to allocate enough blocks (of size {}) to fit {}. Allocate more blocks?",
            self.block_size, size);
        }

        if self.deepest_offset < deepest_offset {
            self.deepest_offset = deepest_offset;
        }

        let start_block_index = start.unwrap();

        // Start of variable in memory
        let start_stack_offset = {
            self.storage.get(start_block_index).unwrap().offset
        };

        // Remove memory blocks
        let allocated = self.storage
            .drain(start_block_index..start_block_index + blocks_required)
            .collect::<Vec<_>>();

        // Map allocated memory to a local / temporary
        self.map.insert(id.into(), allocated);

        DataLocation::Local(start_stack_offset * self.block_size)
    }
}

struct Block {
    offset: usize
}

impl Block {
    fn new(offset: usize) -> Block {
        Block {
            offset: offset
        }
    }

    fn offset(&self) -> usize {
        self.offset
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

    fn alloc(&mut self) -> Option<DataLocation<Register>> {
        self.registers.pop().map(|r| DataLocation::Register(r))
    }

    fn dealloc(&mut self, r: Register) {
        if self.registers.contains(&r) {
            panic!("Attempting to deallocate register {} when it is already available", r);
        }

        self.registers.push(r);
    }
}

struct ParamAllocator {
    param_total: usize,
    param_tracker: usize,
}

impl ParamAllocator {

    fn new(start_offset: usize) -> ParamAllocator {
        ParamAllocator {
            param_total: 0,
            param_tracker: start_offset,
        }
    }

    fn alloc(&mut self, size: usize) -> DataLocation<Register> {
        self.param_total += size;
        let loc = DataLocation::Local(self.param_tracker);
        self.param_tracker += size;

        loc
    }
}
