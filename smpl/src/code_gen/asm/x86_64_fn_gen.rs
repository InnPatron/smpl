use petgraph::graph::NodeIndex;

use analysis::{Traverser, Passenger};
use analysis::{CFG, Node, Expr, LocalVarDecl, Assignment, FnId};

use super::fn_id;
use super::x86_64_gen::*;

pub struct x86_64FnGenerator<'a> {
    id: FnId,
    output: String,
    shift: u32,
    cfg: &'a CFG,
}

impl<'a> x86_64FnGenerator<'a> {
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

impl<'a> Passenger<()> for x86_64FnGenerator<'a> {
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
