extern crate smpl;

mod rt;

use smpl::interpreter::AVM;



fn main() {
    let mut scripts = Vec::new();
    rt::include(&mut scripts);
    let mut vm = AVM::new(scripts).unwrap();
    rt::map_builtins(&mut vm);
    let fn_handle = vm.query_module("rt", "run").unwrap().unwrap();
    vm.eval_fn_sync(fn_handle).unwrap();
}
