use std::process;

mod rt;

use smpl::interpreter::AVM;

fn main() {
    let mut scripts = vec![
        rt::vm_module(), 
        smpl::interpreter::builtins::log::vm_module()
    ];
    let mut vm = match AVM::new(scripts) {
        Ok(vm) => vm,

        Err(e) => {
            println!("{:?}", e);
            process::exit(1);
        }
    };

    let fn_handle = vm.query_module("rt", "run").unwrap().unwrap();
    vm.eval_fn_sync(fn_handle).unwrap();
}
