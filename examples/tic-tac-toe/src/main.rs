use std::process;

mod rt;

use smpl::interpreter::*;

fn main() {
    let scripts = vec![
        rt::vm_module(), 
    ];

    let std = StdBuilder::default().log(true).build().unwrap();
    let mut vm = match AVM::new(std, scripts) {
        Ok(vm) => vm,

        Err(e) => {
            println!("{:?}", e);
            process::exit(1);
        }
    };

    let fn_handle = vm.query_module("rt", "run").unwrap().unwrap();
    vm.eval_fn_sync(fn_handle).unwrap();
}
