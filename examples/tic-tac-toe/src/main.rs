use std::process;

mod rt;

use smpl::interpreter::*;

fn main() {
    let mut options = StdOptions::no_std();
    options.log = true;

    let loader = Loader::new(options)
        .add_module(Box::new(rt::include), Some(Box::new(rt::map_builtins)));
    let mut vm = match AVM::new(&loader) {
        Ok(vm) => vm,

        Err(e) => {
            println!("{:?}", e);
            process::exit(1);
        }
    };
    rt::map_builtins(&mut vm);
    let fn_handle = vm.query_module("rt", "run").unwrap().unwrap();
    vm.eval_fn_sync(fn_handle).unwrap();
}
