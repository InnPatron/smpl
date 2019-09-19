use std::process;

mod rt;

use smpli::*;

fn main() {
    let scripts = vec![
        rt::vm_module(), 
    ];

    let std = StdBuilder::default().log(true).build().unwrap();
    let vm = match AVM::new(std, scripts) {
        Ok(vm) => vm,

        Err(e) => {
            println!("{:?}", e);
            process::exit(1);
        }
    };

    let fn_handle = vm.query_module("rt", "run").unwrap().unwrap();

    let executor = match vm
        .spawn_executor(fn_handle, None, SpawnOptions {
            type_check: false
        }) {

        Ok(executor) => executor,

        Err(e) => {
            println!("{:?}", e);
            process::exit(1);
        }

    };

    let _result = match executor.execute_sync() {

        Ok(val) => val,

        Err(e) => {
            println!("{:?}", e);
            process::exit(1);
        }
    };
}
