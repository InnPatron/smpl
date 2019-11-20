[![Build Status](https://travis-ci.org/InnPatron/smpl.svg?branch=master)](https://travis-ci.org/InnPatron/smpl)

# SMPL
S(ome) M(ore) P(rogramming) L(anguage), pronounced 'simple.' 
## What is it?

SMPL is a simple statically typed programming language meant for easy embedding in [Rust](https://www.rust-lang.org/en-US/) and to write standalone programs.

## Motivation

SMPL was built to replace [Popstcl](https://github.com/InnPatron/Popstcl) as the scripting language of my choose-your-own-adventure engine [CYOA](https://github.com/InnPatron/cyoa).

## The Good

* Rust-like syntax.
* Statically typed
* Lexically scoped
* ~~Compiler with Rust code generator~~
* Embeddable (**asynchronous**) interpreter
* Function piping
* Width-based structural subtyping 
* Generics (with width-based structural constraints)

## The Bad
* Does match Rust 1:1
  * No move semantics
  * No concept of lifetime
* Bare-bones standard library

## Using the Code

The project is split it up into 2 parts:
* smpl
  * The core crate that defines the language
  * Includes:
    * The parser
    * The static analyzer
    * Byte code data structures 
    * Byte code generator
    * Metadata collector
* smpli
  * The interpreter for smpl's byte code
    * Creates an AVM from parsed and analyzed SMPL modules
    * Spawns instruction executors for SMPL functions 
    * Provides an interface for mapping Rust -> SMPL (builtin) functions
  * Runtime data structures

## Example

See `examples/tic-tac-toe` for more embedding examples.

### SMPL code

```
// In a file or as a String
mod test;

// From interpreter's stdlib 
use log;

struct Point3d {
    x: int,
    y: int,
    z: int,
}

fn modify2d(type P)(point: P, x: int, y: int) -> P 
    where P: { x: int, y: int } {

    point.x = x;
    point.y = y;

    return point;
}

fn getX(point: { x: int }) -> int {
    return point.x;
}

fn add(l: int, r: int) -> int {
    return l + r;
}

fn main() {
    let p = init Point {
        x: 0,
        y: 0,
        z: 0,
    };

    let p = modify2d(type Point)(p, 1, 2);

    // Should print '4'
    log::println(add(getX(p), 1) |> add(2));
}
```

### Rust Code

```
// Running the above SMPL code using the 'smpli' crate
let scripts = vec![
    // If you have a path, can use:
    //   parse_module(UnparsedModule::file(path, &str_buff))
    VmModule::new(
      parse_module(UnparsedModule::anonymous(module_string))
        .unwrap()
    )
];

let std = StdBuilder::default().log(true).build().unwrap();
let vm = AVM::new(std, scripts)?;

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
    }
};

```

## Running SMPL code.

The Rust backend is temporarily unsupported.

**SMPL is meant to be embedded in other Rust programs. The interpreter is the only method of SMPL code execution guaranteed to support ALL present and future language features.**

## License
Released under the [MIT License](https://opensource.org/licenses/MIT) (See LICENSE-MIT).
