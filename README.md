# SMPL

S(ome) M(ore) P(rogramming) L(anguage), pronounced 'simple.'

## What is it?

SMPL is a simple statically typed programming language meant for easy embedding in [Rust](https://www.rust-lang.org/en-US/) and to write standalone programs.

## Motivation

SMPL was built to replace [Popstcl](https://gitlab.com/Random_Civvy/popstcl) as the scripting language of my choose-your-own-adventure engine [CYOA](https://gitlab.com/Random_Civvy/cyoa).

Popstcl has dynamic types and dynamic scoping, all of which I found painful to use.

## The Good

* Rust-like syntax.
* Statically typed
* Lexically scoped
* Compiler with Rust code generator

## The Bad
* Types and functions are brought into scope top-to-bottom.
* A lot of unimplemented features
* Does not have the same semantics as Rust code
* * No move semantics
* * No concept of lifetime
* No standard library

## Example

```
struct Point {
    x: i32,
    y: i32,
}

fn default_point_x() -> i32 {
    return 100;
}

fn main() {
    let a: i32 = default_point_x();
    let b: i32 = 10;
    
    let p: Point = init Point {
        x: a,
        y: b,
    };
    
    let result: String = "Success";

    if a != p.x {
        result = "Failure";
    } elif b != p.x {
		result = "Failure";
	} else {
		// Success
	}
}


```

## Running SMPL code.

Currently, SMPL only compiles into Rust (no interpreter or other backends). The easiest way to run SMPL code is to compile it and put it into a Cargo project (see smpl-tests/bin_test.sh)

```
smplc -i INPUT_FILE -o OUTPUT_DIR -b 0
```

The `-b` flag stands for backend. The Rust backend is '0'.

## Build instructions

Install [Rust and Cargo](https://www.rust-lang.org/en-US/). Requires version 1.23+

```
cargo build
cargo run
```

## Feature Backlog

1. ~~Arrays~~
2. Resizable arrays
3. Pointers
4. First-class functions
5. Interpreter
6. More code generators (LLVM, x86_64 ASM).
7. Implement optimizations (constant folding, dead code elimination, etc.)
8. Integrate [Cycle-collecting reference counters](https://gitlab.com/Random_Civvy/cc) for garbage collection.
9. Compiler commands
10. ~~Modules~~
11. Error messages
12. Number casting between ints and floats

## License
Released under the [MIT License](https://opensource.org/licenses/MIT) (See LICENSE-MIT). Good luck using this for any commercial projects :|
