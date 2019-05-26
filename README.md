# SMPL
S(ome) M(ore) P(rogramming) L(anguage), pronounced 'simple.' 
## What is it?

SMPL is a simple statically typed programming language meant for easy embedding in [Rust](https://www.rust-lang.org/en-US/) and to write standalone programs.

## Motivation

SMPL was built to replace [Popstcl](https://github.com/InnPatron/Popstcl) as the scripting language of my choose-your-own-adventure engine [CYOA](https://github.com/InnPatron/cyoa).

Popstcl has dynamic types and dynamic scoping, all of which I found painful to use.

## The Good

* Rust-like syntax.
* Statically typed
* Lexically scoped
* ~~Compiler with Rust code generator~~
* Embeddable (**asynchronous!**) interpreter
* SMPL code is sandboxed (?)
* Function piping
* Width-based structural subtyping 
* Generics (with width-based structural constraints)

## The Bad
* ~~Types and functions are brought into scope top-to-bottom.~~ Declarations can be in any order
* ~~A lot of unimplemented features~~ It works...
* Does not have the same semantics as Rust code
* * No move semantics
* * No concept of lifetime
* ~~No~~ Bare-bones standard library for the interpreter

## Example

```
mod test;

// From interpreter's stdlib 
use log;

struct Point {
    x: int,
    y: int,
}

fn default_point_x() -> int {
    return 100;
}

fn add(l: int, r: int) -> int {
    return l + r;
}

fn main() {
    let a = default_point_x();
    let b = 10;
    
    let p = init Point {
        x: a,
        y: b,
    };
    
    let result: String = "Success";

    if a != p.x {
        result = "Failure";
    } elif b != p.x {
        result = "Failure";
    }

    log::println(result);

    log::println(add(a, 1) |> add(2));
}


```

## Running SMPL code.

The Rust backend is temporarily unsupported.

**SMPL is meant to be embedded in other Rust programs. The interpreter is the only method of SMPL code execution guaranteed to support ALL language features.**

**SMPL now has an asynchronous interpreter (the AVM). The AVM also exposes a synchrnous interface.**

## Build instructions

Install [Rust and Cargo](https://www.rust-lang.org/en-US/). Requires version 1.26+

```
cargo build
cargo run
```

## License
Released under the [MIT License](https://opensource.org/licenses/MIT) (See LICENSE-MIT).
