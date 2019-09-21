[![Build Status](https://travis-ci.org/InnPatron/smpl.svg?branch=master)](https://travis-ci.org/InnPatron/smpl)

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
  * No move semantics
  * No concept of lifetime
* ~~No~~ Bare-bones standard library for the interpreter

## Example

```
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
