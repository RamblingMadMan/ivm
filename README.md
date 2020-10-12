# InfinityVM

> WARNING: this repo is under heavy and sporadic development, errors will occur and everything is subject to change.

InfinityVM compiles portable bytecode to machine instructions. It can be used for AOT and JIT compilation of programs.

IVM bytecode is intended to be a portable way to store the exact final representation of a program.

IVM is designed with the [Lemni programming language](https://github.com/RamblingMadMan/lemni) compiler in mind, but is general purpose and can be used for any runtime code-gen task.

### Features

- C11 API
- Minimal dynamic allocation
- Portable bytecode
- Intermediate representation (IR)

## How it works

The compiler directly emits machine code for the target platform/architecture and returns a pointer to the code. The code pointed to can then be called as a function for native code-gen, or stored to a file.

There is no intermediary and there is no bytecode optimization. This means that for many instructions, there is a 1:1 correspondence with common architectures.

If optimization or a more abstract description of the program is required (i.e. a compiler backend), an IR is included.

For an example C++ program using the C API check out `test/main.cpp` in this repo.

## Bytecode

All bytecode instructions are encoded as a 16-bit header followed by up to three operands between 8 and 64 bits in size, varying by instruction code.

The header consists of 6 bits for the instruction code, 4 bits for operand flags and 6 bits for operand info.

The last operand flag represent signedness/modifier and the rest signal if an operand is an immediate value.

The 6 operand info bits are split up into 3 groups of 2 bits for each operand.
The 2 bits for each operand represent either immediate size or operand type (00 for void, 01 for register, 10 for parameter, 11 for memory). 

## IR

The IR is mainly intended to be used in-memory but a text syntax is also included.
IR source files are formatted in s-expression syntax.

Here are some supported operations:

| Op | Args | Desc |
|:---|:---|:---|
| `fn`     | name, sig, body | Define a function |
| `local`  | name, type | Create a local variable | 
| `store`  | loc, value | Store a value at a location |
| `def`    | label, body | Define a block |
| `ret`    | value | Return a value from a function |
| `branch` | cond, t, f | Conditional branch to one of 2 blocks |

Here is an example factorial program:

```lisp
(fn fact (i32 i32) (
    (local res i32)
    (store res 1)
    (def _t (
        (store res (* res $0))
        (dec $0)))
    (def _f (
        (ret res)))
    (branch (> $0 0) _t _f)))
```
