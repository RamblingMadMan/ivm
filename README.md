# InfinityVM

InfinityVM is a virtual machine that JIT-compiles and executes bytecode.

IVM bytecode is intended to be a portable way to store the exact final representation of a program.

IVM is designed with the [Lemni programming language](https://github.com/RamblingMadMan/lemni) compiler in mind, but is general purpose and can be used for any runtime code-gen task.

## How it works

The compiler directly emits machine code for the target platform/architecture and returns a pointer to the code. The code pointed to can then be called as a function for native code-gen, or stored to a file.

There is no intermediary and there is no bytecode optimization. This means that for many instructions, there is a 1:1 correspondance with common architectures.
