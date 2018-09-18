# This project is abandoned. Please see [Pikelet](https://github.com/pikelet-lang/pikelet) for the same idea, but cleaner code and more defined semantics.

# Nova Programming Language

(Name not finalized)

The Nova programming language is an attempt to bring pure
functional programming to the systems programming domain.

Syntax is largely derived from Haskell with many restrictions.

Memory safety is planned to be maintained through the use of
linear types as opposed to a garbage collector.

## Purpose

The programming language is designed for similar domains as Rust,
but with a syntax and feel intended to appeal to Haskell and functional
programmers. As of the time of creation, Rust is the only mainstream
programming language with memory safety without GC, and my intention
is to provide more competition in this domain and possibly increase
interest in the area.

## Example

The standard library and operator overloading have yet to be implemented,
and the parser needs a lot of work, so the example could be cleaner than it is.
In addition, comments are not yet implemented.

The basic factorial implementation is as follows:

    module Main ()

    data Tuple a b = Tuple a b

    sub a b = llvm_sub_int64 (Tuple a b)

    mul a b = llvm_mul_int64 (Tuple a b)

    fact x = (case x of 0 -> 1; n -> mul n (fact (sub n 1)) )

    main (Tuple n _) = fact n

in this example `llvm_sub_int64` and `llvm_mul_int64` are built-in functions to
subtract and multiply respectively the 2 components of a `Tuple`. Analogous
functions also exist for addition and subtraction. As of right now, they are
the only built-in functions.

main accepts a `Tuple Int (Ptr (Ptr Int8))` as argument to parallel C's
`argc` and `argv`. In the future, command line arguments with be accessed
with a safer interface and pointer dereferencing and arithmetic is not yet
implemented, so `argc` is the only input currently available.

`sub` and `mul` are simple curried wrappers over the built-in arithmetic
functions. In theory, they could be replaced with a generic curry function.

The one line case expression is a workaround for a parser bug.

## Building

    cargo build

The project does depend on LLVM 5 so it must be in the library path when
building. Alternatively, Nix can be used with `nix-build` which would
automatically pull in LLVM 5.

## TODO

* Mutually recursive functions.
* Unifying unlike closures.
* Any static analysis other than type inference.
* Including multiple files.
* More built-in functions.
* Optimization.
* Clean up codebase.
* Rewrite parser.
* Recursive `let`.
* Standard library.
* External functions.
* Inline assembly.
* Type classes.
* Records.
* Operator overloading.
* And whole lot more!

## Possible Ideas

* Call-by-reference.
* Lazy evaluation.
* Uniqueness types vs linear types.
* Ad-hoc vs sub-type polymorphism.
* Probably others.
