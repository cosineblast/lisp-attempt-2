
# lisp-attempt-2

This is my second attempt at making a lisp from scratch.
The language is inspired by clojure and fennel.

It supports:
- Integers `123`
- Basic arithmetic `(+ 1 2)`
- Conditionals `(if true 10 20)`
- Closures `(lambda (x y) (+ x y))`
- Recursive closures `(lambda myself (x) (myself x))`
- Tail call optimization
- Tracing garbage collection
- Variable immutability
- Symbol binding
- Primitive IR

To be done:


Documentation to be done.

## Running the project

The interpreter is implemented in zig 0.14, and the easiest way to build it is by using [nix](https://nixos.org/), by
running `nix develop` and then `zig build run`, to run the project, or `zig build install` to generate a binary at `zig-out/bin/la2`.

Running the program will start the repl.

## License

This codebase is licensed under GPLv3.
