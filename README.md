# UES-LISP

A rewrite of the Unorthodox-Expert-System (UES) project in Rust, to LISP (gcl).

See README and theory.html in the Rust project.

Try new ideas.

Compare and contrast with the UES written in Rust.

Develop smaller, less complicated, more testable functions.

Have fun programming in LISP.

LISP supports numbers of any size, so bits can be represented with a single integer GE zero, intsead of
a vector of integers as in Rust.

LISP does not check function argument types, or result, as RUST does.  But you can code checks.

LISP allows keywords for arguments.  My favorite use is in a function like: (defun is-subset-of (&key sub sup) ...
so its hard to mix up the order of arguments.

To run: gcl

Then: (load #p "main.lisp")

Then: (main) or (all-tests)

This is still in the early development stage.
