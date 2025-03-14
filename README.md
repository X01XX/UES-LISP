# UES-LISP

A rewrite of the Unorthodox-Expert-System (UES) project in Rust, to LISP (gcl).

See README and theory.html in the Rust project.

To try new ideas.

To compare and contrast with the UES written in Rust.

To develop smaller, less complicated, more testable functions.

Have fun programming in LISP.

Unlike Rust, LISP supports numbers of any size, so bits can be represented with a single integer GE zero, intsead of
a vector of integers as in Rust.

Unlike Rust, LISP does not check function argument types, or result.  But you can code checks.

Unlike Rust, LISP allows keywords for arguments.  My favorite use is in a function like: (defun is-subset-of (&key sub sup) ...
so its hard to mix up the order of arguments.

Unlike Rust, LISP does not have an awkward discontinuity between an item and its reference.

On the other hand, Rust has a number of good qualities, like no garbage collection, easy parallelism, speed of execution, avoiding many memory exploits (the future of programming), and compiler suggestions to correct an error.

To run: gcl

Then: (load #p "main.lisp")

Then: (main) or (all-tests)

For basic code checks: (load #p "compile.lisp"), but delete *.o files later.

This is still in the early development stage.

I'm using gcl v2.6.14. It has some improvements, though I'm not sure what version they were first implemented in.
A hash table can have equalp as a test, so a struct can be a key.
There was a version that did not let me give the name "step" to a struct.  I complained about that, the response was kind of "thats the way it is", but v2.6.14 allows it.
