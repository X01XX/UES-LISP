# UES-LISP

A rewrite of the Unorthodox-Expert-System (UES) project in Rust, to LISP (gcl).

See README and theory.html in the Rust project.  There are a lot of good things in theory.html. Even if you don't like the project, see the addendum 
Why does backward-chaining sometimes work, when forward-chaining does not?

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

Rust will not let you serialize a struct that contains a reference.  Lisp silently converts multiple links to the same structure into multiple copies of the structure.

To run: gcl

Then: (load #p "main.lisp")

Then: (main file-name number-runs) or (all-tests)

      file-name:   A string, like "alternate.kmp", using the quotes in the command. Optional. The default is "default.kmp".
      number-runs: A number, like 0, or greater. Optional. The default is 0, run in interactive mode.
                   If greater than zero, run that number of times in non-interactive mode. To exercise the code, since it uses the random command in a number of places.

For basic code checks: From the command line: ./compile.sh (This will display some errors, but often hangs and you have to ctrl-d out of it to see the error).
                       From gcl: (load #p "compile.lisp"), but delete *.o files later.

I'm using gcl v2.6.14. It has some improvements, though I'm not sure what version they were first implemented in.
A hash table can have equalp as a test, so a struct can be a key.
There was a version that did not let me give the name "step" to a struct.  I complained about that, the response was kind of "thats the way it is", but v2.6.14 allows it.

A lot can be done with simple structs, compartmentalization of data and code.

This was a 10,000-pipe problem, at least for me.

Dedicated to those who never had a chance, from someone who did.
