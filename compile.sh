# Script for compiling lisp files to find problems.
# .o files are deleted at end.

/usr/bin/gcl -eval "(progn (load #p \"compile.lisp\") (quit))"

/usr/bin/rm ./*.o

