# Script for compiling lisp files to find problems.
# .o files are deleted at end.
# 
# Some errors will cause this to hang on the gcl side.
# return-from with invalid name.
# illegal binding in let.

/usr/bin/gcl -eval "(progn (load #p \"compile.lisp\") (quit))" > ./tmp.txt 2>&1

/usr/bin/egrep -i "Compiling|Warning:|Error:" ./tmp.txt | /usr/bin/grep -v DEPTH | /usr/bin/egrep -i -B 1 "Warning:|Error:"
/usr/bin/rm ./*.o ./tmp.txt

