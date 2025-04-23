# Script for compiling lisp files to find problems.
# .o files are deleted at end.
# 
# Some errors will cause this to hang on the gcl side.
# return-from with invalid name.
# illegal binding in let.
# Run (load #p "compile.lisp") in gcl to start to resolve the problem.

/usr/bin/gcl -eval "(progn (load #p \"compile.lisp\") (quit))" > ./tmp9.txt 2>&1

/usr/bin/egrep -i "Compiling|Warning:|Error:|undefined" ./tmp9.txt | /usr/bin/grep -v DEPTH | /usr/bin/egrep -i -B 1 "Warning:|Error:|undefined"
/usr/bin/rm ./*.o ./tmp9.txt

# Check some things.
if [ `/usr/bin/grep -c -- squarestore-add action.lisp` -ne 1 ]
then
  /usr/bin/echo "squarestore-add calls in action.lisp gt 1?"
fi

if [ `/usr/bin/grep -c -- square-add-sample action.lisp` -ne 1 ]
then
  /usr/bin/echo "square-add-sample calls in action.lisp gt 1?"
fi

if [ `/usr/bin/grep -- "make-regionscorr " *.lisp | /usr/bin/grep -v ":;" | /usr/bin/wc -l` -ne 1 ]
then
  /usr/bin/echo "make-regionscorr calls gt 1?"
fi

if [ `/usr/bin/grep -- "make-statescorr " *.lisp | /usr/bin/grep -v ":;" | /usr/bin/wc -l` -ne 1 ]
then
  /usr/bin/echo "make-statescorr calls gt 1?"
fi

if [ `/usr/bin/grep -- "make-maskscorr " *.lisp | /usr/bin/grep -v ":;" | /usr/bin/wc -l` -ne 1 ]
then
  /usr/bin/echo "make-maskscorr calls gt 1?"
fi

if [ `/usr/bin/grep -- "make-planscorr " *.lisp | /usr/bin/grep -v ":;" | /usr/bin/wc -l` -ne 1 ]
then
  /usr/bin/echo "make-planscorr calls gt 1?"
fi
