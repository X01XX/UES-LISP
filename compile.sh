# Script for compiling lisp files to find problems.
# .o files are deleted at end.

/usr/bin/gcl -eval "(progn (compile-file \"value.lisp\") (quit))"
sleep 1
/usr/bin/gcl -eval "(progn (compile-file \"value_t.lisp\") (quit))"
sleep 1
/usr/bin/gcl -eval "(progn (compile-file \"mask.lisp\") (quit))"
sleep 1
/usr/bin/gcl -eval "(progn (compile-file \"mask_t.lisp\") (quit))"
sleep 1
/usr/bin/gcl -eval "(progn (compile-file \"state.lisp\") (quit))"
sleep 1
/usr/bin/gcl -eval "(progn (compile-file \"state_t.lisp\") (quit))"
sleep 1
/usr/bin/gcl -eval "(progn (compile-file \"region.lisp\") (quit))"
sleep 1
/usr/bin/gcl -eval "(progn (compile-file \"region_t.lisp\") (quit))"
sleep 1
/usr/bin/gcl -eval "(progn (compile-file \"regionstore.lisp\") (quit))"
sleep 1
/usr/bin/gcl -eval "(progn (compile-file \"regionstore_t.lisp\") (quit))"
sleep 1
/usr/bin/gcl -eval "(progn (compile-file \"sample.lisp\") (quit))"
sleep 1
/usr/bin/gcl -eval "(progn (compile-file \"sample_t.lisp\") (quit))"
sleep 1
/usr/bin/gcl -eval "(progn (compile-file \"rule.lisp\") (quit))"
sleep 1
/usr/bin/gcl -eval "(progn (compile-file \"rule_t.lisp\") (quit))"
sleep 1
/usr/bin/gcl -eval "(progn (compile-file \"statestore.lisp\") (quit))"
sleep 1
/usr/bin/gcl -eval "(progn (compile-file \"statestore_t.lisp\") (quit))"
sleep 1
/usr/bin/gcl -eval "(progn (compile-file \"main.lisp\") (quit))"
sleep 1
/usr/bin/gcl -eval "(progn (compile-file \"err.lisp\") (quit))"
sleep 1
/usr/bin/gcl -eval "(progn (compile-file \"rulestore.lisp\") (quit))"
sleep 1
/usr/bin/gcl -eval "(progn (compile-file \"rulestore_t.lisp\") (quit))"
sleep 1
/usr/bin/gcl -eval "(progn (compile-file \"group.lisp\") (quit))"
sleep 1
/usr/bin/gcl -eval "(progn (compile-file \"group_t.lisp\") (quit))"
sleep 1
/usr/bin/gcl -eval "(progn (compile-file \"groupstore.lisp\") (quit))"
sleep 1
/usr/bin/gcl -eval "(progn (compile-file \"groupstore_t.lisp\") (quit))"
sleep 1
/usr/bin/gcl -eval "(progn (compile-file \"step.lisp\") (quit))"
sleep 1
/usr/bin/gcl -eval "(progn (compile-file \"step_t.lisp\") (quit))"
sleep 1
/usr/bin/gcl -eval "(progn (compile-file \"stepstore.lisp\") (quit))"
sleep 1
/usr/bin/gcl -eval "(progn (compile-file \"stepstore_t.lisp\") (quit))"
sleep 1
/usr/bin/gcl -eval "(progn (compile-file \"change.lisp\") (quit))"
sleep 1
/usr/bin/gcl -eval "(progn (compile-file \"change_t.lisp\") (quit))"
sleep 1
/usr/bin/gcl -eval "(progn (compile-file \"cngstps.lisp\") (quit))"
sleep 1
/usr/bin/gcl -eval "(progn (compile-file \"cngstps_t.lisp\") (quit))"
sleep 1
/usr/bin/gcl -eval "(progn (compile-file \"cngstpsstore.lisp\") (quit))"
sleep 1
/usr/bin/gcl -eval "(progn (compile-file \"cngstpsstore_t.lisp\") (quit))"

/usr/bin/rm ./*.o

