#!/usr/bin/bash
/usr/bin/cat *[^_t].lisp > tmp.txt 
/usr/bin/cat *_t.lisp > tmp2.txt 

for fname in `/usr/bin/grep defun tmp.txt | /usr/bin/cut -d " " -f 2 | /usr/bin/grep -v -e "-print$" | /usr/bin/grep -v -e "^main$" | /usr/bin/grep -v -e "^all-tests$"` 
do
  count=`/usr/bin/grep -c "($fname" tmp.txt`
  if [ $count -eq 0 ]
  then
    count2=`/usr/bin/grep -c "($fname" tmp2.txt`
    /usr/bin/echo "$fname used $count times in source $count2 in tests"
  fi
done
/usr/bin/rm tmp.txt tmp2.txt
