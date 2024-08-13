;;; Run tests.
(defun groupstore-tests ()
  (format t "~&groupstore-tests beginning")

  ; Test groupstore-new.
  (let (store1)
    (setf store1 (groupstore-new (list 
      (group-new :rules (rulestore-new (list
        (rule-union
	  (rule-new (sample-new :initial (state-from-str "#x0") :action 0 :result (state-from-str "#x1")))
	  (rule-new (sample-new :initial (state-from-str "#xf") :action 0 :result (state-from-str "#xe"))))))))))
 
    ;(format t "~&groupstore ~A" (groupstore-str store1))
    (format t "~&  groupstore-new OK")
  )


  (format t "~&groupstore-tests done")
  t
)
