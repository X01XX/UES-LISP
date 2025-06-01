;;; Run tests.
(defun main-tests ()
  (format t "~&main-tests beginning")

  ; Test session serialization.
  (let (sess1 sess2 sess-str)

    ;; Create a session.
    (setf sess1 (sessiondata-from
      '(SD(DS((DOM
                (ACT ("[XX/XX/XX/Xx]")) ;; One rulestore, with one rule, covering all states, that is in XXXX.
                (ACT ("[01/XX/00/XX]") ("[10/XX/00/XX]") ("[01/XX/11/XX]") ("[10/XX/11/XX]")) ;; Show that the program is not just copying this definition, the result will be [Xx/XX/XX/XX].
                (ACT ("[00/XX/01/XX]") ("[11/XX/00/XX]") ("[XX/XX/11/Xx]"))) ;; Three different rulestores, for different regions, in one action.
           (DOM
                (ACT ("[00/Xx/XX]")) ;; 1XXX, not accounted for here, will default to no change.
                (ACT ("[11/Xx/XX]")) 
                (ACT ("[01/XX/XX]" "[00/XX/Xx]")) ;; Two different rules, for same the region, in one rulestore.
                (ACT ("[Xx/XX/XX]" "[XX/XX/Xx]" "[XX/Xx/XX]"))) ;; Three different rules, for same the region, in one rulestore, interpretted as unpredictable.
            )
          )
          (SR (RC (r110X r110)) (RT 0 -1))
          (SR (RC (r11X1 r110)) (RT 4 0))
          (SC (s0101 s111))
      ))
    )

    ;; Convert session to string.
    (setf sess-str (format nil "~S~%" sess1))

    ;; Read session from string.
    (setf sess2 (read-from-string sess-str))
    (assert (sessiondata-p sess2))

    ;; Compare structures.
    (assert (equalp sess1 sess2))
   
    (format t "~&  main session serialization OK")
  )

  (format t "~&main-tests done")
  t
)
