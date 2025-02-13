;; Run group tests.
(defun group-tests ()
  (format t "~&group-tests beginning")

  ; Test group-new
  (let (grp0 errx)
    (setf grp0 (group-new :rules (rulestore-new (list (rule-from "[Xx/XX/XX/XX]")))))
    (assert (group-p grp0))

    (setf grp0 (group-new :rules (rulestore-new (list (rule-from "[XX/Xx/XX/01]") 
						      (rule-from "[XX/Xx/XX/00]")))))

    (setf errx (group-new-na :rules (rulestore-new (list (rule-from "[XX/Xx/XX/01]") 
						      (rule-from "[XX/Xx/XX/11]")))))
    (assert (and (err-p errx) (string= (err-str errx) "Rulestore initial regions do not match")))

    (format t "~&  group-new OK")
  )

  (format t "~&group-tests done")
)
