;; Run group tests.
(defun group-tests ()
  (format t "~&group-tests beginning")

  ; Test group-new
  (let (grp0 grp1 grp2 grp3)
    (setf grp0 (group-new :rules (rulestore-new (list (rule-from-str "[Xx/XX/XX/XX]")))))
    (setf grp1 (group-new :rules (rulestore-new (list (rule-from-str "[XX/Xx/XX/XX]")))))
    (setf grp2 (group-new :rules (rulestore-new (list (rule-from-str "[XX/XX/Xx/XX]")))))
    (setf grp3 (group-new :rules (rulestore-new (list (rule-from-str "[XX/XX/XX/Xx]")))))
    
  )

  (format t "~&group-tests done")
)
