;; Run group tests.
(defun group-tests ()
  (format t "~&group-tests beginning")

  ; Test group-new
  (let (grp0 errx)
    (setf grp0 (group-new :rules (rulestore-new (list (rule-from-str "[Xx/XX/XX/XX]")))))
    (setf grp0 (group-new :rules (rulestore-new (list (rule-from-str "[XX/Xx/XX/01]") 
						      (rule-from-str "[XX/Xx/XX/00]")))))

    (setf errx (group-new-na :rules (rulestore-new (list (rule-from-str "[XX/Xx/XX/01]") 
						      (rule-from-str "[XX/Xx/XX/11]")))))
    (assert (and (err-p errx) (string= (err-str errx) "Rulestore initial regions do not match")))

    (format t "~&  group-new OK")
  )

  ; Test group-get-steps.
  (let (domx rule-needed steps from-reg to-reg)
    (setf domx (domain-new :id 0 :actions
                 (actionstore-new (list
                    (action-new :id 0 :groups
                      (groupstore-new (list (group-new :rules
                        (rulestore-new (list (rule-from-str "[01/10/00/00]")))))))
                    (action-new :id 1 :groups
                      (groupstore-new (list (group-new :rules
                        (rulestore-new (list (rule-from-str "[X1/X0/11/11]")))))))
                    (action-new :id 2 :groups
                      (groupstore-new (list (group-new :rules
                        (rulestore-new (list (rule-from-str "[Xx/Xx/00/00]")))))))
                    (action-new :id 3 :groups
                      (groupstore-new (list (group-new :rules
                        (rulestore-new (list (rule-from-str "[Xx/11/X0/X1]")))))))
                    (action-new :id 4 :groups
                      (groupstore-new (list (group-new :rules
                        (rulestore-new (list (rule-from-str "[Xx/00/Xx/Xx]")))))))
                    ))))

    (setf from-reg (region-from-str "0101"))
    (setf to-reg   (region-from-str "1001"))

    (setf rule-needed (rule-new-region-to-region from-reg to-reg))
    (setf steps (domain-get-steps domx rule-needed from-reg to-reg))

    (assert (= (stepstore-length steps) 5))

    ;; No intersection, 01/10 changes found.
    (assert (stepstore-contains steps (step-new :act-id 0 :rule (rule-from-str "[01/10/00/00]"))))

    ;; No intersection, X1/X0 stays the same.
    (assert (stepstore-contains steps (step-new :act-id 1 :rule (rule-from-str "[X1/X0/11/11]"))))

    ;; No intersection, Xx/Xx parsed to 01/10.
    (assert (stepstore-contains steps (step-new :act-id 2 :rule (rule-from-str "[01/10/00/00]"))))

    ;; Intersection with from region.
    (assert (stepstore-contains steps (step-new :act-id 3 :rule (rule-from-str "[01/11/00/11]"))))

    ;; Intersection with to region.
    (assert (stepstore-contains steps (step-new :act-id 4 :rule (rule-from-str "[01/00/10/01]"))))

    (format t "~&  group-get-steps OK")
  )

  (format t "~&group-tests done")
)
