;; Run group tests.
(defun group-tests ()
  (format t "~&group-tests beginning")

  ; Test group-new
  (let (grp0 errx)
    (setf grp0 (group-new :rules (rulestore-new (list (rule-from-str "[Xx/XX/XX/XX]")))))
    (assert (group-p grp0))

    (setf grp0 (group-new :rules (rulestore-new (list (rule-from-str "[XX/Xx/XX/01]") 
						      (rule-from-str "[XX/Xx/XX/00]")))))

    (setf errx (group-new-na :rules (rulestore-new (list (rule-from-str "[XX/Xx/XX/01]") 
						      (rule-from-str "[XX/Xx/XX/11]")))))
    (assert (and (err-p errx) (string= (err-str errx) "Rulestore initial regions do not match")))

    (format t "~&  group-new OK")
  )

  ; Test group-get-steps.
  (let (domx rule-to-goal steps from-reg to-reg)
    (setf domx (domain-new :id 0 :actions
                 (actionstore-new (list
                    (action-new :id 0 :groups
                      (groupstore-new (list (group-new :rules
                        (rulestore-new (list (rule-from-str "[01/10/00/00]")))))))
                    (action-new :id 1 :groups
                      (groupstore-new (list (group-new :rules
                        (rulestore-new (list (rule-from-str "[X1/X0/X0/X1]")))))))
                    (action-new :id 2 :groups
                      (groupstore-new (list (group-new :rules
                        (rulestore-new (list (rule-from-str "[Xx/Xx/00/00]")))))))
                    (action-new :id 3 :groups
                      (groupstore-new (list (group-new :rules
                        (rulestore-new (list (rule-from-str "[Xx/11/X0/X1]")))))))
                    (action-new :id 4 :groups
                      (groupstore-new (list (group-new :rules
                        (rulestore-new (list (rule-from-str "[Xx/00/Xx/Xx]")))))))
                    )) :current-state (state-from-str "#b0101")))

    (setf from-reg (region-from-str "0101"))
    (setf to-reg   (region-from-str "1001"))

    (setf rule-to-goal (rule-new-region-to-region from-reg to-reg))
    (setf steps (domain-get-steps domx rule-to-goal (domain-max-region domx)))
    (format t "~& steps ~A" steps)

    (assert (= (stepstore-length steps) 5))

    (format t "~&at 1")
    ;; No intersection, no changes to rule.
    (assert (stepstore-contains steps (step-new :act-id 0 :rule (rule-from-str "[01/10/00/00]") :kind 'a :w 2 :u 0)))

    (format t "~&at 2")
    ;; No intersection, bits 3 and 2 isolate-in  wanted changes, bits 1 and 0 isolate-out unwanted changes..
    (assert (stepstore-contains steps (step-new :act-id 1 :rule (rule-from-str "[01/10/00/11]") :kind 's :w 2 :u 0)))

    (format t "~&at 3")
    ;; No intersection, Xx/Xx parsed to 01/10.
    (assert (stepstore-contains steps (step-new :act-id 2 :rule (rule-from-str "[01/10/00/00]") :kind 'a :w 2 :u 0)))

    (format t "~&at 4")
    ;; Intersection with from region.
    (assert (stepstore-contains steps (step-new :act-id 3 :rule (rule-from-str "[01/11/00/11]") :kind 'f :w 1 :u 0)))

    (format t "~&at 5")
    ;; Intersection with to region.
    (assert (stepstore-contains steps (step-new :act-id 4 :rule (rule-from-str "[01/00/10/01]") :kind 'b :w 1 :u 2)))

    (format t "~&  group-get-steps OK")
  )

  (format t "~&group-tests done")
)
