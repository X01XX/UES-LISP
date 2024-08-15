
(load #p "err.lisp")
(load #p "tools.lisp")

(load #p "value.lisp")
(load #p "value_t.lisp")

(load #p "state.lisp")
(load #p "state_t.lisp")
(load #p "statestore.lisp")
(load #p "statestore_t.lisp")

(load #p "mask.lisp")
(load #p "mask_t.lisp")
(load #p "maskstore.lisp")
(load #p "maskstore_t.lisp")

(load #p "region.lisp")
(load #p "region_t.lisp")
(load #p "regionstore.lisp")
(load #p "regionstore_t.lisp")

(load #p "sample.lisp")
(load #p "sample_t.lisp")

(load #p "rule.lisp")
(load #p "rule_t.lisp")
(load #p "rulestore.lisp")
(load #p "rulestore_t.lisp")

(load #p "group.lisp")
(load #p "group_t.lisp")
(load #p "groupstore.lisp")
(load #p "groupstore_t.lisp")

(load #p "action.lisp")
(load #p "action_t.lisp")
(load #p "actionstore.lisp")
(load #p "actionstore_t.lisp")

(load #p "step.lisp")
(load #p "step_t.lisp")
(load #p "stepstore.lisp")
(load #p "stepstore_t.lisp")

(load #p "domain.lisp")

(load #p "anyxofn.lisp")

(defvar true t)
(defvar false nil)

(defun main ()
  (let (domx rule-needed steps from-reg to-reg)
    (setf domx (domain-new :id 0 :actions 
		 (actionstore-new (list
		    (action-new :id 0 :groups
		      (groupstore-new (list (group-new :rules 
		     	(rulestore-new (list (rule-from-str "[00/XX/01/XX]")))))))
		    (action-new :id 1 :groups
		      (groupstore-new (list (group-new :rules 
		     	(rulestore-new (list (rule-from-str "[01/XX/11/XX]")))))))
		    (action-new :id 2 :groups
		      (groupstore-new (list (group-new :rules 
		     	(rulestore-new (list (rule-from-str "[11/XX/10/XX]")))))))
		    (action-new :id 3 :groups
		      (groupstore-new (list (group-new :rules 
		     	(rulestore-new (list (rule-from-str "[Xx/Xx/11/XX]")))))))
		    ))))

    (setf from-reg (region-from-str "0X00"))
    (format t "~&from ~A" from-reg)

    (setf to-reg (region-from-str "100X"))
    (format t "~&to   ~A" to-reg)

    (setf rule-needed (rule-new-region-to-region from-reg to-reg))
    (format t "~&rule-needed ~A" rule-needed)
    (setf steps (domain-get-steps domx rule-needed from-reg to-reg))
    (format t "~&steps ~A" steps)
    true
  )
)

(defun all-tests ()
  (format t "~&All tests beginning")
  (value-tests)

  (state-tests)
  (statestore-tests)

  (mask-tests)
  (maskstore-tests)

  (rule-tests)
  (rulestore-tests)

  (region-tests)
  (regionstore-tests)

  (sample-tests)

  (group-tests)
  (groupstore-tests)

  (action-tests)
  (actionstore-tests)

  (step-tests)
  (stepstore-tests)

  (format t "~&All tests done")
  t
)
