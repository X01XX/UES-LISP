;;;; The Unorthodox-Expert-System, in LISP.
;;;;
;;;; The obvious question is: What is it an expert of?
;;;;
;;;; It is an expert of its own state.
;;;;
;;;; For use outside of the GPL-3.0 license, contact the Wisconsin Alumni Research Foundation (WARF).
;;;;

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

(load #p "change.lisp")
(load #p "change_t.lisp")

(load #p "domain.lisp")
(load #p "domain_t.lisp")

(load #p "anyxofn.lisp")

(load #p "cngstps.lisp")
(load #p "cngstps_t.lisp")

(load #p "cngstpsstore.lisp")
(load #p "cngstpsstore_t.lisp")

(load #p "regionscorr.lisp")
(load #p "regionscorr_t.lisp")

(load #p "pathcorr.lisp")
(load #p "pathcorr_t.lisp")

(load #p "regionscorrstore.lisp")
(load #p "regionscorrstore_t.lisp")

(load #p "domainstore.lisp")
(load #p "domainstore_t.lisp")

(load #p "plan.lisp")
(load #p "plan_t.lisp")

(load #p "planstore.lisp")
(load #p "planstore_t.lisp")

(load #p "selectregions.lisp")
(load #p "selectregions_t.lisp")

(load #p "selectregionsstore.lisp")
(load #p "selectregionsstore_t.lisp")

(load #p "pn.lisp")

(load #p "square.lisp")
(load #p "square_t.lisp")

(defvar true t)
(defvar false nil)

(defun main ()
  (let (domx rule-to-goal steps from-reg to-reg care-mask wanted-changes cngstpsstore1)
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
		    ))
		 :current-state (state-from-str "#b0101")))

    (setf from-reg (region-from-str "0X10"))
    (format t "~&from: ~A" from-reg)

    (setf to-reg (region-from-str "10XX"))
    (format t "~&to:   ~A" to-reg)

    (setf rule-to-goal (rule-new-region-to-region from-reg to-reg))
    (format t "~& ~&rule-to-goal: ~A" rule-to-goal)

    (setf care-mask (mask-not (region-x-mask to-reg)))

    (setf wanted-changes (change-new :b01 (mask-new-and care-mask (rule-b01 rule-to-goal))
                                     :b10 (mask-new-and care-mask (rule-b10 rule-to-goal))))

    ;(format t "~&wanted changes ~A" wanted-changes)

    (setf steps (domain-get-steps domx rule-to-goal (domain-max-region domx)))
    (format t "~& ~&steps found:~&  ~A" steps)

    (setf cngstpsstore1 (cngstpsstore-new wanted-changes))

    (loop for stepx in (stepstore-step-list steps) do
      (cngstpsstore-add cngstpsstore1 stepx)
    )
    (format t "~& ~&steps stored by single-bit changes:~&  ~A" cngstpsstore1)

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

  (sample-tests)

  (group-tests)
  (groupstore-tests)

  (action-tests)
  (actionstore-tests)

  (step-tests)
  (stepstore-tests)

  (change-tests)
  (cngstps-tests)

  (cngstpsstore-tests)
  (regionstore-tests)

  (pathcorr-tests)

  (regionscorr-tests)
  (regionscorrstore-tests)

  (square-tests)

  (domain-tests)
  (domainstore-tests)

  (plan-tests)
  (planstore-tests)

  (selectregions-tests)
  (selectregionsstore-tests)

  (format t "~&All tests done")
  t
)
