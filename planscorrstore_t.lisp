;;; Run tests.
(defun planscorrstore-tests ()
  (format t "~&planscorrstore-tests beginning")

  ; Test planscorrstore-new.
  (let (store1 step1 step2 plan1 plan2 plnsc1 plnsc2)

    ; Test empty planscorrstore.
    (setf store1 (planscorrstore-new nil))
    (assert (planscorrstore-p store1))

    ; Test non-empty planscorrx store.
    (setf step1 (step-new :act-id 0 :rule (rule-from-str "[00/00/01/XX]")))
    (setf step2 (step-new :act-id 2 :rule (rule-from-str "[00/00/10/X0]")))
    (setf plan1 (plan-new (list step1 step2)))
    (format t "~&plan1 ~A" plan1)

    (setf step1 (step-new :act-id 0 :rule (rule-from-str "[01/01/XX]")))
    (setf step2 (step-new :act-id 1 :rule (rule-from-str "[11/10/X0]")))
    (setf plan2 (plan-new (list step1 step2)))
    (format t "~&plan2 ~A" plan2)

    (setf plnsc1 (planscorr-new (list plan1 plan2)))
    (format t "~&plnsc1 ~A" plnsc1)
    (format t "~&initial regions ~A" (planscorr-initial-regions plnsc1))
    (format t "~&result regions  ~A" (planscorr-result-regions plnsc1))

    (format t "~&  planscorrstore-new OK")
  )

  (format t "~&planscorrstore-tests done")
  t
)

