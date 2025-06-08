;;; Run planscorr tests.
(defun planscorr-tests ()

  (format t "~&planscorr-tests beginning")

  ;; Test planscorr-new.
  (let (plncorr1 (*domain-num-bits-list* (list 2)))
    ; Test new, empty, planscorr.
    (setf plncorr1 (planscorr-new (list (plan-new (list (step-new 1 (rule-from-str "[01/Xx]")))))))
    (assert (planscorr-p plncorr1))

    (format t "~&  planscorr-new OK")
  )

  ;; Test planscorr-congruent.
  (let (plnsc1 plnsc2 step1 plan1 plan2 (*domain-num-bits-list* (list 4 2)))
    (setf step1 (step-new 0 (rule-from-str "[00/XX/01/Xx]")))
    (setf plan1 (plan-new (list step1)))

    (setf step1 (step-new 0 (rule-from-str "[01/Xx]")))
    (setf plan2 (plan-new (list step1)))

    (setf plnsc1 (planscorr-new (list plan1 plan2)))

    (setf step1 (step-new 0 (rule-from-str "[00/XX/01/Xx]")))
    (setf plan1 (plan-new (list step1)))

    (setf step1 (step-new 0 (rule-from-str "[00/X1]")))
    (setf plan2 (plan-new (list step1)))

    (setf plnsc2 (planscorr-new (list plan1 plan2)))

    (format t "~&  planscorr-congruent OK")
  )

  ;; Test planscorr-are-sequence.
  (let (plnsc1 plnsc2 step1 plan1 (*domain-num-bits-list* (list 4)))
    (setf step1 (step-new 0 (rule-from-str "[00/11/01/XX]")))
    (setf plan1 (plan-new (list step1)))

    (setf plnsc1 (planscorr-new (list plan1)))
    ;(format t "~&plnsc1 ~A" plnsc1)

    (setf step1 (step-new 0 (rule-from-str "[01/11/11/XX]")))
    (setf plan1 (plan-new (list step1)))

    (setf plnsc2 (planscorr-new (list plan1)))
    ;(format t "~&plnsc2 ~A" plnsc2)

    (assert (planscorr-are-sequence plnsc1 plnsc2))
    (assert (not (planscorr-are-sequence plnsc2 plnsc1)))

    (format t "~&  planscorr-are-sequence OK")
  )

  (format t "~&planscorr-tests done")
  t
)

