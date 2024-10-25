;;; Run plan tests.
(defun plan-tests ()

  (format t "~&plan-tests beginning")

  ; Test plan-new.
  (let (plan1)
    ; Test new, empty, plan.
    (setf plan1 (plan-new nil))
    (assert (plan-p plan1))
    (assert (plan-is-empty plan1))

    (format t "~&  plan-new OK")
  )

  ;; Test plan-restrict-initial-region, using a intersecting but not subset region.
  (let (plan1 plan2 step1 step2)
    (setf step1 (step-new :act-id 0 :rule (rule-from-str "[00/XX/01/Xx]")))
    (setf step2 (step-new :act-id 0 :rule (rule-from-str "[01/XX/11/XX]")))
    (setf plan1 (plan-new (list step1 step2)))

    ;; Test valid restriction.
    (setf plan2 (plan-restrict-initial-region plan1 (region-from-str "x00x")))
    (assert (plan-p plan2))

    (assert (= (plan-length plan2) 2))
    (assert (region-eq (plan-initial-region plan2) (region-from-str "000x")))
    (assert (region-eq (plan-result-region plan2) (region-from-str "101x")))

    ;; Test invalid restriction.
    (setf plan2 (plan-restrict-initial-region plan1 (region-from-str "100x")))
    (assert (null plan2))

    (format t "~&  plan-restrict-initial-region OK")
  )

  ;; Test plan-restrict-result-region, using a intersecting but not subset region.
  (let (plan1 plan2 step1 step2)
    (setf step1 (step-new :act-id 0 :rule (rule-from-str "[00/XX/01/Xx]")))
    (setf step2 (step-new :act-id 0 :rule (rule-from-str "[01/XX/11/XX]")))
    (setf plan1 (plan-new (list step1 step2)))

    ;; Test valid restriction.
    (setf plan2 (plan-restrict-result-region plan1 (region-from-str "10x1")))
    (assert (plan-p plan2))
    (assert (= (plan-length plan2) 2))
    (assert (region-eq (plan-initial-region plan2) (region-from-str "0000")))
    (assert (region-eq (plan-result-region plan2) (region-from-str "1011")))

    ;; Test invalid restriction.
    (setf plan2 (plan-restrict-result-region plan1 (region-from-str "00x1")))
    (assert (null plan2))

    (format t "~&  plan-restrict-result-region OK")
  )

  ;; Test plan-link.
  (let (plan1 plan2 plan3 step1 step2)
    ;; Test link of two non-intersecting plans.
    (setf step1 (step-new :act-id 0 :rule (rule-from-str "[00/XX/01/Xx]")))
    (setf plan1 (plan-new (list step1)))

    (setf step2 (step-new :act-id 0 :rule (rule-from-str "[11/XX/11/XX]")))
    (setf plan2 (plan-new (list step2)))

    (setf plan3 (plan-link plan1 plan2))
    (assert (null plan3))

    ;; Test plans with equal result/initial regions.
    (setf step1 (step-new :act-id 0 :rule (rule-from-str "[00/XX/01/XX]")))
    (setf plan1 (plan-new (list step1)))

    (setf step2 (step-new :act-id 0 :rule (rule-from-str "[01/XX/11/XX]")))
    (setf plan2 (plan-new (list step2)))

    (setf plan3 (plan-link plan1 plan2))
    (assert (plan-p plan3))

    (assert (= (plan-length plan3) 2))
    (assert (region-eq (plan-initial-region plan3) (region-from-str "0X0X")))
    (assert (region-eq (plan-result-region plan3) (region-from-str "1X1X")))

    ;; Test plans with result region superset result region.
    (setf step1 (step-new :act-id 0 :rule (rule-from-str "[00/XX/01/XX]")))
    (setf plan1 (plan-new (list step1)))

    (setf step2 (step-new :act-id 0 :rule (rule-from-str "[01/11/11/XX]")))
    (setf plan2 (plan-new (list step2)))

    (setf plan3 (plan-link plan1 plan2))
    (assert (plan-p plan3))

    (assert (= (plan-length plan3) 2))
    (assert (region-eq (plan-initial-region plan3) (region-from-str "010X")))
    (assert (region-eq (plan-result-region plan3) (region-from-str "111X")))

    ;; Test plans with result region superset initial region.
    (setf step1 (step-new :act-id 0 :rule (rule-from-str "[00/X1/01/XX]")))
    (setf plan1 (plan-new (list step1)))

    (setf step2 (step-new :act-id 0 :rule (rule-from-str "[01/XX/11/XX]")))
    (setf plan2 (plan-new (list step2)))

    (setf plan3 (plan-link plan1 plan2))
    (assert (plan-p plan3))

    (assert (= (plan-length plan3) 2))
    (assert (region-eq (plan-initial-region plan3) (region-from-str "0X0X")))
    (assert (region-eq (plan-result-region plan3) (region-from-str "111X")))

    ;; Test result region and initial region intersect, but neither is superset.
    (setf step1 (step-new :act-id 0 :rule (rule-from-str "[00/X1/01/XX]")))
    (setf plan1 (plan-new (list step1)))

    (setf step2 (step-new :act-id 0 :rule (rule-from-str "[01/XX/11/10]")))
    (setf plan2 (plan-new (list step2)))

    (setf plan3 (plan-link plan1 plan2))
    (assert (plan-p plan3))

    (assert (= (plan-length plan3) 2))
    (assert (region-eq (plan-initial-region plan3) (region-from-str "0X01")))
    (assert (region-eq (plan-result-region plan3) (region-from-str "1110")))

    (format t "~&  plan-link OK")
  )

  ;; Test plan-from-str.
  (let (plan1)

    (setf plan1 (plan-from-str "0x1-0>1x0-1>010"))
    (format t "~&plan-from-str result: ~A" plan1)

    (format t "~&  plan-from-str OK")
  )


  (format t "~&plan-tests done")
  t
)
