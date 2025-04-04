;; Run step tests.
(defun step-tests ()
  (format t "~&step-tests beginning")

  ;; Test step-new
  (let (stp1)
    ;; Test minimum arguments.
    (setf stp1 (step-new :act-id 0 :rule (rule-from "[00/XX/01/Xx]")))
    (assert (eq (type-of stp1) 'STEP))

    ;(format t "~&stp1: ~A" (step-str stp1))

    (assert (= 0 (step-act-id stp1)))
    (assert (rule-eq (step-rule stp1) (rule-from "[00/XX/01/Xx]")))

    (format t "~&  step-new OK")
  )

  ;; Test step-eq.
  (let (stp1 stp2)
    (setf stp1 (step-new :act-id 0 :rule (rule-from "[00/XX/01/Xx]")))
    (setf stp2 (step-new :act-id 0 :rule (rule-from "[00/XX/01/Xx]")))

    (assert (step-eq stp1 stp2))
    (format t "~&  step-eq OK")
  )

  ;; Test step-restrict-initial-region.
  (let (stp1 reg1 stp2)
    (setf stp1 (step-new :act-id 2 :rule (rule-from "[00/01/11/10_X1/X1/X1_X0/X0/X0_Xx/Xx/Xx_XX/XX/XX]")))
    (setf reg1 (region-from 'r0011_01X_01X_01X_01X))
    (setf stp2 (step-restrict-initial-region stp1 reg1))

    ;(format t "~&stp1 ~A" (step-str stp1))
    ;(format t "~&stp2 ~A" (step-str stp2))
    (assert (= 2 (step-act-id stp2)))
    (assert (rule-eq (step-rule stp2) (rule-from "[00/01/11/10_01/11/X1_00/10/X0_01/10/Xx_00/11/XX]")))

    (format t "~&  step-restrict-initial-region OK")
  )

  ;; Test step-restrict-result-region.
  (let (stp1 reg1 stp2)
    (setf stp1 (step-new :act-id 2 :rule (rule-from "[00/01/11/10_X1_X0_Xx/Xx/Xx_XX/XX/XX]")))
    (setf reg1 (region-from 'r0110_1_0_01X_01X))
    (setf stp2 (step-restrict-result-region stp1 reg1))

    ;(format t "~&stp1 ~A" (step-str stp1))
    ;(format t "~&stp2 ~A" (step-str stp2))
    (assert (= 2 (step-act-id stp2)))
    (assert (rule-eq (step-rule stp2) (rule-from "[00_01/11/10/X1_X0_10/01/Xx_00/11/XX]")))

    (format t "~&  step-restrict-result-region OK")
  )

  (format t "~&step-tests done")
)
