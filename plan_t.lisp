;;; Run plan tests.
(defun plan-tests ()

  (format t "~&plan-tests beginning")

  ; Test plan-new.
  (let (plan1)
    ; Test new, empty, plan.
    (setf plan1 (plan-new 0 nil))
    (assert (plan-p plan1))
    (assert (plan-is-empty plan1))

    (format t "~&  plan-new OK")
  )


  (format t "~&plan-tests done")
  t
)
