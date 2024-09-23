;;; Run planscorr tests.
(defun planscorr-tests ()

  (format t "~&planscorr-tests beginning")

  ;; Test planscorr-new.
  (let (plncorr1)
    ; Test new, empty, planscorr.
    (setf plncorr1 (planscorr-new nil))
    (assert (planscorr-p plncorr1))

    (format t "~&  planscorr-new OK")
  )

  (format t "~&planscorr-tests done")
  t
)

