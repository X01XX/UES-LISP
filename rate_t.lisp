;;; Run rate tests.
(defun rate-tests ()
  (format t "~&rate-tests beginning")

  ;; Test rate-new.
  (let (rtx)
    (setf rtx (rate-new :positive 2 :negative -3))
    (assert (rate-p rtx))
    (assert (= (rate-positive rtx) 2))
    (assert (= (rate-negative rtx) -3))

    (format t "~&  rate-new OK")
  )

  (format t "~&rate-tests done")
  t
)
