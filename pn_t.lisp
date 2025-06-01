;;; Run tests.
(defun pn-tests ()
  (format t "~&pn-tests beginning")

  ;; Test pn-new.
  (let (pnx)
    (setf pnx (pn-new 1))
    (assert (pn-p pnx))

    (setf pnx (pn-new 2))
    (assert (pn-p pnx))

    (setf pnx (pn-new 3))
    (assert (pn-p pnx))

    (format t "~&  pn-new OK")
  )

  (format t "~&pn-tests done")
  t
)
