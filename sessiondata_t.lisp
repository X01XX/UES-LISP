;;; Run tests.
(defun sessiondata-tests ()
  (format t "~&sessiondata-tests beginning")

  ; Test sessiondata-new.
  (let (domstr)
    (setf domstr (sessiondata-new))
    (assert (sessiondata-p domstr))

    (format t "~&  sessiondata-new OK")
  )

  (format t "~&sessiondata-tests done")
  t
)
