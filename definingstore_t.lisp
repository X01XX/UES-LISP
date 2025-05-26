;;; Run tests.
(defun definingstore-tests ()
  (format t "~&definingstore-tests beginning")

  ; Test definingstore-new.
  (let (store1)

    (setf store1 (definingstore-new nil))
    (assert (definingstore-p store1))

    (format t "~&  definingstore-new OK")
  )

  (format t "~&definingstore-tests done")
  t
)

