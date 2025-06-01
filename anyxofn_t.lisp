;;; Run tests.
(defun anyxofn-tests ()
  (format t "~&anyxofn-tests beginning")

  (let (in-list out-list)

    (setf in-list '("a" "b" "c" "d"))

    (setf out-list (any-x-of-n 1 in-list))
    (assert (equal out-list '(("a") ("b") ("c") ("d"))))

    (setf out-list (any-x-of-n 2 in-list))
    (assert (equal out-list '(("a" "b") ("a" "c") ("a" "d") ("b" "c") ("b" "d") ("c" "d"))))

    (setf out-list (any-x-of-n 3 in-list))
    (assert (equal out-list '(("a" "b" "c") ("a" "b" "d") ("a" "c" "d") ("b" "c" "d"))))

    (setf out-list (any-x-of-n 4 in-list))
    (assert (equal out-list '(("a" "b" "c" "d"))))

    (format t "~&  any-x-of-n OK")
  )

  (format t "~&anyxofn-tests done")
  t
)
