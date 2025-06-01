;;; Run tests.
(defun any1ofeach-tests ()
  (format t "~&any1ofeach-tests beginning")

  (let (in-list out-list)

     (setf in-list '(('a  'b) ('c) ('d 'e 'f)))

     (setf out-list (any-1-of-each in-list))

    (assert (equal out-list '(('A 'C 'D) ('A 'C 'E) ('A 'C 'F) ('B 'C 'D) ('B 'C 'E) ('B 'C 'F))))

    (format t "~&  any-1-of-each OK")
  )

  (format t "~&any1ofeach-tests done")
  t
)
