;;; Run tests.
(defun statestore-tests ()
  (format t "~&statestore-tests beginning")

  ; Test statestore-new.
  (let (ssx ss1 len)

    ; A state list.
    (setf ss1 (list (state-from-str "#x1") (state-from-str "#x2") (state-from-str "#x8")))
 
    ; Test a good state list.
    (setf ssx (statestore-new ss1))
    (assert (statestore-p ssx))

    (setf len (statestore-length ssx))
    ;(format t "~&len ~A" len)
    (assert (= len 3))
 
    (format t "~&  statestore-new OK")
  )

  ; Test statestore-length.
  (let (ssx len)
    (setf ssx (statestore-new (list (state-from-str "#x1") (state-from-str "#x2"))))
 
    (setf len (statestore-length ssx))
    ;(format t "~&len ~A" len)
    (assert (= len 2))

    (format t "~&  statestore-length OK")
  )

  ; Test statestore-remove-unneeded.
  (let (str1 str2)
    (setf str1 (statestore-new (list (state-from-str "#x1") (state-from-str "#x2") (state-from-str "#x7") (state-from-str "#x4"))))

    (setf str2 (statestore-remove-unneeded str1))
    ;(format t "~&str ~A" str2)
    (assert (= (statestore-length str2) 3))
    (assert (statestore-contains str2 (state-from-str "#x1")))
    (assert (statestore-contains str2 (state-from-str "#x2")))
    (assert (statestore-contains str2 (state-from-str "#x7")))

    (format t "~&  statestore-remove-unneeded OK")
  )

  (format t "~&statestore-tests done")
  t
)
