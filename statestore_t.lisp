;;; Run tests.
(defun statestore-tests ()
  (format t "~&statestore-tests beginning")

  ; Test statestore-new.
  (let (ssx ss1 ss3 len)
    ; A state list with a duplicate.
    (setf ss1 (list (state-from-str "#x1") (state-from-str "#x1") (state-from-str "#x8")))

    ; A state list without duplicates.
    (setf ss3 (list (state-from-str "#x1") (state-from-str "#x2") (state-from-str "#x8")))
 
    ; Test list that contains duplicates.
    (setf ss1 (statestore-new ss1))
    ;(format t "~&errx ~A" errx)
    (assert (= (statestore-length ss1) 2))
 
    ; Test a good state list.
    (setf ssx (statestore-new ss3))
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

  (format t "~&statestore-tests done")
  t
)
