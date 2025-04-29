;;; Run vertex tests.
(defun vertex-tests ()
  (format t "~&vertex-tests beginning")

  ; Test vertex-new.
  (let (vert1)
    (setf vert1 (vertex-new (state-from 's0001) (statestore-new (list (state-from 's0101) (state-from 's0011)))))
    (assert (vertex-p vert1))
    (assert (= (vertex-num-edges vert1) 2)) 

    (format t "~&  vertex-new OK")
  )

  (format t "~&vertex-tests done")
  t
)
