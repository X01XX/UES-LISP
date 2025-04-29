;;; Run vertexstore tests.
(defun vertexstore-tests ()
  (format t "~&vertexstore-tests beginning")

  ; Test vertexstore-new.
  (let (vstr1)
    (setf vstr1 (vertexstore-new (vertex-new (state-from 's0101) (statestore-new (list (state-from 's0001) (state-from 's0111))))))
    (assert (vertexstore-p vstr1))
    (assert (= (vertexstore-length vstr1) 1)) 

    (setf vstr1 (vertexstore-new (list (vertex-new (state-from 's0101) (statestore-new (list (state-from 's0001) (state-from 's0111))))
                                       (vertex-new (state-from 's0111) (statestore-new (list (state-from 's0101) (state-from 's0110)))))))
    (assert (vertexstore-p vstr1))
    (assert (= (vertexstore-length vstr1) 2)) 

    (format t "~&  vertexstore-new OK")
  )

  (format t "~&vertexstore-tests done")
  t
)
