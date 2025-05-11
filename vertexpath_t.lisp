;; Run vertexpath tests.
(defun vertexpath-tests ()
  (format t "~&vertexpath-tests beginning")

  ; Test vertexpath-new
  (let (vtph)
    (setf vtph (vertexpath-new 
         (statestore-new (list (state-from 's0000) (state-from 's0001) (state-from 's0011)))))

    (assert (vertexpath-p vtph))

    (format t "~&  vertexpath-new OK")
  )

  (format t "~&vertexpath-tests done")
)
