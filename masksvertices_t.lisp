;;; Run masksvertices tests.
(defun masksvertices-tests ()

  (format t "~&masksvertices-tests beginning")

  ; Test masksvertices-new.
  (let (masksvertices1 masks1 vertices1)

    ;; Masks and verticies for XX0X vs XX1X.
    (setf masks1 (maskstore-new (list (mask-from 'm0010))))

    (setf vertices1 (vertexstore-new (list (vertex-new (state-from 's0001) (statestore-new (list (state-from 's0011))))
                                           (vertex-new (state-from 's0011) (statestore-new (list (state-from 's0001)))))))

    (setf masksvertices1 (masksvertices-new masks1 vertices1))
    (assert (masksvertices-p masksvertices1))

    (format t "~&  masksvertices-new OK")
  )

  (format t "~&masksvertices-tests done")
  t
)

