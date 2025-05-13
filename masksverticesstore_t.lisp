;;;; Run masksverticesstore tests.
(defun masksverticesstore-tests ()
  (format t "~&regionstore-tests beginning")

  ;; Test masksverticesstore-new
  (let (mksvts masksvertices1 masks1 vertices1)

    ;; Test null arguments.
    (setf mksvts (masksverticesstore-new nil))
    (assert (masksverticesstore-p mksvts))

    ;; Test single masksvertices argument.
    ;; Masks and verticies for XX0X vs XX1X.
    (setf masks1 (maskstore-new (list (mask-from 'm0010))))

    (setf vertices1 (vertexstore-new (list (vertex-new (state-from 's0001) (statestore-new (list (state-from 's0011))))
                                           (vertex-new (state-from 's0011) (statestore-new (list (state-from 's0001)))))))

    (setf masksvertices1 (masksvertices-new masks1 vertices1))
    (setf mksvts (masksverticesstore-new masksvertices1))
    (assert (masksverticesstore-p mksvts))

    ;; Test list of masksvertices argument.
    (setf mksvts (masksverticesstore-new (list masksvertices1)))
    (assert (masksverticesstore-p mksvts))

    (format t "~&  masksverticesstore-new OK")
  )

  (format t "~&masksverticesstore-tests done")
  t
)
