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

  ;; Test masksverticesstore-push-nosubs.
  ;; Also masksverticesstore-push, masksverticesstore-find.
  (let (mksvts masks1 masks2 vertices1 vertices2 masksvertices1 masksvertices2)
    ;; Init store.
    (setf mksvts (masksverticesstore-new nil))

    ;; Build masksvertices1.
    ;; Masks and verticies for XX0X vs XX1X.
    (setf masks1 (maskstore-new (list (mask-from 'm0010))))

    (setf vertices1 (vertexstore-new (list (vertex-new (state-from 's0001) (statestore-new (list (state-from 's0011))))
                                           (vertex-new (state-from 's0011) (statestore-new (list (state-from 's0001)))))))

    (setf masksvertices1 (masksvertices-new masks1 vertices1))

    ;; Build masksvertices2.
    ;; Masks and verticies for 0X0X, 1X0X and XX1X.
    (setf masks2 (maskstore-new (list (mask-from 'm0010) (mask-from 'm1000))))

    (setf vertices2 (vertexstore-new (list (vertex-new (state-from 's0101) (statestore-new (list (state-from 's0111) (state-from 's1101))))
                                           (vertex-new (state-from 's0111) (statestore-new (list (state-from 's0101))))
                                           (vertex-new (state-from 's1101) (statestore-new (list (state-from 's0101) (state-from 's1111)))))))

    (setf masksvertices2 (masksvertices-new masks2 vertices2))

    ;; Add masksvertices1
    (masksverticesstore-push mksvts masksvertices1)
    (assert (masksverticesstore-find mksvts masks1))

    ;; Add masksvertices2, it should displace masksvertices1.
    (assert (masksverticesstore-push-nosubs mksvts masksvertices2))
    (assert (= (masksverticesstore-length mksvts) 1))
    (assert (masksverticesstore-find mksvts masks2))

    ;; Add masksvertices1, it should not be added.
    (assert (not (masksverticesstore-push-nosubs mksvts masksvertices1)))
    (assert (= (masksverticesstore-length mksvts) 1))
    (assert (masksverticesstore-find mksvts masks2))

    (format t "~&  masksverticesstore-push-nosubs OK")
  )

  (format t "~&masksverticesstore-tests done")
  t
)
