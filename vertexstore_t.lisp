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

  ;; Test vertexstore-structure-implied.
  (let (vert1 vert2 rslt1 rslt2 rslt3 vstr1 rslt4 (*max-region* (region-new (list (state-from 's1111) (state-from 's0000)))))

    ;; Get the result of intersecting the implied regions of twe vertices.
    (setf vert1 (vertex-new (state-from 's0001) (statestore-new (list (state-from 's1001) (state-from 's0011)))))
    (setf rslt1 (vertex-structure-implied vert1))
    ;(format t "~&rslt1: ~A" (regionstore-str rslt1))

    (setf vert2 (vertex-new (state-from 's1001) (statestore-new (list (state-from 's0001) (state-from 's1011)))))
    (setf rslt2 (vertex-structure-implied vert2))
    ;(format t "~&rslt2: ~A" (regionstore-str rslt2))

    (setf rslt3 (regionstore-intersection rslt1 rslt2))
    ;(format t "~&rslt3: ~A" (regionstore-str rslt3))

    ;; Do the same with a store of the two vertices.
    (setf vstr1 (vertexstore-new (list vert1 vert2)))
    (assert (vertexstore-p vstr1))
    (setf rslt4 (vertexstore-structure-implied vstr1))
    ;(format t "~&rslt4: ~A" (regionstore-str rslt4))

    (assert (regionstore-eq rslt3 rslt4))
    (assert (= (regionstore-length rslt4) 5))

    (format t "~&  vertexstore-structure-implied OK")
  )

  ;; Test vertexstore-vertices-in-region.
  (let (vert1 vert2 vert3 verts rslt1 reg1)

    ;; Init vertexstore.
    (setf vert1 (vertex-new (state-from 's0101) (statestore-new (list (state-from 's0111) (state-from 's1101)))))
    (setf vert2 (vertex-new (state-from 's0111) (statestore-new (list (state-from 's0101) (state-from 's1111)))))
    (setf vert3 (vertex-new (state-from 's1111) (statestore-new (list (state-from 's0111) (state-from 's1110)))))
    (setf verts (vertexstore-new (list vert1 vert2 vert3)))

    ;; Test no vertices in region.
    (setf reg1 (region-from 'r1x0x))
    (setf rslt1 (vertexstore-vertices-in-region verts reg1))
    ;(format t "~&rslt1: ~A" (vertexstore-str rslt1))
    (assert (vertexstore-is-empty rslt1))

    ;; Test one vertices in region.
    (setf reg1 (region-from 'rx10x))
    (setf rslt1 (vertexstore-vertices-in-region verts reg1))
    ;(format t "~&rslt1: ~A" (vertexstore-str rslt1))
    (assert (= (vertexstore-length rslt1) 1))
    (assert (vertexstore-member rslt1 vert1))

    ;; Test two vertices in region.
    (setf reg1 (region-from 'r0XXX))
    ;(format t "~&verts: ~A" (vertexstore-str verts))
    (setf rslt1 (vertexstore-vertices-in-region verts reg1))
    ;(format t "~&rslt1: ~A" (vertexstore-str rslt1))
    (assert (= (vertexstore-length rslt1) 2))
    (assert (vertexstore-member rslt1 vert1))
    (assert (vertexstore-member rslt1 vert2))

    (format t "~&  vertexstore-verticies-in-region OK")
  )

  ;; Test vertexstore-edges-in-region.
  (let (vert1 vert2 vert3 verts rslt1 reg1)
    ;; Init vertexstore.
    (setf vert1 (vertex-new (state-from 's0101) (statestore-new (list (state-from 's0111) (state-from 's1101)))))
    (setf vert2 (vertex-new (state-from 's0111) (statestore-new (list (state-from 's0101) (state-from 's1111)))))
    (setf vert3 (vertex-new (state-from 's1111) (statestore-new (list (state-from 's0111) (state-from 's1110)))))
    (setf verts (vertexstore-new (list vert1 vert2 vert3)))

    ;; Test no edges in region.
    (setf reg1 (region-from 'rxx00))
    (setf rslt1 (vertexstore-edges-in-region verts reg1))
    ;(format t "~&rslt1: ~A" (statestore-str rslt1))
    (assert (statestore-is-empty rslt1))

    ;; Test one edges in region.
    (setf reg1 (region-from 'rxx10))
    (setf rslt1 (vertexstore-edges-in-region verts reg1))
    ;(format t "~&rslt1: ~A" (statestore-str rslt1))
    (assert (= (statestore-length rslt1) 1))
    (assert (statestore-member rslt1 (state-from 's1110)))

    ;; Test two edges in region.
    (setf reg1 (region-from 'rxx01))
    (setf rslt1 (vertexstore-edges-in-region verts reg1))
    ;(format t "~&rslt1: ~A" (statestore-str rslt1))
    (assert (= (statestore-length rslt1) 2))
    (assert (statestore-member rslt1 (state-from 's1101)))
    (assert (statestore-member rslt1 (state-from 's0101)))

    (format t "~&  vertexstore-edges-in-region OK")
  )

  ;; Test vertexstore-states-connected.
  (let (vert1 vert2 vert3 verts rslt)
    ;; Test with no verticies.
    (setf verts (vertexstore-new nil))
    (setf rslt (vertexstore-states-connected verts (state-from 's0000)))
    ;(format t "~&rslt1: ~A" (statestore-str rslt))
    (assert (= (statestore-length rslt) 1))

    ;; Test with one vertex, no connection found.
    (setf vert1 (vertex-new (state-from 's0000) (statestore-new (list (state-from 's0001) (state-from 's0100)))))
    (setf verts (vertexstore-new (list vert1)))
    (setf rslt (vertexstore-states-connected verts (state-from 's0011)))
    ;(format t "~&rslt1: ~A" (statestore-str rslt))
    (assert (= (statestore-length rslt) 1))

    ;; Test with one vertex, connection found.
    (setf vert1 (vertex-new (state-from 's0000) (statestore-new (list (state-from 's0001) (state-from 's0100)))))
    (setf verts (vertexstore-new (list vert1)))
    (setf rslt (vertexstore-states-connected verts (state-from 's0001)))
    ;(format t "~&rslt1: ~A" (statestore-str rslt))
    (assert (= (statestore-length rslt) 3))

    ;; Test with two connected vertices, connection found.
    (setf vert1 (vertex-new (state-from 's0000) (statestore-new (list (state-from 's0001) (state-from 's0100)))))
    (setf vert2 (vertex-new (state-from 's0101) (statestore-new (list (state-from 's0001) (state-from 's0100)))))
    (setf verts (vertexstore-new (list vert1 vert2)))
    (setf rslt (vertexstore-states-connected verts (state-from 's0001)))
    ;(format t "~&rslt1: ~A" (statestore-str rslt))
    (assert (= (statestore-length rslt) 4))

    ;; Test with two connected vertices, connection found.
    (setf vert1 (vertex-new (state-from 's0000) (statestore-new (list (state-from 's0001) (state-from 's0100)))))
    (setf vert2 (vertex-new (state-from 's0101) (statestore-new (list (state-from 's0001) (state-from 's0100)))))
    (setf vert3 (vertex-new (state-from 's1111) (statestore-new (list (state-from 's1011) (state-from 's1110)))))
    (setf verts (vertexstore-new (list vert1 vert2 vert3)))
    (setf rslt (vertexstore-states-connected verts (state-from 's0001)))
    ;(format t "~&rslt1: ~A" (statestore-str rslt))
    (assert (= (statestore-length rslt) 4))

    (format t "~&  vertexstore-states-connected OK")
  )

  ;; Test making defining region X1X1 without/with a state and two adjacent, dissimilar, states.
  (let (vert1 vert2 vert3 vert4 verts poss defining (*max-region* (region-new (list (state-from 's1111) (state-from 's0000)))))
    ;; Test making defining region X1X1 without a state and two adjacent, dissimilar, states.
    (setf vert1 (vertex-new (state-from 's0101) (statestore-new (list (state-from 's0100)))))
    (setf vert2 (vertex-new (state-from 's1111) (statestore-new (list (state-from 's1110)))))
    (setf vert3 (vertex-new (state-from 's1101) (statestore-new (list (state-from 's1001)))))
    (setf vert4 (vertex-new (state-from 's0111) (statestore-new (list (state-from 's0011)))))
    (setf verts (vertexstore-new (list vert1 vert2 vert3 vert4)))

    (setf poss (vertexstore-structure-implied verts))
    (setf defining (regionstore-defining-regions poss))
    ;(format t "~&poss ~A defining ~A" (regionstore-str poss) (regionstore-str defining))
    (assert (not (regionstore-member defining (region-from 'rX1X1))))

    ;; Test making defining region X1X1 with one state and two adjacent, dissimilar, states.
    (setf vert1 (vertex-new (state-from 's0101) (statestore-new (list (state-from 's0100) (state-from 's0001)))))
    (setf verts (vertexstore-new (list vert1)))
    (setf poss (vertexstore-structure-implied verts))
    (setf defining (regionstore-defining-regions poss))
    ;(format t "~&poss ~A defining ~A" (regionstore-str poss) (regionstore-str defining))
    (assert (regionstore-member defining (region-from 'rX1X1)))

    (format t "~&  vertexstore suplementary test OK")
  )


  (format t "~&vertexstore-tests done")
  t
)
