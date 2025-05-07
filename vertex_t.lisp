;;; Run vertex tests.
(defun vertex-tests ()
  (format t "~&vertex-tests beginning")

  ;; Test vertex-new.
  (let (vert1)
    (setf vert1 (vertex-new (state-from 's0001) (statestore-new (list (state-from 's0101) (state-from 's0011)))))
    (assert (vertex-p vert1))
    (assert (= (vertex-num-edges vert1) 2)) 

    (format t "~&  vertex-new OK")
  )

  ;; Test vertex-structure-implied.
  (let (vert1 rslt1 rslt2 (*max-region* (region-new (list (state-from 's1111) (state-from 's0000)))))
    (setf vert1 (vertex-new (state-from 's0001) (statestore-new (list (state-from 's0101) (state-from 's0011)))))
    (setf rslt1 (vertex-structure-implied vert1))
    ;(format t "~&rslt1: ~A" (regionstore-str rslt1))

    (setf rslt2 (regionstore-intersection
                  (regionstore-union (state-complement (state-from 's0001)) (state-complement (state-from 's0101)))
                  (regionstore-union (state-complement (state-from 's0001)) (state-complement (state-from 's0011)))))
    ;(format t "~&rslt2: ~A" (regionstore-str rslt2))
    (assert (regionstore-eq rslt1 rslt2))
    (assert (regionstore-eq rslt1 (regionstore-from '(rX1XX rXX1X rXXX0 rX00X r1XXX))))

    (format t "~&  vertex-structure-implied OK")
  )

  ;; Test vertex-contains-state.
  (let (vert1)
    (setf vert1 (vertex-new (state-from 's0001) (statestore-new (list (state-from 's0101) (state-from 's0011)))))

    (assert (vertex-contains-state vert1 (state-from 's0001)))
    (assert (vertex-contains-state vert1 (state-from 's0101)))
    (assert (not (vertex-contains-state vert1 (state-from 's0111))))

    (format t "~&  vertex-contains-state OK")
  )

  ;; Test vertex-states.
  (let (vert1 stas)
    (setf vert1 (vertex-new (state-from 's0001) (statestore-new (list (state-from 's0101) (state-from 's0011)))))

    (setf stas (vertex-states vert1))
    (assert (statestore-p stas))
    (assert (= (statestore-length stas) 3))
    (assert (statestore-member stas (state-from 's0001)))
    (assert (statestore-member stas (state-from 's0101)))
    (assert (statestore-member stas (state-from 's0011)))
    
    (format t "~&  vertex-states OK")
  )

  (format t "~&vertex-tests done")
  t
)
