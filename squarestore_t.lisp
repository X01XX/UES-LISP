;;; Run squarestore tests.
(defun squarestore-tests ()
  (format t "~&squarestore-tests beginning")

  ;; Test squarestore-new.
  (let (sqrs sqr1 sqr2)
    (setf sqr1 (square-new (sample-new :initial (state-from 's0101) :result (state-from 's1010))))
    (setf sqr2 (square-new (sample-new :initial (state-from 's0111) :result (state-from 's1010))))

    ;; Test -new with no states.
    (setf sqrs (squarestore-new nil))
    (assert (squarestore-p sqrs))
    (assert (squarestore-is-empty sqrs))

    ;; Test -new with single states.
    (setf sqrs (squarestore-new sqr1 sqr2))
    (assert (squarestore-p sqrs))
    (assert (= (squarestore-length sqrs) 2))

    ;; Test -new with a list of states.
    (setf sqrs (squarestore-new (list sqr1 sqr2)))
    (assert (squarestore-p sqrs))
    (assert (= (squarestore-length sqrs) 2))

    (format t "~&  squarestore-new OK")
  )

  (format t "~&squarestore-tests done")
  t
)
