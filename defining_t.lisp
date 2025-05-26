;;; Run tests.
(defun defining-tests ()
  (format t "~&defining-tests beginning")

  ; Test defining-new.
  (let (defx)
    (setf defx (defining-new (region-from 'r00XX)
               (vertex-new (state-from 's0011) (statestore-new (list (state-from 's0111) (state-from 's1011))))))
    (assert (defining-p defx))

    (format t "~&  defining-new OK")
  )

  (format t "~&defining-tests done")
  t
)
