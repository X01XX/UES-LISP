;; Run action tests.
(defun action-tests ()
  (format t "~&action-tests beginning")

  ; Test action-non-adjacent-incompatible-square-needs
  (let (actx needs (*dom-id* 0) (*act-id* 1) (*max-region* (region-new (list (state-from 's1111) (state-from 's0000)))))

    (setf actx (action-from '(ACT ("[00/XX/XX/Xx]") ("[11/XX/XX/XX]"))))
    (assert (action-p actx))
    (action-set-id actx 1)

    (action-take-sample-arbitrary actx (state-from 's0101))
    (action-take-sample-arbitrary actx (state-from 's0101))
    (action-take-sample-arbitrary actx (state-from 's0101))
    (action-take-sample-arbitrary actx (state-from 's1111))
    (action-take-sample-arbitrary actx (state-from 's1111))
    (action-take-sample-arbitrary actx (state-from 's1111))

    ;(action-print actx)

    (setf needs (action-structure-needs actx (regionstore-from '(rXXXX))))
    ;(format t "~&needs: ~A" (needstore-str needs))

    (format t "~&  action-non-adjacent-incompatible-square-needs OK")
  )

  (format t "~&action-tests done")
)
