;;; Run statescorr tests.
(defun statescorr-tests ()
  (format t "~&statescorr-tests beginning")

  ;; Test statescorr-new.
  (let (stacorr1  (*domain-num-bits-list* (list 2)))
    (setf stacorr1 (statescorr-new (list (state-from 's01))))
    (assert (statescorr-p stacorr1))

    (format t "~&  statescorr-new OK")
  )

  ;; Test statescorr-eq.
  (let (stacorr1  stacorr2 stacorr3 (*domain-num-bits-list* (list 2)))
    (setf stacorr1 (statescorr-new (list (state-from 's01))))
    (assert (statescorr-p stacorr1))

    (setf stacorr2 (statescorr-new (list (state-from 's01))))
    (assert (statescorr-p stacorr2))

    (setf stacorr3 (statescorr-new (list (state-from 's11))))
    (assert (statescorr-p stacorr3))

    (assert (statescorr-eq stacorr1 stacorr2))
    (assert (not (statescorr-eq stacorr1 stacorr3)))

    (format t "~&  statescorr-eq OK")
  )

  ;; Test statescorr-from.
  (let (stacorr1  (*domain-num-bits-list* (list 2 3)))
 
    (setf stacorr1 (statescorr-from '(SC (s01 s011))))
    (assert (statescorr-p stacorr1))

    (assert (state-eq (car (statescorr-state-list stacorr1)) (state-from 's01)))
    (assert (state-eq (second (statescorr-state-list stacorr1)) (state-from 's011)))

    (format t "~&  statescorr-eq OK")
  )

  (format t "~&statescorr-tests done")
  t
)
