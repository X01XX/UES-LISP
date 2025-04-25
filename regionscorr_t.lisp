;;; Run regionscorr tests.
(defun regionscorr-tests ()

  (format t "~&regionscorr-tests beginning")

  ;; Test regionscorr-new.
  (let (regcorr1  (*domain-num-bits-list* (list 2)))
    (setf regcorr1 (regionscorr-new (list (region-from 'r01))))
    (assert (regionscorr-p regcorr1))

    (format t "~&  regionscorr-new OK")
  )

  ;; Test regionscorr-intersect.
  (let (regcorr1 regcorr2 regcorr3 (*domain-num-bits-list* (list 2 2)))
    (setf regcorr1 (regionscorr-new (list (region-from 'r0X) (region-from 'r1X))))
    (setf regcorr2 (regionscorr-new (list (region-from 'r00) (region-from 'rX0))))
    (setf regcorr3 (regionscorr-new (list (region-from 'r00) (region-from 'r01))))

    (assert (regionscorr-intersects regcorr1 regcorr2))
    (assert (not (regionscorr-intersects regcorr1 regcorr3)))

    (format t "~&  regionscorr-intersects OK")
  )

  ;; Test regionscorr-intersection.
  (let (regcorr1 regcorr2 regcorr3 (*domain-num-bits-list* (list 2 2)))
    (setf regcorr1 (regionscorr-new (list (region-from 'r0X) (region-from 'r1X))))
    (setf regcorr2 (regionscorr-new (list (region-from 'r00) (region-from 'rX0))))

    (setf regcorr3 (regionscorr-intersection regcorr1 regcorr2))
    (assert (regionscorr-p regcorr3))

    (assert (regionstore-member (regionscorr-regionstore regcorr3) (region-from 'r00)))

    (assert (regionstore-member (regionscorr-regionstore regcorr3) (region-from 'r10)))

    (format t "~&  regionscorr-intersection OK")
  )

  ;; Test regionscorr-eq.
  (let (regcorr1 regcorr2 regcorr3 (*domain-num-bits-list* (list 2 2)))
    (setf regcorr1 (regionscorr-new (list (region-from 'r0X) (region-from 'r1X))))
    (setf regcorr2 (regionscorr-new (list (region-from 'r0X) (region-from 'r1X))))
    (setf regcorr3 (regionscorr-new (list (region-from 'r00) (region-from 'rX0))))

    (assert (regionscorr-eq regcorr1 regcorr2))

    (assert (not (regionscorr-eq regcorr1 regcorr3)))

    (format t "~&  regionscorr-eq OK")
  )

  ;; Test regionscorr-subtract.
  (let (regcorr1 regcorr2 list1 (*domain-num-bits-list* (list 2 2)))
    (setf regcorr1 (regionscorr-new (list (region-from 'r0X) (region-from 'r1X))))
    (setf regcorr2 (regionscorr-new (list (region-from 'r00) (region-from 'rX0))))
    (setf list1 (regionscorr-subtract :min regcorr1 :sub regcorr2))

    (assert (= 2 (regionscorrstore-length list1)))

    (assert (regionscorrstore-member list1 (regionscorr-new (list (region-from 'r0X) (region-from 'r11)))))

    (assert (regionscorrstore-member list1 (regionscorr-new (list (region-from 'r01) (region-from 'r1X)))))

    (format t "~&  regionscorr-subtract OK")
  )

  ;; Test regionscorr-from.
  (let (regs1 (*domain-num-bits-list* (list 4 3)))

    (setf regs1 (regionscorr-from '(RC (r1011 r111))))
    (assert (regionscorr-p regs1))

    (format t "~&  regionscorr-from OK")
  )

  (format t "~&regionscorr-tests done")
  t
)

