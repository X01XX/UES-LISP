;;; Run regionpair tests.
(defun regionpair-tests ()
  (format t "~&regionpair-tests beginning")

  ;; Test regionpair-new
  ;; Also regionpair-num-bits.
  (let (rp1)

    (setf rp1 (regionpair-new (list (region-from 'r101X) (region-from 'r001X))))
    (assert (regionpair-p rp1))
    (assert (= (regionpair-num-bits rp1) 4))

    (format t "~&  regionpair-new OK")
  )

  ;; Test regionpair-eq
  (let ()
    ;; Test positive, corresponding regions.
    (assert (regionpair-eq (regionpair-new (list (region-from 'r101X) (region-from 'r001X)))
                           (regionpair-new (list (region-from 'r101X) (region-from 'r001X)))))

    ;; Test positive, not corresponding regions.
    (assert (regionpair-eq (regionpair-new (list (region-from 'r101X) (region-from 'r001X)))
                           (regionpair-new (list (region-from 'r001X) (region-from 'r101X)))))

    ;; Test negative.
    (assert (not (regionpair-eq (regionpair-new (list (region-from 'r101X) (region-from 'r001X)))
                                (regionpair-new (list (region-from 'r011X) (region-from 'r111X))))))

    (format t "~&  regionpair-eq OK")
  )

  ;; Test regionpair-superset-of.
  (let (rp1 rp2)

    ;; Test positive.
    (setf rp1 (regionpair-new (list (region-from 'r101X) (region-from 'r001X))))
    (setf rp2 (regionpair-new (list (region-from 'r1010) (region-from 'r0010))))
    (assert (regionpair-superset-of :sup rp1 :sub rp2))

    ;; Test negative.
    (setf rp1 (regionpair-new (list (region-from 'r101X) (region-from 'r001X))))
    (setf rp2 (regionpair-new (list (region-from 'r1000) (region-from 'r0000))))
    (assert (not (regionpair-superset-of :sup rp1 :sub rp2)))

    (format t "~&  regionpair-superset-of OK")
  )

  ;; Test regionpair-superset-of-state.
  (let (rp1)

    ;; Test positive.
    (setf rp1 (regionpair-new (list (region-from 'r101X) (region-from 'r001X))))
    (assert (regionpair-superset-of-state rp1 (state-from 's1010)))
    (assert (regionpair-superset-of-state rp1 (state-from 's0010)))

    ;; Test negative.
    (assert (not (regionpair-superset-of-state rp1 (state-from 's0110))))

    (format t "~&  regionpair-superset-of-state OK")
  )

  ;; Test regionpair-symmetric-state
  (let (rp1 sta1)

    (setf rp1 (regionpair-new (list (region-from 'r101X) (region-from 'r001X))))
    (assert (regionpair-p rp1))

    ;; Test state in the first region.
    (setf sta1 (regionpair-symmetric-state rp1 (state-from 's1010)))
    (assert (state-p sta1))
    (assert (state-eq sta1 (state-from 's0010)))

    ;; Test state in the second region.
    (setf sta1 (regionpair-symmetric-state rp1 (state-from 's0011)))
    (assert (state-p sta1))
    (assert (state-eq sta1 (state-from 's1011)))

    (format t "~&  regionpair-symmetric-state OK")
  )

  ;; Test regionpair-dif-mask
  (let (rp1 msk1)

    (setf rp1 (regionpair-new (list (region-from 'r101X) (region-from 'r001X))))
    (setf msk1 (regionpair-dif-mask rp1))
    (assert (mask-p msk1))
    (assert (mask-eq msk1 (mask-from 'm1000)))

    (format t "~&  regionpair-dif-mask OK")
  )

  (format t "~&regionpair-tests done")
  t
)
