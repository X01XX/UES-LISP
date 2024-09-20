;;; Run regionscorr tests.
(defun regionscorr-tests ()

  (format t "~&regionscorr-tests beginning")

  ;; Test regionscorr-new.
  (let (regcorr1)
    ; Test new, empty, regionscorr.
    (setf regcorr1 (regionscorr-new nil))
    (assert (regionscorr-p regcorr1))

    (format t "~&  regionscorr-new OK")
  )

  ;; Test regionscorr-intersect.
  (let (regcorr1 regcorr2 regcorr3)
    (setf regcorr1 (regionscorr-new (list (region-from-str "0X") (region-from-str "1X"))))
    (setf regcorr2 (regionscorr-new (list (region-from-str "00") (region-from-str "X0"))))
    (setf regcorr3 (regionscorr-new (list (region-from-str "00") (region-from-str "01"))))
  
    (assert (regionscorr-intersects regcorr1 regcorr2))
    (assert (not (regionscorr-intersects regcorr1 regcorr3)))

    (format t "~&  regionscorr-intersects OK")
  )

  ;; Test regionscorr-intersection.
  (let (regcorr1 regcorr2 regcorr3)
    (setf regcorr1 (regionscorr-new (list (region-from-str "0X") (region-from-str "1X"))))
    (setf regcorr2 (regionscorr-new (list (region-from-str "00") (region-from-str "X0"))))

    (setf regcorr3 (regionscorr-intersection regcorr1 regcorr2))
    (assert regcorr3)
    (assert (= (regionscorr-length regcorr3) 2))

    (assert (region-eq (regionstore-first-region (regionscorr-regionstore regcorr3)) (region-from-str "00")))

    (assert (region-eq (regionstore-last-region (regionscorr-regionstore regcorr3)) (region-from-str "10")))

    (format t "~&  regionscorr-intersection OK")
  )

  ;; Test regionscorr-eq.
  (let (regcorr1 regcorr2 regcorr3)
    (setf regcorr1 (regionscorr-new (list (region-from-str "0X") (region-from-str "1X"))))
    (setf regcorr2 (regionscorr-new (list (region-from-str "0X") (region-from-str "1X"))))
    (setf regcorr3 (regionscorr-new (list (region-from-str "00") (region-from-str "X0"))))
  
    (assert (regionscorr-eq regcorr1 regcorr2))

    (assert (not (regionscorr-eq regcorr1 regcorr3)))

    (format t "~&  regionscorr-eq OK")
  )

  ;; Test regionscorr-subtract.
  (let (regcorr1 regcorr2 list1)
    (setf regcorr1 (regionscorr-new (list (region-from-str "0X") (region-from-str "1X"))))
    (setf regcorr2 (regionscorr-new (list (region-from-str "00") (region-from-str "X0"))))
    (setf list1 (regionscorr-subtract :min-regscorr regcorr1 :sub-regscorr regcorr2))

    (assert (= 2 (regionscorrstore-length list1)))

    (assert (regionscorrstore-contains list1 (regionscorr-new (list (region-from-str "0X") (region-from-str "11")))))

    (assert (regionscorrstore-contains list1 (regionscorr-new (list (region-from-str "01") (region-from-str "1X")))))

    (format t "~&  regionscorr-subtract OK")
  )

  ;; Test regionscorr-x-mask.
  (let (regcorr1 mskx)
    (setf regcorr1 (regionscorr-new (list (region-from-str "0X") (region-from-str "1X"))))
    (setf mskx (regionscorr-x-mask regcorr1))
    (assert (maskscorr-eq mskx (maskscorr-new (list (mask-from-str "#b01") (mask-from-str "#b01")))))

    (format t "~&  regionscorr-x-mask OK")
  )

  ;; Test regionscorr-1-mask.
  (let (regcorr1 mskx)
    (setf regcorr1 (regionscorr-new (list (region-from-str "0X") (region-from-str "1X"))))
    (setf mskx (regionscorr-1-mask regcorr1))
    (assert (maskscorr-eq mskx (maskscorr-new (list (mask-from-str "#b00") (mask-from-str "#b10")))))

    (format t "~&  regionscorr-1-mask OK")
  )

  ;; Test regionscorr-0-mask.
  (let (regcorr1 mskx)
    (setf regcorr1 (regionscorr-new (list (region-from-str "0X") (region-from-str "1X"))))
    (setf mskx (regionscorr-0-mask regcorr1))
    (assert (maskscorr-eq mskx (maskscorr-new (list (mask-from-str "#b10") (mask-from-str "#b00")))))

    (format t "~&  regionscorr-0-mask OK")
  )

  ;; Test regionscorr-set-to-zeros.
  (let (regcorr1 msk1 regcorr2)
    (setf regcorr1 (regionscorr-new (list (region-from-str "01X") (region-from-str "10X"))))
    (setf msk1 (maskscorr-new (list (mask-from-str "#b111") (mask-from-str "#b010"))))
    (setf regcorr2 (regionscorr-set-to-zeros regcorr1 msk1))
    (assert (regionscorr-eq regcorr2 (regionscorr-new (list (region-from-str "000") (region-from-str "10X")))))

    (format t "~&  regionscorr-set-to-zeros OK")
  )

  ;; Test regionscorr-set-to-ones.
  (let (regcorr1 msk1 regcorr2)
    (setf regcorr1 (regionscorr-new (list (region-from-str "01X") (region-from-str "10X"))))
    (setf msk1 (maskscorr-new (list (mask-from-str "#b111") (mask-from-str "#b010"))))
    (setf regcorr2 (regionscorr-set-to-ones regcorr1 msk1))
    (assert (regionscorr-eq regcorr2 (regionscorr-new (list (region-from-str "111") (region-from-str "11X")))))

    (format t "~&  regionscorr-set-to-ones OK")
  )

  ;; Test regionscorr-translate-to.
  (let (regcorr1 regcorr2 regcorr3)
    (setf regcorr1 (regionscorr-new (list (region-from-str "0_1XX1") (region-from-str "1_0XX0"))))
    (setf regcorr2 (regionscorr-new (list (region-from-str "1_01XX") (region-from-str "1_00XX"))))
    (setf regcorr3 (regionscorr-translate-to regcorr1 regcorr2))
    (assert (regionscorr-eq regcorr3 (regionscorr-new (list (region-from-str "1_01X1") (region-from-str "1_00X0")))))

    (format t "~&  regionscorr-translate-to OK")
  )

  (format t "~&regionscorr-tests done")
  t
)

