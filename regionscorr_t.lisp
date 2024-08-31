;;; Run regionscorr tests.
(defun regionscorr-tests ()

  (format t "~&regionscorr-tests beginning")

  ; Test regionscorr-new.
  (let (regcorr1)
    ; Test new, empty, regionscorr.
    (setf regcorr1 (regionscorr-new (regionstore-new nil)))
    (assert (regionscorr-p regcorr1))

    (format t "~&  regionscorr-new OK")
  )

  ; Test regionscorr-intersect.
  (let (regcorr1 regcorr2 regcorr3)
    (setf regcorr1 (regionscorr-new (regionstore-new (list (region-from-str "0X1X") (region-from-str "1X")))))
    (setf regcorr2 (regionscorr-new (regionstore-new (list (region-from-str "001X") (region-from-str "X0")))))
    (setf regcorr3 (regionscorr-new (regionstore-new (list (region-from-str "001X") (region-from-str "01")))))
  
    (assert (regionscorr-intersect regcorr1 regcorr2))
    (assert (not (regionscorr-intersect regcorr1 regcorr3)))

    (format t "~&  regionscorr-intersect OK")
  )

  ; Test regionscorr-intersection.
  (let (regcorr1 regcorr2 regcorr3)
    (setf regcorr1 (regionscorr-new (regionstore-new (list (region-from-str "0X1X") (region-from-str "1X")))))
    (setf regcorr2 (regionscorr-new (regionstore-new (list (region-from-str "001X") (region-from-str "X0")))))
  
    (setf regcorr3 (regionscorr-intersection regcorr1 regcorr2))
    (assert regcorr3)
    (assert (= (regionscorr-length regcorr3) 2))

    (assert (region-eq (regionstore-first-region (regionscorr-regions regcorr3)) (region-from-str "001X")))
    (assert (region-eq (regionstore-last-region (regionscorr-regions regcorr3)) (region-from-str "10")))
    (format t "~&  regionscorr-intersection OK")
  )

  ; Test regionscorr-eq.
  (let (regcorr1 regcorr2 regcorr3)
    (setf regcorr1 (regionscorr-new (regionstore-new (list (region-from-str "0X1X") (region-from-str "1X")))))
    (setf regcorr2 (regionscorr-new (regionstore-new (list (region-from-str "0X1X") (region-from-str "1X")))))
    (setf regcorr3 (regionscorr-new (regionstore-new (list (region-from-str "001X") (region-from-str "X0")))))
  
    (assert (regionscorr-eq regcorr1 regcorr2))

    (assert (not (regionscorr-eq regcorr1 regcorr3)))

    (format t "~&  regionscorr-eq OK")
  )

  ; Test regionscorr-subtract.
  (let (regcorr1 regcorr2 list1)
    (setf regcorr1 (regionscorr-new (regionstore-new (list (region-from-str "0X1X") (region-from-str "1X")))))
    (setf regcorr2 (regionscorr-new (regionstore-new (list (region-from-str "001X") (region-from-str "X0")))))

    (setf list1 (regionscorr-subtract :min-regcorr regcorr1 :sub-regcorr regcorr2))

    ;(format t "~&results ~A" list1)
    (assert (= 2 (length list1)))
    (assert (member (regionscorr-new (regionstore-new (list (region-from-str "0X1X") (region-from-str "11"))))
	    list1 :test #'regionscorr-eq))

    (assert (member (regionscorr-new (regionstore-new (list (region-from-str "011X") (region-from-str "1X"))))
	    list1 :test #'regionscorr-eq))

    (format t "~&  regionscorr-subtract OK")
  )

  (format t "~&regionscorr-tests done")
  t
)

