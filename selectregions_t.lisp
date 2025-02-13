;;; Run selectregions tests.
(defun selectregions-tests ()

  (format t "~&selectregions-tests beginning")

  ; Test selectregions-new.
  (let (selectregions1)
    ; Test new, empty, selectregions.
    (setf selectregions1 (selectregions-new
			   (regionscorr-new (list (region-from "0X") (region-from "1X"))) -3))
    ;(format t "~&~A" selectregions1)
    (assert (selectregions-p selectregions1))
    (format t "~&  selectregions-new OK")
  )

  ;; Test selectregions-from.
  (let (sr1)
    
    (setf sr1 (selectregions-from "SR[RC[x0xx1, XXXX_XXX1_1XXX_XXXX], 2]"))
    (assert (selectregions-p sr1))
    (assert (= (selectregions-net-value sr1) 2))
    (assert (= (regionscorr-length (selectregions-regionscorr sr1)) 2))

    (format t "~&  selectregions-from OK")
  )

  (format t "~&selectregions-tests done")
  t
)

