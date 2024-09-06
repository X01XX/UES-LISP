;;; Run selectregions tests.
(defun selectregions-tests ()

  (format t "~&selectregions-tests beginning")

  ; Test selectregions-new.
  (let (selectregions1)
    ; Test new, empty, selectregions.
    (setf selectregions1 (selectregions-new
			   (regionscorr-new (list (region-from-str "0X") (region-from-str "1X"))) 2 -3))
    ;(format t "~&~A" selectregions1)
    (assert (selectregions-p selectregions1))
  )

  (format t "~&selectregions-tests done")
  t
)

