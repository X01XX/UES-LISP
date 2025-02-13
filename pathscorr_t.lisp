;;; Run pathscorr tests.
(defun pathscorr-tests ()

  (format t "~&pathscorr-tests beginning")

  ; Test pathscorr-new.
  (let (pathscorr1)
    ; Test new, empty, pathscorr.
    (setf pathscorr1 (pathscorr-new nil))
    (assert (pathscorr-p pathscorr1))
    (assert (pathscorr-is-empty pathscorr1))

    ; Test new, non-empty, pathscorr.
    (setf pathscorr1 (pathscorr-new (list (regionscorr-new (list (region-from "01") (region-from "00"))))))
    (assert (pathscorr-p pathscorr1))
    (assert (pathscorr-is-not-empty pathscorr1))

    (format t "~&  pathscorr-new OK")
  )

  ; Test pathscorr-add-start.
  (let (pathscorr1)
    (setf pathscorr1 (pathscorr-new (list (regionscorr-new (list (region-from "0x") (region-from "0x"))))))
    (pathscorr-add-start pathscorr1 (regionscorr-new (list (region-from "01") (region-from "01"))))
    (assert (= (pathscorr-length pathscorr1) 2))
    (assert (regionscorr-eq (pathscorr-first-region pathscorr1) (regionscorr-new (list (region-from "01") (region-from "01")))))
    (assert (regionscorr-eq (pathscorr-last-region pathscorr1) (regionscorr-new (list (region-from "0x") (region-from "0x")))))

    (format t "~&  pathscorr-add-start OK")
  )

  ; Test pathscorr-add-end.
  (let (pathscorr1)
    (setf pathscorr1 (pathscorr-new (list (regionscorr-new (list (region-from "01") (region-from "01"))))))
    (pathscorr-add-end pathscorr1 (regionscorr-new (list (region-from "0x") (region-from "0x"))))
    (assert (= (pathscorr-length pathscorr1) 2))
    (assert (regionscorr-eq (pathscorr-first-region pathscorr1) (regionscorr-new (list (region-from "01") (region-from "01")))))
    (assert (regionscorr-eq (pathscorr-last-region pathscorr1) (regionscorr-new (list (region-from "0x") (region-from "0x")))))

    (format t "~&  pathscorr-add-end OK")
  )

  (format t "~&pathscorr-tests done")
  t
)

