;;; Run pathcorr tests.
(defun pathcorr-tests ()

  (format t "~&pathcorr-tests beginning")

  ; Test pathcorr-new.
  (let (pathcorr1)
    ; Test new, empty, pathcorr.
    (setf pathcorr1 (pathcorr-new nil))
    (assert (pathcorr-p pathcorr1))
    (assert (pathcorr-is-empty pathcorr1))

    ; Test new, non-empty, pathcorr.
    (setf pathcorr1 (pathcorr-new (list (regionscorr-new (list (region-from-str "01") (region-from-str "00"))))))
    (assert (pathcorr-p pathcorr1))
    (assert (pathcorr-is-not-empty pathcorr1))

    (format t "~&  pathcorr-new OK")
  )

  ; Test pathcorr-add-start.
  (let (pathcorr1)
    (setf pathcorr1 (pathcorr-new (list (regionscorr-new (list (region-from-str "0x") (region-from-str "0x"))))))
    (pathcorr-add-start pathcorr1 (regionscorr-new (list (region-from-str "01") (region-from-str "01"))))
    (assert (= (pathcorr-length pathcorr1) 2))
    (assert (regionscorr-eq (pathcorr-first-region pathcorr1) (regionscorr-new (list (region-from-str "01") (region-from-str "01")))))
    (assert (regionscorr-eq (pathcorr-last-region pathcorr1) (regionscorr-new (list (region-from-str "0x") (region-from-str "0x")))))

    (format t "~&  pathcorr-add-start OK")
  )

  ; Test pathcorr-add-end.
  (let (pathcorr1)
    (setf pathcorr1 (pathcorr-new (list (regionscorr-new (list (region-from-str "01") (region-from-str "01"))))))
    (pathcorr-add-end pathcorr1 (regionscorr-new (list (region-from-str "0x") (region-from-str "0x"))))
    (assert (= (pathcorr-length pathcorr1) 2))
    (assert (regionscorr-eq (pathcorr-first-region pathcorr1) (regionscorr-new (list (region-from-str "01") (region-from-str "01")))))
    (assert (regionscorr-eq (pathcorr-last-region pathcorr1) (regionscorr-new (list (region-from-str "0x") (region-from-str "0x")))))

    (format t "~&  pathcorr-add-end OK")
  )

  (format t "~&pathcorr-tests done")
  t
)

