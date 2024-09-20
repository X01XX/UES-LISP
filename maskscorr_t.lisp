;;;; Run maskscorr tests.
(defun maskscorr-tests ()
  (format t "~&maskscorr-tests beginning")

  ;; Test maskscorr-new
  (let (msksc1)
    (setf msksc1 (maskscorr-new (list (mask-from-str "#x01") (mask-from-str "#x0110"))))
    (assert (maskscorr-p msksc1))
    (assert (= (maskscorr-length msksc1) 2))
    (assert (mask-eq (maskstore-first-mask (maskscorr-maskstore msksc1)) (mask-from-str "#x01")))
    (assert (mask-eq (maskstore-last-mask (maskscorr-maskstore msksc1)) (mask-from-str "#x0110")))

    (format t "~&  maskscorr-new OK")
  )

  ;; Test maskscorr-eq
  (let (msksc1 msksc2 msksc3)
    (setf msksc1 (maskscorr-new (list (mask-from-str "#x01") (mask-from-str "#x0110"))))
    (setf msksc2 (maskscorr-new (list (mask-from-str "#x01") (mask-from-str "#x0110"))))
    (setf msksc3 (maskscorr-new (list (mask-from-str "#x01") (mask-from-str "#x1110"))))

    (assert (maskscorr-eq msksc1 msksc2))
    (assert (not (maskscorr-eq msksc1 msksc3)))

    (format t "~&  maskscorr-eq OK")
  )

  ;; Test maskscorr-num-ones.
  (let (msksc1)
    (setf msksc1 (maskscorr-new (list (mask-from-str "#x01") (mask-from-str "#x0110"))))
    (assert (= (maskscorr-num-ones msksc1) 3))

    (format t "~&  maskscorr-num-ones OK")
  )

  (format t "~&maskscorr-tests done")
)
