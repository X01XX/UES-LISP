;;; Run tests.
(defun maskstore-tests ()
  (format t "~&maskstore-tests beginning")

  ; Test maskstore-new.
  (let (store1)
    (setf store1 (maskstore-new nil))
    (assert (maskstore-p store1))

    (format t "~&  maskstore-new OK")
  )

  ;; Test maskstore-eq.
  (let (mskstr1 mskstr2)
    ;; Test equal.
    (setf mskstr1 (maskstore-new (list (mask-from 'm0001) (mask-from 'm0100) (mask-from 'm1000))))
    (setf mskstr2 (maskstore-new (list (mask-from 'm0100) (mask-from 'm1000) (mask-from 'm0001))))
    (assert (maskstore-eq mskstr1 mskstr2))
    (assert (maskstore-eq mskstr2 mskstr2))

    ;; Test not equal.
    (setf mskstr1 (maskstore-new (list (mask-from 'm0001) (mask-from 'm0100) (mask-from 'm1000))))
    (setf mskstr2 (maskstore-new (list (mask-from 'm0100) (mask-from 'm0010) (mask-from 'm0001))))
    (assert (not (maskstore-eq mskstr1 mskstr2)))
    (assert (not (maskstore-eq mskstr2 mskstr1)))

    ;; Test not equal by length.
    (setf mskstr1 (maskstore-new (list (mask-from 'm0001) (mask-from 'm0100))))
    (setf mskstr2 (maskstore-new (list (mask-from 'm0100) (mask-from 'm0010) (mask-from 'm0001))))
    (assert (not (maskstore-eq mskstr1 mskstr2)))
    (assert (not (maskstore-eq mskstr2 mskstr1)))

    (format t "~&  maskstore-eq OK")
  )

  ;; Test maskstore-subset-of.
  (let (mskstr1 mskstr2)
    ;; Test subset. 
    (setf mskstr1 (maskstore-new (list (mask-from 'm0001) (mask-from 'm0100) (mask-from 'm1000))))
    (setf mskstr2 (maskstore-new (list (mask-from 'm0100) (mask-from 'm1000))))
    (assert (maskstore-subset-of :sub mskstr2 :sup mskstr1))

    ;; Test not subset. 
    (assert (not (maskstore-subset-of :sub mskstr1 :sup mskstr2)))

    (format t "~&  maskstore-subset-of OK")
  )
  (format t "~&maskstore-tests done")
  t
)
