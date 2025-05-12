;;; Run tests.
(defun maskstore-tests ()
  (format t "~&maskstore-tests beginning")

  ; Test maskstore-new.
  (let (msks msk1 msk2)

    (setf msk1 (mask-from 'm0101))
    (setf msk2 (mask-from 'm0111))

    ;; Test -new with no masks.
    (setf msks (maskstore-new nil))
    (assert (maskstore-p msks))
    (assert (maskstore-is-empty msks))

    ;; Test -new with single masks.
    (setf msks (maskstore-new msk1 msk2))
    (assert (maskstore-p msks))
    (assert (= (maskstore-length msks) 2)) 

    ;; Test -new with a list of masks.
    (setf msks (maskstore-new (list msk1 msk2)))
    (assert (maskstore-p msks))
    (assert (= (maskstore-length msks) 2)) 

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
