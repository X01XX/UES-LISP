;;;; Run tests.
(defun value-tests ()
  (format t "~&value-tests beginning")

  ; Test value-new.
  (let (valx)
    (setf valx (value-new :num-bits 4 :bits 7))
    (assert (value-p valx))
    (assert (and (value-p valx) (= (value-num-bits valx) 4) (= (value-bits valx) 7)))

    (format t "~&  value-new OK")
  )

  ; Test value-from.
  (let (valx errx)

    ; Test string does not start with the v character.
    (setf errx (value-from-str "x1"))
    (assert (err-p errx))

    ; Test string for invalid binary digit.
    (setf errx (value-from-str "v012"))
    (assert (err-p errx))

    ; Test valid bits, with leading zero.
    (setf valx (value-from 'v0101_0010))
    (assert (and (value-p valx) (= (value-num-bits valx) 8) (= (value-bits valx) #x52)))

    ; Test single bit, uppercase prefix.
    (setf valx (value-from 'V0))
    (assert (and (value-p valx) (= (value-num-bits valx) 1) (= (value-bits valx) 0)))
    (setf valx (value-from 'V1))
    (assert (and (value-p valx) (= (value-num-bits valx) 1) (= (value-bits valx) 1)))

    (format t "~&  value-from OK")
  )

  ; Test value-str.
  (let (strx)
    (setf strx (value-str (value-from 'v0101_1000)))
    (assert (and (stringp strx) (string= strx "v0101_1000")))

    (format t "~&  value-str OK")
  )

  ; Test value-zerop.
  (let (boolx)
    ; Test a non-zero value.
    (setf boolx (value-zerop (value-from 'v01)))
    (assert (and (bool-p boolx) (null boolx)))

    ; Test a zero value.
    (setf boolx (value-zerop (value-from 'v00)))
    (assert (and (bool-p boolx) boolx))

    (format t "~&  value-zerop OK")
  )

  ; Test value-num-ones.
  (let (numx)
    ; Test a non-zero value.
    (setf numx (value-num-ones (value-from 'v10101)))
    (assert (and (integerp numx) (= numx 3)))

    ; Test a zero value.
    (setf numx (value-num-ones (value-from 'v0)))
    (assert (and (integerp numx) (zerop numx)))

    (format t "~&  value-num-ones OK")
  )

  ; Test value-not.
  (let (valx)
    (setf valx (value-not (value-from 'v0101_1010)))
    (assert (and (value-p valx) (value-eq valx (value-from 'v1010_0101))))

    (format t "~&  value-not OK")
  )

  ; Test value-eq.
  (let (boolx val1 val2 val3)
    (setf val1 (value-from 'v01))
    (setf val2 (value-from 'v10))
    (setf val3 (value-from 'v01))

    ; Test two values that are eq.
    (setf boolx (value-eq val1 val3))
    (assert (and (bool-p boolx) boolx))

    ; Test two values that are not eq.
    (setf boolx (value-eq val1 val2))
    (assert (and (bool-p boolx) (not boolx)))

    (format t "~&  value-eq OK")
  )

  ; Test value-or.
  (let (valx val1 val2 val6)
    (setf val1 (value-from 'v0001))
    (setf val2 (value-from 'v0010))
    (setf val6 (value-from 'v0110))

    ; Test or of three values.
    (setf valx (value-or val1 val2 val6))
    (assert (and (value-p valx) (value-eq valx (value-from 'v0111))))

    (format t "~&  value-or OK")
  )

  ; Test value-and.
  (let (valx val2 val7 vala)
    (setf val7 (value-from 'v0111))
    (setf val2 (value-from 'v1110))
    (setf vala (value-from 'v1011))

    ; Test and of three values.
    (setf valx (value-and val7 val2 vala))
    (assert (and (value-p valx) (value-eq valx (value-from 'v0010))))

    (format t "~&  value-and OK")
  )

  ; Test value-xor.
  (let (valx val1 val2)
    (setf val1 (value-from 'v0011))
    (setf val2 (value-from 'v0110))

    (setf valx (value-xor val1 val2))
    (assert (and (value-p valx) (value-eq valx (value-from 'v0101))))

    (format t "~&  value-xor OK")
  )

  ; Test value-eqv.
  (let (valx val1 val2)
    (setf val1 (value-from 'v0011))
    (setf val2 (value-from 'v0110))

    (setf valx (value-eqv val1 val2))
    (assert (and (value-p valx) (value-eq valx (value-from 'v1010))))

    (format t "~&  value-eqv OK")
  )

  ; Test value-split.
  (let (lstx)
    ; Test splitting 5, to (1, 4).
    (setf lstx (value-split (value-from 'v0101)))
    (assert (and (listp lstx) (= (length lstx) 2)))
    (assert (member (value-from 'v0001) lstx :test #'value-eq))
    (assert (member (value-from 'v0100) lstx :test #'value-eq))

    ; Test splitting 0.
    (setf lstx (value-split (value-from 'v0)))
    (assert (and (listp lstx) (= (length lstx) 0)))

    ; Test splitting 1.
    (setf lstx (value-split (value-from 'v1)))
    (assert (and (listp lstx) (= (length lstx) 1)))
    (assert (member (value-from 'v1) lstx :test #'value-eq))

    (format t "~&  value-split OK")
  )

  ; Test value-msb.
  (let (valx)
    ; Test one-bit value.
    (setf valx (value-msb (value-from 'v0)))
    (assert (and (value-p valx) (value-eq valx (value-from 'v1))))

    ; Test three-bit value.
    (setf valx (value-msb (value-from 'v000)))
    (assert (and (value-p valx) (value-eq valx (value-from 'v100))))

    (format t "~&  value-msb OK")
  )

  ; Test value-shift-right.
  (let (valx val5)
    (setf val5 (value-from 'v0101))

    (setf valx (value-shift-right val5))
    (assert (and (value-p valx) (value-eq valx (value-from 'v0010))))

    (format t "~&  value-shift-right OK")
  )

  (format t "~&value-tests done")
  t
)
