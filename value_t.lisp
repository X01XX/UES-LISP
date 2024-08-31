;;;; Run tests.
(defun value-tests ()
  (format t "~&value-tests beginning")

  ; Test value-new.
  (let (valx)
    ; Test valid value parameters.
    (setf valx (value-new :num-bits 4 :bits 7))
    (assert (value-p valx))
    (assert (and (value-p valx) (= (value-num-bits valx) 4) (= (value-bits valx) 7)))

    (format t "~&  value-new OK")
  )

  ; Test value-from-str.
  (let (valx errx)
    ; Test string second character is not b, B, x or X.
    (setf errx (value-from-str-na "#c1"))
    (assert (and (err-p errx) (string= (err-str errx) "Second character is not b, B, x or X")))

    ; Test string does not start with the # character.
    (setf errx (value-from-str-na "0b1"))
    (assert (and (err-p errx) (string= (err-str errx) "String does not begin with the # character")))

    ; Test string for invalid binary digit.
    (setf errx (value-from-str-na "#b012"))
    (assert (and (err-p errx) (string= (err-str errx) "Invalid binary digit")))

    ; Test string for invalid hexadecimal digit.
    (setf errx (value-from-str-na "#x0123456789abcdefABCDEFh"))
    (assert (and (err-p errx) (string= (err-str errx) "Invalid hexadecimal digit")))

    ; Test valid binary string.
    (setf valx (value-from-str "#b1101"))
    (assert (and (value-p valx) (= (value-num-bits valx) 4) (= (value-bits valx) 13)))

    ; Test valid binary string.
    (setf valx (value-from-str "#B1101"))
    (assert (and (value-p valx) (= (value-num-bits valx) 4) (= (value-bits valx) 13)))

    ; Test valid hexadecimal string.
    (setf valx (value-from-str "#x1b"))
    (assert (and (value-p valx) (= (value-num-bits valx) 8) (= (value-bits valx) 27)))

    ; Test valid hexadecimal string.
    (setf valx (value-from-str "#X1B"))
    (assert (and (value-p valx) (= (value-num-bits valx) 8) (= (value-bits valx) 27)))

    (format t "~&  value-from-str OK")
  )

  ; Test string-add-underscores.
  (let (strx errx)
    ; Test string that contains an underscore.
    (setf errx (string-add-underscores-na "1_234"))
    (assert (and (err-p errx) (string= (err-str errx) "Argument contains underscores")))

    ; Test empty string.
    (setf strx (string-add-underscores ""))
    (assert (and (stringp strx) (string= strx "")))

    (setf strx (string-add-underscores "12345"))
    (assert (and (stringp strx) (string= strx "1_2345")))

    (format t "~&  string-add-underscores OK")
  )

  ; Test value-str.
  (let (strx)
    ; Test binary value.
    (setf strx (value-str (value-from-str "#b010")))
    (assert (and (stringp strx) (string= strx "#b010")))

    ; Test binary value, that can be expresed in hexadecimal.
    (setf strx (value-str (value-from-str "#b0101_1000")))
    (assert (and (stringp strx) (string= strx "#x58")))

    (format t "~&  value-str OK")
  )

  ; Test value-str-bin.
  (let (strx)
    ; Test good value.
    (setf strx (value-str-bin (value-from-str "#x01")))
    (assert (and (stringp strx) (string= strx "#b0000_0001")))

    (format t "~&  value-str-bin OK")
  )

  ; Test value-str-hex.
  (let (strx)
    ; Test good value.
    (setf strx (value-str-hex (value-from-str "#b0000_0001")))
    ;(format t "~&val is ~A" val)
    (assert (and (stringp strx) (string= strx "#x01")))

    (format t "~&  value-str-hex OK")
  )

  ; Test value-zerop.
  (let (boolx)
    ; Test a non-zero value.
    (setf boolx (value-zerop (value-from-str "#x01")))
    (assert (null boolx))

    ; Test a zero value.
    (setf boolx (value-zerop (value-from-str "#x00")))
    (assert (and (bool-p boolx) boolx))

    (format t "~&  value-zerop OK")
  )

  ; Test value-num-ones.
  (let (numx)
    ; Test a non-zero value.
    (setf numx (value-num-ones (value-from-str "#x51")))
    (assert (and (integerp numx) (= numx 3)))

    ; Test a zero value.
    (setf numx (value-num-ones (value-from-str "#x00")))
    (assert (and (integerp numx) (zerop numx)))

    (format t "~&  value-num-ones OK")
  )

  ; Test value-not.
  (let (valx)
    ; Test a value.
    (setf valx (value-not (value-from-str "#x5a")))
    (assert (and (value-p valx) (value-eq valx (value-from-str "#xa5"))))

    (format t "~&  value-not OK")
  )

  ; Test value-is-adjacent.
  (let (boolx val1 val2 val3 val4)
    (setf val1 (value-from-str "#x1"))
    (setf val2 (value-from-str "#x2"))
    (setf val3 (value-from-str "#x3"))
    (setf val4 (value-from-str "#x33"))

    ; Test two values that are adjacent.
    (setf boolx (value-is-adjacent val1 val3))
    (assert (and (bool-p boolx) boolx))

    ; Test two values that are not adjacent.
    (setf boolx (value-is-adjacent val1 val2))
    (assert (and (bool-p boolx) (not boolx)))

    (format t "~&  value-is-adjacent OK")
  )

  ; Test value-eq.
  (let (boolx val1 val2 val3)
    (setf val1 (value-from-str "#x1"))
    (setf val2 (value-from-str "#x2"))
    (setf val3 (value-from-str "#x1"))

    ; Test two values that are eq.
    (setf boolx (value-eq val1 val3))
    (assert (and (bool-p boolx) boolx))

    ; Test two values that are not adjacent.
    (setf boolx (value-eq val1 val2))
    (assert (and (bool-p boolx) (not boolx)))

    (format t "~&  value-eq OK")
  )

  ; Test value-or.
  (let (valx val1 val2 val6)
    (setf val1 (value-from-str "#x1"))
    (setf val2 (value-from-str "#x2"))
    (setf val6 (value-from-str "#x6"))

    ; Test or of three values.
    (setf valx (value-or val1 val2 val6))
    ;(format t "~& val: ~A" val)
    (assert (and (value-p valx) (value-eq valx (value-from-str "#x7"))))

    (format t "~&  value-or OK")
  )

  ; Test value-and.
  (let (valx val2 val7 vala)
    (setf val7 (value-from-str "#x7"))
    (setf val2 (value-from-str "#x6"))
    (setf vala (value-from-str "#xa"))

    ; Test or of two values.
    (setf valx (value-and val7 val2 vala))
    ;(format t "~& val: ~A" val)
    (assert (and (value-p valx) (value-eq valx (value-from-str "#x2"))))

    (format t "~&  value-and OK")
  )

  ; Test value-xor.
  (let (valx val1 val2)
    (setf val1 (value-from-str "#x3"))
    (setf val2 (value-from-str "#x6"))

    ; Test or of two values.
    (setf valx (value-xor val1 val2))
    ;(format t "~& val: ~A" val)
    (assert (and (value-p valx) (value-eq valx (value-from-str "#x5"))))

    (format t "~&  value-xor OK")
  )

  ; Test value-eqv.
  (let (valx val1 val2)
    (setf val1 (value-from-str "#x3"))
    (setf val2 (value-from-str "#x6"))

    ; Test eqv of two values.
    (setf valx (value-eqv val1 val2))
    ;(format t "~& val: ~A" val)
    (assert (and (value-p valx) (value-eq valx (value-from-str "#xa"))))

    (format t "~&  value-eqv OK")
  )

  ; Test value-split.
  (let (lstx)
    ; Test splitting 5, to (1, 4).
    (setf lstx (value-split (value-from-str "#x5")))
    (assert (and (listp lstx) (= (length lstx) 2)))
    (assert (member (value-from-str "#x1") lstx :test #'value-eq))
    (assert (member (value-from-str "#x4") lstx :test #'value-eq))

    ; Test splitting 0.
    (setf lstx (value-split (value-from-str "#x0")))
    (assert (and (listp lstx) (= (length lstx) 0)))

    (format t "~&  value-split OK")
  )

  ; Test value-between.
  (let (boolx val1 val8 val9 valf)
    (setf val1 (value-from-str "#x1"))
    (setf val8 (value-from-str "#x8"))
    (setf val9 (value-from-str "#x9"))
    (setf valf (value-from-str "#xf"))

    ; Test a value between two others.
    (setf boolx (value-between :target val9 :from val1 :to valf))
    (assert (and (bool-p boolx) boolx))

    ; Test a value not between two others.
    (setf boolx (value-between :target val8 :from val1 :to valf))
    (assert (and (bool-p boolx) (null boolx)))

    (format t "~&  value-between OK")
  )

  ; Test value-msb.
  (let (valx)
    ; Test one-bit value.
    (setf valx (value-msb (value-from-str "#b0")))
    (assert (and (value-p valx) (value-eq valx (value-from-str "#b1"))))

    ; Test three-bit value.
    (setf valx (value-msb (value-from-str "#b000")))
    (assert (and (value-p valx) (value-eq valx (value-from-str "#b100"))))

    ; Test four-bit value.
    (setf valx (value-msb (value-from-str "#x0")))
    (assert (and (value-p valx) (value-eq valx (value-from-str "#x8"))))

    (format t "~&  value-msb OK")
  )

  ; Test value-shift.
  (let (valx val5)
    (setf val5 (value-from-str "#x5"))

    ; Test shift left by two.
    (setf valx (value-shift val5 2))
    (assert (and (value-p valx) (value-eq valx (value-from-str "#x4"))))

    ; Test shift right by two.
    (setf valx (value-shift val5 -2))
    ;(format t "~&val ~A" val);
    (assert (and (value-p valx) (value-eq valx (value-from-str "#x1"))))

    (format t "~&  value-shift OK")
  )

  (format t "~&value-tests done")
  t
)
