;;; Run tests.
(defun mask-tests ()
  (format t "~&mask-tests beginning")

  ; Test mask-new.
  (let (mskx)
    ; Test creating a valid mask.
    (setf mskx (mask-new (value-from-str "#x03")))
    (assert (and (mask-p mskx) (= (value-bits (mask-value mskx)) 3)))
 
    (format t "~&  mask-new OK")
  )

  ; Test mask-str.
  (let (strx mskx)
     (setf mskx (mask-from-str "#x1"))
     (setf strx (mask-str mskx))
     (assert (and (stringp strx) (string= strx "#S(MASK #x1)")))
 
     (format t "~&  mask-str OK")
  )

  ; Test mask-num-bits.
  (let (numx msk1)
     (setf msk1 (mask-from-str "#x5"))
 
     ; Test a valid mask.
     (setf numx (mask-num-bits msk1))
     (assert (and (integerp numx) (= numx 4)))
 
     (format t "~&  mask-num-bits OK")
  )

  ; Test mask-eq.
  (let (boolx msk1 msk2 msk3)
    (setf msk1 (mask-from-str "#x1"))
    (setf msk2 (mask-from-str "#x2"))
    (setf msk3 (mask-from-str "#x1"))

    ; Test two masks that are eq.
    (setf boolx (mask-eq msk1 msk3))
    (assert (and (bool-p boolx) boolx))

    ; Test two masks that are not eq.
    (setf boolx (mask-eq msk1 msk2))
    (assert (and (bool-p boolx) (not boolx)))

    (format t "~&  mask-eq OK")
  )

 
  ; Test mask-from-str.
  (let (mskx)
     (setf mskx (mask-from-str "#x23"))
     (assert (mask-p mskx))
     (assert (= (value-num-bits (mask-value mskx)) 8))
     (assert (= (value-bits (mask-value mskx)) #x23))
 
     (format t "~&  mask-from-str OK")
  )

  ; Test mask-msb.
  (let (mskx)
    ; Test one-bit mask.
    (setf mskx (mask-msb (mask-from-str "#b0")))
    (assert (mask-eq mskx (mask-from-str "#b1")))

    ; Test three-bit mask.
    (setf mskx (mask-msb (mask-from-str "#b000")))
    (assert (mask-eq mskx (mask-from-str "#b100")))

    ; Test four-bit mask.
    (setf mskx (mask-msb (mask-from-str "#x0")))
    (assert (and (mask-p mskx) (mask-eq mskx (mask-from-str "#x8"))))

    (format t "~&  mask-msb OK")
  )

  ; Test mask-shift.
  (let (mskx msk5)
    (setf msk5 (mask-from-str "#x5"))

    ; Test shift left by two.
    (setf mskx (mask-shift msk5 2))
    (assert (and (mask-p mskx) (mask-eq mskx (mask-from-str "#x4"))))

    ; Test shift right by two.
    (setf mskx (mask-shift msk5 -2))
    ;(format t "~&msk ~A" msk);
    (assert (and (mask-p mskx) (mask-eq mskx (mask-from-str "#x1"))))

    (format t "~&  mask-shift OK")
  )

  ; Test mask-zerop.
  (let (mskx)
    ; Test a non-zero mask.
    (setf mskx (mask-zerop (mask-from-str "#x01")))
    (assert (null mskx))

    ; Test a zero mask.
    (setf mskx (mask-zerop (mask-from-str "#x00")))
    (assert (and (bool-p mskx) mskx))

    (format t "~&  mask-zerop OK")
  )

  ; Test mask-and.
  (let (valx msk3 msk6 sta6)
    (setf msk3 (mask-from-str "#x3"))
    (setf msk6 (mask-from-str "#x6"))
    (setf sta6 (state-from-str "#x6"))

    ; Test and of two masks.
    (setf valx (mask-and msk3 msk6))
    ;(format t "~& msk: ~A" msk)
    (assert (and (value-p valx) (value-eq valx (value-from-str "#x2"))))

    ; Test and of a mask and a state.
    (setf valx (mask-and msk3 sta6))
    ;(format t "~& msk: ~A" msk)
    (assert (and (value-p valx) (value-eq valx (value-from-str "#x2"))))

    (format t "~&  mask-and OK")
  )

  ; Test mask-and-not.
  (let (valx msk3 msk6 sta6)
    (setf msk3 (mask-from-str "#x3"))
    (setf msk6 (mask-from-str "#x6"))
    (setf sta6 (state-from-str "#x6"))

    ; Test and-not of two masks.
    (setf valx (mask-and-not msk3 msk6))
    ;(format t "~& msk: ~A" msk)
    (assert (and (value-p valx) (value-eq valx (value-from-str "#x1"))))

    ; Test and-not of a mask and a state.
    (setf valx (mask-and-not msk3 sta6))
    ;(format t "~& msk: ~A" msk)
    (assert (and (value-p valx) (value-eq valx (value-from-str "#x1"))))

    (format t "~&  mask-and-not OK")
  )

  ; Test mask-or.
  (let (valx msk3 msk6)
    (setf msk3 (mask-from-str "#x3"))
    (setf msk6 (mask-from-str "#x6"))

    ; Test or of two masks.
    (setf valx (mask-or msk3 msk6))
    ;(format t "~& msk: ~A" msk)
    (assert (and (value-p valx) (value-eq valx (value-from-str "#x7"))))

    (format t "~&  mask-or OK")
  )

  ; Test mask-not.
  (let (mskx)
    (setf mskx (mask-not (mask-from-str "#x5a")))
    (assert (and (mask-p mskx) (mask-eq mskx (mask-from-str "#xa5"))))

    (format t "~&  mask-not OK")
  )

  ; Test mask-subset-of.
  (let (boolx msk1 msk2)
    (setf msk1 (mask-from-str "#x5a"))
    (setf msk2 (mask-from-str "#x42"))

    (setf boolx (mask-subset-of :sub-mask msk2 :sup-mask msk1))
    (assert boolx)

    (setf boolx (mask-subset-of :sub-mask msk1 :sup-mask msk2))
    (assert (not boolx))

    (format t "~&  mask-subset-of OK")
  )

  (format t "~&mask-tests done")
  t
)
