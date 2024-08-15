;;;; Run tests.
(defun rule-tests ()
  (format t "~&rule-tests beginning")

  ; Test rule-new.
  (let (rulx sta1 sta2)
    (setf sta1 (state-from-str "#x5"))
    (setf sta2 (state-from-str "#x6"))

    ; Test making a rule.
    (setf rulx (rule-new (sample-new :initial sta1 :action 0 :result sta2)))
    (assert (rule-p rulx))
    (assert (mask-eq (rule-b00 rulx) (mask-from-str "#x8")))
    (assert (mask-eq (rule-b01 rulx) (mask-from-str "#x2")))
    (assert (mask-eq (rule-b11 rulx) (mask-from-str "#x4")))
    (assert (mask-eq (rule-b10 rulx) (mask-from-str "#x1")))

    (format t "~&  rule-new OK")
  )

  ; Test rule-from-str.
  (let (rulx errx)
    ; Test string too short.
    (setf errx (rule-from-str-na "[]"))
    (assert (and (err-p errx) (string= (err-str errx) "String is too short")))

    ; Test string does not start with the # character.
    (setf errx (rule-from-str-na "0b11"))
    (assert (and (err-p errx) (string= (err-str errx) "String should start with a right bracket")))

    ; Test malformed bit position. is not b, B, x or X.
    (setf errx (rule-from-str-na "[00/1]"))
    (assert (and (err-p errx) (string= (err-str errx) "Too few characters in bit position")))

    ; Test string for invalid combination.
    (setf errx (rule-from-str-na "[00/1M]"))
    (assert (and (err-p errx) (string= (err-str errx) "Invalid character or combination")))

    ; Test string for invalid combination.
    (setf errx (rule-from-str-na "[000]"))
    (assert (and (err-p errx) (string= (err-str errx) "Too many characters in a bit position")))

    ; Test string for invalid combination.
    (setf errx (rule-from-str-na "[00/01"))
    (assert (and (err-p errx) (string= (err-str errx) "Missing closing bracket?")))

    ; Test rule generation.
    (setf rulx (rule-from-str "[00/01/11/10_X0/X1/X0/X1_XX/XX/Xx/xX]"))
    ;(format t "~&rul ~A" rul)
    (assert (and (rule-p rulx)))
    (assert (mask-eq (rule-b00 rulx) (mask-from-str "#b1000_1010_1100")))
    (assert (mask-eq (rule-b01 rulx) (mask-from-str "#b0100_0101_0011")))
    (assert (mask-eq (rule-b11 rulx) (mask-from-str "#b0010_0101_1100")))
    (assert (mask-eq (rule-b10 rulx) (mask-from-str "#b0001_1010_0011")))

    (format t "~&  rule-from-str OK")
  )

  ; Test rule-str.
  (let (strx)
    (setf strx (rule-str (rule-from-str "[00/01/11/10/X0/X1/x0/x1/XX/xx/Xx/xX]")))
    (assert (and (stringp strx) (string= strx "[00/01/11/10_X0/X1/X0/X1_XX/XX/Xx/Xx]")))

    (format t "~&  rule-str OK")
  )

  ; Test rule-union.
  (let (rulx rul1 rul2)

    ; Init rules.
    (setf rul1 (rule-from-str "[00/00/00_01/01/01_11/11/11_10/10/10_X0/X0/X0_X1/X1/X1_XX/XX/XX_Xx/Xx/Xx]"))
    (setf rul2 (rule-from-str "[00/11/10_01/11/10_11/00/01_10/00/01_X0/00/10_X1/11/01_XX/00/11_Xx/01/10]"))

    (setf rulx (rule-union rul1 rul2))
    (assert (and (rule-p rulx) (rule-eq rulx (rule-from-str "[00/XX/X0_01/X1/Xx_11/XX/X1_10/X0/Xx_X0/X0/X0_X1/X1/X1_XX/XX/XX_Xx/Xx/Xx]"))))

    (format t "~&  rule-union OK")
  )

  ; Test rule-is-valid-union.
  (let (rulx rul1 rul2 boolx)

    ; Test invalid unions.
    (setf rul1 (rule-from-str "[00]"))
    (setf rul2 (rule-from-str "[01]"))
    (setf rulx (rule-union rul1 rul2))
    (assert (rule-p rulx))
    (setf boolx (rule-is-valid-union rulx))
    (assert (and (bool-p boolx) (null boolx)))

    (setf rul1 (rule-from-str "[11]"))
    (setf rul2 (rule-from-str "[10]"))
    (setf rulx (rule-union rul1 rul2))
    (assert (rule-p rulx))
    (setf boolx (rule-is-valid-union rulx))
    (assert (and (bool-p boolx) (null boolx)))

    ; Test valid unions.
    (setf rul1 (rule-from-str "[00/00/00_01/01/01_11/11/11_10/10/10_X0/X0/X0_X1/X1/X1_XX/XX/XX_Xx/Xx/Xx]"))
    (setf rul2 (rule-from-str "[00/XX/X0_01/Xx/X1_11/XX/X1_10/X0/Xx_X0/00/10_X1/11/01_XX/00/11_Xx/01/10]"))
    (setf rulx (rule-union rul1 rul2))
    (assert (rule-p rulx))
    (setf boolx (rule-is-valid-union rulx))
    (assert (and (bool-p boolx) boolx))

    (format t "~&  rule-is-valid-union OK")
  )

  ; Test rule-intersection.
  (let (rulx rul1 rul2)

    ; Init rules.
    (setf rul1 (rule-from-str "[00/00/00_01/01/01_11/11/11_10/10/10_X0/X0/X0_X1/X1/X1_XX/XX/XX_Xx/Xx/Xx]"))
    (setf rul2 (rule-from-str "[00/XX/X0_01/Xx/X1_11/XX/X1_10/X0/Xx_X0/00/10_X1/11/01_XX/00/11_Xx/01/10]"))

    ; Test good intersection.
    (setf rulx (rule-intersection rul1 rul2))
    ;(format t "~&rul ~A" (rule-str rul))
    (assert (and (rule-p rulx) (rule-eq rulx (rule-from-str "[00/00/00_01/01/01_11/11/11_10/10/10_X0/00/10_X1/11/01_XX/00/11_Xx/01/10]"))))

    (format t "~&  rule-intersection OK")
  )

  ; Test rule-is-valid-intersection.
  (let (rulx rul1 rul2 boolx)
    ; Init rules.
    (setf rul1 (rule-from-str "[00/00/00_01/01/01_11/11/11_10/10/10_X0/X0/X0_X1/X1/X1_XX/XX/XX_Xx/Xx/Xx]"))
    (setf rul2 (rule-from-str "[00/XX/X0_01/Xx/X1_11/XX/X1_10/X0/Xx_X0/00/10_X1/11/01_XX/00/11_Xx/01/10]"))

    ; Test good intersection.
    (setf rulx (rule-intersection rul1 rul2))
    (assert (rule-p rulx))
    (setf boolx (rule-is-valid-intersection rulx))
    (assert boolx)

    ; Test bad intersections.
    (assert (null (rule-is-valid-intersection (rule-intersection (rule-from-str "[00]") (rule-from-str "[01]")))))
    (assert (null (rule-is-valid-intersection (rule-intersection (rule-from-str "[00]") (rule-from-str "[11]")))))
    (assert (null (rule-is-valid-intersection (rule-intersection (rule-from-str "[00]") (rule-from-str "[10]")))))
    (assert (null (rule-is-valid-intersection (rule-intersection (rule-from-str "[00]") (rule-from-str "[X1]")))))
    (assert (null (rule-is-valid-intersection (rule-intersection (rule-from-str "[00]") (rule-from-str "[Xx]")))))

    (assert (null (rule-is-valid-intersection (rule-intersection (rule-from-str "[01]") (rule-from-str "[00]")))))
    (assert (null (rule-is-valid-intersection (rule-intersection (rule-from-str "[01]") (rule-from-str "[11]")))))
    (assert (null (rule-is-valid-intersection (rule-intersection (rule-from-str "[01]") (rule-from-str "[10]")))))
    (assert (null (rule-is-valid-intersection (rule-intersection (rule-from-str "[01]") (rule-from-str "[X0]")))))
    (assert (null (rule-is-valid-intersection (rule-intersection (rule-from-str "[01]") (rule-from-str "[XX]")))))

    (assert (null (rule-is-valid-intersection (rule-intersection (rule-from-str "[11]") (rule-from-str "[00]")))))
    (assert (null (rule-is-valid-intersection (rule-intersection (rule-from-str "[11]") (rule-from-str "[01]")))))
    (assert (null (rule-is-valid-intersection (rule-intersection (rule-from-str "[11]") (rule-from-str "[10]")))))
    (assert (null (rule-is-valid-intersection (rule-intersection (rule-from-str "[11]") (rule-from-str "[X0]")))))
    (assert (null (rule-is-valid-intersection (rule-intersection (rule-from-str "[11]") (rule-from-str "[Xx]")))))

    (assert (null (rule-is-valid-intersection (rule-intersection (rule-from-str "[10]") (rule-from-str "[00]")))))
    (assert (null (rule-is-valid-intersection (rule-intersection (rule-from-str "[10]") (rule-from-str "[01]")))))
    (assert (null (rule-is-valid-intersection (rule-intersection (rule-from-str "[10]") (rule-from-str "[11]")))))
    (assert (null (rule-is-valid-intersection (rule-intersection (rule-from-str "[10]") (rule-from-str "[X1]")))))
    (assert (null (rule-is-valid-intersection (rule-intersection (rule-from-str "[10]") (rule-from-str "[XX]")))))

    (assert (null (rule-is-valid-intersection (rule-intersection (rule-from-str "[X0]") (rule-from-str "[01]")))))
    (assert (null (rule-is-valid-intersection (rule-intersection (rule-from-str "[X0]") (rule-from-str "[11]")))))
    (assert (null (rule-is-valid-intersection (rule-intersection (rule-from-str "[X0]") (rule-from-str "[X1]")))))

    (assert (null (rule-is-valid-intersection (rule-intersection (rule-from-str "[X1]") (rule-from-str "[00]")))))
    (assert (null (rule-is-valid-intersection (rule-intersection (rule-from-str "[X1]") (rule-from-str "[10]")))))
    (assert (null (rule-is-valid-intersection (rule-intersection (rule-from-str "[X1]") (rule-from-str "[X0]")))))

    (assert (null (rule-is-valid-intersection (rule-intersection (rule-from-str "[XX]") (rule-from-str "[01]")))))
    (assert (null (rule-is-valid-intersection (rule-intersection (rule-from-str "[XX]") (rule-from-str "[10]")))))
    (assert (null (rule-is-valid-intersection (rule-intersection (rule-from-str "[XX]") (rule-from-str "[Xx]")))))

    (assert (null (rule-is-valid-intersection (rule-intersection (rule-from-str "[Xx]") (rule-from-str "[00]")))))
    (assert (null (rule-is-valid-intersection (rule-intersection (rule-from-str "[Xx]") (rule-from-str "[11]")))))
    (assert (null (rule-is-valid-intersection (rule-intersection (rule-from-str "[Xx]") (rule-from-str "[XX]")))))

    (format t "~&  rule-is-valid-intersection OK")
  )

  ; Test rule-eq.
  (let (boolx rul1 rul2 rul3)
    ; Init rules.
    (setf rul1 (rule-from-str "[00/00/00]"))
    (setf rul2 (rule-from-str "[00/11/10]"))
    (setf rul3 (rule-from-str "[00/11/10]"))

    ; Test equal rules.
    (setf boolx (rule-eq rul2 rul3))
    (assert (and (bool-p boolx) boolx))

    ; Test not equal rules.
    (setf boolx (rule-eq rul1 rul2))
    (assert (and (bool-p boolx) (null boolx)))

    (format t "~&  rule-eq OK")
  )

  ; Test rule-subset-of
  (let (boolx rul1 rul2)
    ; Init rules.
    (setf rul1 (rule-from-str "[00/X0/XX_01/X1/Xx_11/X1/XX_10/X0/Xx_X0/X1/XX/Xx]"))
    (setf rul2 (rule-from-str "[00/00/00_01/01/01_11/11/11_10/10/10_X0/X1/XX/Xx]"))

    ; Test valid subsets.
    (setf boolx (rule-subset-of :sub-rule rul2 :sup-rule rul1))
    (assert boolx)

    ; Test 16 invalid subsets.
    (setf rul1 (rule-from-str "[01]"))
    (setf rul2 (rule-from-str "[00]"))
    (setf boolx (rule-subset-of :sub-rule rul2 :sup-rule rul1))
    (assert (not boolx))

    (setf rul1 (rule-from-str "[11]"))
    (setf rul2 (rule-from-str "[00]"))
    (setf boolx (rule-subset-of :sub-rule rul2 :sup-rule rul1))
    (assert (not boolx))

    (setf rul1 (rule-from-str "[10]"))
    (setf rul2 (rule-from-str "[00]"))
    (setf boolx (rule-subset-of :sub-rule rul2 :sup-rule rul1))
    (assert (not boolx))

    (setf rul1 (rule-from-str "[00]"))
    (setf rul2 (rule-from-str "[01]"))
    (setf boolx (rule-subset-of :sub-rule rul2 :sup-rule rul1))
    (assert (not boolx))

    (setf rul1 (rule-from-str "[11]"))
    (setf rul2 (rule-from-str "[01]"))
    (setf boolx (rule-subset-of :sub-rule rul2 :sup-rule rul1))
    (assert (not boolx))

    (setf rul1 (rule-from-str "[10]"))
    (setf rul2 (rule-from-str "[01]"))
    (setf boolx (rule-subset-of :sub-rule rul2 :sup-rule rul1))
    (assert (not boolx))

    (setf rul1 (rule-from-str "[00]"))
    (setf rul2 (rule-from-str "[11]"))
    (setf boolx (rule-subset-of :sub-rule rul2 :sup-rule rul1))
    (assert (not boolx))

    (setf rul1 (rule-from-str "[01]"))
    (setf rul2 (rule-from-str "[11]"))
    (setf boolx (rule-subset-of :sub-rule rul2 :sup-rule rul1))
    (assert (not boolx))

    (setf rul1 (rule-from-str "[10]"))
    (setf rul2 (rule-from-str "[11]"))
    (setf boolx (rule-subset-of :sub-rule rul2 :sup-rule rul1))
    (assert (not boolx))

    (setf rul1 (rule-from-str "[00]"))
    (setf rul2 (rule-from-str "[10]"))
    (setf boolx (rule-subset-of :sub-rule rul2 :sup-rule rul1))
    (assert (not boolx))

    (setf rul1 (rule-from-str "[01]"))
    (setf rul2 (rule-from-str "[10]"))
    (setf boolx (rule-subset-of :sub-rule rul2 :sup-rule rul1))
    (assert (not boolx))

    (setf rul1 (rule-from-str "[11]"))
    (setf rul2 (rule-from-str "[10]"))
    (setf boolx (rule-subset-of :sub-rule rul2 :sup-rule rul1))
    (assert (not boolx))

    (format t "~&  rule-subset-of OK")
  )

  ; Test rule-new-region-to-region.
  (let (rul1)
    (setf rul1 (rule-new-region-to-region
	 (region-from-str "000_111_xxx")
	 (region-from-str "01x_01x_01x"))
	 )
    ;(format t "~&rul1 ~A" rul1)
    (assert (rule-eq rul1 (rule-from-str "[00/01/00_10/11/11_x0/x1/xx]")))

    (format t "~&  rule-new-region-to-region OK")
  )


  ; Test rule-mask-off-ones.
  (let (rul1 rul2 msk1)
    (setf rul1 (rule-from-str "[X1/X0/XX/Xx]"))
    (setf msk1 (mask-from-str "#b1111"))
    (setf rul2 (rule-mask-off-ones rul1 msk1))
    ;(format t "~&rul2 ~A" rul2)
    (assert (rule-eq rul2 (rule-from-str "[01/00/00/01]")))

    (format t "~&  rule-mask-off-ones OK")
  )

  ; Test rule-mask-off-zeros.
  (let (rul1 rul2 msk1)
    (setf rul1 (rule-from-str "[X1/X0/XX/Xx]"))
    (setf msk1 (mask-from-str "#b1111"))
    (setf rul2 (rule-mask-off-zeros rul1 msk1))
    ;(format t "~&rul2 ~A" rul2)
    (assert (rule-eq rul2 (rule-from-str "[11/10/11/10]")))

    (format t "~&  rule-mask-off-zeros OK")
  )

  ; Test rule-combine-sequence.
  (let (rul1 rul2 rul3)
    ; Test two rules that intersect.
    (setf rul1 (rule-from-str "[01/00/xx/11]"))
    (setf rul2 (rule-from-str "[11/xx/00/10]"))

    (setf rul3 (rule-combine-sequence rul1 rul2))
    ;(format t "~&rul3 ~A" rul3)
    (assert (rule-eq rul3 (rule-from-str "[01/00/00/10]")))

    ; Test two rules that do not intersect.
    (setf rul1 (rule-from-str "[00/11/01/XX]"))
    (setf rul2 (rule-from-str "[11/00/10/XX]"))

    (setf rul3 (rule-combine-sequence rul1 rul2))
    ;(format t "~&rul3 ~A" rul3)
    (assert (rule-eq rul3 (rule-from-str "[01/10/00/XX]")))

    (format t "~&  rule-combine-sequence OK")
  )

  ; Test rule-restrict-initial-region.
  (let (rul1 rul2 reg1)
    (setf reg1 (region-from-str "0x____1x____0x____1x____01x______01x______01x______01x"))
    (setf rul1 (rule-from-str  "[00/00_11/11_01/01_10/10_x0/x0/x0_x1/x1/x1_xx/xx/xx_Xx/Xx/Xx]"))

    (setf rul2 (rule-restrict-initial-region rul1 reg1))
    ;(format t "~&rul2 ~A" rul2)

    (assert (rule-eq rul2 (rule-from-str "[00/00_11/11_01/01_10/10_00/10/x0_01/11/x1_00/11/XX_01/10/Xx]")))

    (format t "~&  rule-restrict-initial-region OK")
  )

  ; Test rule-restrict-result-region.
  (let (rul1 rul2 reg1)
    (setf rul1 (rule-from-str  "[00/00_11/11_01/01_10/10_x0/x0_x1/x1_xx/xx/xx_Xx/Xx/Xx]"))
    (setf reg1 (region-from-str "0x____1x____1x____0x____0x____1x____01x______01x"))

    (setf rul2 (rule-restrict-result-region rul1 reg1))
    ;(format t "~&rul2 ~A" rul2)

    (assert (rule-eq rul2 (rule-from-str "[00/00_11/11_01/01_10/10_x0/x0_x1/X1_00/11/xx_10/01/Xx]")))

    (format t "~&  rule-restrict-result-region OK")
  )
  (format t "~&rule-tests done")
  t
)
