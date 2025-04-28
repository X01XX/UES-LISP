;;;; Run tests.
(defun rule-tests ()
  (format t "~&rule-tests beginning")

  ; Test rule-new.
  (let (rulx sta1 sta2)
    (setf sta1 (state-from 's0101))
    (setf sta2 (state-from 's0110))

    ; Test making a rule.
    (setf rulx (rule-new (sample-new :initial sta1 :result sta2)))
    (assert (rule-p rulx))

    (assert (mask-eq (rule-m00 rulx) (mask-from 'm1000)))
    (assert (mask-eq (rule-m01 rulx) (mask-from 'm0010)))
    (assert (mask-eq (rule-m11 rulx) (mask-from 'm0100)))
    (assert (mask-eq (rule-m10 rulx) (mask-from 'm0001)))

    (format t "~&  rule-new OK")
  )

  ; Test rule-from.
  (let (rulx errx)
    ; Test string too short.
    (setf errx (rule-from-str2 "[]"))
    (assert (err-p errx))

    ; Test malformed bit position. is not b, B, x or X.
    (setf errx (rule-from-str2 "[00/1]"))
    (assert (err-p errx))

    ; Test string for invalid combination.
    (setf errx (rule-from-str2 "[00/1M]"))
    (assert (err-p errx))

    ; Test string for invalid combination.
    (setf errx (rule-from-str2 "[000]"))
    (assert (err-p errx))

    ; Test string for invalid combination.
    (setf errx (rule-from-str2 "[00/01"))
    (assert (err-p errx))

    ; Test rule generation.
    (setf rulx (rule-from-str "[00/01/11/10_X0/X1/X0/X1_XX/XX/Xx/xX]"))
    (assert (and (rule-p rulx)))
    (assert (mask-eq (rule-m00 rulx) (mask-from 'm1000_1010_1100)))
    (assert (mask-eq (rule-m01 rulx) (mask-from 'm0100_0101_0011)))
    (assert (mask-eq (rule-m11 rulx) (mask-from 'm0010_0101_1100)))
    (assert (mask-eq (rule-m10 rulx) (mask-from 'm0001_1010_0011)))

    (format t "~&  rule-from-str OK")
  )

  ; Test rule-str.
  (let (strx)
    (setf strx (rule-str (rule-from-str "[00/01/11/10/X0/X1/x0/x1/XX/xx/Xx/xX]")))

    (assert (and (stringp strx) (string= strx "[00/01/11/10_X0/X1/X0/X1_XX/XX/Xx/Xx]")))

    (format t "~&  rule-str OK")
  )

  ; Test rule-union.
  (let (rul1 rul2 rul3)
    ;; There are 8 possible bit-position values,                                                                                                    
    ;; Since order does not matter, there are 8 + 7 + 6 + 5 + 4 + 3 + 2 + 1 = 36 combinations.

    ;; Test 20 combinations that should work.
    (setf rul1 (rule-from-str                 "[00/00/00/00/00_01/01/01/01/01_11/11/11_10/10/10_Xx_XX_X0_X1]"))
    (setf rul2 (rule-from-str                 "[00/11/10/XX/X0_01/11/10/Xx/X1_11/XX/X1_10/Xx/X0_Xx_XX_X0_X1]"))
    (setf rul3 (rule-union rul1 rul2))
    (assert (rule-p rul3))
    (assert (rule-eq rul3 (rule-from-str      "[00/XX/X0/XX/X0_01/X1/Xx/Xx/X1_11/XX/X1_10/Xx/X0_Xx_XX_X0_X1]")))

    ;; Combinations that should fail union, 16.
    ;; For example, union Xx (10, 01) and X1 (11, 01), contains more than two items (10, 11, 01). 1X is disallowed, as is 0X.
    ;; [00/00/00_01/01_11/11/11_10/10_Xx/Xx/Xx_XX/XX_X0]
    ;; [01/Xx/X1_XX/X0_10/Xx/X0_XX/X1_XX/X0/X1_X0/X1_X1]

    (setf rul1 (rule-from-str "[00]"))
    (setf rul2 (rule-from-str "[01]"))
    (setf rul3 (rule-union rul1 rul2))
    (assert (null rul3))

    (setf rul1 (rule-from-str "[00]"))
    (setf rul2 (rule-from-str "[Xx]"))
    (setf rul3 (rule-union rul1 rul2))
    (assert (null rul3))

    (setf rul1 (rule-from-str "[00]"))
    (setf rul2 (rule-from-str "[X1]"))
    (setf rul3 (rule-union rul1 rul2))
    (assert (null rul3))

    (setf rul1 (rule-from-str "[01]"))
    (setf rul2 (rule-from-str "[XX]"))
    (setf rul3 (rule-union rul1 rul2))
    (assert (null rul3))

    (setf rul1 (rule-from-str "[01]"))
    (setf rul2 (rule-from-str "[X0]"))
    (setf rul3 (rule-union rul1 rul2))
    (assert (null rul3))

    (setf rul1 (rule-from-str "[11]"))
    (setf rul2 (rule-from-str "[10]"))
    (setf rul3 (rule-union rul1 rul2))
    (assert (null rul3))

    (setf rul1 (rule-from-str "[11]"))
    (setf rul2 (rule-from-str "[Xx]"))
    (setf rul3 (rule-union rul1 rul2))
    (assert (null rul3))

    (setf rul1 (rule-from-str "[11]"))
    (setf rul2 (rule-from-str "[X0]"))
    (setf rul3 (rule-union rul1 rul2))
    (assert (null rul3))

    (setf rul1 (rule-from-str "[10]"))
    (setf rul2 (rule-from-str "[XX]"))
    (setf rul3 (rule-union rul1 rul2))
    (assert (null rul3))

    (setf rul1 (rule-from-str "[10]"))
    (setf rul2 (rule-from-str "[X1]"))
    (setf rul3 (rule-union rul1 rul2))
    (assert (null rul3))

    (setf rul1 (rule-from-str "[Xx]"))
    (setf rul2 (rule-from-str "[XX]"))
    (setf rul3 (rule-union rul1 rul2))
    (assert (null rul3))

    (setf rul1 (rule-from-str "[Xx]"))
    (setf rul2 (rule-from-str "[X0]"))
    (setf rul3 (rule-union rul1 rul2))
    (assert (null rul3))

    (setf rul1 (rule-from-str "[Xx]"))
    (setf rul2 (rule-from-str "[X1]"))
    (setf rul3 (rule-union rul1 rul2))
    (assert (null rul3))

    (setf rul1 (rule-from-str "[XX]"))
    (setf rul2 (rule-from-str "[X0]"))
    (setf rul3 (rule-union rul1 rul2))
    (assert (null rul3))

    (setf rul1 (rule-from-str "[XX]"))
    (setf rul2 (rule-from-str "[X1]"))
    (setf rul3 (rule-union rul1 rul2))
    (assert (null rul3))

    (setf rul1 (rule-from-str "[X0]"))
    (setf rul2 (rule-from-str "[X1]"))
    (setf rul3 (rule-union rul1 rul2))
    (assert (null rul3))

    (format t "~&  rule-union OK")
  )

  ;; Test rule-is-valid-union.
  ;; That is, no 0->X or 1->X positions.
  ;; Note zero one bits set is the hallmark of an invalid intersection, but passes the
  ;; valid union test.
  (let (rul1)

    ; The vertical column of 4 bit positions can be treated as a 4-bit number, so 16 different patterns.

    ; Test valid unions.                    01245689A = 9 bit patterns.
    (setf rul1 (make-rule :m00 (mask-from 'm000000111)
                          :m01 (mask-from 'm000111000)
                          :m11 (mask-from 'm001001001)
                          :m10 (mask-from 'm010010010)))
               
    (assert (rule-is-valid-union  rul1))

    ; Test invalid unions.                  37BCDEF = 7 bit patterns.

    ; Test invalid union.                   3
    (setf rul1 (make-rule :m00 (mask-from 'm0)
                          :m01 (mask-from 'm0)
                          :m11 (mask-from 'm1)
                          :m10 (mask-from 'm1)))
               
    (assert (not (rule-is-valid-union  rul1)))

    ; Test invalid union.                   7
    (setf rul1 (make-rule :m00 (mask-from 'm0)
                          :m01 (mask-from 'm1)
                          :m11 (mask-from 'm1)
                          :m10 (mask-from 'm1)))
               
    (assert (not (rule-is-valid-union  rul1)))

    ; Test invalid union.                   B
    (setf rul1 (make-rule :m00 (mask-from 'm1)
                          :m01 (mask-from 'm0)
                          :m11 (mask-from 'm1)
                          :m10 (mask-from 'm1)))
               
    (assert (not (rule-is-valid-union  rul1)))

    ; Test invalid union.                   C
    (setf rul1 (make-rule :m00 (mask-from 'm1)
                          :m01 (mask-from 'm1)
                          :m11 (mask-from 'm0)
                          :m10 (mask-from 'm0)))
               
    (assert (not (rule-is-valid-union  rul1)))

    ; Test invalid union.                   D
    (setf rul1 (make-rule :m00 (mask-from 'm1)
                          :m01 (mask-from 'm1)
                          :m11 (mask-from 'm0)
                          :m10 (mask-from 'm1)))
               
    (assert (not (rule-is-valid-union  rul1)))

    ; Test invalid union.                   E
    (setf rul1 (make-rule :m00 (mask-from 'm1)
                          :m01 (mask-from 'm1)
                          :m11 (mask-from 'm1)
                          :m10 (mask-from 'm0)))
               
    (assert (not (rule-is-valid-union  rul1)))

    ; Test invalid union.                   F
    (setf rul1 (make-rule :m00 (mask-from 'm1)
                          :m01 (mask-from 'm1)
                          :m11 (mask-from 'm1)
                          :m10 (mask-from 'm1)))
               
    (assert (not (rule-is-valid-union  rul1)))

    (format t "~&  rule-is-valid-union OK")
  )

  ; Test rule-intersection.
  (let (rul1 rul2 rul3)
    ;; There are 8 possible bit-position values,                                                                                                    
    ;; Since order does not matter, there are 8 + 7 + 6 + 5 + 4 + 3 + 2 + 1 = 36 combinations.

    ;; Test 20 combinations that should work.
    (setf rul1 (rule-from-str                 "[00/00/00_01/01/01_11/11/11_10/10/10_Xx/Xx/Xx_XX/XX/XX_X0_X1]"))
    (setf rul2 (rule-from-str                 "[00/XX/X0_01/Xx/X1_11/XX/X1_10/Xx/X0_Xx/X0/X1_XX/X0/X1_X0_X1]"))
    (setf rul3 (rule-intersection rul1 rul2))
    (assert (rule-p rul3))
    (assert (rule-eq rul3 (rule-from-str      "[00/00/00_01/01/01_11/11/11_10/10/10_Xx/10/01_XX/00/11_X0_X1]")))

    ;; Combinations that should fail intersection, 16.
    ;; For example, Xx (10, 01) has no intersection with XX (00, 11).
    ;; [00/00/00/00/00_01/01/01/01_11/11/11_10/10_Xx_X0]
    ;; [01/11/10/Xx/X1_11/10/XX/X0_10/Xx/X0_XX/X1_XX_X1]

    (setf rul1 (rule-from-str "[00]"))
    (setf rul2 (rule-from-str "[01]"))
    (setf rul3 (rule-intersection rul1 rul2))
    (assert (null rul3))

    (setf rul1 (rule-from-str "[00]"))
    (setf rul2 (rule-from-str "[11]"))
    (setf rul3 (rule-intersection rul1 rul2))
    (assert (null rul3))

    (setf rul1 (rule-from-str "[00]"))
    (setf rul2 (rule-from-str "[10]"))
    (setf rul3 (rule-intersection rul1 rul2))
    (assert (null rul3))

    (setf rul1 (rule-from-str "[00]"))
    (setf rul2 (rule-from-str "[Xx]"))
    (setf rul3 (rule-intersection rul1 rul2))
    (assert (null rul3))

    (setf rul1 (rule-from-str "[00]"))
    (setf rul2 (rule-from-str "[X1]"))
    (setf rul3 (rule-intersection rul1 rul2))
    (assert (null rul3))

    (setf rul1 (rule-from-str "[01]"))
    (setf rul2 (rule-from-str "[11]"))
    (setf rul3 (rule-intersection rul1 rul2))
    (assert (null rul3))

    (setf rul1 (rule-from-str "[01]"))
    (setf rul2 (rule-from-str "[10]"))
    (setf rul3 (rule-intersection rul1 rul2))
    (assert (null rul3))

    (setf rul1 (rule-from-str "[01]"))
    (setf rul2 (rule-from-str "[XX]"))
    (setf rul3 (rule-intersection rul1 rul2))
    (assert (null rul3))

    (setf rul1 (rule-from-str "[01]"))
    (setf rul2 (rule-from-str "[X0]"))
    (setf rul3 (rule-intersection rul1 rul2))
    (assert (null rul3))

    (setf rul1 (rule-from-str "[11]"))
    (setf rul2 (rule-from-str "[10]"))
    (setf rul3 (rule-intersection rul1 rul2))
    (assert (null rul3))

    (setf rul1 (rule-from-str "[11]"))
    (setf rul2 (rule-from-str "[Xx]"))
    (setf rul3 (rule-intersection rul1 rul2))
    (assert (null rul3))

    (setf rul1 (rule-from-str "[11]"))
    (setf rul2 (rule-from-str "[X0]"))
    (setf rul3 (rule-intersection rul1 rul2))
    (assert (null rul3))

    (setf rul1 (rule-from-str "[10]"))
    (setf rul2 (rule-from-str "[XX]"))
    (setf rul3 (rule-intersection rul1 rul2))
    (assert (null rul3))

    (setf rul1 (rule-from-str "[10]"))
    (setf rul2 (rule-from-str "[X1]"))
    (setf rul3 (rule-intersection rul1 rul2))
    (assert (null rul3))

    (setf rul1 (rule-from-str "[Xx]"))
    (setf rul2 (rule-from-str "[XX]"))
    (setf rul3 (rule-intersection rul1 rul2))
    (assert (null rul3))

    (setf rul1 (rule-from-str "[X0]"))
    (setf rul2 (rule-from-str "[X1]"))
    (setf rul3 (rule-intersection rul1 rul2))
    (assert (null rul3))

    (format t "~&  rule-intersection OK")
  )

  ;; Test rule-is-valid-intersection, that is, no column of four zeros.
  ;; Note: 37BCDEF are invalid unions, but pass the valid intersection test.
  (let (rul1)

    ; The vertical column of 4 bit positions can be treated as a 4-bit number, so 16 different patterns.

    ; Test valid intersections.             123456789ABCDEF, 15 patterns.
    (setf rul1 (make-rule :m00 (mask-from 'm000000011111111)
                          :m01 (mask-from 'm000111100001111)
                          :m11 (mask-from 'm011001100110011)
                          :m10 (mask-from 'm101010101010101)))
               
    (assert (rule-is-valid-intersection  rul1))

    ; Test invalid union.                   0
    (setf rul1 (make-rule :m00 (mask-from 'm0)
                          :m01 (mask-from 'm0)
                          :m11 (mask-from 'm0)
                          :m10 (mask-from 'm0)))
               
    (assert (not (rule-is-valid-intersection  rul1)))

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

  ; Test rule-num-bits.
  (let (rul1 num1)
    (setf rul1 (rule-from-str "[00/01/11/10_x0/x1/Xx/XX]"))
    (setf num1 (rule-num-bits rul1))
    ;(format t "~&num1 ~A" num1)

    (assert (= num1 8))

    (format t "~&  rule-num-bits OK")
  )

  ; Test rule-initial-region.
  (let (rul1 reg1)
    (setf rul1 (rule-from-str "[00/01/11/10_x0/x1/Xx/XX]"))
    (setf reg1 (rule-initial-region rul1))

    (assert (region-eq reg1 (region-from 'r0011_xxxx)))

    (format t "~&  rule-initial-region OK")
  )

  ; Test rule-subset-of
  (let (boolx rul1 rul2)
    ; Init rules.
    (setf rul1 (rule-from-str "[00/X0/XX_01/X1/Xx_11/X1/XX_10/X0/Xx_X0/X1/XX/Xx]"))
    (setf rul2 (rule-from-str "[00/00/00_01/01/01_11/11/11_10/10/10_X0/X1/XX/Xx]"))

    ; Test valid subsets.
    (assert (setf boolx (rule-subset-of :sub rul2 :sup rul1)))

    ; Test 16 invalid subsets.
    (setf rul1 (rule-from-str "[01]"))
    (setf rul2 (rule-from-str "[00]"))
    (setf boolx (rule-subset-of :sub rul2 :sup rul1))
    (assert (not boolx))

    (setf rul1 (rule-from-str "[11]"))
    (setf rul2 (rule-from-str "[00]"))
    (setf boolx (rule-subset-of :sub rul2 :sup rul1))
    (assert (not boolx))

    (setf rul1 (rule-from-str "[10]"))
    (setf rul2 (rule-from-str "[00]"))
    (setf boolx (rule-subset-of :sub rul2 :sup rul1))
    (assert (not boolx))

    (setf rul1 (rule-from-str "[00]"))
    (setf rul2 (rule-from-str "[01]"))
    (setf boolx (rule-subset-of :sub rul2 :sup rul1))
    (assert (not boolx))

    (setf rul1 (rule-from-str "[11]"))
    (setf rul2 (rule-from-str "[01]"))
    (setf boolx (rule-subset-of :sub rul2 :sup rul1))
    (assert (not boolx))

    (setf rul1 (rule-from-str "[10]"))
    (setf rul2 (rule-from-str "[01]"))
    (setf boolx (rule-subset-of :sub rul2 :sup rul1))
    (assert (not boolx))

    (setf rul1 (rule-from-str "[00]"))
    (setf rul2 (rule-from-str "[11]"))
    (setf boolx (rule-subset-of :sub rul2 :sup rul1))
    (assert (not boolx))

    (setf rul1 (rule-from-str "[01]"))
    (setf rul2 (rule-from-str "[11]"))
    (setf boolx (rule-subset-of :sub rul2 :sup rul1))
    (assert (not boolx))

    (setf rul1 (rule-from-str "[10]"))
    (setf rul2 (rule-from-str "[11]"))
    (setf boolx (rule-subset-of :sub rul2 :sup rul1))
    (assert (not boolx))

    (setf rul1 (rule-from-str "[00]"))
    (setf rul2 (rule-from-str "[10]"))
    (setf boolx (rule-subset-of :sub rul2 :sup rul1))
    (assert (not boolx))

    (setf rul1 (rule-from-str "[01]"))
    (setf rul2 (rule-from-str "[10]"))
    (setf boolx (rule-subset-of :sub rul2 :sup rul1))
    (assert (not boolx))

    (setf rul1 (rule-from-str "[11]"))
    (setf rul2 (rule-from-str "[10]"))
    (setf boolx (rule-subset-of :sub rul2 :sup rul1))
    (assert (not boolx))

    (format t "~&  rule-subset-of OK")
  )

  ; Test rule-region-to-region.
  (let (rul1 reg1 reg2)
    (setf reg1 (region-from 'r000_111_xxx_Xx))
    (setf reg2 (region-from 'r01x_01x_01x_xX))

    (setf rul1 (rule-region-to-region reg1 reg2))
    ;(format t "~&rul1 ~A" (rule-str rul1))

    (assert (rule-eq rul1 (rule-from-str "[00/01/00_10/11/11/x0_x1/xx/XX/XX]")))

    (format t "~&  rule-region-to-region OK")
  )

  ; Test rule-mask-off-ones.
  (let (rul1 rul2 msk1)
    (setf rul1 (rule-from-str "[X1/X0/XX/Xx]"))
    (setf msk1 (mask-from 'm1111))
    (setf rul2 (rule-mask-off-ones rul1 msk1))
    ;(format t "~&rul2 ~A" rul2)
    (assert (rule-eq rul2 (rule-from-str "[01/00/00/01]")))

    (format t "~&  rule-mask-off-ones OK")
  )

  ; Test rule-mask-off-zeros.
  (let (rul1 rul2 msk1)
    (setf rul1 (rule-from-str "[X1/X0/XX/Xx]"))
    (setf msk1 (mask-from 'm1111))
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
    (setf reg1 (region-from 'r0x____1x____0x____1x____01x______01x______01x______01x))
    (setf rul1 (rule-from-str  "[00/00_11/11_01/01_10/10_x0/x0/x0_x1/x1/x1_xx/xx/xx_Xx/Xx/Xx]"))

    (setf rul2 (rule-restrict-initial-region rul1 reg1))
    ;(format t "~&rul2 ~A" rul2)

    (assert (rule-eq rul2 (rule-from-str "[00/00_11/11_01/01_10/10_00/10/x0_01/11/x1_00/11/XX_01/10/Xx]")))

    (format t "~&  rule-restrict-initial-region OK")
  )

  ; Test rule-restrict-result-region.
  (let (rul1 rul2 reg1)
    (setf rul1 (rule-from-str  "[00/00_11/11_01/01_10/10_x0/x0_x1/x1_xx/xx/xx_Xx/Xx/Xx]"))
    (setf reg1 (region-from 'r0x____1x____1x____0x____0x____1x____01x______01x))

    (setf rul2 (rule-restrict-result-region rul1 reg1))
    ;(format t "~&rul2 ~A" rul2)

    (assert (rule-eq rul2 (rule-from-str "[00/00_11/11_01/01_10/10_x0/x0_x1/X1_00/11/xx_10/01/Xx]")))

    (format t "~&  rule-restrict-result-region OK")
  )

  ; Test rule-changes.
  (let (rul1 reg1 reg2 wanted)
    (setf reg1 (region-from 'r000_111_xxx))
    (setf reg2 (region-from 'r01x_01x_01x))

    (setf rul1 (rule-region-to-region reg1 reg2))
    ;(format t "~&rul1       ~A" rul1)

    (setf wanted (rule-changes rul1))
    ;(format t "~& wanted ~A" wanted)

    ;                                                "000_111_xxx"
    ;                                                "01x_01x_01x"
    (assert (mask-eq (change-m01 wanted) (mask-from 'm010_000_010)))
    (assert (mask-eq (change-m10 wanted) (mask-from 'm000_100_100)))

    (format t "~&  rule-changes OK")
  )

  ; Test rule-sequence-blocks-changes.
  (let (rul1 rul2 wanted bx)
    (setf wanted (change-new :m01 (mask-from 'm10) :m10 (mask-from 'm01)))
    (setf rul1 (rule-from-str "[01/00]"))
    (setf rul2 (rule-from-str "[11/10]"))

    (setf bx (rule-sequence-blocks-changes :first rul1 :next rul2 :wanted wanted))
    (assert (not bx))

    (setf wanted (change-new :m01 (mask-from 'm01) :m10 (mask-from 'm10)))
    (setf rul1 (rule-from-str "[10/00]"))
    (setf rul2 (rule-from-str "[00/01]"))

    (setf bx (rule-sequence-blocks-changes :first rul1 :next rul2 :wanted wanted))
    (assert (not bx))

    (setf wanted (change-new :m01 (mask-from 'm0) :m10 (mask-from 'm1)))
    (setf rul1 (rule-from-str "[10]"))
    (setf rul2 (rule-from-str "[10]"))

    (setf bx (rule-sequence-blocks-changes :first rul1 :next rul2 :wanted wanted))
    (assert (not bx))

    (setf wanted (change-new :m01 (mask-from 'm01) :m10 (mask-from 'm10)))
    (setf rul1 (rule-from-str "[10/00]"))
    (setf rul2 (rule-from-str "[01/01]"))

    (setf bx (rule-sequence-blocks-changes :first rul1 :next rul2 :wanted wanted))
    (assert bx)

    (setf wanted (change-new :m01 (mask-from 'm01) :m10 (mask-from 'm10)))
    (setf rul1 (rule-from-str "[10/00]"))
    (setf rul2 (rule-from-str "[11/01]"))

    (setf bx (rule-sequence-blocks-changes :first rul1 :next rul2 :wanted wanted))
    (assert bx)

    (setf wanted (change-new :m01 (mask-from 'm1) :m10 (mask-from 'm0)))
    (setf rul1 (rule-from-str "[01]"))
    (setf rul2 (rule-from-str "[01]"))

    (setf bx (rule-sequence-blocks-changes :first rul1 :next rul2 :wanted wanted))
    (assert (not bx))

    (setf wanted (change-new :m01 (mask-from 'm10) :m10 (mask-from 'm01)))
    (setf rul1 (rule-from-str "[01/00]"))
    (setf rul2 (rule-from-str "[00/10]"))

    (setf bx (rule-sequence-blocks-changes :first rul1 :next rul2 :wanted wanted))
    (assert bx)

    (setf wanted (change-new :m01 (mask-from 'm10) :m10 (mask-from 'm01)))
    (setf rul1 (rule-from-str "[01/00]"))
    (setf rul2 (rule-from-str "[10/10]"))

    (setf bx (rule-sequence-blocks-changes :first rul1 :next rul2 :wanted wanted))
    (assert bx)

    (setf wanted (change-new :m01 (mask-from 'm1) :m10 (mask-from 'm0)))
    (setf rul1 (rule-from-str "[01]"))
    (setf rul2 (rule-from-str "[00]"))

    (setf bx (rule-sequence-blocks-changes :first rul1 :next rul2 :wanted wanted))

    (setf wanted (change-new :m01 (mask-from 'm1) :m10 (mask-from 'm0)))
    (setf rul1 (rule-from-str "[01]"))
    (setf rul2 (rule-from-str "[01]"))

    (setf bx (rule-sequence-blocks-changes :first rul1 :next rul2 :wanted wanted))

    (setf wanted (change-new :m01 (mask-from 'm1) :m10 (mask-from 'm0)))
    (setf rul1 (rule-from-str "[01]"))
    (setf rul2 (rule-from-str "[11]"))

    (setf bx (rule-sequence-blocks-changes :first rul1 :next rul2 :wanted wanted))

    (setf wanted (change-new :m01 (mask-from 'm1) :m10 (mask-from 'm0)))
    (setf rul1 (rule-from-str "[01]"))
    (setf rul2 (rule-from-str "[10]"))

    (setf bx (rule-sequence-blocks-changes :first rul1 :next rul2 :wanted wanted))

    (format t "~&  rule-sequence-blocks-changes OK")
  )

  ; Test rule-mutually-exclusive.
  (let (rul1 rul2 wanted bx)

    (setf wanted (change-new :m01 (mask-from 'm10) :m10 (mask-from 'm01)))
    (setf rul1 (rule-from-str "[01/01]"))
    (setf rul2 (rule-from-str "[10/10]"))

    (setf bx (rule-mutually-exclusive rul1 rul2 wanted))
    ;(format t "~& rule mutually-exclusive ~A ~A is ~A" rul1 rul2 bx)
    (assert bx)

    (setf wanted (change-new :m01 (mask-from 'm10) :m10 (mask-from 'm01)))
    (setf rul1 (rule-from-str "[01/01]"))
    (setf rul2 (rule-from-str "[11/10]"))

    (setf bx (rule-mutually-exclusive rul1 rul2 wanted))
    (assert (not bx))

    (setf wanted (change-new :m01 (mask-from 'm10) :m10 (mask-from 'm01)))
    (setf rul1 (rule-from-str "[01/11]"))
    (setf rul2 (rule-from-str "[00/10]"))

    (setf bx (rule-mutually-exclusive rul1 rul2 wanted))
    (assert bx)

    (format t "~&  rule-mutually-excusive OK")
  )

  ;; Test rule-reverse.
  (let (rul1 rul2)
    (setf rul1 (rule-from-str "[00/01/11/10/XX/Xx]"))
    (setf rul2 (rule-reverse rul1))

    (assert (rule-eq rul2 (rule-from-str "[00/10/11/01/XX/Xx]")))

    (format t "~&  rule-reverse OK")
  )

  ;; Test rule-initial-region, rule-result-region.
  (let (rul1 reg-initial reg-result)
    (setf rul1 (rule-from-str "[XX/Xx/xX/xx]"))

    (setf reg-initial (rule-initial-region rul1))
    ;(format t "~&reg-initial ~A" (region-str reg-initial))
    (assert (string= (region-str reg-initial) "rXXXX"))

    (setf reg-result (rule-result-region rul1))
    ;(format t "~&reg-result  ~A" (region-str reg-result))
    (assert (string= (region-str reg-result) "rXxxX"))

    (format t "~&  rule-result-region OK")
  )

  ;; Test rule-split-xb.
  (let (rul1 rules)
    (setf rul1 (rule-from-str "[00/01/11/10/XX/Xx]"))
    (setf rules (rule-split-xb rul1))
    (assert (= 1 (rulestore-length rules)))
    (assert (rulestore-member rules (rule-from-str "[00/01/11/10/XX/Xx]")))

    (setf rul1 (rule-from-str "[00/01/11/10/XX/Xx/X1]"))
    (setf rules (rule-split-xb rul1))
    (assert (= 2 (rulestore-length rules)))
    (assert (rulestore-member rules (rule-from-str "[00/01/11/10/XX/Xx/01]")))
    (assert (rulestore-member rules (rule-from-str "[00/01/11/10/XX/Xx/11]")))

    (setf rul1 (rule-from-str "[00/01/11/10/XX/Xx/X1/X0]"))
    (setf rules (rule-split-xb rul1))
    (assert (= 4 (rulestore-length rules)))
    (assert (rulestore-member rules (rule-from-str "[00/01/11/10/XX/Xx/01/00]")))
    (assert (rulestore-member rules (rule-from-str "[00/01/11/10/XX/Xx/01/10]")))
    (assert (rulestore-member rules (rule-from-str "[00/01/11/10/XX/Xx/11/00]")))
    (assert (rulestore-member rules (rule-from-str "[00/01/11/10/XX/Xx/11/10]")))

    (setf rul1 (rule-from-str "[00/01/11/10/XX/Xx/X0/X1/X0]"))
    (setf rules (rule-split-xb rul1))
    (assert (= 8 (rulestore-length rules)))

    (format t "~&  rule-split-xb OK")
  )

  ;; Test rule-restrict-by-change
  (let (rul1 rul2 cngs rul3)
    (setf rul1 (rule-from-str "[00/01/11/10/XX/Xx/X0/X1]"))
    (setf rul2 (rule-from-str "[00/01/11/10/XX/Xx/X0/X1]"))
    (setf cngs (rule-changes rul1))
    (setf rul3 (rule-restrict-by-change rul1 cngs))
    (assert (rule-p rul3))
    (assert (rule-is-valid-intersection rul3))
    (assert (rule-eq rul3 (rule-from-str "[00/01/11/10_XX/Xx/10/01]")))

    (format t "~&  rule-restrict-by-change OK")
  )

  ;; Test rule-restrict-by-within.
  ;; Test all combinations of (0 1 X) and (00 01 11 10 XX X0 X1 Xx), 3 x 8 = 24
  (let (rul1 rul2 reg1)
    ;; Test all good combinations, 14.
    (setf reg1 (region-from   'r0__X__X__1__X__X__0__1__X__X__0__X__1__X))
    (setf rul1 (rule-from-str "[00/00/01/11/11/10_XX/XX/XX/Xx/X0/X0/X1/X1]"))
    (setf rul2 (rule-restrict-by-within rul1 reg1))
    (assert (rule-p rul2))
    (assert(rule-eq rul2 (rule-from-str "[00/00/01/11/11/10/00/11/XX/Xx/00/X0/11/X1]")))

    ;; Test all bad combinations, 10.

    ;; Test combinations with no intersection by initial region, 4.
    (setf reg1 (region-from   'r1))
    (setf rul1 (rule-from-str "[00]"))
    (setf rul2 (rule-restrict-by-within rul1 reg1))
    (assert (null rul2))

    (setf reg1 (region-from   'r1))
    (setf rul1 (rule-from-str "[01]"))
    (setf rul2 (rule-restrict-by-within rul1 reg1))
    (assert (null rul2))

    (setf reg1 (region-from   'r0))
    (setf rul1 (rule-from-str "[11]"))
    (setf rul2 (rule-restrict-by-within rul1 reg1))
    (assert (null rul2))

    (setf reg1 (region-from   'r0))
    (setf rul1 (rule-from-str "[10]"))
    (setf rul2 (rule-restrict-by-within rul1 reg1))
    (assert (null rul2))

    ;; Test combinations with no intersection by result region, 6.
    (setf reg1 (region-from   'r0))
    (setf rul1 (rule-from-str "[01]"))
    (setf rul2 (rule-restrict-by-within rul1 reg1))
    (assert (null rul2))

    (setf reg1 (region-from   'r0))
    (setf rul1 (rule-from-str "[Xx]"))
    (setf rul2 (rule-restrict-by-within rul1 reg1))
    (assert (null rul2))

    (setf reg1 (region-from   'r0))
    (setf rul1 (rule-from-str "[X1]"))
    (setf rul2 (rule-restrict-by-within rul1 reg1))
    (assert (null rul2))

    (setf reg1 (region-from   'r1))
    (setf rul1 (rule-from-str "[10]"))
    (setf rul2 (rule-restrict-by-within rul1 reg1))
    (assert (null rul2))

    (setf reg1 (region-from   'r1))
    (setf rul1 (rule-from-str "[Xx]"))
    (setf rul2 (rule-restrict-by-within rul1 reg1))
    (assert (null rul2))

    (setf reg1 (region-from   'r1))
    (setf rul1 (rule-from-str "[X0]"))
    (setf rul2 (rule-restrict-by-within rul1 reg1))
    (assert (null rul2))

    (format t "~&  rule-restrict-by-within OK")
  )

  ;; Test rule-result-from-state.
  (let (rul1 sta1 sta2)
    ;; Test all combinations.
    (setf sta1 (state-from   's0__0__1__1__0__1__0__1__0__1__0__1))
    (setf rul1 (rule-from-str "[00/01/11/10/XX/XX/Xx/Xx/X0/X0/X1/X1]"))
    (setf sta2 (rule-result-from-state rul1 sta1))
    (assert (state-p sta2))
    (assert (state-eq sta2 (state-from 's0110_0110_0011)))

    (format t "~&  rule-result-from-state OK")
  )

  (format t "~&rule-tests done")
  t
)
