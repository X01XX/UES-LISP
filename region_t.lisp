;;; Run tests.
(defun region-tests ()
 (format t "~&region-tests beginning")

 ; Test region-new
 (let (states regx)
   (setf states (statestore-new (list (state-from-str "#x1") (state-from-str "#x2") (state-from-str "#x4"))))
   (setf regx (region-new states))
   (assert (and (region-p regx) (string= (region-str regx) "#S(REGION 0xxX)+")))

   (format t "~&  region-new OK")
 )

 ; Test high state.
 (let (regx stax states)
   ; Test one-state region.
   (setf regx (region-from-str "10X1"))
   (setf stax (region-high-state regx))
   (assert (and (state-p stax) (state-eq stax (state-from-str "#b1011"))))

   ; Test two-state region.
   (setf regx (region-from-str "01X0"))
   (setf stax (region-high-state regx))
   (assert (and (state-p stax) (state-eq stax (state-from-str "#b0110"))))

   ; Test three-state region.
   (setf states (statestore-new (list (state-from-str "#x1") (state-from-str "#x2") (state-from-str "#x4"))))
   (setf regx (region-new states))
   (setf stax (region-high-state regx))
   (assert (and (state-p stax) (state-eq stax (state-from-str "#b0111"))))

   (format t "~&  region-high-state OK")
 )

 ; Test low state.
 (let (regx stax states)
   ; Test one-state region.
   (setf regx (region-from-str "10X1"))
   (setf stax (region-low-state regx))
   (assert (and (state-p stax) (state-eq stax (state-from-str "#b1001"))))

   ; Test two-state region.
   (setf regx (region-from-str "01X1"))
   (setf stax (region-low-state regx))
   (assert (and (state-p stax) (state-eq stax (state-from-str "#b0101"))))

   ; Test three-state region.
   (setf states (statestore-new (list (state-from-str "#x1") (state-from-str "#x3") (state-from-str "#x5"))))
   (setf regx (region-new states))
   (setf stax (region-low-state regx))
   (assert (and (state-p stax) (state-eq stax (state-from-str "#b0001"))))

   (format t "~&  region-low-state OK")
 )

 ; Test region-x-mask.
 (let (regx mskx)
   ; Test single-state region.
   (setf regx (region-from-str "0101"))
   (setf mskx (region-x-mask regx))
   (assert (and (mask-p mskx) (mask-eq mskx (mask-from-str "#b0000"))))

   ; Test two-state region.
   (setf regx (region-from-str "01X1"))
   (setf mskx (region-x-mask regx))
   (assert (and (mask-p mskx) (mask-eq mskx (mask-from-str "#b0010"))))

   ; Test three-state region.
   (setf regx (region-new (statestore-new (list (state-from-str "#b0001")
                                                (state-from-str "#b0010")
                                                (state-from-str "#b0101")))))
   (setf mskx (region-x-mask regx))
   (assert (and (mask-p mskx) (mask-eq mskx (mask-from-str "#b0111"))))

   (format t "~&  region-x-mask OK")
 )

 ; Test region-1-mask.
 (let (regx mskx)
   ; Test single-state region.
   (setf regx (region-from-str "0101"))
   (setf mskx (region-1-mask regx))
   (assert (and (mask-p mskx) (mask-eq mskx (mask-from-str "#b0101"))))

   ; Test two-state region.
   (setf regx (region-from-str "01X1"))
   (setf mskx (region-1-mask regx))
   (assert (and (mask-p mskx) (mask-eq mskx (mask-from-str "#b0101"))))

   ; Test three-state region.
   (setf regx (region-new (statestore-new (list (state-from-str "#b1000")
                                                (state-from-str "#b1010")
						(state-from-str "#b1100")))))
   (setf mskx (region-1-mask regx))
   (assert (and (mask-p mskx) (mask-eq mskx (mask-from-str "#b1000"))))

   (format t "~&  region-1-mask OK")
 )

 ; Test region-0-mask.
 (let (regx mskx)
   ; Test single-state region.
   (setf regx (region-from-str "0101"))
   (setf mskx (region-0-mask regx))
   (assert (and (mask-p mskx) (mask-eq mskx (mask-from-str "#b1010"))))

   ; Test two-state region.
   (setf regx (region-from-str "01X1"))
   (setf mskx (region-0-mask regx))
   (assert (and (mask-p mskx) (mask-eq mskx (mask-from-str "#b1000"))))

   ; Test three-state region.
   (setf regx (region-new (statestore-new (list (state-from-str "#b1000")
                                                (state-from-str "#b1010")
						(state-from-str "#b1100")))))
   (setf mskx (region-0-mask regx))
   (assert (and (mask-p mskx) (mask-eq mskx (mask-from-str "#b0001"))))

   (format t "~&  region-0-mask OK")
 )

 ; Test region-second-state.
 (let (regx stax)
   ; Test single-state region.
   (setf regx (region-from-str "0101"))
   (setf stax (region-second-state regx))
   (assert (and (state-p stax) (state-eq stax (state-from-str "#b0101"))))

   ; Test two-state region.
   (setf regx (region-from-str "01X1"))
   (setf stax (region-second-state regx))
   (assert (and (state-p stax) (state-eq stax (state-from-str "#b0101"))))

   ; Test three-state region.
   (setf regx (region-new (statestore-new (list (state-from-str "#b0001")
                                                (state-from-str "#b0010")
                                                (state-from-str "#b0111")))))
   (setf stax (region-second-state regx))
   (assert (and (state-p stax) (state-eq stax (state-from-str "#b0110"))))

   (format t "~&  region-second-state OK")
 )

 ; Test region-str.
 (let (strx) 
   (setf strx (region-str (region-from-str "01Xx")))

   (assert (and (stringp strx) (string= strx "#S(REGION 01Xx)")))

   (format t "~&  region-str OK")
 )

 ; Test region-from-str.
 (let (errx)
   ; Test arg with invalid character.
   (setf errx (region-from-str-na "01X3"))
   (assert (and (err-p errx) (string= (err-str errx) "Invalid character")))

   ; Test arg with no valid character.
   (setf errx (region-from-str-na "_"))
   (assert (and (err-p errx) (string= (err-str errx) "No valid character found")))

   ; Test good string.
   (assert (string= (region-str (region-from-str "01Xx")) "#S(REGION 01Xx)"))

   (format t "~&  region-from-str OK")
 )

 ; Test region-eq.
 (let (boolx reg1 reg2 reg3)
   (setf reg1 (region-from-str "01Xx"))
   (setf reg2 (region-from-str "01xx"))
   (setf reg3 (region-from-str "Xx01"))

   ; Test true condition.
   (setf boolx (region-eq reg1 reg2))
   (assert (and (bool-p boolx) boolx))

   ; Test false condition.
   (setf boolx (region-eq reg1 reg3))
   (assert (and (bool-p boolx) (not boolx)))

   (format t "~&  region-eq OK")
 )

 ; Test region-intersection.
 (let (reg1 reg2 reg3)
   (setf reg1 (region-from-str "01Xxx"))
   (setf reg2 (region-from-str "Xx01x"))

   (setf reg3 (region-intersection reg1 reg2))
   ;(format t "~&reg3 ~A" (region-str reg3))
   (assert (and (region-p reg3) (region-eq reg3 (region-from-str "0_101x"))))

   (format t "~&  region-intersection OK")
 )

 ; Test region-union.
 (let (reg1 reg2 reg3)
   (setf reg1 (region-from-str "01Xx_0101"))
   (setf reg2 (region-from-str "1001_xx01"))

   (setf reg3 (region-union reg1 reg2))
   ;(format t "~&reg3 ~A" (region-str reg3))
   (assert (and (region-p reg3) (region-eq reg3 (region-from-str "xxxx_xx01"))))

   (format t "~&  region-union OK")
 )

  ; Test region-edge-mask.
  (let (reg1 msk1)
    (setf reg1 (region-from-str "01Xx_0101"))
    (setf msk1 (region-edge-mask reg1))
    ;(format t "~&mask ~A" (mask-str msk1))
    (assert (and (mask-p msk1) (mask-eq msk1 (mask-from-str "#xcf"))))

    (format t "~&  region-edge-mask OK")
  )

  ; Test region-distance.
  (let (reg1 reg2 dist)

    ; Test non-intersecting, non-adjacent regions.
    (setf reg1 (region-from-str "0XX1"))
    (setf reg2 (region-from-str "1XX0"))
    (setf dist (region-distance reg1 reg2))
    (assert (and (integerp dist) (= dist 2)))

    ; Test intersecting regions.
    (setf reg2 (region-from-str "X01X"))
    (setf dist (region-distance reg1 reg2))
    (assert (and (integerp dist) (= dist 0)))

    ; Test adjacent regions.
    (setf reg2 (region-from-str "1XX1"))
    (setf dist (region-distance reg1 reg2))
    (assert (and (integerp dist) (= dist 1)))

    ; Test subset/superset regions.
    (setf reg1 (region-from-str "0XX1"))
    (setf reg2 (region-from-str "01X1"))
    (setf dist (region-distance reg1 reg2))
    ;(format t "~&reg1 ~A reg2 ~A distance ~D" (region-str reg1) (region-str reg2) dist)
    (assert (and (integerp dist) (= dist 0)))

    (format t "~&  region-distance OK")
  )

  ; Test region-intersects.
  (let (reg1 reg2 reg3 boolx)
    (setf reg1 (region-from-str "0XX1"))
    (setf reg2 (region-from-str "X101"))
    (setf reg3 (region-from-str "XX00"))
 
    ; Test true condition.
    (setf boolx (region-intersects reg1 reg2))
    (assert (and (bool-p boolx) boolx))
 
    ; Test false condition.
    (setf boolx (region-intersects reg1 reg3))
    (assert (and (bool-p boolx) (not boolx)))

    (format t "~&  region-intersects OK")
  )

  ; Test region-superset-of.
  (let (reg1 reg2 reg3 reg4 boolx)
    (setf reg1 (region-from-str "0XX1"))
    (setf reg2 (region-from-str "01x1"))
    (setf reg3 (region-from-str "XX00"))
    (setf reg4 (region-from-str "1X00"))

    ; Test subset.
    (setf boolx (region-superset-of :sup reg1 :sub reg2))
    (assert boolx)

    ; Test not subset.
    (setf boolx (region-superset-of :sup reg1 :sub reg3))
    (assert (not boolx))

    ; Test not subset, no intersection.
    (setf boolx (region-superset-of :sup reg1 :sub reg4))
    (assert (not boolx))

    (format t "~&  region-superset-of OK")
  )
 
  ; Test region-set-to-ones.
  (let (reg1 reg2 mskx)
    (setf reg1 (region-from-str "0XX1"))
    (setf mskx (mask-from-str "#b1100"))

    (setf reg2 (region-set-to-ones reg1 mskx))
    (assert (region-eq reg2 (region-from-str "11X1")))

    (format t "~&  region-set-to-ones OK")
  )
 
  ; Test region-set-to-zeros.
  (let (reg1 reg2 mskx)
    (setf reg1 (region-from-str "1XX1"))
    (setf mskx (mask-from-str "#b1100"))

    (setf reg2 (region-set-to-zeros reg1 mskx))
    (assert (region-eq reg2 (region-from-str "00X1")))

    (format t "~&  region-set-to-zeros OK")
  )
 
  ; Test region-subtract.
  (let (reg1 reg2 regstr)
    (setf reg1 (region-from-str "0XX1"))
    (setf reg2 (region-from-str "0011"))

    ; Test subtracting a subset region.
    (setf regstr (region-subtract :min-reg reg1 :sub-reg reg2))
    ;(format t "~&regstr 1: ~A" regstr)
    (assert (= (regionstore-length regstr) 2))
    (assert (regionstore-contains regstr (region-from-str "01X1")))
    (assert (regionstore-contains regstr (region-from-str "0x01")))

    ; Test subtracting an intersecting region.
    (setf reg2 (region-from-str "X01x"))
    (setf regstr (region-subtract :min-reg reg1 :sub-reg reg2))
    ;(format t "~&regstr 2: ~A" regstr)
    (assert (= (regionstore-length regstr) 2))
    (assert (regionstore-contains regstr (region-from-str "01X1")))
    (assert (regionstore-contains regstr (region-from-str "0x01")))

    ; Test subtracting a non-intersecting region.
    (setf reg2 (region-from-str "X010"))
    (setf regstr (region-subtract :min-reg reg1 :sub-reg reg2))
    ;(format t "~&regstr 3: ~A" regstr)
    (assert (= (regionstore-length regstr) 1))
    (assert (regionstore-contains regstr (region-from-str "0XX1")))

    ; Test subtracting a superset region.
    (setf reg2 (region-from-str "XXXX"))
    (setf regstr (region-subtract :min-reg reg1 :sub-reg reg2))
    ;(format t "~&regstr 4: ~A" regstr)
    (assert (= (regionstore-length regstr) 0))

    (format t "~&  region-subtract OK")
  )
 
 (format t "~&region-tests done")
 t
)

