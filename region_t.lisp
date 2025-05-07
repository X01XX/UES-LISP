;; Run tests.
(defun region-tests ()
 (format t "~&region-tests beginning")

 ; Test region-new
 (let (regx)
   (setf regx (region-new (list (state-from 's0001) (state-from 's0010) (state-from 's0100))))
   (assert (and (region-p regx) (string= (region-str regx) "r0xxX+")))

   (format t "~&  region-new OK")
 )

 ; Test high state.
 (let (regx stax states)
   ; Test one-state region.
   (setf regx (region-from 'r10X1))
   (setf stax (region-high-state regx))
   (assert (and (state-p stax) (state-eq stax (state-from 's1011))))

   ; Test two-state region.
   (setf regx (region-from 'r01X0))
   (setf stax (region-high-state regx))
   (assert (and (state-p stax) (state-eq stax (state-from 's0110))))

   ; Test three-state region.
   (setf states (list (state-from 's0001) (state-from 's0010) (state-from 's0100)))
   (setf regx (region-new states))
   (setf stax (region-high-state regx))
   (assert (and (state-p stax) (state-eq stax (state-from 's0111))))

   (format t "~&  region-high-state OK")
 )

 ; Test low state.
 (let (regx stax)
   ; Test one-state region.
   (setf regx (region-from 'r10X1))
   (setf stax (region-low-state regx))
   (assert (and (state-p stax) (state-eq stax (state-from 's1001))))

   ; Test two-state region.
   (setf regx (region-from 'r01X1))
   (setf stax (region-low-state regx))
   (assert (and (state-p stax) (state-eq stax (state-from 's0101))))

   ; Test three-state region.
   (setf regx (region-new (list (state-from 's0001) (state-from 's0011) (state-from 's0101))))
   (setf stax (region-low-state regx))
   (assert (and (state-p stax) (state-eq stax (state-from 's0001))))

   (format t "~&  region-low-state OK")
 )

 ; Test region-x-mask.
 (let (regx mskx)
   ; Test single-state region.
   (setf regx (region-from 'r0101))
   (setf mskx (region-x-mask regx))
   (assert (and (mask-p mskx) (mask-eq mskx (mask-from 'm0000))))

   ; Test two-state region.
   (setf regx (region-from 'r01X1))
   (setf mskx (region-x-mask regx))
   (assert (and (mask-p mskx) (mask-eq mskx (mask-from 'm0010))))

   ; Test three-state region.
   (setf regx (region-new (list (state-from 's0001) (state-from 's0010) (state-from 's0101))))
   (setf mskx (region-x-mask regx))
   (assert (and (mask-p mskx) (mask-eq mskx (mask-from 'm0111))))

   (format t "~&  region-x-mask OK")
 )

 ; Test region-1-mask.
 (let (regx mskx)
   ; Test single-state region.
   (setf regx (region-from 'r0101))
   (setf mskx (region-1-mask regx))
   (assert (and (mask-p mskx) (mask-eq mskx (mask-from 'm0101))))

   ; Test two-state region.
   (setf regx (region-from 'r01X1))
   (setf mskx (region-1-mask regx))
   (assert (and (mask-p mskx) (mask-eq mskx (mask-from 'm0101))))

   ; Test three-state region.
   (setf regx (region-new (list (state-from 's1000) (state-from 's1010) (state-from 's1100))))
   (setf mskx (region-1-mask regx))
   (assert (and (mask-p mskx) (mask-eq mskx (mask-from 'm1000))))

   (format t "~&  region-1-mask OK")
 )

 ; Test region-0-mask.
 (let (regx mskx)
   ; Test single-state region.
   (setf regx (region-from 'r0101))
   (setf mskx (region-0-mask regx))
   (assert (and (mask-p mskx) (mask-eq mskx (mask-from 'm1010))))

   ; Test two-state region.
   (setf regx (region-from 'r01X1))
   (setf mskx (region-0-mask regx))
   (assert (and (mask-p mskx) (mask-eq mskx (mask-from 'm1000))))

   ; Test three-state region.
   (setf regx (region-new (list (state-from 's1000) (state-from 's1010) (state-from 's1100))))
   (setf mskx (region-0-mask regx))
   (assert (and (mask-p mskx) (mask-eq mskx (mask-from 'm0001))))

   (format t "~&  region-0-mask OK")
 )

 ; Test region-second-state.
 (let (regx stax)
   ; Test single-state region.
   (setf regx (region-from 'r0101))
   (setf stax (region-second-state regx))
   (assert (and (state-p stax) (state-eq stax (state-from 's0101))))

   ; Test two-state region.
   (setf regx (region-from 'r01X1))
   (setf stax (region-second-state regx))
   (assert (and (state-p stax) (state-eq stax (state-from 's0101))))

   ; Test three-state region.
   (setf regx (region-new (list (state-from 's0001) (state-from 's0010) (state-from 's0111))))
   (setf stax (region-second-state regx))
   (assert (and (state-p stax) (state-eq stax (state-from 's0110))))

   (format t "~&  region-second-state OK")
 )

 ; Test region-str.
 (let (strx)
   (setf strx (region-str (region-from 'r01Xx)))

   (assert (and (stringp strx) (string-equal strx "r01Xx")))

   (format t "~&  region-str OK")
 )

 ; Test region-from.
 (let (errx)
   ; Test arg with invalid character.
   (setf errx (region-from-str "01X3"))
   (assert (err-p errx))

   ; Test arg with no valid character.
   (setf errx (region-from-str "r"))
   (assert (err-p errx))

   ; Test good string.
   (assert (string-equal (region-str (region-from 'r01Xx)) "r01Xx"))

   (format t "~&  region-from OK")
 )

 ; Test region-eq.
 (let (boolx reg1 reg2 reg3)
   (setf reg1 (region-from 'r01Xx))
   (setf reg2 (region-from 'r01xx))
   (setf reg3 (region-from 'rXx01))

   ; Test true condition.
   (setf boolx (region-eq reg1 reg2))
   (assert (and (bool-p boolx) boolx))

   ; Test false condition.
   (setf boolx (region-eq reg1 reg3))
   (assert (and (bool-p boolx) (not boolx)))

   (format t "~&  region-eq OK")
 )

 ; Test region-intersection.
 (let (reg1 reg2 reg3 reg4)
   (setf reg1 (region-from 'r01Xxx))
   (setf reg2 (region-from 'rXx01x))
   (setf reg3 (region-from 'rXx00x))

   (setf reg4 (region-intersection reg1 reg2))
   (assert (and (region-p reg4) (region-eq reg4 (region-from 'r0_101x))))

   (assert (null (region-intersection reg2 reg3)))

   (format t "~&  region-intersection OK")
 )

 ; Test region-union.
 (let (reg1 reg2 reg3)
   (setf reg1 (region-from 'r01Xx_0101))
   (setf reg2 (region-from 'r1001_xx01))

   (setf reg3 (region-union reg1 reg2))
   (assert (and (region-p reg3) (region-eq reg3 (region-from 'rxxxx_xx01))))

   (format t "~&  region-union OK")
 )

  ; Test region-edge-mask.
  (let (reg1 msk1)
    (setf reg1 (region-from 'r01Xx_0101))
    (setf msk1 (region-edge-mask reg1))
    (assert (and (mask-p msk1) (mask-eq msk1 (mask-from 'm1100_1111))))

    (format t "~&  region-edge-mask OK")
  )

  ; Test region-distance.
  (let (reg1 reg2 dist)

    ; Test non-intersecting, non-adjacent regions.
    (setf reg1 (region-from 'r0XX1))
    (setf reg2 (region-from 'r1XX0))
    (setf dist (region-distance reg1 reg2))
    (assert (and (integerp dist) (= dist 2)))

    ; Test intersecting regions.
    (setf reg2 (region-from 'rX01X))
    (setf dist (region-distance reg1 reg2))
    (assert (and (integerp dist) (= dist 0)))

    ; Test adjacent regions.
    (setf reg2 (region-from 'r1XX1))
    (setf dist (region-distance reg1 reg2))
    (assert (and (integerp dist) (= dist 1)))

    ; Test subset/superset regions.
    (setf reg1 (region-from 'r0XX1))
    (setf reg2 (region-from 'r01X1))
    (setf dist (region-distance reg1 reg2))
    (assert (and (integerp dist) (= dist 0)))

    (format t "~&  region-distance OK")
  )

  ; Test region-intersects.
  (let (reg1 reg2 reg3 boolx)
    (setf reg1 (region-from 'r0XX1))
    (setf reg2 (region-from 'rX101))
    (setf reg3 (region-from 'rXX00))

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
    (setf reg1 (region-from 'r0XX1))
    (setf reg2 (region-from 'r01x1))
    (setf reg3 (region-from 'rXX00))
    (setf reg4 (region-from 'r1X00))

    ; Test subset.
    (setf boolx (region-superset-of :sup reg1 :sub reg2))
    (assert boolx)

    ; Test intersection, but not subset.
    (setf boolx (region-superset-of :sup reg1 :sub reg3))
    (assert (not boolx))

    ; Test no intersection, not subset.
    (setf boolx (region-superset-of :sup reg1 :sub reg4))
    (assert (not boolx))

    (format t "~&  region-superset-of OK")
  )

  ; Test region-set-to-ones.
  (let (reg1 reg2 mskx)
    (setf reg1 (region-from 'r0XX1))
    (setf mskx (mask-from   'm1101))

    (setf reg2 (region-set-to-ones reg1 mskx))
    (assert (region-eq reg2 (region-from 'r11X1)))

    (format t "~&  region-set-to-ones OK")
  )

  ; Test region-set-to-zeros.
  (let (reg1 reg2 mskx)
    (setf reg1 (region-from 'r1XX0))
    (setf mskx (mask-from   'm1101))

    (setf reg2 (region-set-to-zeros reg1 mskx))
    (assert (region-eq reg2 (region-from 'r00X0)))

    (format t "~&  region-set-to-zeros OK")
  )

  ; Test region-subtract.
  (let (reg1 reg2 regstr)
    (setf reg1 (region-from 'r0XX1))

    ; Test subtracting a subset region.
    (setf reg2 (region-from 'r0011))
    (setf regstr (region-subtract :min-reg reg1 :sub-reg reg2))
    (assert (= (regionstore-length regstr) 2))
    (assert (regionstore-member regstr (region-from 'r01X1)))
    (assert (regionstore-member regstr (region-from 'r0x01)))

    ; Test subtracting an intersecting, non-subset, region.
    (setf reg2 (region-from 'rX01x))
    (setf regstr (region-subtract :min-reg reg1 :sub-reg reg2))
    (assert (= (regionstore-length regstr) 2))
    (assert (regionstore-member regstr (region-from 'r01X1)))
    (assert (regionstore-member regstr (region-from 'r0x01)))

    ; Test subtracting a non-intersecting region.
    (setf reg2 (region-from 'rX010))
    (setf regstr (region-subtract :min-reg reg1 :sub-reg reg2))
    (assert (= (regionstore-length regstr) 1))
    (assert (regionstore-member regstr (region-from 'r0XX1)))

    ; Test subtracting a superset region.
    (setf reg2 (region-from 'rXXXX))
    (setf regstr (region-subtract :min-reg reg1 :sub-reg reg2))
    (assert (= (regionstore-length regstr) 0))

    (format t "~&  region-subtract OK")
  )

  ; Test region-list-p.
  (let (lst1)
    ;; Test null list.
    (assert (region-list-p lst1))

    ;; Test not a list.
    (assert (not (region-list-p 1)))

    ;; Test a list with one region.
    (setf lst1 (list (region-from 'rXXXX)))
    (assert (region-list-p lst1))

    ;; Test a list with one region and one not-a-region.
    (setf lst1 (list (region-from 'rXXXX) 1))
    (assert (not (region-list-p lst1)))

    ;; Test a list with two regions.
    (setf lst1 (list (region-from 'r01XX) (region-from 'r10XX)))
    (assert (region-list-p lst1))

    ;; Test a list with two regions of differing number bits.
    (setf lst1 (list (region-from 'r01XX) (region-from 'r10X)))
    (assert (region-list-p lst1))

    (format t "~&  region-list-p OK")
  )

  ;; Test region-edge-dif-mask.
  (let (reg1 reg2 msk1)
    (setf reg1 (region-from 'rX01X))
    (setf reg2 (region-from 'rX101))

    (setf msk1 (region-edge-dif-mask reg1 reg2))
    (assert (and (mask-p msk1) (mask-eq msk1 (mask-from 'm0110))))

    (format t "~&  region-edge-dif-mask OK")
  )

  ;; Test region-is-adjacent.
  (let (reg1 reg2 reg3 bl1)
    (setf reg1 (region-from 'rX01X))
    (setf reg2 (region-from 'rXX01))
    (setf reg3 (region-from 'rXX10))

    ;; Test two adjacent regions.
    (setf bl1 (region-is-adjacent reg1 reg2))
    (assert bl1)

    ;; Test two non-adjacent regions.
    (setf bl1 (region-is-adjacent reg2 reg3))
    (assert (not bl1))

    (format t "~&  region-is-adjacent OK")
  )

  ;; Test region-symmetric-overlapping-region.
  (let (reg1 reg2 reg3)
    (setf reg1 (region-from 'r1100_XXXX_0))
    (setf reg2 (region-from 'rXXXX_1100_1))
    (setf reg3 (region-symmetric-overlapping-region reg1 reg2))
    (assert (region-p reg3))
    (assert (region-eq reg3 (region-from 'r1100_1100_X)))

    (format t "~&  region-symmetric-overlapping-region OK")
  )

  ;; Test region-adjacent-external-states.
  (let (reg1 sta1 ext1)
    (setf reg1 (region-from 'r1X0X))
    (assert (region-p reg1))

    (setf sta1 (state-from  's1001))
    (assert (state-p sta1))

    (setf ext1 (region-adjacent-external-states reg1 sta1))
    (assert (statestore-p ext1))

    (assert (= (statestore-length ext1) 2))
    (assert (statestore-member ext1 (state-from  's0001)))
    (assert (statestore-member ext1 (state-from  's1011)))
 
    (format t "~&  region-adjacent-external-states OK")
  )

  (format t "~&region-tests done")
  t
)

