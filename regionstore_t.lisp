;;; Run regionstore tests.
(defun regionstore-tests ()
  (format t "~&regionstore-tests beginning")

  ; Test regionstore-new.
  (let (reg1 reg2 store1)

    (setf reg1 (region-from 'r0x10))
    (setf reg2 (region-from 'r1x10))

    (setf store1 (regionstore-new (list reg1 reg2)))
    (assert (regionstore-p store1))
    (assert (= (regionstore-length store1) 2))

    (format t "~&  regionstore-new OK")
  )

  ; Test regionstore-subtract-region.
  (let (store1 store2 store3)

    (setf store1 (regionstore-new (list (region-from 'rXXXX))))

    (setf store2 (regionstore-subtract-region store1 (region-from 'rX111)))
    (assert (= (regionstore-length store2) 3))
    (assert (regionstore-member store2 (region-from 'rXXX0)))
    (assert (regionstore-member store2 (region-from 'rXX0X)))
    (assert (regionstore-member store2 (region-from 'rX0XX)))

    (setf store3 (regionstore-subtract-region store2 (region-from 'r000x)))
    (assert (= (regionstore-length store3) 7))
    (assert (regionstore-member store3 (region-from 'rX01X)))
    (assert (regionstore-member store3 (region-from 'r10XX)))
    (assert (regionstore-member store3 (region-from 'rX10X)))
    (assert (regionstore-member store3 (region-from 'r1X0X)))
    (assert (regionstore-member store3 (region-from 'rXX10)))
    (assert (regionstore-member store3 (region-from 'rX1X0)))
    (assert (regionstore-member store3 (region-from 'r1XX0)))

    (format t "~&  regionstore-subtract-region OK")
  )

  ;; Test regionstore-from.
  (let (regs1 regs2 regs3)
    (setf regs1 (regionstore-from nil))
    (assert (= (regionstore-length regs1) 0))

    (setf regs2 (regionstore-from '(r1010)))
    (assert (= (regionstore-length regs2) 1))

    (setf regs3 (regionstore-from '(r1011 r111)))
    (assert (= (regionstore-length regs3) 2))

    (format t "~&  regionstore-from OK")
  )

  ;; Test adjacent, dissimilar squares.
  (let* (pos-57 pos-8c pos-regs storex (*max-region* (region-new (list (state-from 's1111) (state-from 's0000)))))

      (setf pos-57 (state-regions-implied-by-dissimilarity (state-from 's0101) (state-from 's0111)))

      (setf pos-8c (state-regions-implied-by-dissimilarity (state-from 's1000) (state-from 's1100)))

      (setf pos-regs (regionstore-intersection pos-57 pos-8c))
      (assert (regionstore-eq pos-regs
               (regionstore-from '(rX1X0 r0XX0 rXX1X rX0XX r1XX1 r11XX rXX01 rX10X r0X0X))))

      (setf storex (regionstore-regions-state-in pos-regs (state-from 's0101)))
      (assert (= (regionstore-length storex) 3))

      (setf storex (regionstore-regions-state-in pos-regs (state-from 's0111)))
      (assert (= (regionstore-length storex) 1))

      (setf storex (regionstore-regions-state-in pos-regs (state-from 's1000)))
      (assert (= (regionstore-length storex) 1))

      (setf storex (regionstore-regions-state-in pos-regs (state-from 's1100)))
      (assert (= (regionstore-length storex) 3))

    (format t "~&  regionstore adjacent similar squares OK")
  )

  ;; Test regionstore-regions-state-in.
  (let (storex stax storey)
    (setf storex (regionstore-from (read-from-string "(r0x0x r0xx1)")))
    (setf stax (state-from 's0101))
    (setf storey (regionstore-regions-state-in storex stax))
    (assert (= (regionstore-length storey) 2))

    (setf stax (state-from 's0100))
    (setf storey (regionstore-regions-state-in storex stax))
    (assert (= (regionstore-length storey) 1))

    (format t "~&  regionstore-regions-state-in OK")
  )

  ;; Test regionstore-defining-regions.
  (let (storex storey defining-regions (*max-region* (region-new (list (state-from 's1111) (state-from 's0000)))))
    (setf storex (regionstore-new nil))
    (setf defining-regions (regionstore-defining-regions storex))
    (assert (regionstore-p defining-regions))
    (assert (= (regionstore-length defining-regions) 0))

    (setf storex (regionstore-from (read-from-string "(rXXXX)")))
    (setf defining-regions (regionstore-defining-regions storex))
    (assert (regionstore-p defining-regions))
    (assert (= (regionstore-length defining-regions) 1))

    (setf storex (state-regions-implied-by-dissimilarity (state-from 's0101) (state-from 's0111)))
    (setf defining-regions (regionstore-defining-regions storex))
    (assert (regionstore-p defining-regions))
    (assert (= (regionstore-length defining-regions) 2))

    (setf storex (state-regions-implied-by-dissimilarity (state-from 's0101) (state-from 's0111)))
    (setf storey (state-regions-implied-by-dissimilarity (state-from 's0111) (state-from 's1111)))
    (setf storex (regionstore-intersection storex storey))

    (setf defining-regions (regionstore-defining-regions storex))
    (assert (regionstore-p defining-regions))
    (assert (= (regionstore-length defining-regions) 3))
    (assert (not (regionstore-member defining-regions (region-from 'r010x))))

    (format t "~&  regionstore-defining-regions OK")
  )

  ;; Test regionstore-split-by-intersections.
  (let ((regstr (regionstore-from (read-from-string "(rX10X  rX1X1  r1X01 r0X11)")))      
        rslt
       )

    ;(format t "~&regstr: ~A" (regionstore-str regstr))
    ;(setf rslt (regionstore-max-intersections regstr))
    ;(assert (regionstore-p rslt))
    ;(format t "~&rslt: ~A" (regionstore-str rslt))

    (setf rslt (regionstore-split-by-intersections regstr))
    (format t "~&rslt: ~A" (regionstore-str rslt))
    (assert (= (regionstore-length rslt) 7))

    (format t "~&  regionstore-split-by-intersections OK")
  )

  (format t "~&regionstore-tests done")
  t
)

