;;; Run tests.
(defun regionstore-tests ()
  (format t "~&regionstore-tests beginning")

  ; Test regionstore-new.
  (let (reg1 reg2 store1)

    (setf reg1 (region-from "0x10"))
    (setf reg2 (region-from "1x10"))

    (setf store1 (regionstore-new (list reg1 reg2)))
    (assert (regionstore-p store1))
    (assert (= (regionstore-length store1) 2))

    (format t "~&  regionstore-new OK")
  )

  ; Test regionstore-subtract-region.
  (let (store1 store2 store3)

    (setf store1 (regionstore-new (list (region-from "XXXX"))))

    (setf store2 (regionstore-subtract-region store1 (region-from "X111")))
    ;(format t "~&store2 ~A" store2)
    (assert (= (regionstore-length store2) 3))
    (assert (regionstore-contains store2 (region-from "XXX0")))
    (assert (regionstore-contains store2 (region-from "XX0X")))
    (assert (regionstore-contains store2 (region-from "X0XX")))

    (setf store3 (regionstore-subtract-region store2 (region-from "000x")))
    ;(format t "~&store3 ~A" store3)
    (assert (= (regionstore-length store3) 7))
    (assert (regionstore-contains store3 (region-from "X01X")))
    (assert (regionstore-contains store3 (region-from "10XX")))
    (assert (regionstore-contains store3 (region-from "X10X")))
    (assert (regionstore-contains store3 (region-from "1X0X")))
    (assert (regionstore-contains store3 (region-from "XX10")))
    (assert (regionstore-contains store3 (region-from "X1X0")))
    (assert (regionstore-contains store3 (region-from "1XX0")))

    (format t "~&  regionstore-subtract-region OK")
  )

  ; Test regionstore-append.
  (let (store1 store2 store3)
    (setf store1 (regionstore-new (list (region-from "X000") (region-from "X001"))))
    (setf store2 (regionstore-new (list (region-from "X001") (region-from "X011"))))
    (setf store3 (regionstore-append store1 store2))
    ;(format t "~&store3 ~A" store3)
    (assert (= 4 (regionstore-length store3)))
    (assert (regionstore-contains store3 (region-from "X000")))
    (assert (regionstore-contains store3 (region-from "X001")))
    (assert (regionstore-contains store3 (region-from "X011")))

    (format t "~&  regionstore-append OK")
  )

  ;; Test regionstore-from.
  (let (regs1 regs2 regs3)
    (setf regs1 (regionstore-from "[]"))
    (assert (= (regionstore-length regs1) 0))

    (setf regs2 (regionstore-from "[1010]"))
    (assert (= (regionstore-length regs2) 1))

    (setf regs3 (regionstore-from "[1011, 111]"))
    (assert (= (regionstore-length regs3) 2))

    (format t "~&  regionstore-from OK")
  )

  ;; Test regionstore-largest-intersections
  (let (regs1 ints)
    (setf regs1 (regionstore-from "[0x0x, 0XX1, 0x1x]"))

    (setf ints (regionstore-largest-intersections regs1))
    ;(format t "~&ints ~A" ints)
    (assert (= 2 (regionstore-length ints)))
    (assert (regionstore-contains ints (region-from "0X11")))
    (assert (regionstore-contains ints (region-from "0X01")))

    (format t "~&  regionstore-largest-intersections OK")
  )

  ;; Test regionstore-split-by-intersections
  (let (regst1 fragments)
    (setf regst1 (regionstore-from "[01x1, 011x]"))
    (setf fragments (regionstore-split-by-intersections regst1))
    ;(format t "~&fragments ~A" fragments)
    (assert (= (regionstore-length fragments) 3))
    (assert (regionstore-contains fragments (region-from "0101")))
    (assert (regionstore-contains fragments (region-from "0110")))
    (assert (regionstore-contains fragments (region-from "0101")))

    (setf regst1 (regionstore-from "[01x1, 011x, 0x11]"))
    (setf fragments (regionstore-split-by-intersections regst1))
    ;(format t "~&fragments ~A" fragments)
    (assert (= (regionstore-length fragments) 4))
    (assert (regionstore-contains fragments (region-from "0101")))
    (assert (regionstore-contains fragments (region-from "0110")))
    (assert (regionstore-contains fragments (region-from "0011")))
    (assert (regionstore-contains fragments (region-from "0111")))

    (setf regst1 (regionstore-from "[01x1, 011x, xx11]"))
    (setf fragments (regionstore-split-by-intersections regst1))
    ;(format t "~&fragments ~A" fragments)
    (assert (= (regionstore-length fragments) 5))
    (assert (regionstore-contains fragments (region-from "0101")))
    (assert (regionstore-contains fragments (region-from "0110")))
    (assert (regionstore-contains fragments (region-from "X011")))
    (assert (regionstore-contains fragments (region-from "1X11")))
    (assert (regionstore-contains fragments (region-from "0111")))

    (setf regst1 (regionstore-from "[x10x, x1x1]"))
    (setf fragments (regionstore-split-by-intersections regst1))
    ;(format t "~&fragments ~A" fragments)
    (assert (= (regionstore-length fragments) 3))
    (assert (regionstore-contains fragments (region-from "X100")))
    (assert (regionstore-contains fragments (region-from "X111")))
    (assert (regionstore-contains fragments (region-from "X101")))

    ;; Test regions with subsets.
    (setf regst1 (regionstore-from "[xxxx, x1x1, 01x1, x101]"))
    (setf fragments (regionstore-split-by-intersections regst1))
    ;(format t "~&fragments ~A" fragments)
    (assert (= (regionstore-length fragments) 6))
    (assert (regionstore-contains fragments (region-from "XXX0")))
    (assert (regionstore-contains fragments (region-from "X0XX")))
    (assert (regionstore-contains fragments (region-from "1111")))
    (assert (regionstore-contains fragments (region-from "0111")))
    (assert (regionstore-contains fragments (region-from "1101")))
    (assert (regionstore-contains fragments (region-from "0101")))

    (format t "~&  regionstore-split-by-intersections OK")
  )

  (format t "~&regionstore-tests done")
  t
)
