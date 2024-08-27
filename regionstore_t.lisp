;;; Run tests.
(defun regionstore-tests ()
  (format t "~&regionstore-tests beginning")

  ; Test regionstore-new.
  (let (reg1 reg2 reg3 store1)

    (setf reg1 (region-from-str "0x10"))
    (setf reg2 (region-from-str "1x10"))
    (setf reg3 (region-from-str "0x10"))

    (setf store1 (regionstore-new (list reg1 reg2 reg3)))
    (assert (= (regionstore-length store1) 2))

    (format t "~&  regionstore-new OK")
  )

  ; Test regionstore-subtract-region.
  (let (store1 store2)

    (setf store1 (regionstore-new (list (region-from-str "XXXX"))))

    (setf store2 (regionstore-subtract-region store1 (region-from-str "X111")))
    ;(format t "~&store2 ~A" store2)
    (assert (= (regionstore-length store2) 3))
    (assert (regionstore-contains store2 (region-from-str "XXX0")))
    (assert (regionstore-contains store2 (region-from-str "XX0X")))
    (assert (regionstore-contains store2 (region-from-str "X0XX")))

    (setf store3 (regionstore-subtract-region store2 (region-from-str "000x")))
    ;(format t "~&store3 ~A" store3)
    (assert (= (regionstore-length store3) 7))
    (assert (regionstore-contains store3 (region-from-str "X01X")))
    (assert (regionstore-contains store3 (region-from-str "10XX")))
    (assert (regionstore-contains store3 (region-from-str "X10X")))
    (assert (regionstore-contains store3 (region-from-str "1X0X")))
    (assert (regionstore-contains store3 (region-from-str "XX10")))
    (assert (regionstore-contains store3 (region-from-str "X1X0")))
    (assert (regionstore-contains store3 (region-from-str "1XX0")))

    (format t "~&  regionstore-subtract-region OK")
  )

  (format t "~&regionstore-tests done")
  t
)
