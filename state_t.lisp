;;; Run tests.
(defun state-tests ()
  (format t "~&state-tests beginning")

  ; Test state-new.
  (let (stax)
    (setf stax (state-new (value-from-str "#x03")))
    (assert (and (state-p stax) (= (value-bits (state-value stax)) 3)))
    (format t "~&  state-new OK")
  )

  ; Test state-from-str.
  (let (stax)
     (setf stax (state-from-str "#x23"))
     (assert (state-p stax))
     (assert (= (value-num-bits (state-value stax)) 8))
     (assert (= (value-bits (state-value stax)) #x23))
 
     (format t "~&  state-from-str OK")
  )

  ; Test state-str.
  (let (strx sta1)
     (setf sta1 (state-from-str "#x1"))
     (setf strx (state-str sta1))
     (assert (and (stringp strx) (string= strx "#S(STATE #x1)")))
     (format t "~&  state-str OK")
  )
 
  ; Test state-num-bits.
  (let (numx sta1)
     (setf sta1 (state-from-str "#x5"))
     (setf numx (state-num-bits sta1))
     (assert (and (integerp numx) (= numx 4)))
     (format t "~&  state-num-bits OK")
  )

  ; Test state-eq.
  (let (boolx sta1 sta2 sta3)
    (setf sta1 (state-from-str "#x1"))
    (setf sta2 (state-from-str "#x2"))
    (setf sta3 (state-from-str "#x1"))

    ; Test two states that are eq.
    (setf boolx (state-eq sta1 sta3))
    (assert (and (bool-p boolx) boolx))

    ; Test two states that are not eq.
    (setf boolx (state-eq sta1 sta2))
    (assert (and (bool-p boolx) (not boolx)))

    (format t "~&  state-eq OK")
  )

  ; Test state-xor.
  (let (sta1 sta2 msk1 valx)
 
     (setf sta1 (state-from-str "#x5a"))
     (setf sta2 (state-from-str "#x11"))
 
     (setf msk1 (mask-from-str "#x22"))
 
     ; Test state xor state.
     (setf valx (state-xor sta1 sta2))
     (assert (and (value-p valx) (value-eq valx (value-from-str "#x4b"))))
 
     ; Test state xor mask.
     (setf valx (state-xor sta1 msk1))
     (assert (and (value-p valx) (value-eq valx (value-from-str "#x78"))))
 
     (format t "~&  state-xor OK")
  )

  ; Test state-between.
  (let (sta5 sta2 sta7 boolx) 
 
     (setf sta5 (state-from-str "#x5"))
     (setf sta2 (state-from-str "#x2"))
     (setf sta7 (state-from-str "#x7"))
 
     ; Test square between two others.
     (setf boolx (state-between sta7 sta5 sta2))
     (assert (and (bool-p boolx) boolx))
 
     ; Test square not between two others.
     (setf boolx (state-between sta5 sta2 sta7))
     (assert (and (bool-p boolx) (not boolx)))
 
     (format t "~&  state-between OK")
  )

  ; Test state-neq.
  (let (boolx sta1 sta2 sta3)
    (setf sta1 (state-from-str "#x1"))
    (setf sta2 (state-from-str "#x2"))
    (setf sta3 (state-from-str "#x1"))

    ; Test two states that are eq.
    (setf boolx (state-neq sta1 sta3))
    (assert (and (bool-p boolx) (not boolx)))

    ; Test two states that are not eq.
    (setf boolx (state-eq sta1 sta2))
    (assert (and (bool-p boolx) (not boolx)))

    (format t "~&  state-neq OK")
  )

  ; Test state-list-x-mask.
  (let (sta0 sta1 sta2 mskx) 
 
     (setf sta0 (state-from-str "#x0"))
     (setf sta1 (state-from-str "#x1"))
     (setf sta2 (state-from-str "#x2"))
 
     (setf mskx (state-list-x-mask (list sta0 sta1 sta2)))
     (assert (and (mask-p mskx) (mask-eq mskx (mask-from-str "#x3"))))
 
     (format t "~&  state-list-x-mask OK")
  )

  ; Test state-list-no-dups.
  (let (lstx sta5 sta2 sta7) 
 
     (setf sta5 (state-from-str "#x5"))
     (setf sta2 (state-from-str "#x2"))
     (setf sta7 (state-from-str "#x7"))
 
     (setf lstx (state-list-no-dups (list sta5)))
     (assert (and (listp lstx) (= (length lstx) 1)))
 
     (setf lstx (state-list-no-dups (list sta5 sta2 sta7)))
     (assert (and (listp lstx) (= (length lstx) 3)))
 
     (setf lstx (state-list-no-dups (list sta5 sta2 sta5 sta7 sta2)))
     ;(format t "~&lstx ~A" lstx)
     (assert (and (listp lstx) (= (length lstx) 3)))
 
     (format t "~&  state-list-no-dups OK")
  )

  ; Test state-and.
  (let (sta1 sta2 msk1 valx)
 
     (setf sta1 (state-from-str "#x5a"))
     (setf sta2 (state-from-str "#x33"))
 
     (setf msk1 (mask-from-str "#x66"))
 
     ; Test state and state.
     (setf valx (state-and sta1 sta2))
     (assert (and (value-p valx) (value-eq valx (value-from-str "#x12"))))
 
     ; Test state and mask.
     (setf valx (state-and sta1 msk1))
     (assert (and (value-p valx) (value-eq valx (value-from-str "#x42"))))
 
     (format t "~&  state-and OK")
  )

  ; Test state-or.
  (let (sta1 sta2 msk1 valx)
 
     (setf sta1 (state-from-str "#x5a"))
     (setf sta2 (state-from-str "#x11"))
 
     (setf msk1 (mask-from-str "#x22"))
 
     ; Test state or state.
     (setf valx (state-or sta1 sta2))
     (assert (and (value-p valx) (value-eq valx (value-from-str "#x5b"))))
 
     ; Test state or mask.
     (setf valx (state-or sta1 msk1))
     (assert (and (value-p valx) (value-eq valx (value-from-str "#x7a"))))
 
     (format t "~&  state-or OK")
  )

  (format t "~&state-tests done")
  t
)
