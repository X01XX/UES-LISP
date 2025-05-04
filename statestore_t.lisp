;;; Run tests.
(defun statestore-tests ()
  (format t "~&statestore-tests beginning")

  ;; Test statestore-new.
  (let (ssx ss1)

    ; A state list.
    (setf ss1 (list (state-from 's0001) (state-from 's0010) (state-from 's1000)))

    ; Test a state list.
    (setf ssx (statestore-new ss1))
    (assert (statestore-p ssx))

    (assert (= (statestore-length ssx) 3))

    ;; Test a single state.
    (setf ssx (statestore-new (state-from 's0010)))
    (assert (statestore-p ssx))

    (assert (= (statestore-length ssx) 1))

    (format t "~&  statestore-new OK")
  )

  ;; Test statestore-remove-unneeded.
  (let (str1 str2)
    (setf str1 (statestore-new (list (state-from 's0001) (state-from 's0010) (state-from 's0111) (state-from 's0100))))
    (assert (statestore-p str1))

    (setf str2 (statestore-remove-unneeded str1))
    (assert (statestore-p str1))

    (assert (= (statestore-length str2) 3))
    (assert (statestore-member str2 (state-from 's0001)))
    (assert (statestore-member str2 (state-from 's0010)))
    (assert (statestore-member str2 (state-from 's0111)))

    (format t "~&  statestore-remove-unneeded OK")
  )

  ;; Test statestore-congruent.
  (let (store1 (*domain-num-bits-list* (list 2 3)))
    ;; Test congruent statestore.
    (setf store1 (statestore-new (list (state-from 's01) (state-from 's010))))
    (assert (statestore-p store1))
    
    (assert (statestore-congruent store1))

    ;; Test non-congruent statestore.
    (setf store1 (statestore-new (list (state-from 's01) (state-from 's0100))))
    (assert (statestore-p store1))
    
    (assert (not (statestore-congruent store1)))

    (format t "~&  statestore-congruent OK")
  )

  ;; Test statestore-same-num-bits.
  (let (store1)

    ;; Test empty statestore.
    (setf store1 (statestore-new nil))
    (assert (statestore-p store1))

    (assert (statestore-same-num-bits store1))

    ;; Test statestore with one state.
    (setf store1 (statestore-new (list (state-from 's0101_1111))))
    (assert (statestore-p store1))

    (assert (statestore-same-num-bits store1))

    ;; Test statestore with GT one state, the same number bits.
    (setf store1 (statestore-new (list (state-from 's0101_1111) (state-from 's0101_1101) (state-from 's0101_1110))))
    (assert (statestore-p store1))

    (assert (statestore-same-num-bits store1))

    ;; Test statestore with GT one state, not the same number bits..
    (setf store1 (statestore-new (list (state-from 's0101_1111) (state-from 's101_1101)(state-from 's0101_1110))))
    (assert (statestore-p store1))

    (assert (not (statestore-same-num-bits store1)))

    (format t "~&  statestore-same-num-bits OK")
  )


  ;; Test statestore-x-mask.
  (let (store1)

    ;; Test statestore with one state.
    (setf store1 (statestore-new (list (state-from 's0101_1111))))
    (assert (statestore-p store1))

    (assert (mask-eq (statestore-x-mask store1) (mask-from 'm0000_0000)))

    ;; Test statestore with GT one state, the same number bits.
    (setf store1 (statestore-new (list (state-from 's0101_1111)
                                       (state-from 's0101_1101)
                                       (state-from 's0001_1110))))
    (assert (statestore-p store1))

    (assert (mask-eq (statestore-x-mask store1) (mask-from 'm0100_0011)))

    (format t "~&  statestore-x-mask OK")
  )

  ;; Test statestore-from.
  (let (store1)

    ;; Test statestore with no states.
    (setf store1 (statestore-from '()))
    (assert (statestore-p store1))

    ;; Test statestore with one state.
    (setf store1 (statestore-from '(s1010)))
    (assert (statestore-p store1))
    (assert (= (statestore-length store1) 1))
    (assert (statestore-member store1 (state-from 's1010)))
    

    ;; Test statestore with two states.
    (setf store1 (statestore-from '(s1010 s101)))
    (assert (statestore-p store1))
    (assert (= (statestore-length store1) 2))
    (assert (state-eq (statestore-first-state store1) (state-from 's1010)))
    (assert (state-eq (statestore-last-state store1) (state-from 's101)))

    (format t "~&  statestore-from OK")
  )

  ; Test statestore-intersection.
  (let (storex storey storez state1 state2 state3 state4)
    (setf state1 (state-from 's0001))
    (setf state2 (state-from 's0010))
    (setf state3 (state-from 's0011))
    (setf state4 (state-from 's0100))

    (setf storex (statestore-new (list state1 state2 state3)))
    (setf storey (statestore-new (list state3 state4 state2)))
    (setf storez (statestore-intersection storex storey))

    (assert (= 2 (statestore-length storez)))
    (assert (statestore-member storez state2))
    (assert (statestore-member storez state3))

    (format t "~&  statestore-intersection OK")
  )

  ; Test statestore-union.
  (let (storex storey storez state1 state2 state3 state4)
    (setf state1 (state-from 's0001))
    (setf state2 (state-from 's0010))
    (setf state3 (state-from 's0011))
    (setf state4 (state-from 's0100))

    (setf storex (statestore-new (list state1 state2)))
    (setf storey (statestore-new (list state3 state4)))
    (setf storez (statestore-union storex storey))

    (assert (= 4 (statestore-length storez)))
    (assert (statestore-member storez state1))
    (assert (statestore-member storez state2))
    (assert (statestore-member storez state3))
    (assert (statestore-member storez state4))

    (format t "~&  statestore-union OK")
  )

  ; Test statestore-difference.
  (let (storex storey storez state1 state2 state3 state4)
    (setf state1 (state-from 's0001))
    (setf state2 (state-from 's0010))
    (setf state3 (state-from 's0011))
    (setf state4 (state-from 's0100))

    (setf storex (statestore-new (list state1 state2)))
    (setf storey (statestore-new (list state3 state4 state2)))
    (setf storez (statestore-difference storex storey))

    (assert (= 1 (statestore-length storez)))
    (assert (statestore-member storez state1))

    (format t "~&  statestore-difference OK")
  )

  (format t "~&statestore-tests done")
  t
)
