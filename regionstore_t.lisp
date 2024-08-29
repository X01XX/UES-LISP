;;; Run tests.
(defun regionstore-tests ()
  (format t "~&regionstore-tests beginning")

  ; Test regionstore-new.
  (let (reg1 reg2 reg3 store1)

    (setf reg1 (region-from-str "0x10"))
    (setf reg2 (region-from-str "1x10"))

    (setf store1 (regionstore-new (list reg1 reg2)))
    (assert (= (regionstore-length store1) 2))

    (format t "~&  regionstore-new OK")
  )

  ; Test regionstore-subtract-region.
  (let (store1 store2 store3)

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

  ; Test regionstore-append.
  (let (store1 store2 store3)
    (setf store1 (regionstore-new (list (region-from-str "X000") (region-from-str "X001"))))
    (setf store2 (regionstore-new (list (region-from-str "X001") (region-from-str "X011"))))
    (setf store3 (regionstore-append store1 store2))
    ;(format t "~&store3 ~A" store3)
    (assert (= 4 (regionstore-length store3)))
    (assert (regionstore-contains store3 (region-from-str "X000")))
    (assert (regionstore-contains store3 (region-from-str "X001")))
    (assert (regionstore-contains store3 (region-from-str "X011")))

    (format t "~&  regionstore-append OK")
  )

  ; Test regionstore-find-links.
  (let (store1 store2 store3)
      ;; Calculate regions available to link together.
      (setf store1 (regionstore-new (list (region-from-str "XXXX"))))
      (setf store2 (regionstore-subtract-region store1 (region-from-str "0111"))) ; Maybe square 0111 causes something bad to happen.
      (setf store2 (regionstore-subtract-region store2 (region-from-str "1101"))) ; Maybe square 1101 causes something bad to happen.

      ; Test two regions that cannot be linked.
      (setf store3 (regionstore-find-links store2 (region-from-str "0111") (region-from-str "1101")))
      (assert (regionstore-is-empty store3))

      ; Test two regions that can be linked by one region.
      (setf store3 (regionstore-find-links store2 (region-from-str "0000") (region-from-str "0101")))
      (assert (= 3 (regionstore-length store3)))
      (assert (region-eq (regionstore-first-region store3) (region-from-str "0000")))
      (assert (regionstore-contains store3 (region-from-str "0X0X")))
      (assert (region-eq (regionstore-last-region store3) (region-from-str "0101")))

      ; Test regions that can be linked by two regions.
      ; Try many times, until all region options are used.
      ; At one point, two options are available, random needs to give two different results over some number of tries.
      (let ((reg1 (region-from-str "0101"))
            (reg2 (region-from-str "1111"))
            regs-used
            (link1 (region-from-str "X0XX")) ; Option 1.
            (link2 (region-from-str "XXX0")) ; Option 2.
          )
          (loop for num from 1 to 100
            while (< (length regs-used) 2) do
    
              (setf store3 (regionstore-find-links store2 reg1 reg2))
	      ;(format t "~&regionstore-find-links result ~A" store3)
    
              (when (/= 5 (regionstore-length store3))
                (format t "~&Unexpected number of regs-used found ~A" store3)
                (error "regionstore-find-links failed")
              )
    
	      ;; Check list of regions.
	      (let ((last-reg (regionstore-first-region store3)))
	        (loop for regx in (cdr (regionstore-regions store3)) do
		  ;; Each two successive regions must intersect.
		  (when (not (region-intersects regx last-reg))
		    (format t "~&region ~A does not intersect ~A" last-reg regx)
                    (format t "~&store3 ~A" store3)
		    (error "done")
		  )
		  ;; Each two successive regions cannot be the same.
		  (when (region-eq regx last-reg)
		    (format t "~&region ~A duplicated" last-reg)
                    (format t "~&store3 ~A" store3)
		    (error "done")
		  )
		  (setf last-reg regx)
	        )
	      )	

	      ;; Check for the two options.
              (cond ((regionstore-contains store3 link1)
        	     (if (not (member link1 regs-used :test #'region-eq))
        	       (push link1 regs-used))
        	     )
                    ((regionstore-contains store3 link2)
        	     (if (not (member link2 regs-used :test #'region-eq))
        	       (push link2 regs-used))
        	    )
        	   (t (format t "~&Unexpected result ~A" (regionstore-first-region store3)) 
        	      (error "regionstore-find-links failed"))
              )
          )
          (when (/= 2 (length regs-used))
            (format t "~&Not all region options used ~A" regs-used)
            (error "regionstore-find-links failed")
          )
    )

    ; Test two regions that cannot be linked by one region.
    (setf store3 (regionstore-find-links store2 (region-from-str "0010") (region-from-str "0101")))
    (assert (= 4 (regionstore-length store3)))
    (assert (regionstore-contains store3 (region-from-str "0X0X")))
    (assert (region-eq (regionstore-first-region store3) (region-from-str "0010")))
    (assert (region-eq (regionstore-last-region store3) (region-from-str "0101")))

    (format t "~&  regionstore-find-links OK")
  )

  (format t "~&regionstore-tests done")
  t
)
