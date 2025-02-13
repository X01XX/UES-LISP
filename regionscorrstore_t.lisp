;;; Run tests.
(defun regionscorrstore-tests ()
  (format t "~&regionscorrstore-tests beginning")

  ; Test regionscorrstore-new.
  (let (store1)

    (setf store1 (regionscorrstore-new (list
	 (regionscorr-new (list (region-from "0x") (region-from "10")))
	 (regionscorr-new (list (region-from "1x") (region-from "10"))))))

    (assert (regionscorrstore-p store1))
    (assert (= (regionscorrstore-length store1) 2))

    (format t "~&  regionscorrstore-new OK")
  )

  ; Test regionscorrstore-subtract-regionscorr.
  (let (store1 store2 store3)

    (setf store1 (regionscorrstore-new (list (regionscorr-new (list
	(region-from "XX") (region-from "XX"))))))

    (setf store2 (regionscorrstore-subtract-regionscorr store1 (regionscorr-new (list
	(region-from "X1") (region-from "11")))))

    (assert (= (regionscorrstore-length store2) 3))

    (assert (regionscorrstore-contains store2 (regionscorr-new (list (region-from "XX") (region-from "X0")))))
    (assert (regionscorrstore-contains store2 (regionscorr-new (list (region-from "XX") (region-from "0X")))))
    (assert (regionscorrstore-contains store2 (regionscorr-new (list (region-from "X0") (region-from "XX")))))

    (setf store3 (regionscorrstore-subtract-regionscorr store2 (regionscorr-new (list (region-from "00") (region-from "0x")))))

    (assert (= (regionscorrstore-length store3) 7))
    (assert (regionscorrstore-contains store3 (regionscorr-new (list (region-from "X0") (region-from "1X")))))
    (assert (regionscorrstore-contains store3 (regionscorr-new (list (region-from "10") (region-from "XX")))))
    (assert (regionscorrstore-contains store3 (regionscorr-new (list (region-from "X1") (region-from "0X")))))
    (assert (regionscorrstore-contains store3 (regionscorr-new (list (region-from "1X") (region-from "0X")))))
    (assert (regionscorrstore-contains store3 (regionscorr-new (list (region-from "XX") (region-from "10")))))
    (assert (regionscorrstore-contains store3 (regionscorr-new (list (region-from "X1") (region-from "X0")))))
    (assert (regionscorrstore-contains store3 (regionscorr-new (list (region-from "1X") (region-from "X0")))))

    (format t "~&  regionscorrstore-subtract-regionscorr OK")
  )

  ; Test regionscorrstore-append.
  (let (store1 store2 store3)
    (setf store1 (regionscorrstore-new (list (regionscorr-new (list (region-from "X0") (region-from "00")))
					     (regionscorr-new (list (region-from "X0") (region-from "01"))))))
    (setf store2 (regionscorrstore-new (list (regionscorr-new (list (region-from "X0") (region-from "01")))
					     (regionscorr-new (list (region-from "X0") (region-from "11"))))))
    (setf store3 (regionscorrstore-append store1 store2))

    (assert (= 4 (regionscorrstore-length store3)))
    (assert (regionscorrstore-contains store3 (regionscorr-new (list (region-from "X0") (region-from "00")))))
    (assert (regionscorrstore-contains store3 (regionscorr-new (list (region-from "X0") (region-from "01")))))
    (assert (regionscorrstore-contains store3 (regionscorr-new (list (region-from "X0") (region-from "11")))))

    (format t "~&  regionscorrstore-append OK")
  )

  ; Test regionscorrstore-find-path.
  (let (path1 path2 path3)
      ;; Calculate regions available to link together.
      (setf path1 (regionscorrstore-new (list (regionscorr-new (list (region-from "XX") (region-from "XX"))))))
      (setf path2 (regionscorrstore-subtract-regionscorr path1
	 (regionscorr-new (list (region-from "01") (region-from "11"))))) ; Maybe squares 01 and 11 cause something bad to happen.

      (setf path2 (regionscorrstore-subtract-regionscorr path2
	 (regionscorr-new (list (region-from "11") (region-from "01"))))) ; Maybe squares 11 and 01 cause something bad to happen.

      ; Test two regions that cannot be linked.
      (setf path3 (regionscorrstore-find-path path2
	 (regionscorr-new (list (region-from "01") (region-from "11")))
	 (regionscorr-new (list (region-from "11") (region-from "01")))))

      (assert (null path3))

      ; Test two regions that can be linked by one region.
      (setf path3 (regionscorrstore-find-path path2
	(regionscorr-new (list (region-from "00") (region-from "00")))
	(regionscorr-new (list (region-from "01") (region-from "01")))))

      (assert (= 3 (pathscorr-length path3)))
      (assert (regionscorr-eq (pathscorr-first-region path3) (regionscorr-new (list (region-from "00") (region-from "00")))))
      (assert (pathscorr-contains path3 (regionscorr-new (list (region-from "0X") (region-from "0X")))))
      (assert (regionscorr-eq (pathscorr-last-region path3) (regionscorr-new (list (region-from "01") (region-from "01")))))

      ; Test regions that can be linked by two regions.
      ; Try many times, until all region options are used.
      ; At one point, two options are available, the random function needs to give two different results over some number of tries.
      (let ((reg1 (regionscorr-new (list (region-from "01") (region-from "01"))))
            (reg2 (regionscorr-new (list (region-from "11") (region-from "11"))))
            regs-used
            (link1 (regionscorr-new (list (region-from "X0") (region-from "XX")))) ; Option 1.
            (link2 (regionscorr-new (list (region-from "XX") (region-from "X0")))) ; Option 2.
          )
          (loop for num from 1 to 100
            while (< (length regs-used) 2) do
    
              ;; Choose an order, it should work either way.
              (setf path3
                (if (zerop (random 2))
                  (regionscorrstore-find-path path2 reg1 reg2)
                  (regionscorrstore-find-path path2 reg2 reg1)))
    
              (when (/= 5 (pathscorr-length path3))
                (format t "~&Unexpected number of regs-used found ~A" path3)
                (error "regionscorrstore-find-path failed")
              )
    
              ;; Check path of regions.
              (when (null path3)
                (format t "~&Path not found")
                (error "done")
              )

              ;; Check for the two options.
              (cond ((pathscorr-contains path3 link1)
        	     (if (not (member link1 regs-used :test #'regionscorr-eq))
        	       (push link1 regs-used))
        	     )
                    ((pathscorr-contains path3 link2)
        	     (if (not (member link2 regs-used :test #'regionscorr-eq))
        	       (push link2 regs-used))
        	    )
        	   (t (format t "~&Unexpected result ~A" (pathscorr-first-region path3)) 
        	      (error "regionscorrstore-find-path failed"))
              )
          )
          (when (/= 2 (length regs-used))
            (format t "~&Not all region options used ~A" regs-used)
            (error "regionscorrstore-find-path failed")
          )
    )

    ; Test two regions that cannot be linked by one region.
    (setf path3 (regionscorrstore-find-path path2
	(regionscorr-new (list (region-from "00") (region-from "10")))
	(regionscorr-new (list (region-from "01") (region-from "01")))))

    (assert (= 4 (pathscorr-length path3)))

    (assert (pathscorr-contains path3 (regionscorr-new (list (region-from "0X") (region-from "0X")))))

    (assert (regionscorr-eq (pathscorr-first-region path3) (regionscorr-new (list (region-from "00") (region-from "10")))))

    (assert (regionscorr-eq (pathscorr-last-region path3) (regionscorr-new (list (region-from "01") (region-from "01")))))

    (format t "~&  regionscorrstore-find-path OK")
  )

  ; Test regionscorrstore-intersections-of-intersections.
  (let (store1 store2)
    ;; Test three overlapping regions, with 0101 overlaped by all three.
    (setf store1 (regionscorrstore-new (list
      (regionscorr-new (list (region-from "X10X")))
      (regionscorr-new (list (region-from "X1X1")))
      (regionscorr-new (list (region-from "0XX1"))))))

    (setf store2 (regionscorrstore-intersections-of-intersections store1))
    ;(format t "~& ~&store2: ~A" store2)
    (assert (= (regionscorrstore-length store2) 6))
    (assert (regionscorrstore-contains store2 (regionscorr-new (list (region-from "0101")))))
    (assert (regionscorrstore-contains store2 (regionscorr-new (list (region-from "X100")))))
    (assert (regionscorrstore-contains store2 (regionscorr-new (list (region-from "1111")))))
    (assert (regionscorrstore-contains store2 (regionscorr-new (list (region-from "00X1")))))
    (assert (regionscorrstore-contains store2 (regionscorr-new (list (region-from "0111")))))
    (assert (regionscorrstore-contains store2 (regionscorr-new (list (region-from "1101")))))

    ;; Test one region that is a proper subset of another.
    (setf store1 (regionscorrstore-new (list
      (regionscorr-new (list (region-from "X1XX")))
      (regionscorr-new (list (region-from "01X1"))))))

    (setf store2 (regionscorrstore-intersections-of-intersections store1))
    ;(format t "~& ~&store2: ~A" store2)
    (assert (= (regionscorrstore-length store2) 3))
    (assert (regionscorrstore-contains store2 (regionscorr-new (list (region-from "X1X0")))))
    (assert (regionscorrstore-contains store2 (regionscorr-new (list (region-from "11XX")))))
    (assert (regionscorrstore-contains store2 (regionscorr-new (list (region-from "01X1")))))

    ;; Test duplicate regionscorr.
    (setf store1 (regionscorrstore-new (list
      (regionscorr-new (list (region-from "X1XX")))
      (regionscorr-new (list (region-from "X1XX"))))))

    (setf store2 (regionscorrstore-intersections-of-intersections store1))
    ;(format t "~& ~&store2: ~A" store2)
    (assert (= (regionscorrstore-length store2) 1))
    (assert (regionscorrstore-contains store2 (regionscorr-new (list (region-from "X1XX")))))

    ;; Test no regionscorr.
    (setf store1 (regionscorrstore-new nil))

    (setf store2 (regionscorrstore-intersections-of-intersections store1))
    ;(format t "~& ~&store2: ~A" store2)
    (assert (= (regionscorrstore-length store2) 0))

    (format t "~&  regionscorrstore-intersections-of-intersections OK")
  )

  (format t "~&regionscorrstore-tests done")
  t
)
