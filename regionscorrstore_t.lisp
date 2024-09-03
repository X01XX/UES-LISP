;;; Run tests.
(defun regionscorrstore-tests ()
  (format t "~&regionscorrstore-tests beginning")

  ; Test regionscorrstore-new.
  (let (store1)

    (setf store1 (regionscorrstore-new (list
	 (regionscorr-new (list (region-from-str "0x") (region-from-str "10")))
	 (regionscorr-new (list (region-from-str "1x") (region-from-str "10"))))))

    (assert (regionscorrstore-p store1))
    (assert (= (regionscorrstore-length store1) 2))

    (format t "~&  regionscorrstore-new OK")
  )

  ; Test regionscorrstore-subtract-regionscorr.
  (let (store1 store2 store3)

    (setf store1 (regionscorrstore-new (list (regionscorr-new (list
	(region-from-str "XX") (region-from-str "XX"))))))

    (setf store2 (regionscorrstore-subtract-regionscorr store1 (regionscorr-new (list
	(region-from-str "X1") (region-from-str "11")))))

    (assert (= (regionscorrstore-length store2) 3))

    (assert (regionscorrstore-contains store2 (regionscorr-new (list (region-from-str "XX") (region-from-str "X0")))))
    (assert (regionscorrstore-contains store2 (regionscorr-new (list (region-from-str "XX") (region-from-str "0X")))))
    (assert (regionscorrstore-contains store2 (regionscorr-new (list (region-from-str "X0") (region-from-str "XX")))))

    (setf store3 (regionscorrstore-subtract-regionscorr store2 (regionscorr-new (list (region-from-str "00") (region-from-str "0x")))))

    (assert (= (regionscorrstore-length store3) 7))
    (assert (regionscorrstore-contains store3 (regionscorr-new (list (region-from-str "X0") (region-from-str "1X")))))
    (assert (regionscorrstore-contains store3 (regionscorr-new (list (region-from-str "10") (region-from-str "XX")))))
    (assert (regionscorrstore-contains store3 (regionscorr-new (list (region-from-str "X1") (region-from-str "0X")))))
    (assert (regionscorrstore-contains store3 (regionscorr-new (list (region-from-str "1X") (region-from-str "0X")))))
    (assert (regionscorrstore-contains store3 (regionscorr-new (list (region-from-str "XX") (region-from-str "10")))))
    (assert (regionscorrstore-contains store3 (regionscorr-new (list (region-from-str "X1") (region-from-str "X0")))))
    (assert (regionscorrstore-contains store3 (regionscorr-new (list (region-from-str "1X") (region-from-str "X0")))))

    (format t "~&  regionscorrstore-subtract-regionscorr OK")
  )

  ; Test regionscorrstore-append.
  (let (store1 store2 store3)
    (setf store1 (regionscorrstore-new (list (regionscorr-new (list (region-from-str "X0") (region-from-str "00")))
					     (regionscorr-new (list (region-from-str "X0") (region-from-str "01"))))))
    (setf store2 (regionscorrstore-new (list (regionscorr-new (list (region-from-str "X0") (region-from-str "01")))
					     (regionscorr-new (list (region-from-str "X0") (region-from-str "11"))))))
    (setf store3 (regionscorrstore-append store1 store2))

    (assert (= 4 (regionscorrstore-length store3)))
    (assert (regionscorrstore-contains store3 (regionscorr-new (list (region-from-str "X0") (region-from-str "00")))))
    (assert (regionscorrstore-contains store3 (regionscorr-new (list (region-from-str "X0") (region-from-str "01")))))
    (assert (regionscorrstore-contains store3 (regionscorr-new (list (region-from-str "X0") (region-from-str "11")))))

    (format t "~&  regionscorrstore-append OK")
  )

  ; Test regionscorrstore-find-path.
  (let (path1 path2 path3)
      ;; Calculate regions available to link together.
      (setf path1 (regionscorrstore-new (list (regionscorr-new (list (region-from-str "XX") (region-from-str "XX"))))))
      (setf path2 (regionscorrstore-subtract-regionscorr path1
	 (regionscorr-new (list (region-from-str "01") (region-from-str "11"))))) ; Maybe square 0111 causes something bad to happen.

      (setf path2 (regionscorrstore-subtract-regionscorr path2
	 (regionscorr-new (list (region-from-str "11") (region-from-str "01"))))) ; Maybe square 1101 causes something bad to happen.

      ; Test two regions that cannot be linked.
      (setf path3 (regionscorrstore-find-path path2
	 (regionscorr-new (list (region-from-str "01") (region-from-str "11")))
	 (regionscorr-new (list (region-from-str "11") (region-from-str "01")))))

      (assert (null path3))

      ; Test two regions that can be linked by one region.
      (setf path3 (regionscorrstore-find-path path2
	(regionscorr-new (list (region-from-str "00") (region-from-str "00")))
	(regionscorr-new (list (region-from-str "01") (region-from-str "01")))))

      (assert (= 3 (pathcorr-length path3)))
      (assert (regionscorr-eq (pathcorr-first-region path3) (regionscorr-new (list (region-from-str "00") (region-from-str "00")))))
      (assert (pathcorr-contains path3 (regionscorr-new (list (region-from-str "0X") (region-from-str "0X")))))
      (assert (regionscorr-eq (pathcorr-last-region path3) (regionscorr-new (list (region-from-str "01") (region-from-str "01")))))

      ; Test regions that can be linked by two regions.
      ; Try many times, until all region options are used.
      ; At one point, two options are available, the random function needs to give two different results over some number of tries.
      (let ((reg1 (regionscorr-new (list (region-from-str "01") (region-from-str "01"))))
            (reg2 (regionscorr-new (list (region-from-str "11") (region-from-str "11"))))
            regs-used
            (link1 (regionscorr-new (list (region-from-str "X0") (region-from-str "XX")))) ; Option 1.
            (link2 (regionscorr-new (list (region-from-str "XX") (region-from-str "X0")))) ; Option 2.
          )
          (loop for num from 1 to 100
            while (< (length regs-used) 2) do
    
              ;; Choose an order, it should work either way.
              (setf path3
                (if (zerop (random 2))
                  (regionscorrstore-find-path path2 reg1 reg2)
                  (regionscorrstore-find-path path2 reg2 reg1)))
    
              (when (/= 5 (pathcorr-length path3))
                (format t "~&Unexpected number of regs-used found ~A" path3)
                (error "regionscorrstore-find-path failed")
              )
    
              ;; Check path of regions.
              (when (null path3)
                (format t "~&Path not found")
                (error "done")
              )

              ;; Check for the two options.
              (cond ((pathcorr-contains path3 link1)
        	     (if (not (member link1 regs-used :test #'regionscorr-eq))
        	       (push link1 regs-used))
        	     )
                    ((pathcorr-contains path3 link2)
        	     (if (not (member link2 regs-used :test #'regionscorr-eq))
        	       (push link2 regs-used))
        	    )
        	   (t (format t "~&Unexpected result ~A" (pathcorr-first-region path3)) 
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
	(regionscorr-new (list (region-from-str "00") (region-from-str "10")))
	(regionscorr-new (list (region-from-str "01") (region-from-str "01")))))

    (assert (= 4 (pathcorr-length path3)))

    (assert (pathcorr-contains path3 (regionscorr-new (list (region-from-str "0X") (region-from-str "0X")))))

    (assert (regionscorr-eq (pathcorr-first-region path3) (regionscorr-new (list (region-from-str "00") (region-from-str "10")))))

    (assert (regionscorr-eq (pathcorr-last-region path3) (regionscorr-new (list (region-from-str "01") (region-from-str "01")))))

    (format t "~&  regionscorrstore-find-path OK")
  )

  (format t "~&regionscorrstore-tests done")
  t
)
