;;;; Run tests for change struct functions.

(defun change-tests ()
  (format t "~&change-tests beginning")

  ; Test change-new

  ; Test change-num-changes.
  (let (cng1 msk01 msk10 num)
    (setf msk01 (mask-from 'm1001))
    (setf msk10 (mask-from 'm0100))

    (setf cng1 (change-new :m01 msk01 :m10 msk10))
    (assert (change-p cng1))

    (setf num (change-num-changes cng1))
    (assert (= num 3))

    (format t "~&  change-num-changes OK")
  )

  ; Test change-split.
  (let (cng1 cng-lst)
    (setf cng1 (change-new :m01 (mask-from 'm1001) :m10 (mask-from 'm0110)))
    (setf cng-lst (change-split cng1))
    ;(format t "~&cng-lst ~A" cng-lst)

    (assert (= (length cng-lst) 4))
    (assert (member (change-new :m01 (mask-from 'm0000) :m10 (mask-from 'm0100)) cng-lst :test #'change-eq))
    (assert (member (change-new :m01 (mask-from 'm0000) :m10 (mask-from 'm0010)) cng-lst :test #'change-eq))
    (assert (member (change-new :m01 (mask-from 'm1000) :m10 (mask-from 'm0000)) cng-lst :test #'change-eq))
    (assert (member (change-new :m01 (mask-from 'm0001) :m10 (mask-from 'm0000)) cng-lst :test #'change-eq))

    (format t "~&  change-split OK")
  )

  ; Test change-or.
  (let (cng1 cng2 cng3)
    (setf cng1 (change-new :m01 (mask-from 'm1001) :m10 (mask-from 'm0101)))
    (setf cng2 (change-new :m01 (mask-from 'm0101) :m10 (mask-from 'm0110)))
    (setf cng3 (change-or cng1 cng2))
    (assert (change-eq cng3 (change-new :m01 (mask-from 'm1101) :m10 (mask-from 'm0111))))

    (format t "~&  change-or OK")
  )

  ; Test change-and.
  (let (cng1 cng2 cng3)
    (setf cng1 (change-new :m01 (mask-from 'm1001) :m10 (mask-from 'm0101)))
    (setf cng2 (change-new :m01 (mask-from 'm0101) :m10 (mask-from 'm0110)))
    (setf cng3 (change-and cng1 cng2))
    (assert (change-eq cng3 (change-new :m01 (mask-from 'm0001) :m10 (mask-from 'm0100))))

    (format t "~&  change-and OK")
  )

  ; Test change-not.
  (let (cng1 cng2)
    (setf cng1 (change-new :m01 (mask-from 'm1001) :m10 (mask-from 'm0101)))
    (setf cng2 (change-not cng1))
    (assert (change-eq cng2 (change-new :m01 (mask-from 'm0110) :m10 (mask-from 'm1010))))

    (format t "~&  change-not OK")
  )

  ; Test change-and-not.
  (let (cng1 cng2 cng3)
    (setf cng1 (change-new :m01 (mask-from 'm1001) :m10 (mask-from 'm0101)))
    (setf cng2 (change-new :m01 (mask-from 'm0101) :m10 (mask-from 'm0110)))
    (setf cng3 (change-and-not cng1 cng2))
    (assert (change-eq cng3 (change-new :m01 (mask-from 'm1000) :m10 (mask-from 'm0001))))

    (format t "~&  change-and-not OK")
  )

  ; Test change-intersects
  (let (cng1 cng2 bool1)
    (setf cng1 (change-new :m01 (mask-from 'm1001) :m10 (mask-from 'm0101)))
    (setf cng2 (change-new :m01 (mask-from 'm0101) :m10 (mask-from 'm0110)))
    (setf bool1 (change-intersects cng1 cng2))
    (assert bool1)

    (setf cng1 (change-new :m01 (mask-from 'm1001) :m10 (mask-from 'm0101)))
    (setf cng2 (change-new :m01 (mask-from 'm0110) :m10 (mask-from 'm1010)))
    (setf bool1 (change-intersects cng1 cng2))
    (assert (not bool1))

    (format t "~&  change-intersects OK")
  )

  ; Test change-not.
  ; Test change-not.
  (format t "~&change-tests done")
)
