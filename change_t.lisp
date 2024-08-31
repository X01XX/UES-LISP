;;;; Run tests for change struct functions.

(defun change-tests ()
  (format t "~&change-tests beginning")

  ; Test change-new

  ; Test change-num-changes.
  (let (cng1 msk01 msk10 num)
    (setf msk01 (mask-from-str "#b1001"))
    (setf msk10 (mask-from-str "#b0100"))

    (setf cng1 (change-new :b01 msk01 :b10 msk10))
    (assert (change-p cng1))

    (setf num (change-num-changes cng1))
    (assert (= num 3))

    (format t "~&  change-num-changes OK")
  )

  ; Test change-split.
  (let (cng1 cng-lst)
    (setf cng1 (change-new :b01 (mask-from-str "#b1001") :b10 (mask-from-str "#b0110")))
    (setf cng-lst (change-split cng1))
    ;(format t "~&cng-lst ~A" cng-lst)

    (assert (= (length cng-lst) 4))
    (assert (member (change-new :b01 (mask-from-str "#x0") :b10 (mask-from-str "#x4")) cng-lst :test #'change-eq))
    (assert (member (change-new :b01 (mask-from-str "#x0") :b10 (mask-from-str "#x2")) cng-lst :test #'change-eq))
    (assert (member (change-new :b01 (mask-from-str "#x8") :b10 (mask-from-str "#x0")) cng-lst :test #'change-eq))
    (assert (member (change-new :b01 (mask-from-str "#x1") :b10 (mask-from-str "#x0")) cng-lst :test #'change-eq))

    (format t "~&  change-split OK")
  )

  (format t "~&change-tests done")
)
