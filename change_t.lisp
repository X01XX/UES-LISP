;;;; Run tests for change struct functions.

(defun change-tests ()
  (format t "~&change-tests beginning")

  ; Test change-new

  ; Test change-num-changes.
  (let (cng1 msk01 msk10 num)
    (setf msk01 (mask-from-str "#b1001"))
    (setf msk10 (mask-from-str "#b0100"))
    (setf cng1 (change-new :b01 msk01 :b10 msk10))
    (setf num (change-num-changes cng1))
    (assert (= num 3))

    (format t "~&  change-num-changes Ok")
  )

  (format t "~&change-tests done")
)
