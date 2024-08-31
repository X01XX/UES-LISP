;;; Run tests.
(defun stepstore-tests ()
  (format t "~&stepstore-tests beginning")

  ; Test stepstore-new.
  (let (store1 step1)

    (setf step1 (step-new :act-id 0 :rule (rule-from-str "[XX]") :kind 'a :w 1 :u 0))

    (setf store1 (stepstore-new (list step1)))
    (assert (stepstore-p store1))

    (assert (= (stepstore-length store1) 1))

    (format t "~&  stepstore-new OK")
  )


  (format t "~&stepstore-tests done")
  t
)
