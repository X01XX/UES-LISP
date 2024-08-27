;;; Run tests.
(defun cngstpsstore-tests ()
  (format t "~&cngstpsstore-tests beginning")

  ; Test cngstpsstore-new.
  (let (store1 stp1 cngx)

    (setf stp1 (step-new :act-id 0 :rule (rule-from-str "[01/11/00/01]") :kind 'a :w 1 :u 0))
    (setf cngx (change-new :b01 (mask-from-str "#b0001") :b10 (mask-from-str "#b0100")))

    (setf store1 (cngstpsstore-new cngx))

    ; Test adding first change/steps.
    (assert (cngstpsstore-add store1 stp1))
    (assert (= (cngstpsstore-num-steps store1) 1))

    ; Test adding a duplicate.
    (setf stp1 (step-new :act-id 0 :rule (rule-from-str "[01/11/00/01]") :kind 'a :w 1 :u 0))
    (assert (not (cngstpsstore-add store1 stp1)))
    (assert (= (cngstpsstore-num-steps store1) 1))

    ; Test adding a second change/steps, to the same cngstps instance.
    (setf stp1 (step-new :act-id 0 :rule (rule-from-str "[01/10/11/01]") :kind 'a :w 1 :u 0))
    (assert (cngstpsstore-add store1 stp1))
    (assert (= (cngstpsstore-num-steps store1) 3))

    ; Test adding a second change/steps, with a new change.
    (setf stp1 (step-new :act-id 0 :rule (rule-from-str "[01/10/11/11]") :kind 'a :w 1 :u 0))
    (assert (cngstpsstore-add store1 stp1))
    ;(format t "~&store1: ~A" store1)
    (assert (= (cngstpsstore-num-steps store1) 4))

    (format t "~&  cngstpsstore-new OK")

  )
  
  (format t "~&cngstpsstore-tests done")
  t
)
