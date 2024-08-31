
(defun rulestore-tests ()
  (format t "~&rulestore-tests beginning")

  ; Test rulestore-new.
  (let (store1)
    (setf store1 (rulestore-new (list (rule-from-str "[Xx/XX/XX/XX]"))))
    (assert (rulestore-p store1))

    (format t "~&  rulestore-new OK")
  )

  ; Test rulestore-eq.
  (let (boolx store1 store2)
    (setf store1 (rulestore-new (list (rule-from-str "[Xx/XX/XX/XX]") (rule-from-str "[X1/XX/XX/XX]"))))
    (setf store2 (rulestore-new (list (rule-from-str "[X1/XX/XX/XX]") (rule-from-str "[Xx/XX/XX/XX]"))))
    (setf boolx (rulestore-eq store1 store2))
    (assert boolx)

    (setf store2 (rulestore-new (list (rule-from-str "[X1/XX/XX/XX]") (rule-from-str "[Xx/X0/XX/XX]"))))
    (setf boolx (rulestore-eq store1 store2))
    (assert (not boolx))

    (format t "~&  rulestore-eq OK")
  )

  ; Test rulestore-subset-of.
  (let (boolx store1 store2)
    (setf store1 (rulestore-new (list (rule-from-str "[Xx/XX/XX/XX]") (rule-from-str "[X1/XX/XX/XX]"))))
    (setf store2 (rulestore-new (list (rule-from-str "[11/00/11/XX]"))))
    (setf boolx (rulestore-subset-of :sup-store store1 :sub-store store2))
    (assert boolx)

    (setf store1 (rulestore-new (list (rule-from-str "[Xx/XX/XX/XX]") (rule-from-str "[X1/XX/XX/XX]"))))
    (setf store2 (rulestore-new (list (rule-from-str "[10/00/11/XX]"))))
    (setf boolx (rulestore-subset-of :sup-store store1 :sub-store store2))
    (assert boolx)

    (setf store1 (rulestore-new (list (rule-from-str "[Xx/XX/XX/XX]") (rule-from-str "[X1/XX/XX/XX]"))))
    (setf store2 (rulestore-new (list (rule-from-str "[10/00/11/XX]") (rule-from-str "[11/00/11/XX]"))))
    (setf boolx (rulestore-subset-of :sup-store store1 :sub-store store2))
    (assert boolx)

    (setf boolx (rulestore-subset-of :sup-store store2 :sub-store store1))
    (assert (not boolx))

    (format t "~&  rulestore-subset-of OK")
  )

  (format t "~&rulestore-tests done")
  t

)
