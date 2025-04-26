
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
    (setf boolx (rulestore-subset-of :sup store1 :sub store2))
    (assert boolx)

    (setf store1 (rulestore-new (list (rule-from-str "[Xx/XX/XX/XX]") (rule-from-str "[X1/XX/XX/XX]"))))
    (setf store2 (rulestore-new (list (rule-from-str "[10/00/11/XX]"))))
    (setf boolx (rulestore-subset-of :sup store1 :sub store2))
    (assert boolx)

    (setf store1 (rulestore-new (list (rule-from-str "[Xx/XX/XX/XX]") (rule-from-str "[X1/XX/XX/XX]"))))
    (setf store2 (rulestore-new (list (rule-from-str "[10/00/11/XX]") (rule-from-str "[11/00/11/XX]"))))
    (setf boolx (rulestore-subset-of :sup store1 :sub store2))
    (assert boolx)

    (setf boolx (rulestore-subset-of :sup store2 :sub store1))
    (assert (not boolx))

    (format t "~&  rulestore-subset-of OK")
  )

  ;; Test rulestore-from-str.
  (let (ruls1 ruls2 ruls3)
    (setf ruls1 (rulestore-from-str "[]"))
    (assert (= (rulestore-length ruls1) 0))

    (setf ruls2 (rulestore-from-str "[[01/10]]"))
    (assert (= (rulestore-length ruls2) 1))

    (setf ruls3 (rulestore-from-str "[[10/X0/XX], [11/Xx/XX]]"))
    (assert (= (rulestore-length ruls3) 2))

    (format t "~&  rulestore-from-str OK")
  )

  ;; Test rulestore-intersection.
  (let (ruls1 ruls2 ruls3)
    (setf ruls1 (rulestore-from-str "[[00/00/00_01/01/01_11/11/11_10/10/10_Xx/Xx/Xx_XX/XX/XX]]"))
    (setf ruls2 (rulestore-from-str "[[00/XX/X0_01/Xx/X1_11/XX/X1_10/Xx/X0_Xx/X0/X1_XX/X0/X1]]"))
    (setf ruls3 (rulestore-intersection ruls1 ruls2))
    (assert (rulestore-p ruls3))
    (assert (rulestore-eq ruls3 (rulestore-from-str "[[00/00/00_01/01/01_11/11/11_10/10/10_Xx/10/01_XX/00/11]]")))

    (format t "~&  rulestore-intersection OK")
  )

  (format t "~&rulestore-tests done")
  t
)
