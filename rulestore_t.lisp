
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
  ;; There are 8 possible bit-position values,  
  ;; Since order does not matter, there are 8 + 7 + 6 + 5 + 4 + 3 + 2 + 1 = 36 combinations.
  (let (ruls1 ruls2 ruls3)
    ;; Test 20 combinations that should work.
    (setf ruls1 (rulestore-from-str                 "[[00/00/00_01/01/01_11/11/11_10/10/10_Xx/Xx/Xx_XX/XX/XX_X0_X1]]"))
    (setf ruls2 (rulestore-from-str                 "[[00/XX/X0_01/Xx/X1_11/XX/X1_10/Xx/X0_Xx/X0/X1_XX/X0/X1_X0_X1]]"))
    (setf ruls3 (rulestore-intersection ruls1 ruls2))
    (assert (rulestore-p ruls3))
    (assert (rulestore-eq ruls3 (rulestore-from-str "[[00/00/00_01/01/01_11/11/11_10/10/10_Xx/10/01_XX/00/11_X0_X1]]")))

    ;; Combinations that should fail intersection, 16.
    ;; For example, Xx (10, 01) has no intersection with XX (00, 11).
    ;; [00/00/00/00/00_01/01/01/01_11/11/11_10/10_Xx_X0]
    ;; [01/11/10/Xx/X1_11/10/XX/X0_10/Xx/X0_XX/X1_XX_X1]

    (setf ruls1 (rulestore-from-str "[[00]]"))
    (setf ruls2 (rulestore-from-str "[[01]]"))
    (setf ruls3 (rulestore-intersection ruls1 ruls2))
    (assert (null ruls3))

    (setf ruls1 (rulestore-from-str "[[00]]"))
    (setf ruls2 (rulestore-from-str "[[11]]"))
    (setf ruls3 (rulestore-intersection ruls1 ruls2))
    (assert (null ruls3))

    (setf ruls1 (rulestore-from-str "[[00]]"))
    (setf ruls2 (rulestore-from-str "[[10]]"))
    (setf ruls3 (rulestore-intersection ruls1 ruls2))
    (assert (null ruls3))

    (setf ruls1 (rulestore-from-str "[[00]]"))
    (setf ruls2 (rulestore-from-str "[[Xx]]"))
    (setf ruls3 (rulestore-intersection ruls1 ruls2))
    (assert (null ruls3))

    (setf ruls1 (rulestore-from-str "[[00]]"))
    (setf ruls2 (rulestore-from-str "[[X1]]"))
    (setf ruls3 (rulestore-intersection ruls1 ruls2))
    (assert (null ruls3))

    (setf ruls1 (rulestore-from-str "[[01]]"))
    (setf ruls2 (rulestore-from-str "[[11]]"))
    (setf ruls3 (rulestore-intersection ruls1 ruls2))
    (assert (null ruls3))

    (setf ruls1 (rulestore-from-str "[[01]]"))
    (setf ruls2 (rulestore-from-str "[[10]]"))
    (setf ruls3 (rulestore-intersection ruls1 ruls2))
    (assert (null ruls3))

    (setf ruls1 (rulestore-from-str "[[01]]"))
    (setf ruls2 (rulestore-from-str "[[XX]]"))
    (setf ruls3 (rulestore-intersection ruls1 ruls2))
    (assert (null ruls3))

    (setf ruls1 (rulestore-from-str "[[01]]"))
    (setf ruls2 (rulestore-from-str "[[X0]]"))
    (setf ruls3 (rulestore-intersection ruls1 ruls2))
    (assert (null ruls3))

    (setf ruls1 (rulestore-from-str "[[11]]"))
    (setf ruls2 (rulestore-from-str "[[10]]"))
    (setf ruls3 (rulestore-intersection ruls1 ruls2))
    (assert (null ruls3))

    (setf ruls1 (rulestore-from-str "[[11]]"))
    (setf ruls2 (rulestore-from-str "[[Xx]]"))
    (setf ruls3 (rulestore-intersection ruls1 ruls2))
    (assert (null ruls3))

    (setf ruls1 (rulestore-from-str "[[11]]"))
    (setf ruls2 (rulestore-from-str "[[X0]]"))
    (setf ruls3 (rulestore-intersection ruls1 ruls2))
    (assert (null ruls3))

    (setf ruls1 (rulestore-from-str "[[10]]"))
    (setf ruls2 (rulestore-from-str "[[XX]]"))
    (setf ruls3 (rulestore-intersection ruls1 ruls2))
    (assert (null ruls3))

    (setf ruls1 (rulestore-from-str "[[10]]"))
    (setf ruls2 (rulestore-from-str "[[X1]]"))
    (setf ruls3 (rulestore-intersection ruls1 ruls2))
    (assert (null ruls3))

    (setf ruls1 (rulestore-from-str "[[Xx]]"))
    (setf ruls2 (rulestore-from-str "[[XX]]"))
    (setf ruls3 (rulestore-intersection ruls1 ruls2))
    (assert (null ruls3))

    (setf ruls1 (rulestore-from-str "[[X0]]"))
    (setf ruls2 (rulestore-from-str "[[X1]]"))
    (setf ruls3 (rulestore-intersection ruls1 ruls2))
    (assert (null ruls3))

    ;; Test two-result rulestore intersections.
    (setf ruls1 (rulestore-from-str "[[11] [10]]"))
    (setf ruls2 (rulestore-from-str "[[X1] [X0]]"))
    (setf ruls3 (rulestore-intersection ruls1 ruls2))
    (assert (rulestore-p ruls3))
    (assert (rulestore-eq ruls3 (rulestore-from-str "[[11] [10]]")))

    (setf ruls1 (rulestore-from-str "[[11] [10]]"))
    (setf ruls2 (rulestore-from-str "[[X0] [X1]]"))
    (setf ruls3 (rulestore-intersection ruls1 ruls2))
    (assert (rulestore-p ruls3))
    (assert (rulestore-eq ruls3 (rulestore-from-str "[[11] [10]]")))

    (format t "~&  rulestore-intersection OK")
  )

  ;; Test rulestore-union.
  ;; There are 8 possible bit-position values,  
  ;; Since order does not matter, there are 8 + 7 + 6 + 5 + 4 + 3 + 2 + 1 = 36 combinations.
  (let (ruls1 ruls2 ruls3)
    ;; Test 20 combinations that should work.
    (setf ruls1 (rulestore-from-str                 "[[00/00/00/00/00_01/01/01/01/01_11/11/11_10/10/10_Xx_XX_X0_X1]]"))
    (setf ruls2 (rulestore-from-str                 "[[00/11/10/XX/X0_01/11/10/Xx/X1_11/XX/X1_10/Xx/X0_Xx_XX_X0_X1]]"))
    (setf ruls3 (rulestore-union ruls1 ruls2))
    (assert (rulestore-p ruls3))
    (assert (rulestore-eq ruls3 (rulestore-from-str "[[00/XX/X0/XX/X0_01/X1/Xx/Xx/X1_11/XX/X1_10/Xx/X0_Xx_XX_X0_X1]]")))

    ;; Combinations that should fail union, 16.
    ;; For example, union Xx (10, 01) and X1 (11, 01), contains more than two items (10, 11, 01). 1X is disallowed, as is 0X.
    ;; [00/00/00_01/01_11/11/11_10/10_Xx/Xx/Xx_XX/XX_X0]
    ;; [01/Xx/X1_XX/X0_10/Xx/X0_XX/X1_XX/X0/X1_X0/X1_X1]

    (setf ruls1 (rulestore-from-str "[[00]]"))
    (setf ruls2 (rulestore-from-str "[[01]]"))
    (setf ruls3 (rulestore-union ruls1 ruls2))
    (assert (null ruls3))

    (setf ruls1 (rulestore-from-str "[[00]]"))
    (setf ruls2 (rulestore-from-str "[[Xx]]"))
    (setf ruls3 (rulestore-union ruls1 ruls2))
    (assert (null ruls3))

    (setf ruls1 (rulestore-from-str "[[00]]"))
    (setf ruls2 (rulestore-from-str "[[X1]]"))
    (setf ruls3 (rulestore-union ruls1 ruls2))
    (assert (null ruls3))

    (setf ruls1 (rulestore-from-str "[[01]]"))
    (setf ruls2 (rulestore-from-str "[[XX]]"))
    (setf ruls3 (rulestore-union ruls1 ruls2))
    (assert (null ruls3))

    (setf ruls1 (rulestore-from-str "[[01]]"))
    (setf ruls2 (rulestore-from-str "[[X0]]"))
    (setf ruls3 (rulestore-union ruls1 ruls2))
    (assert (null ruls3))

    (setf ruls1 (rulestore-from-str "[[11]]"))
    (setf ruls2 (rulestore-from-str "[[10]]"))
    (setf ruls3 (rulestore-union ruls1 ruls2))
    (assert (null ruls3))

    (setf ruls1 (rulestore-from-str "[[11]]"))
    (setf ruls2 (rulestore-from-str "[[Xx]]"))
    (setf ruls3 (rulestore-union ruls1 ruls2))
    (assert (null ruls3))

    (setf ruls1 (rulestore-from-str "[[11]]"))
    (setf ruls2 (rulestore-from-str "[[X0]]"))
    (setf ruls3 (rulestore-union ruls1 ruls2))
    (assert (null ruls3))

    (setf ruls1 (rulestore-from-str "[[10]]"))
    (setf ruls2 (rulestore-from-str "[[XX]]"))
    (setf ruls3 (rulestore-union ruls1 ruls2))
    (assert (null ruls3))

    (setf ruls1 (rulestore-from-str "[[10]]"))
    (setf ruls2 (rulestore-from-str "[[X1]]"))
    (setf ruls3 (rulestore-union ruls1 ruls2))
    (assert (null ruls3))

    (setf ruls1 (rulestore-from-str "[[Xx]]"))
    (setf ruls2 (rulestore-from-str "[[XX]]"))
    (setf ruls3 (rulestore-union ruls1 ruls2))
    (assert (null ruls3))

    (setf ruls1 (rulestore-from-str "[[Xx]]"))
    (setf ruls2 (rulestore-from-str "[[X0]]"))
    (setf ruls3 (rulestore-union ruls1 ruls2))
    (assert (null ruls3))

    (setf ruls1 (rulestore-from-str "[[Xx]]"))
    (setf ruls2 (rulestore-from-str "[[X1]]"))
    (setf ruls3 (rulestore-union ruls1 ruls2))
    (assert (null ruls3))

    (setf ruls1 (rulestore-from-str "[[XX]]"))
    (setf ruls2 (rulestore-from-str "[[X0]]"))
    (setf ruls3 (rulestore-union ruls1 ruls2))
    (assert (null ruls3))

    (setf ruls1 (rulestore-from-str "[[XX]]"))
    (setf ruls2 (rulestore-from-str "[[X1]]"))
    (setf ruls3 (rulestore-union ruls1 ruls2))
    (assert (null ruls3))

    (setf ruls1 (rulestore-from-str "[[X0]]"))
    (setf ruls2 (rulestore-from-str "[[X1]]"))
    (setf ruls3 (rulestore-union ruls1 ruls2))
    (assert (null ruls3))

    ;; Test two-result rulestore intersections.
    (setf ruls1 (rulestore-from-str "[[11] [10]]"))
    (setf ruls2 (rulestore-from-str "[[X1] [X0]]"))
    (setf ruls3 (rulestore-union ruls1 ruls2))
    (assert (rulestore-p ruls3))
    (assert (rulestore-eq ruls3 (rulestore-from-str "[[X1] [X0]]")))

    (setf ruls1 (rulestore-from-str "[[11] [10]]"))
    (setf ruls2 (rulestore-from-str "[[X0] [X1]]"))
    (setf ruls3 (rulestore-union ruls1 ruls2))
    (assert (rulestore-p ruls3))
    (assert (rulestore-eq ruls3 (rulestore-from-str "[[X1] [X0]]")))

    ;; Test the problem when both possible orders work.
    ;; 11 + 01 = X1, 10 + 00 = X0.
    ;; 11 + 00 = XX, 10 + 01 = Xx.
    (setf ruls1 (rulestore-from-str "[[11] [10]]"))
    (setf ruls2 (rulestore-from-str "[[01] [00]]"))
    (setf ruls3 (rulestore-union ruls1 ruls2))
    (assert (null ruls3))

    (format t "~&  rulestore-union OK")
  )

  (format t "~&rulestore-tests done")
  t
)
