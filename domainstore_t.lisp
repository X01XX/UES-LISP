;;; Run tests.
(defun domainstore-tests ()
  (format t "~&domainstore-tests beginning")

  ; Test domainstore-new.
  (let (domstr)
    (setf domstr (domainstore-new nil))
    (assert (domainstore-p domstr))

    (format t "~&  domainstore-new OK")
  )

  ; Test generating a plan.
  (let (domstr)
    (setf domstr (domainstore-new nil))

    (domainstore-push domstr
      (domain-new
	:id 0
	:actions
          (actionstore-new (list
            (action-new :id 0 :groups
              (groupstore-new (list (group-new :rules
                (rulestore-new (list (rule-from-str "[XX/XX/XX/Xx]")))))))
            (action-new :id 1 :groups
              (groupstore-new (list (group-new :rules
                (rulestore-new (list (rule-from-str "[XX/XX/Xx/XX]")))))))
            (action-new :id 2 :groups
              (groupstore-new (list (group-new :rules
                (rulestore-new (list (rule-from-str "[XX/Xx/XX/XX]")))))))
            (action-new :id 3 :groups
              (groupstore-new (list (group-new :rules
                (rulestore-new (list (rule-from-str "[Xx/XX/XX/XX]")))))))
             )
           )
         :current-state (state-from-str "#b0101")
      )
    )

    (domainstore-push domstr
      (domain-new
	:id 1
	:actions
          (actionstore-new (list
            (action-new :id 0 :groups
              (groupstore-new (list (group-new :rules
                (rulestore-new (list (rule-from-str "[00/XX/XX/XX/Xx]")))))))
            (action-new :id 1 :groups
              (groupstore-new (list (group-new :rules
                (rulestore-new (list (rule-from-str "[00/XX/XX/Xx/XX]")))))))
            (action-new :id 2 :groups
              (groupstore-new (list (group-new :rules
                (rulestore-new (list (rule-from-str "[00/XX/Xx/XX/XX]")))))))
            (action-new :id 3 :groups
              (groupstore-new (list (group-new :rules
                (rulestore-new (list (rule-from-str "[00/Xx/XX/XX/XX]")))))))
             )
           )
         :current-state (state-from-str "#b01010")
      )
    )

    (domainstore-add-selectregions domstr
	(selectregions-new (regionscorr-new (list (region-from-str "00X00") (region-from-str "0X00"))) 2 -3))

    (domainstore-add-selectregions domstr
	(selectregions-new (regionscorr-new (list (region-from-str "000X0") (region-from-str "00X0"))) 5 -7))

    (domainstore-add-selectregions domstr
	(selectregions-new (regionscorr-new (list (region-from-str "0010X") (region-from-str "010X"))) 11 -13))

    (domainstore-calc-select domstr)
    (assert (= (selectregionsstore-length (domainstore-selectregionsstore-fragments domstr)) 8))

    (assert (selectregionsstore-contains (domainstore-selectregionsstore-fragments domstr)
					 (selectregions-new (regionscorr-new (list (region-from-str "00100") (region-from-str "0100"))) 13 -16)))
 
    (assert (selectregionsstore-contains (domainstore-selectregionsstore-fragments domstr)
					 (selectregions-new (regionscorr-new (list (region-from-str "00000") (region-from-str "0000"))) 7 -10)))

    ;(format t "~&  domainstore-generate-plan OK")
  )

  (format t "~&domainstore-tests done")
  t
)
