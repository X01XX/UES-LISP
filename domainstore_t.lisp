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

    (domainstore-add-selectregions domstr
	(selectregions-new (regionscorr-new (list (region-from-str "0111"))) 0 -1))

    (domainstore-add-selectregions domstr
	(selectregions-new (regionscorr-new (list (region-from-str "1101"))) 0 -1))


    (domainstore-calc-select domstr)

    (format t "~&non-negative ~A" (domainstore-non-negative-regionscorrstore domstr))

    (domainstore-get-plans domstr (regionscorr-new (list (region-from-str "0101")))
                                  (regionscorr-new (list (region-from-str "1111"))))

    ;(format t "~&  domainstore-generate-plan OK")
  )

  (format t "~&domainstore-tests done")
  t
)
