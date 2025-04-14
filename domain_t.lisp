;; Run domain tests.
(defun domain-tests ()
  (format t "~&domain-tests beginning")

  ;; Test domain-new
  (let (domx) 
    (setf domx (domain-new :id 0 :initial-state (state-from 's0010)))
    (assert (domain-p domx))
    (assert (= 0 (domain-id domx)))
    (assert (state-eq (domain-current-state domx) (state-from 's0010)))
    (assert (= 1 (actionstore-length (domain-actions domx))))
    (format t "~&  domain-new OK")
  )

  ;; Test domain-set-id
  (let (domx) 
    (setf domx (domain-new :id 0 :initial-state (state-from 's0010)))
    (domain-set-id domx 2)
    (assert (= 2 (domain-id domx)))
    (format t "~&  domain-set-id OK")
  )

  ;; Test domain-set-state
  (let (domx)
    (setf domx (domain-new :id 0 :initial-state (state-from 's0010)))
    (domain-set-state domx (state-from 's1000))
    (assert (state-eq (domain-current-state domx) (state-from 's1000)))

    (format t "~&  domain-set-state OK")
  )

  ;; Test domain get-plan for an Xx position in a rule.
  ;; This is needed for a step alt-plan, the reverse of Xx in the wanted rule, which is the same Xx,
  ;; that can survive restriction by initial, or result, region.
  (let (domx act1 plan)
    (setf domx (domain-from '(DOM (ACT ("[XX/XX/XX/Xx]")))))

    (setf act1 (actionstore-nth (domain-actions domx) 1))
    (action-take-sample-arbitrary act1 (state-from 's0000))
    (action-take-sample-arbitrary act1 (state-from 's1111))

    (setf plan (domain-get-plan domx (rule-from-str "[XX/XX/XX/Xx]") (region-from 'rXXXX)))
    (if plan
      (progn
        (format t "~&  plan: ~A" (plan-str plan))
        (format t "~&  domain-get-plan1 OK")
      )
      (format t "~&  domain-get-plan1 Failed")
    )
  )

  (format t "~&domain-tests done")
)
