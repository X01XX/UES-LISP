;; Run group tests.
(defun group-tests ()
  (format t "~&group-tests beginning")

  ; Test group-new
  (let (grp0)
    (setf grp0 (group-new (region-from 'rxxxx) (pn-new *pn-one*) (rulestore-new (list (rule-from-str "[Xx/XX/XX/XX]")))))
    (assert (group-p grp0))

    (setf grp0 (group-new (region-from 'rxxx0) (pn-new *pn-two*)
          (rulestore-new (list (rule-from-str "[XX/Xx/XX/01]") (rule-from-str "[XX/Xx/XX/00]")))))
    (assert (group-p grp0))

    (setf grp0 (group-new (region-from 'rxxx0) (pn-new *pn-none*) (rulestore-new nil)))
    (assert (group-p grp0))

    (format t "~&  group-new OK")
  )

  (format t "~&group-tests done")
)
