;;;; Run rulescorr tests.
(defun rulescorr-tests ()
  (format t "~&rulescorr-tests beginning")

  ;; Test rulescorr-new
  (let (rulsc1)
    (setf rulsc1 (rulescorr-new (list (rule-from "[01]") (rule-from "[01/10]"))))
    (assert (rulescorr-p rulsc1))
    (assert (= (rulescorr-length rulsc1) 2))
    (assert (rule-eq (rulescorr-first-rule rulsc1) (rule-from "[01]")))
    (assert (rule-eq (rulescorr-last-rule  rulsc1) (rule-from "[01/10]")))

    (format t "~&  rulescorr-new OK")
  )

  ;; Test rulescorr-eq
  (let (rulsc1 rulsc2 rulsc3)
    (setf rulsc1 (rulescorr-new (list (rule-from "[01]") (rule-from "[01/10]"))))
    (setf rulsc2 (rulescorr-new (list (rule-from "[01]") (rule-from "[01/10]"))))
    (setf rulsc3 (rulescorr-new (list (rule-from "[01]") (rule-from "[11/10]"))))

    (assert (rulescorr-eq rulsc1 rulsc2))
    (assert (not (rulescorr-eq rulsc1 rulsc3)))

    (format t "~&  rulescorr-eq OK")
  )

  ;; Test rulescorr-new-regionscorr-to-regionscorr.
  (let (rulsc1 regionscorr1 regionscorr2)
    
    (setf regionscorr1 (regionscorr-new (list (region-from "000111") (region-from "XXX"))))
    (setf regionscorr2 (regionscorr-new (list (region-from "01X01X") (region-from "01X"))))
    (setf rulsc1 (rulescorr-new-regionscorr-to-regionscorr regionscorr1 regionscorr2))
    ;(format t "~&rulsc1 ~A" rulsc1)
    (assert (= (rulescorr-length rulsc1) 2))
    (assert (rule-eq (rulescorr-first-rule rulsc1) (rule-from "[00/01/00/10/11/11]")))
    (assert (rule-eq (rulescorr-last-rule rulsc1)  (rule-from "[X0/X1/XX]")))
  )

  (format t "~&rulescorr-tests done")
)
