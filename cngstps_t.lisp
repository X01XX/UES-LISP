;;; Run tests.
(defun cngstps-tests ()
 (format t "~&cngstps-tests beginning")

 ; Test cngstps-new.
 (let (cngstpsx cngx stp1)
   (setf stp1 (step-new :act-id 0 :rule (rule-from "[01/10/00/01]")))

   (setf cngx (change-new :m01 (mask-from "m0001") :m10 (mask-from "m0000")))

   ; Test creating a cngstps.
   (setf cngstpsx (cngstps-new cngx))
   (cngstps-add cngstpsx stp1)

   (assert (= 1 (cngstps-num-steps cngstpsx)))

   ;(format t "~&cngstps ~A" cngstpsx)

   (format t "~&  cngstps-new OK")
 )

 (format t "~&cngstps-tests done")
  t
)
