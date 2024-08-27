;;; Run tests.
(defun cngstps-tests ()
 (format t "~&cngstps-tests beginning")

 ; Test cngstps-new.
 (let (cngstpsx cngx stp1)
   (setf stp1 (step-new :act-id 0 :rule (rule-from-str "[01/10/00/01]") :kind 'a :w 1 :u 0))

   (setf cngx (change-new :b01 (mask-from-str "#b0001") :b10 (mask-from-str "#b0000")))

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
