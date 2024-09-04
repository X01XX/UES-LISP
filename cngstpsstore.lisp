; Implement a store of change steps, and functions.

(defvar true t)
(defvar false nil)

; Implement a store of cngstpss.
(defstruct cngstpsstore
  cngstps-list  ; A list of zero, or more, non-duplicate, same number bits, change steps.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (cngstpsstore-<field name> <instance>) -> struct field.
;   (cngstpsstore-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> cngstpsstore
;   (typep <instance> 'cngstpsstore) -> bool
;
; Probably shouldn't use:
;   (make-cngstpsstore [:<field-name> <field-cngstpsstore>]*), use cngstpsstore-new instead.
;   (copy-cngstpsstore <instance>) copies a cngstpsstore instance.

;;; Return a new cngstpsstore.
(defun cngstpsstore-new (wanted-changes) ; -> cngstpsstore.
  (assert (change-p wanted-changes))
  (assert (change-is-not-low wanted-changes))

  (let ((ret-store (make-cngstpsstore :cngstps-list nil))
	(changes-each-bit (change-split wanted-changes)))
    (loop for one-bit-change in changes-each-bit do
        (push (cngstps-new one-bit-change) (cngstpsstore-cngstps-list ret-store))
    )
    ret-store
  )
)

; Push a new step to a cngstpsstore.
(defun cngstpsstore-add (storex stpx) ; -> bool, true if added.
  (assert (cngstpsstore-p storex))
  (assert (step-p stpx))

  ; Update existing cngstps, if any.
  (let (ret)
    (loop for cngstpsx in (cngstpsstore-cngstps-list storex) do 
      (when (change-is-not-low (rule-intersection-change (step-rule stpx) (cngstps-change cngstpsx)))
        (if (cngstps-add cngstpsx stpx)
	  (setf ret true)
	)
      )
    )
    ret
  )	
)

; Return the number of cngstpss in a cngstpsstore.
(defun cngstpsstore-length (storex) ; -> number.
  (assert (cngstpsstore-p storex))

  (length (cngstpsstore-cngstps-list storex))
)

; Return true if a cngstpsstore is empty.
(defun cngstpsstore-is-empty (storex) ; -> bool
  (assert (cngstpsstore-p storex))

  (zerop (length (cngstpsstore-cngstps-list storex)))
)

; Return true if a cngstpsstore is not empty.
(defun cngstpsstore-is-not-empty (storex) ; -> bool
  (assert (cngstpsstore-p storex))

  (plusp (length (cngstpsstore-cngstps-list storex)))
)

; Return a string representing a cngstpsstore.
(defun cngstpsstore-str (storex) ; -> string.
  (assert (cngstpsstore-p storex))

  (let ((ret "#S(CNGSTPSSTORE ") (start t))

    (loop for cngstpsx in (cngstpsstore-cngstps-list storex) do
      (if start (setf start nil) (setf ret (concatenate 'string ret ",\n")))    

      (setf ret (concatenate 'string ret (change-str (cngstps-cng cngstpsx))))
      (setf ret (concatenate 'string ret ", "))
      (setf ret (concatenate 'string ret (stepstore-str (cngstps-steps cngstpsx))))
    )
    (setf ret (concatenate 'string ret ")"))
    ret
  )
)

;;; Return the number of steps in a cngstpsstore.
(defun cngstpsstore-num-steps (storex) ; -> integer GE 0.
  (assert (cngstpsstore-p storex))

  (let ((cnt 0))
    (loop for cngstpsx in (cngstpsstore-cngstps-list storex) do
      (setf cnt (+ cnt (cngstps-num-steps cngstpsx)))
    )
    cnt
  )
)
