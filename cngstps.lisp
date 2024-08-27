;;;; Implement the change-steps struct, and functions.
;;;; It represents steps that produce a specifed one-bit change, 
;;;; possibly with other changes.

;;; The cngstps struct.
(defstruct (cngstps (:print-function cngstps-print))
  change   ; A change, with a one-bit change.
  steps    ; A stepstore of steps that accomplish at least that change.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (cngstps-<field name> <instance>) -> struct field.
;   (cngstps-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> cngstps
;   (typep <instance> 'cngstps) -> bool
;
; Probably shouldn't use:
;   (make-cngstps [:<field-name> <field-cngstps>]*), use cngstps-new instead.
;   (copy-cngstps <instance>) copies a cngstps instance.

;;; Return a new cngstps, given a change and step.
(defun cngstps-new (cngx) ; -> cngstps.
  (assert (change-p cngx))
  (assert (= (change-num-changes cngx) 1))

  (make-cngstps :change cngx :steps (stepstore-new nil))
)

;;; Push a step into a stepstore.
(defun cngstps-add (cngstpsx stepx) ; -> bool, true if added.
  ;(format t "~&cngstps-push cngstpsx ~A stepx ~A" cngstpsx stepx)
  (assert (cngstps-p cngstpsx))
  (assert (step-p stepx))
  (assert (= (cngstps-num-bits cngstpsx) (step-num-bits stepx)))
  
  (stepstore-push (cngstps-steps cngstpsx) stepx)
)

; Return a string to represent a cngstps.
(defun cngstps-str (cngstpsx) ; -> string.
  (assert (cngstps-p cngstpsx))

  (let ((str "#S(CNGSTPS "))
    (setf str (concatenate 'string str (change-str (cngstps-change cngstpsx))))
    (setf str (concatenate 'string str ", "))
    (setf str (concatenate 'string str (stepstore-str (cngstps-steps cngstpsx))))
    (setf str (concatenate 'string str ")"))
    str
  )
)

; Print a cngstps.
(defun cngstps-print (instance stream depth)
  ;(assert (zerop depth))
  (format stream (cngstps-str instance))
)

;;; Return the number of bits used is elements of a cngstps.
(defun cngstps-num-bits (cngstpsx) ; -> integer GT 0.
  (change-num-bits (cngstps-change cngstpsx))
)

;;; Return the number of steps in a cngstps.
(defun cngstps-num-steps (cngstpsx) ; -> integer GE 1.
  (stepstore-length (cngstps-steps cngstpsx))
)

