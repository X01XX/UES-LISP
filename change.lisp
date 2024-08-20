;;;; Implement a change struct and functions.

;;; The change struct.
(defstruct (change (:print-function change-print))

  b01  ; 0->1 mask.
  b10  ; 1->0 mask.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (change-<field name> <instance>) -> struct field.
;   (change-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> change
;   (typep <instance> 'change) -> bool
;
; Probably shouldn't use:
;   (make-change [:<field-name> <field-change>]*), use change-new instead.
;   (copy-change <instance>) copies a change instance.
(defun change-new (&key b01 b10) ; -> change instance.
  (assert (mask-p b01))
  (assert (mask-p b10))
  (assert (= (mask-num-bits b01) (mask-num-bits b10)))

  (make-change :b01 b01 :b10 b10)
)

;;; Print a change.
(defun change-print (instance stream depth)
  ;(assert (zerop depth))
  (format stream (change-str instance))
)

;;; Return a string for a change.
(defun change-str (cng) ; -> string
  (let ((str "(0->1 "))
    (setf str (concatenate 'string str (value-str (mask-value (change-b01 cng)))))
    (setf str (concatenate 'string str ", 1->0 "))
    (setf str (concatenate 'string str (value-str (mask-value (change-b10 cng)))))
    (setf str (concatenate 'string str ")"))
    str
  )
)
