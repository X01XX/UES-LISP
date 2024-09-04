; Implement a store of plans.

(defvar true t)
(defvar false nil)

; Implement a store of plans.
(defstruct (planstore (:print-function planstore-print))
  plan-list  ; A list of zero, or more, plans.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (planstore-<field name> <instance>) -> struct field.
;   (planstore-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> planstore
;   (typep <instance> 'planstore) -> bool
;
; Probably shouldn't use:
;   (make-planstore [:<field-name> <field-planstore>]*), use planstore-new instead.
;   (copy-planstore <instance>) copies a planstore instance.

;;; Return a new planstore instance, from a list of plans.
(defun planstore-new (plans) ; -> planstore.
  ;(format t "~&plans ~A" plans)
  (assert (plan-list-p plans))

  (make-planstore :plan-list plans)
)

;;; Print a planstore.
(defun planstore-print (instance stream depth)
  ;(assert (zerop depth))
  (format stream (planstore-str instance))
)
;;; Add plan to the end of a planstore.
(defun planstore-add-end (storex regx) ; -> nothing, side-effect planstore changed.
  (assert (planstore-p storex))
  (assert (plan-p regx))

  (setf (planstore-plan-list storex) (append (planstore-plan-list storex) (list regx)))
)

;;; Return the number of plans in a planstore.
(defun planstore-length (storex) ; -> number.
  (assert (planstore-p storex))

  (length (planstore-plan-list storex))
)

;;; Return true if a planstore is empty.
(defun planstore-is-empty (storex) ; -> bool
  (assert (planstore-p storex))

  (zerop (planstore-length storex))
)

;;; Return true if a planstore is not empty.
(defun planstore-is-not-empty (storex) ; -> bool
  (assert (planstore-p storex))

  (plusp (planstore-length storex))
)

;;; Return a string representing a planstore.
(defun planstore-str (storex) ; -> string.
  (assert (planstore-p storex))

  (let ((ret "#S(planSTORE ") (start t))

    (loop for regx in (planstore-plan-list storex) do
      (if start (setf start nil) (setf ret (concatenate 'string ret ", ")))

      (setf ret (concatenate 'string ret (plan-str regx)))
    )
    (if (zerop (planstore-length storex))
      (setf ret (concatenate 'string ret "NIL)"))
      (setf ret (concatenate 'string ret ")"))
    )
    ret
  )
)



