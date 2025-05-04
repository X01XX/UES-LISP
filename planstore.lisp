;;;; Implement a store of plans.

(defstruct planstore
  plans  ; A list of zero, or more, plans.
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
  ;; Check argument.
  (assert (plan-list-p plans))

  ;; Construct result.
  (make-planstore :plans plans)
)

;;; Return the number of plans in a planstore.
(defun planstore-length (storex) ; -> number.
  ;; Check argument.
  (assert (planstore-p storex))

  (length (planstore-plans storex))
)

;;; Return true if a planstore is empty.
(defun planstore-is-empty (storex) ; -> bool
  ;; Check argument.
  (assert (planstore-p storex))

  ;; Calc result.
  (zerop (planstore-length storex))
)

;;; Return true if a planstore is not empty.
(defun planstore-is-not-empty (storex) ; -> bool
  ;; Check argument.
  (assert (planstore-p storex))

  ;; Calc result.
  (plusp (planstore-length storex))
)

;;; Return a string representing a planstore.
(defun planstore-str (storex) ; -> string.
  ;; Check argument.
  (assert (planstore-p storex))

  ;; Construct result.
  (let ((ret "#S(PLST ") (start t))

    (loop for plnx in (planstore-plans storex) do
      (if start (setf start nil) (setf ret (concatenate 'string ret " ")))

      (setf ret (concatenate 'string ret (plan-str plnx)))
    )
    (if (zerop (planstore-length storex))
      (setf ret (concatenate 'string ret "NIL)"))
      (setf ret (concatenate 'string ret ")"))
    )
    ;; Return result.
    ret
  )
)

;;; Return true if a planstore is congruent, by plan number bits, with the domain list.
(defun planstore-congruent (planstore1) ; -> bool
  ;; Check argument.
  (assert (planstore-p planstore1))

  ;; Check length.
  (if (/= (planstore-length planstore1) (length *domain-num-bits-list*))
    (return-from planstore-congruent false)) ; Return negative result.

  ;; Check the number bits of each item against the corresponding domain.
  (loop for plnx in (planstore-plans planstore1)
        for numx in *domain-num-bits-list* do

    (if (/= (plan-num-bits plnx) numx)
      (return-from planstore-congruent false)) ; Return negative result.
  )
  ;; Return positive result.
  true 
)

