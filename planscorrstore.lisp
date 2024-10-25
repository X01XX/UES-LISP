
;;;; Implement a store of planscorr.
;;;;
;;;; From first to last, each planscorr result regions should equal the next
;;;; planscorr initial regions.

(defvar true t)
(defvar false nil)

; Implement a store of plans.
(defstruct (planscorrstore (:print-function planscorrstore-print))
  planscorr-list 	; A list of zero, or more, planscorr.
  value		; A value representing select regions the plans pass through.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (planscorrstore-<field name> <instance>) -> struct field.
;   (planscorrstore-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> planscorrstore
;   (typep <instance> 'planscorrstore) -> bool
;
; Probably shouldn't use:
;   (make-planscorrstore [:<field-name> <field-planscorrstore>]*), use planscorrstore-new instead.
;   (copy-planscorrstore <instance>) copies a planscorrstore instance.

;;; Return a new planscorrstore instance, from a list of planscorr.
;;; Default value to 0.
(defun planscorrstore-new (planscorr-list) ; -> planscorrstore.
  ;(format t "~&planscorrstore ~A" planscorr-list)
  (assert (planscorr-list-p planscorr-list))

  (let (ret)
    (setf ret (make-planscorrstore :planscorr-list planscorr-list :value 0))
    (assert (planscorrstore-is-valid ret))
    ret
  )	
)

;;; Seh the value of a planscorrstore.
(defun planscorrstore-set-value (storex val) ; -> nothing, side-effect planscorrstore-value changed.
  (setf (planscorrstore-value storex) val)
)

;;; Print a planscorrstore.
(defun planscorrstore-print (instance stream depth)
  ;(assert (zerop depth))
  (format stream (planscorrstore-str instance))
)

;;; Add planscorr to the end of a planscorrstore.
(defun planscorrstore-add-end (storex plnx) ; -> nothing, side-effect planscorrstore changed.
  (assert (planscorrstore-p storex))
  (assert (plan-p plnx))

  (setf (planscorrstore-planscorr-list storex) (append (planscorrstore-planscorr-list storex) (list plnx)))
  (assert (planscorrstore-is-valid storex))
)

;;; Return the number of plans in a planscorrstore.
(defun planscorrstore-length (storex) ; -> number.
  (assert (planscorrstore-p storex))

  (length (planscorrstore-planscorr-list storex))
)

;;; Return true if a planscorrstore is empty.
(defun planscorrstore-is-empty (storex) ; -> bool
  (assert (planscorrstore-p storex))

  (zerop (planscorrstore-length storex))
)

;;; Return true if a planscorrstore is not empty.
(defun planscorrstore-is-not-empty (storex) ; -> bool
  (assert (planscorrstore-p storex))

  (plusp (planscorrstore-length storex))
)

;;; Return a string representing a planscorrstore.
(defun planscorrstore-str (storex) ; -> string.
  (assert (planscorrstore-p storex))

  (let ((ret "#S(PLANSCORRSTORE ") (start t))

    (loop for plnx in (planscorrstore-planscorr-list storex) do
      (if start (setf start nil) (setf ret (concatenate 'string ret ", ")))

      (setf ret (concatenate 'string ret (planscorr-str plnx)))
    )
    (if (zerop (planscorrstore-length storex))
      (setf ret (concatenate 'string ret "NIL)"))
      (setf ret (concatenate 'string ret ")"))
    )
    ret
  )
)

;;; Check that planscorr items are linked.
(defun planscorrstore-is-valid (storex) ; -> bool
  (loop for plnx in (planscorrstore-planscorr-list storex)
        for plny in (cdr (planscorrstore-planscorr-list storex)) do
    (if (not (planscorr_is_linked_to(plnx plny)))
      (return-from planscorrstore-is-valid false))
  )
  true
)

