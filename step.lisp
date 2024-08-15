
(defvar true t)
(defvar false nil)

;;;; Implement the Step type.
;;;;
(defstruct (step (:print-function step-print))
  act-id	; An stp2ion ID, GE zero.
  rule		; A rule.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (step-<field name> <instance>) returns struct field.
;   (step-p <instance>) -> t
;
; Least used:
;   (type-of <instance>) -> step
;   (typep <instance> 'step) -> t
;
; Don't use:
;   (make-step [:<field-name> <field-value>]*), use step-new instead.
;   (copy-step <instance>) copies a step instance.

;;; Return an step instance
(defun step-new (&key act-id rule)
  (assert (rule-p rule))
  (assert (>= act-id 0))

  (make-step :act-id act-id :rule rule)
)

;;; Print a step.
(defun step-print (instance stream depth)
    (format stream (step-str instance))
)

;;; Return a string representing a step
(defun step-str (stpx)
    (assert (step-p stpx))

    (let ((str "#S(STEP "))
        (setf str (concatenate 'string str (format nil "act-id ~D" (step-act-id stpx))))
        (setf str (concatenate 'string str (format nil " rule ~A" (rule-str (step-rule stpx)))))
        (setf str (concatenate 'string str ")"))
        str
    )
)

; Return true if the argument is a list of steps.
(defun step-list-p (steps) ; -> bool

  (if (not (listp steps))
    (return-from step-list-p false))

  ; Check for a non-step.
  (loop for stpx in steps do
    (if (not (step-p stpx))
      (return-from step-list-p false))
  )
  true
)

(defun step-eq (stp1 stp2) ; -> bool
  (assert (step-p stp1))
  (assert (step-p stp2))

  (and (= (step-act-id stp1) (step-act-id stp2))
       (rule-eq (step-rule stp1) (step-rule stp2)))
)

