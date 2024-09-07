;;;; Implement the plan struct and functions.

(defvar true t)
(defvar false nil)

;;; The plan struct.
(defstruct (plan (:print-function plan-print))
  dom-id           ; Domain ID.
  stepstore        ; A store of steps to go from one state to another.  May be empty.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (plan-<field name> <instance>) -> struct field.
;   (plan-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> plan
;   (typep <instance> 'plan) -> bool
;
; Probably shouldn't use:
;   (make-plan [:<field-name> <field-plan>]*), use plan-new instead.
;   (copy-plan <instance>) copies a plan instance.

;;; Return a new plan, made up of zero, or more, steps.
(defun plan-new (id steps) ; -> plan.
  (assert (integerp id))
  (assert (>= id 0))
  (assert (step-list-p steps))

  (let ((planx (make-plan :dom-id id :stepstore (stepstore-new steps))))
    (assert (plan-is-valid planx))
    planx
  )
)

;;; Return a list of steps representing a plan.
(defun plan-step-list (planx) ; -> a list of steps.
  (assert (plan-p planx))

  (stepstore-step-list (plan-stepstore planx))
)

;;; Print a plan.
(defun plan-print (instance stream depth)
  ;(assert (zerop depth))
  (format stream (plan-str instance))
)

;;; Return a string representing a plan.
(defun plan-str (planx) ; -> A string.
  (assert (plan-p planx))

  (let ((strs "#S(PLAN ") (at-start true))

    (loop for stepx in (plan-step-list planx) do
      (if at-start
        (setf at-start nil)
        (setf strs (concatenate 'string strs " -> ")))
   
      (setf strs (concatenate 'string strs (step-str stepx)))
    )
    strs
  )
)

;;; Return true if a plan is empty.
(defun plan-is-empty (planx) ; -> bool
  (assert (plan-p planx))

  (stepstore-is-empty (plan-stepstore planx))
)

;;; Return the number of steps in a plan.
(defun plan-length (planx) ; -> an integer.
  (assert (plan-p planx))

  (stepstore-length (plan-stepstore planx))
)

;;; Return the first step of a non-empty plan.
(defun plan-first-step (planx) ; -> step.
  (assert (plan-p planx))

  (stepstore-first-step (plan-stepstore planx))
)

;;; Return the last step of a non-empty plan.
(defun plan-last-step (planx) ; -> step.
  (assert (plan-p planx))

  (stepstore-last-step (plan-stepstore planx))
)

;;; Return true if a plan is valid.
;;; That is, all steps link together.
(defun plan-is-valid (planx) ; -> bool
  (if (< (plan-length planx) 2)
    (return-from plan-is-valid true))

  ;; Check that all steps are linked together.
  (let ((last-step))
    (loop for stepx in (plan-step-list planx) do
        (when last-step
	  (if (region-neq (step-result-region last-step) (step-initial-region stepx))
            (return-from plan-is-valid false))
	)
	(setf last-step stepx)
    )
  )
  ;; Check that plan goes somewhere.
  (if (region-eq (step-initial-region (plan-first-step planx)) (step-result-region (plan-last-step planx)))
    (return-from plan-is-valid false))

  true
)

;; Return true if a list is a list of plans.
;;; An empty list will return true.
(defun plan-list-p (plnlst) ; -> bool
  ;(format t "~&plan-list-p: ~A" plnlst)
  (if (not (listp plnlst))
    (return-from plan-list-p false))

  (loop for plnx in plnlst do
    (if (not (plan-p plnx))
      (return-from plan-list-p false))
  )
  true
)

