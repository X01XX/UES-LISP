;;;; Implement a series of plans, corresponding to a list of domains.
;;;;
;;;; Arranged to implement a path within a selectplans fragment, the plans can
;;;; be run in parallel.

; Implement a store of corresponding plans.
;;; If this is tightly controlled, checking domain congruency of arguments to other functions is unneeded.
;;; Don't use make-planscorr anywhere else.
(defstruct planscorr
  planstore  ; A planstore of zero, or more, plans.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (planscorr-<field name> <instance>) -> struct field.
;   (planscorr-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> planscorr
;   (typep <instance> 'planscorr) -> bool
;
; Probably shouldn't use:
;   (make-planscorr [:<field-name> <field-planscorr>]*), use planscorr-new instead.
;   (copy-planscorr <instance>) copies a planscorr instance.

;;; Return a new planscorr instance, from a list of plans.
(defun planscorr-new (plans) ; -> planscorr, or nil.
  ;(format t "~&planscorr-new: plans ~A" (type-of plans))

  (let (storex)

    (cond ((listp plans)
           (eval (append (list 'and) (mapcar #'(lambda (x) (plan-p x)) plans)))
           (setf storex (planstore-new plans)))
          ((planstore-p plans) (setf storex plans))
          (t (error "unexpected argument")))
    
    (assert (planstore-congruent storex))

    ;; Construct result.
    (make-planscorr :planstore storex)
  )
)

;;; Return a string representing a planscorr.
(defun planscorr-str (plansc) ; -> string.
  (assert (planscorr-p plansc))

  (format nil "PC~A" (planstore-str (planscorr-planstore plansc)))
)

;;; Return a plan list.
(defun planscorr-plan-list (plnsc) ; -> plan list.
  (assert (planscorr-p plnsc))

  (planstore-plans (planscorr-planstore plnsc))
)

;;; Return true if two planscorrs are a sequence, that is
;;; The results of the first planscorr match the initial regions of the second.
(defun planscorr-are-sequence (plnsc1 plnsc2) ; -> bool
  (assert (planscorr-p plnsc1))
  (assert (planscorr-p plnsc2))

  (loop for plnx1 in (planscorr-plan-list plnsc1)
        for plnx2 in (planscorr-plan-list plnsc2) do
	(if (region-ne (plan-result-region plnx1) (plan-initial-region plnx2))
	  (return-from planscorr-are-sequence false))
  )
  true
)

;;; Return a planscorr initial regions.
(defun planscorr-initial-regions (plnscr1) ; -> regionscorr
  (assert (planscorr-p plnscr1))

  (let (regs)
    (loop for plnx in (planscorr-plan-list plnscr1) do
      (push (plan-initial-region plnx) regs)
    )
    (regionscorr-new (regionstore-new (reverse regs)))
  )
)

;;; Return a planscorr result regions.
(defun planscorr-result-regions (plnscr1) ; -> regionscorr
  (assert (planscorr-p plnscr1))

  (let (regs)
    (loop for plnx in (planscorr-plan-list plnscr1) do
      (push (plan-result-region plnx) regs)
    )
    (regionscorr-new (regionstore-new (reverse regs)))
  )
)

;;; Return true if the result regions of a planscorr instance matches the initial regions of another.
(defun planscorr-is-linked-to (plnscr1 plnscr2) ; -> bool
  (assert (planscorr-p plnscr1))
  (assert (planscorr-p plnscr2))

  (regionscorr-eq (planscorr-result-regions plnscr1) (planscorr-initial-regions plnscr2))
)

