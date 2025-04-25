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

    (cond ((listp plans) (setf storex (planstore-new plans)))
          ((planstore-p plans) (setf storex plans))
          (t (error "unexpected argument")))
    
    (assert (planstore-congruent storex))

    ;; Construct result.
    (make-planscorr :planstore storex)
  )
)

;;; Chegk if use of act 0 steps is valid.
(defun planscorr-act0-steps-valid (plansc) ; -> bool
  (assert (planscorr-p plansc))

  (loop for plnx in (planscorr-plan-list plansc) do
    (if (not (plan-act0-steps-valid plnx))
      (return-from planscorr-act0-steps-valid false))
  )
  true
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

;;; Return true if two planscorrs can be linked into a sequence, that is
;;; The results of the first planscorr intersect the initial regions of the second.
(defun planscorr-can-be-linked (plnsc1 plnsc2) ; -> bool
  (assert (planscorr-p plnsc1))
  (assert (planscorr-p plnsc2))

  (loop for plnx1 in (planscorr-plan-list plnsc1)
        for plnx2 in (planscorr-plan-list plnsc2) do
	(if (not (region-intersects (plan-result-region plnx1) (plan-initial-region plnx2)))
	  (return-from planscorr-can-be-linked false))
  )
  true
)

;;; Return a list of two planscorrs, restricting the plans by the result regions
;;; of the first and the initial regions of the second.
(defun planscorr-link (plnsc1 plnsc2) ; -> (planscorr1', planscorr2'), nil.
  (assert (planscorr-p plnsc1))
  (assert (planscorr-p plnsc2))

  (let (plans-list1 plans-list2 pln1 pln2)

    (loop for plnx1 in (planscorr-plan-list plnsc1)
          for plnx2 in (planscorr-plan-list plnsc2) do

	(if (not (region-intersects (plan-result-region plnx1) (plan-initial-region plnx2)))
	  (return-from planscorr-link nil))

	(setf pln1 (plan-restrict-result-region  plnx1 (plan-initial-region plnx2)))
	(setf pln2 (plan-restrict-initial-region plnx2 (plan-result-region plnx1)))

	(if (or (null pln1) (null pln2))
	  (return-from planscorr-link nil))

	(setf plans-list1 (append plans-list1 (list pln1)))
	(setf plans-list2 (append plans-list2 (list pln2)))
    )
    (list (planscorr-new plans-list1) (planscorr-new plans-list2))
  )
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

;;; Return true if a list is a list of planscorr.
;;; An empty list will return true.
(defun planscorr-list-p (plnlst) ; -> bool
  ;(format t "~&planscorr-list-p: ~A" plnlst)
  (if (not (listp plnlst))
    (return-from planscorr-list-p false))

  (loop for plnx in plnlst do
    (if (not (planscorr-p plnx))
      (return-from planscorr-list-p false))
  )
  true
)

;;; Return true if the result regions of a planscorr instance matches the initial regions of another.
(defun planscorr-is-linked-to (plnscr1 plnscr2) ; -> bool
  (assert (planscorr-p plnscr1))
  (assert (planscorr-p plnscr2))

  (regionscorr-eq (planscorr-result-regions plnscr1) (planscorr-initial-regions plnscr2))
)

