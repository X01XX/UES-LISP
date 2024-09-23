;;;; Implement a series of plans, corresponding to a list of domains.
;;;;
;;;; Arranged to implement a path within a selectplans fragment, the plans can
;;;; be run in parallel.

(defvar true t)
(defvar false nil)

; Implement a store of corresponding plans.
(defstruct (planscorr (:print-function planscorr-print))
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
  ;(format t "~&planscorr-new: plans ~A" plans)
  (assert (plan-list-p plans))

  (make-planscorr :planstore (planstore-new plans))
)

;;; Print a planscorr.
(defun planscorr-print (instance stream depth)
  ;(assert (zerop depth))
  (format stream (planscorr-str instance))
)

;;; Return a string representing a planscorr.
(defun planscorr-str (plansc) ; -> string.
  (assert (planscorr-p plansc))

  (format nil "#S(PLANSCORR ~A)" (planscorr-planstore plansc))
)

;;; Return the number of plans in a planscorr.
(defun planscorr-length (plnsc) ; -> integer ge 0.
  (assert (planscorr-p plnsc))

  (planstore-length (planscorr-planstore plnsc))
)

;;; Return a plan list.
(defun planscorr-plan-list (plnsc) ; -> plan list.
  (assert (planscorr-p plnsc))

  (planstore-plan-list (planscorr-planstore plnsc))
)

;;; Return true if two plans are congruent.
(defun planscorr-congruent (plnsc1 plnsc2) ; -> bool
  (assert (planscorr-p plnsc1))
  (assert (planscorr-p plnsc2))

  (if (/= (planscorr-length plnsc1) (planscorr-length plnsc2))
    (return-from planscorr-congruent false))

  (loop for plnx1 in (planscorr-plan-list plnsc1)
        for plnx2 in (planscorr-plan-list plnsc2) do

	(if (/= (plan-dom-id plnx1) (plan-dom-id plnx2))
	  (return-from planscorr-congruent false))
  )
  true
)

;;; Return true if two planscorrs are a sequence, that is 
;;; The results of the first planscorr match the initial regions of the second.
(defun planscorr-are-sequence (plnsc1 plnsc2) ; -> bool
  (assert (planscorr-p plnsc1))
  (assert (planscorr-p plnsc2))
  (assert (planscorr-congruent plnsc1 plnsc2))

  (loop for plnx1 in (planscorr-plan-list plnsc1)
        for plnx2 in (planscorr-plan-list plnsc2) do
	(if (region-neq (plan-result-region plnx1) (plan-initial-region plnx2))
	  (return-from planscorr-are-sequence false))
  )
  true
)

;;; Return true if two planscorrs can be linked into a sequence, that is 
;;; The results of the first planscorr intersect the initial regions of the second.
(defun planscorr-can-be-linked (plnsc1 plnsc2) ; -> bool
  (assert (planscorr-p plnsc1))
  (assert (planscorr-p plnsc2))
  (assert (planscorr-congruent plnsc1 plnsc2))

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
  (assert (planscorr-congruent plnsc1 plnsc2))

  (let (plans-list1 plans-list2)

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

