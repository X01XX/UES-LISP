;;;; Implement a series of plans, corresponding to a list of domains.
;;;;
;;;; Arranged to implement a path within a selectregions fragment, the plans can
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

