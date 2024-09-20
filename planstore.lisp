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
(defun _planstore-add-end (storex plnx) ; -> nothing, side-effect planstore changed.
  (assert (planstore-p storex))
  (assert (plan-p plnx))

  (setf (planstore-plan-list storex) (append (planstore-plan-list storex) (list plnx)))
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

  (let ((ret "#S(PLANSTORE ") (start t))

    (loop for plnx in (planstore-plan-list storex) do
      (if start (setf start nil) (setf ret (concatenate 'string ret ", ")))

      (setf ret (concatenate 'string ret (plan-str plnx)))
    )
    (if (zerop (planstore-length storex))
      (setf ret (concatenate 'string ret "NIL)"))
      (setf ret (concatenate 'string ret ")"))
    )
    ret
  )
)

;;; Return the last plan for a given domain ID
(defun planstore-last-domain-plan (storex dom-id) ; -> plan, or nil.
  (assert (planstore-p storex))

  (let (last-plan)
    ;; Find last plan of the same domain id.
    (loop for plny in (planstore-plan-list storex) do
      (if (= (plan-dom-id plny) dom-id)
        (setf last-plan plny))
    )
    last-plan
  )
)

;;; Return last step of all plans in a planstore, for a particular domain.
(defun planstore-last-domain-step (storex dom-id) ; -> step, or nil.
  (let ((last-plan (planstore-last-domain-plan storex dom-id)))
    (if last-plan
      (plan-last-step last-plan)
      nil
    )
  )
)

;;; Add plan to the end of a planstore, check link to a previous plan for the same domain, if any.
(defun planstore-add-end-link (storex plnx) ; -> nothing, side-effect planstore changed.
  (assert (planstore-p storex))
  (assert (plan-p plnx))

  (if (plan-is-empty plnx)
    (return-from planstore-add-end-link))

  (let ((last-plan (planstore-last-domain-plan storex (plan-dom-id plnx))))
    ;; Check link.
    (format t "~&planstore-add-end-link: last plan: ~A" last-plan)
    (if last-plan
      (assert (region-eq (step-result-region (plan-last-step last-plan))
                         (step-initial-region (plan-first-step plnx)))))
  )
  (setf (planstore-plan-list storex) (append (planstore-plan-list storex) (list plnx)))
)



