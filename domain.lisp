
(defvar true t)
(defvar false nil)

;;;; Implement the Action type.
;;;;
(defstruct (domain (:print-function domain-print))
  id		; A number id, GE zero.
  actions	; A actionstore.
  current-state	; The current state of the domain. Actions change this.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (domain-<field name> <instance>) returns struct field.
;   (domain-p <instance>) -> t
;
; Least used:
;   (type-of <instance>) -> domain
;   (typep <instance> 'domain) -> t
;
; Don't use:
;   (make-domain [:<field-name> <field-value>]*), use domain-new instead.
;   (copy-domain <instance>) copies a domain instance.

;;; Return an domain instance
(defun domain-new (&key id actions current-state)
  (assert (actionstore-p actions))
  (assert (>= id ))

  (make-domain :id id :actions actions :current-state current-state)
)

;;; Print a domain.
(defun domain-print (instance stream depth)
    ;(assert (zerop depth))
    (format stream (domain-str instance)))

;;; Return a string representing a domain
(defun domain-str (domx)
    (assert (domain-p domx))

    (let ((str "#S(DOMAIN "))
        (setf str (concatenate 'string str (format nil "id ~D" (domain-id domx))))
        (setf str (concatenate 'string str (format nil " actions ~A" (actionstore-str (domain-actions domx)))))
        (setf str (concatenate 'string str ")"))
        str
    )
)

;;; Return possible steps, given a rule.
(defun domain-get-steps (domx rule-to-goal) ; -> stepstore.
  (assert (domain-p domx))
  (assert (rule-p rule-to-goal))
  (assert (= (domain-num-bits domx) (rule-num-bits rule-to-goal)))

  (actionstore-get-steps (domain-actions domx) rule-to-goal)
)

;;; Return the number of bits used by a domain.
(defun domain-num-bits (domx) ; -> integer, gt zero.
  (assert (domain-p domx))

  (state-num-bits (domain-current-state domx))
)

