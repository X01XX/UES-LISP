
(defvar true t)
(defvar false nil)

;;;; Implement the Action type.
;;;;
(defstruct (domain (:print-function domain-print))
  id		; A number id, GE zero.
  actions	; A actionstore.
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
(defun domain-new (&key id actions)
  (assert (actionstore-p actions))
  (assert (>= id ))

  (make-domain :id id :actions actions)
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

; Return possible steps, given a rule.
(defun domain-get-steps (domx rule) ; -> stepstore.
  (assert (domain-p domx))
  (assert (rule-p rule))

  (actionstore-get-steps (domain-actions domx) rule)
)


