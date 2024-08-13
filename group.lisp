
(defvar true t)
(defvar false nil)

;;;; Implement the Group type.
;;;;
;;;; The state values of the region will be the same as the state values
;;;; of the squares used to form the group.
;;;;
;;;; In the case of a group with no X positions in its region, the states
;;;; making up the region will be duplicate.
;;;;
;;;; The pn value will be the same for the both squares.
;;;;
;;;; The pnc value will be true when both squares pnc value is true.
;;;;
;;;; The rules will be a union of the rules of the two squares.
;;;;
(defstruct (group (:print-function group-print))
    region    ; Region defined by two* compatible squares.
    rules     ; The combined rule of two* compatible squares.
)
; * Sometimes a group is made of just one square, region state1 == state2.

; Functions automatically created by defstruct:
;
; Most used:
;   (group-<field name> <instance>) returns struct field.
;   (group-p <instance>) -> t
;
; Least used:
;   (type-of <instance>) -> group
;   (typep <instance> 'group) -> t
;
; Don't use:
;   (make-group [:<field-name> <field-value>]*), use group-new instead.
;   (copy-group <instance>) copies a group instance.

;;; Return an group instance
(defun group-new (&key rules)
  (assert (rulestore-p rules))
  (assert (plusp (rulestore-length rules)))
  (assert (< (rulestore-length rules) 3))

  (make-group :region (rulestore-initial-region rules) :rules rules)
)

;;; Print a group.
(defun group-print (instance stream depth)
    ;(assert (zerop depth))
    (format stream (group-str instance)))

;;; Return a string representing a group
(defun group-str (agrp)
    (assert (group-p agrp))

    (let ((str "#S(GROUP "))
        (setf str (concatenate 'string str (format nil "region ~A" (region-str (group-region agrp)))))
        (setf str (concatenate 'string str (format nil " rules ~A" (rulestore-str (group-rules agrp)))))
        (setf str (concatenate 'string str ")"))
        str
    )
)

; Return true if the argument is a list of groups.
(defun group-list-p (groups) ; -> bool

  (if (not (listp groups))
    (return-from group-list-p false))

  ; Check for a non-group.
  (loop for stax in groups do
    (if (not (group-p stax))
      (return-from group-list-p false))
  )
  true
)

; Return true if two groups are equal.
(defun group-eq (grp1 grp2) ; -> bool
  (region-eq (group-region grp1) (group-region grp2))
)

; Return possible steps, given a rule.
(defun group-get-steps (grpx rulx) ; -> stepstore.
  ;(format t "~&group-get-steps")
  ;(format t "~&group-get-steps group ~A rule ~A" grpx rulx)
  (assert (group-p grpx))
  (assert (rule-p rulx))

  (let ((ret-steps (stepstore-new nil)) msk01 msk10 msk-not rulz stpx)

    ;(format t "~&rules ~A" (rulestore-rules (group-rules grpx)))

    (loop for ruly in (rulestore-rules (group-rules grpx)) do

      ;(format t "~&ruly ~A" ruly)

      (setf msk01 (mask-new (mask-and (rule-b01 ruly) (rule-b01 rulx))))
      (setf msk10 (mask-new (mask-and (rule-b10 ruly) (rule-b10 rulx))))

      (when (mask-is-not-low msk01)
        ;(format t "~&found group 01 mask ~A in ~A for rule ~A" msk01 ruly rulx)
	; Mask out 1X bits to isolate b01.
	(setf rulz (rule-mask-off-ones ruly msk01))
	(setf stpx (step-new :id 0 :rule rulz))

	(stepstore-push ret-steps stpx)
      )
      (when (mask-is-not-low msk10)
        ;(format t "~&found group 10 mask ~A in ~A for rule ~A" msk10 ruly rulx)
	; Mask out 0X bits to isolate b10.
	(setf rulz (rule-mask-off-zeros ruly msk10))
	(setf stpx (step-new :id 0 :rule rulz))

	(stepstore-push ret-steps stpx)
      )
    )
    ret-steps
  )
)
