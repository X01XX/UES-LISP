
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

  (let ((ret (group-new-na :rules rules)))
    (cond ((err-p ret) (error (err-str ret)))
          ((group-p ret) ret)
           (t (error "Result is not a group"))))
)
;;; group-new no abort (na).
(defun group-new-na (&key rules) ; -> group or err.
  (when (= (rulestore-length rules) 2)
    (if (region-neq (rule-initial-region (rulestore-first rules))
		    (rule-initial-region (rulestore-second rules)))
      (return-from group-new-na (err-new "Rulestore initial regions do not match")))
  )
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
(defun group-get-steps (grpx rulx from-reg to-reg) ; -> stepstore.
  ;(format t "~&group-get-steps")
  ;(format t "~&group-get-steps group ~A rule ~A" grpx rulx)
  (assert (group-p grpx))
  (assert (rule-p rulx))
  (assert (not (region-intersects from-reg to-reg)))

  (let ((ret-steps (stepstore-new nil)) msk01 msk10 rul-found x-not-x rulz to-zeros to-ones stpz)

    ;(format t "~&rules ~A" (rulestore-rules (group-rules grpx)))

    (loop for ruly in (rulestore-rules (group-rules grpx)) do

      ;(format t "~&ruly ~A" ruly)

      (setf rul-found false)

      ;; Restrict rule that intersects the from region.
      (when (region-intersects from-reg (rule-initial-region ruly)) 
        (setf rul-found true)
	(setf stpz (step-new :act-id 0 :rule (rule-restrict-initial-region ruly from-reg)))
        (stepstore-push ret-steps stpz)
      )

      ;; Restrict rule that intersects the to region.
      (when (region-intersects to-reg (rule-result-region ruly)) 
        (setf rul-found true)
        (setf stpz (step-new :act-id 0 :rule (rule-restrict-result-region ruly to-reg)))
	(if (not (stepstore-contains ret-steps stpz))
          (stepstore-push ret-steps stpz)
	)
      )

      (when (not rul-found)
	;; Where wanted changes are at a position where the rule is X->x,
	;; remove the unneeded part.
	;; Leave X->0, X->1 alone.
        (setf msk01 (mask-new (mask-and (rule-b01 ruly) (rule-b01 rulx))))
        (setf msk10 (mask-new (mask-and (rule-b10 ruly) (rule-b10 rulx))))

	(setf x-not-x (mask-new (mask-and (rule-b01 ruly) (rule-b10 ruly)))) ; Get mask of X->x.

	(setf rulz ruly)

	(when (mask-is-not-low msk01)
	  (setf to-zeros (mask-new (mask-and x-not-x msk01)))
	  (if (mask-is-not-low to-zeros)
	    (setf rulz (rule-mask-off-ones rulz to-zeros))) ; Change Xx to 0->1.
	)

	(when (mask-is-not-low msk10)
	  (setf to-ones (mask-new (mask-and x-not-x msk10)))
	  (if (mask-is-not-low to-ones)
	    (setf rulz (rule-mask-off-zeros rulz to-ones))) ; Change Xx to 1->0.
	)
        (setf stpz (step-new :act-id 0 :rule rulz))
        (stepstore-push ret-steps stpz)
      )
    ) ; end-loop
    ret-steps
  )
)
