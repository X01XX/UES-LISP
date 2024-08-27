
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

;;; Return a new group.
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

; Return true if two groups are equal.
(defun group-eq (grp1 grp2) ; -> bool
  (region-eq (group-region grp1) (group-region grp2))
)

; Return possible steps, given group, rule, from-region, to-region.
(defun group-get-steps (grpx rule-to-goal) ; -> stepstore.
  ;(format t "~&group-get-steps")
  ;(format t "~&group-get-steps group ~A rule ~A" grpx rule-to-goal)
  (assert (group-p grpx))
  (assert (rule-p rule-to-goal))
  (assert (= (group-num-bits grpx) (rule-num-bits rule-to-goal)))

  (let ((ret-steps (stepstore-new nil)) x-not-x rulz w01 w10 msk-change from-reg to-reg
	(wanted-changes (rule-wanted-changes rule-to-goal))
	(unwanted-changes (rule-unwanted-changes rule-to-goal))
	num-wanted num-unwanted
       )

    (setf from-reg (rule-initial-region rule-to-goal))
    (setf to-reg (rule-result-region rule-to-goal))

    ;(format t "~&rules ~A" (rulestore-rules (group-rules grpx)))

    (loop for ruly in (rulestore-rules (group-rules grpx)) do

      (when (or (value-is-not-low (mask-and (rule-b01 ruly) (change-b01 wanted-changes)))
                (value-is-not-low (mask-and (rule-b10 ruly) (change-b10 wanted-changes))))
        ;(format t "~&ruly ~A" ruly)
  
        ;; Where wanted changes are at a position where the rule is X->x,
        ;; remove the unwanted part.
  
        ;; Calc needed masks.
        (setf x-not-x (mask-new-and (rule-b01 ruly) (rule-b10 ruly))) ; Get mask of X->x.
  
        ;; Get wanted change masks.
        (setf w01 (change-b01 wanted-changes)) ; get wanted 0->1s.
        (setf w10 (change-b10 wanted-changes)) ; get wanted 1->0s.
  
        (setf rulz ruly)
  
        ;; Parse wanted 0->1 changes in X->x
        (setf msk-change (mask-new-and w01 x-not-x))
        (if (mask-is-not-low msk-change)
  	  (setf rulz (rule-mask-off-ones rulz msk-change))
        )
  
        ;; Parse wanted 1->0 changes in X->x
        (setf msk-change (mask-new-and w10 x-not-x))
        (if (mask-is-not-low msk-change)
  	  (setf rulz (rule-mask-off-zeros rulz msk-change))
        )
  
        ;; Restrict rule that intersects the from region.
        (cond ((region-intersects from-reg (rule-initial-region rulz)) 
               (let ((new-rule (rule-restrict-initial-region rulz from-reg)) new-rule2)
                 (if (region-intersects to-reg (rule-result-region new-rule)) 
                   (progn
                     (setf new-rule2 (rule-restrict-result-region new-rule to-reg))
		     (setf num-wanted (change-num-changes (rule-intersection-change new-rule2 wanted-changes)))
		     (setf num-unwanted (change-num-changes (rule-intersection-change new-rule2 unwanted-changes)))
                     (if (region-intersects (rule-initial-region new-rule2) from-reg)
                       (stepstore-push ret-steps (step-new :act-id 0 :rule new-rule2 :kind 's :w num-wanted :u num-unwanted))
                       (stepstore-push ret-steps (step-new :act-id 0 :rule new-rule :kind 'f :w num-wanted :u num-unwanted))
                     )
                   )
                   (progn
		     (setf num-wanted (change-num-changes (rule-intersection-change new-rule wanted-changes)))
		     (setf num-unwanted (change-num-changes (rule-intersection-change new-rule unwanted-changes)))
                     (stepstore-push ret-steps (step-new :act-id 0 :rule new-rule :kind 'f :w num-wanted :u num-unwanted))
		   )
                 )
               )
             )
             ((region-intersects (rule-result-region rulz) to-reg) 
                (setf rulz (rule-restrict-result-region rulz to-reg))
		(setf num-wanted (change-num-changes (rule-intersection-change rulz wanted-changes)))
		(setf num-unwanted (change-num-changes (rule-intersection-change rulz unwanted-changes)))
                (stepstore-push ret-steps (step-new :act-id 0 :rule rulz :kind 'b :w num-wanted :u num-unwanted))
             )
             (t 
		(setf num-wanted (change-num-changes (rule-intersection-change rulz wanted-changes)))
		(setf num-unwanted (change-num-changes (rule-intersection-change rulz unwanted-changes)))
	        (stepstore-push ret-steps (step-new :act-id 0 :rule rulz :kind 'a :w num-wanted :u num-unwanted)))
        )
      ) ; end-when
    ) ; end-loop
    ret-steps
  )
)

;;; Return the number of bits used by elements withn a group.
(defun group-num-bits (grpx) ; -> integer ge 0.
  (region-num-bits (group-region grpx))
)
