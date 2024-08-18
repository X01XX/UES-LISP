
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

; Return true if two groups are equal.
(defun group-eq (grp1 grp2) ; -> bool
  (region-eq (group-region grp1) (group-region grp2))
)

; Return possible steps, given group, rule, from-region, to-region.
(defun group-get-steps (grpx rulx) ; -> stepstore.
  ;(format t "~&group-get-steps")
  ;(format t "~&group-get-steps group ~A rule ~A" grpx rulx)
  (assert (group-p grpx))
  (assert (rule-p rulx))
  (assert (= (group-num-bits grpx) (rule-num-bits rulx)))

  (let ((ret-steps (stepstore-new nil)) x-not-x rulz x-0 x-1 w01 w10 u01 u10 msk-change from-reg to-reg)

    (setf from-reg (rule-initial-region rulx))
    (setf to-reg (rule-result-region rulx))

    ;(format t "~&rules ~A" (rulestore-rules (group-rules grpx)))

    (loop for ruly in (rulestore-rules (group-rules grpx)) do

      ;(format t "~&ruly ~A" ruly)

      ;; Where wanted changes are at a position where the rule is X->x, X->0, X->1,
      ;; remove the unneeded part.
      ;;
      ;; Where unwanted changes are at a position where the rule is X->0, X->1,
      ;; remove the unwanted part.

      ;; Calc needed masks.
      (setf x-not-x (mask-new (mask-and (rule-b01 ruly) (rule-b10 ruly)))) ; Get mask of X->x.
      (setf x-0 (mask-new (mask-and (rule-b00 ruly) (rule-b10 ruly)))) ; Get mask of X->0.
      (setf x-1 (mask-new (mask-and (rule-b01 ruly) (rule-b11 ruly)))) ; Get mask of X->1.

      ;; Get wanted change masks.
      (setf w01 (mask-new (mask-and (rule-b01 ruly) (rule-b01 rulx)))) ; get wanted 0->1s.
      (setf w10 (mask-new (mask-and (rule-b10 ruly) (rule-b10 rulx)))) ; get wanted 1->0s.

      ;; Get unwanted change masks.
      (setf u01 (mask-new (mask-and-not (rule-b01 ruly) (rule-b01 rulx)))) ; get unwanted 0->1s.
      (setf u10 (mask-new (mask-and-not (rule-b10 ruly) (rule-b10 rulx)))) ; get unwanted 1->0s.

      (setf rulz ruly)

      ;; Parse wanted 0->1 changes in X->x
      (setf msk-change (mask-new (mask-and w01 x-not-x)))
      (if (mask-is-not-low msk-change)
	(setf rulz (rule-mask-off-ones rulz msk-change))
      )

      ;; Parse wanted 1->0 changes in X->x
      (setf msk-change (mask-new (mask-and w10 x-not-x)))
      (if (mask-is-not-low msk-change)
	(setf rulz (rule-mask-off-zeros rulz msk-change))
      )

      ;; Parse wanted 0->1 changes in X->1
      (setf msk-change (mask-new (mask-and w01 x-1)))
      (if (mask-is-not-low msk-change)
	(setf rulz (rule-mask-off-ones rulz msk-change))
      )

      ;; Parse wanted 1->0 changes in X->1
      (setf msk-change (mask-new (mask-and w10 x-0)))
      (if (mask-is-not-low msk-change)
	(setf rulz (rule-mask-off-zeros rulz msk-change))
      )

      ;; Parse unwanted 1->0 changes in X->0
      (setf msk-change (mask-new (mask-and u10 x-0)))
      (if (mask-is-not-low msk-change)
	(setf rulz (rule-mask-off-ones rulz msk-change))
      )

      ;; Parse unwanted 0->1 changes in X->1
      (setf msk-change (mask-new (mask-and u01 x-1)))
      (if (mask-is-not-low msk-change)
	(setf rulz (rule-mask-off-zeros rulz msk-change))
      )

      ;; Restrict rule that intersects the from region.
      (cond ((region-intersects from-reg (rule-initial-region ruly)) 
             (let ((new-rule (rule-restrict-initial-region ruly from-reg)) new-rule2)
               (if (region-intersects to-reg (rule-result-region new-rule)) 
                 (progn
                   (setf new-rule2 (rule-restrict-result-region new-rule to-reg))
                   (if (region-intersects (rule-initial-region new-rule2) from-reg)
                     (stepstore-push ret-steps (step-new :act-id 0 :rule new-rule2 :kind 's))
                     (stepstore-push ret-steps (step-new :act-id 0 :rule new-rule :kind 'f))
                   )
                 )
                 (stepstore-push ret-steps (step-new :act-id 0 :rule new-rule :kind 'f))
               )
             )
           )
           ((region-intersects (rule-result-region ruly) to-reg) 
              (stepstore-push ret-steps (step-new :act-id 0 :rule (rule-restrict-result-region rulz to-reg) :kind 'b))
           )
           (t (stepstore-push ret-steps (step-new :act-id 0 :rule rulz :kind 'a)))
      )
    ) ; end-loop
    ret-steps
  )
)

;;; Return the number of bits used by elements withn a group.
(defun group-num-bits (grpx) ; -> integer ge 0.
  (region-num-bits (group-region grpx))
)
