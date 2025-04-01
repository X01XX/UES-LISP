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
(defstruct group
    region    ; A Region defined by one, or more, states, keys of compatible squares.
    pn        ; A pn struct instance.
    pnc       ; bool.
    rules     ; zero, one, or two rules, from the union of compatible square rules.
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
(defun group-new (regx pn pnc rules)
  ;(format t "~&group-new ~A ~A ~A ~A" (region-str regx) (pn-str pn) pnc (rulestore-str rules))
  (assert (region-p regx))
  (assert (pn-p pn))
  (assert (bool-p pnc))
  (assert (rulestore-p rules))
  (assert (or (< (region-number-states regx) 3) (not pnc)))

  (let ((ret (group-new-na regx pn pnc rules)))
    (cond ((err-p ret) (error (err-str ret)))
          ((group-p ret) ret)
           (t (error "Result is not a group"))))
)
;;; group-new no abort (na).
(defun group-new-na (regx pn pnc rules) ; -> group or err.
  (assert (region-p regx))
  (assert (pn-p pn))
  (assert (bool-p pnc))
  (assert (rulestore-p rules))

  (cond ((pn-eq pn *pn-one*)
           (if (/= 1 (rulestore-length rules))
             (return-from group-new-na "Rules length does not match pn value"))

           (if (region-ne regx (rule-initial-region (rulestore-first rules)))
             (return-from group-new-na "Region does not match rules"))
         )
        ((pn-eq pn *pn-two*)
           (if (/= 2 (rulestore-length rules))
             (return-from group-new-na "Rules length does not match pn value"))

           (if (region-ne regx (rule-initial-region (rulestore-first rules)))
             (return-from group-new-na "Region does not match rules"))

          (if (region-ne (rule-initial-region (rulestore-first rules))
                          (rule-initial-region (rulestore-second rules)))
             (return-from group-new-na (err-new "Rulestore initial regions do not match")))
         )
        ((pn-eq pn *pn-none*)
           (if (/= 0 (rulestore-length rules))
             (return-from group-new-na "Rules length does not match pn value"))
           (if (< (region-number-states regx) 3)
             (setf pnc t))
         )
        (t (return-from group-new-na "unrecognized pn value")))

  (make-group :region regx :pn pn :pnc pnc :rules rules)
)

;;; Return a string representing a group
(defun group-str (agrp)
    (assert (group-p agrp))

    (let ((str "("))
        (setf str (concatenate 'string str (format nil "region ~A" (region-str (group-region agrp)))))
        (if (< (region-number-states (group-region agrp)) 3)
          (setf str (concatenate 'string str " ")))

        (setf str (concatenate 'string str (format nil " pnc ~A" (group-pnc agrp))))
        (setf str (concatenate 'string str (format nil " rules ~A" (rulestore-str (group-rules agrp)))))
        (setf str (concatenate 'string str ")"))
        str
    )
)

;;; Print a group.
(defun group-print (agrp)
    (assert (group-p agrp))

    (let ((str "("))
        (setf str (concatenate 'string str (format nil "group ~A " (region-str (group-region agrp)))))
        (if (< (region-number-states (group-region agrp)) 3)
          (setf str (concatenate 'string str " ")))

        (setf str (concatenate 'string str (format nil "pnc ~A" (group-pnc agrp))))
        (if (group-pnc agrp)
          (setf str (concatenate 'string str "  ")))

        (setf str (concatenate 'string str (format nil " rules ~A" (rulestore-str (group-rules agrp)))))
        (setf str (concatenate 'string str ")"))
        (format t "~A" str)
    )
)

; Return true if two groups are equal.
(defun group-eq (grp1 grp2) ; -> bool
  (region-eq (group-region grp1) (group-region grp2))
)

; Return possible steps, given group, rule, from-region, to-region.
(defun group-get-steps (grpx from-reg to-reg within) ; -> stepstore.
  ;(format t "~&group-get-steps")
  ;(format t "~&group-get-steps group ~A" grpx)
  (assert (group-p grpx))
  (assert (region-p from-reg))
  (assert (region-p to-reg))
  (assert (region-p within))

  (let ((ret-steps (stepstore-new nil)) rulz
        rule-num-wanted rule-num-unwanted
       (dont-care-changes (change-new :m01 (region-x-mask to-reg) :m10 (region-x-mask to-reg)))
       (wanted-changes (rule-changes (rule-region-to-region from-reg to-reg)))
       rule-unwanted-changes
       rule-wanted-changes
       tmp-rule-from tmp-rule-to)

    ;; TODO Handle *pn-two* groups.
    (if (pn-ne (group-pn grpx) *pn-one*)
      (return-from group-get-steps ret-steps))

    (if (not (region-intersects (group-region grpx) within))
      (return-from group-get-steps ret-steps))

    (loop for ruly in (rulestore-rules (group-rules grpx)) do
      ;(format t "~&ruly ~A" ruly)

      (setf rulz (rule-restrict-initial-region ruly within))

      (when (region-intersects (rule-result-region rulz) within)

        (setf rulz (rule-restrict-result-region rulz within))

        (if (region-intersects (rule-initial-region rulz) from-reg)
          (setf rulz (rule-restrict-initial-region rulz from-reg)))

        (if (region-intersects (rule-result-region rulz) to-reg)
          (setf rulz (rule-restrict-result-region rulz to-reg)))

        (setf rule-wanted-changes (change-and (rule-changes rulz) wanted-changes))

        (when (change-is-not-low rule-wanted-changes)

          ;; Parse wanted 0->1 changes in X->x, X->1.
          (if (mask-is-not-low (change-m01 rule-wanted-changes))
  	        (setf rulz (rule-mask-off-ones rulz (change-m01 rule-wanted-changes)))
          )
  
          ;; Parse wanted 1->0 changes in X->x, X->0.
          (if (mask-is-not-low (change-m10 rule-wanted-changes))
  	        (setf rulz (rule-mask-off-zeros rulz (change-m10 rule-wanted-changes)))
          )

          (setf tmp-rule-from (rule-region-to-region from-reg (rule-initial-region rulz)))
          (setf tmp-rule-to (rule-region-to-region (rule-result-region rulz) to-reg))
          (setf rule-unwanted-changes (change-and-not
                                        (change-and-not
                                          (change-or (rule-changes tmp-rule-from) (rule-changes tmp-rule-to)) wanted-changes)
                                       dont-care-changes))

    	  (setf rule-num-wanted (change-num-changes rule-wanted-changes))
    	  (setf rule-num-unwanted (change-num-changes rule-unwanted-changes))
    
          ;(when (> rule-num-unwanted 0)
          ;  (format t "~&from ~A to ~A using rule ~A with wanted changes ~A unwanted changes ~A num ~D"
          ;  (region-str from-reg) (region-str to-reg) (rule-str rulz)
          ;  (change-str rule-wanted-changes) (change-str rule-unwanted-changes) rule-num-unwanted)
          ;)

          (stepstore-push ret-steps (step-new :act-id 0   ; Caller to change. By convention, act 0 does not do anything.
                                              :rule rulz
                                              :num-wanted   rule-num-wanted
                                              :num-unwanted rule-num-unwanted))
        ) ; end when 2
      ) ; end when 1
    ) ; end-loop
    ret-steps
  ) ; end-let
)

;;; Return the number of bits used by elements withn a group.
(defun group-num-bits (grpx) ; -> integer ge 0.
  (region-num-bits (group-region grpx))
)

;;; Set the group pnc to a different value.
(defun group-set-pnc (grpx pnc) ; side-effect, group pnc changed.
  (assert (group-p grpx))
  (assert (bool-p pnc))

  (when (and (< (region-number-states (group-region grpx)) 3) (xor pnc (group-pnc grpx)))
    (format t "~&group ~A pnc changed from ~A to ~A" (region-str (group-region grpx)) (group-pnc grpx) pnc)
    (setf (group-pnc grpx) pnc)
    (return-from group-set-pnc )
  )
)

;;; Set the group region to a equal value, with different states.
(defun group-set-region (grpx regx) ; side-effect, group region changed.
  (assert (group-p grpx))
  (assert (region-p regx))
  (assert (region-eq regx (group-region grpx)))

  (when (or (/= (region-number-states regx) (region-number-states (group-region grpx)))
            (state-ne (region-first-state regx) (region-first-state grpx)))
    (format t "~&group ~A region changed from ~A to ~A" (region-str regx)
                                                        (statestore-str (region-states (group-region grpx)))
                                                        (statestore-str (region-states regx)))
    (setf (group-region grpx) regx)
    (return-from group-set-region)
  )
  (format t "~&Problem: group-set-region: region not changed?")
)

;;; Return true if a group makes a predicable change.
(defun group-makes-predictable-change (grpx) ; -> bool
  (assert (group-p grpx))

  (if (pn-eq (group-pn grpx) *pn-none*)
    (return-from group-makes-predictable-change false))

  (if (pn-eq (group-pn grpx) *pn-two*)
    (return-from group-makes-predictable-change true))

  (change-is-not-low (rule-changes (rulestore-first (group-rules grpx))))
)

