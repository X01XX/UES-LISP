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
    anchor    ; A Vertex, or nil.
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
(defun group-new (regx pn rules)
  ;(format t "~&group-new ~A ~A ~A ~A" (region-str regx) (pn-str pn) (rulestore-str rules))
  (assert (region-p regx))
  (assert (pn-p pn))
  (assert (rulestore-p rules))

  (let ((ret (group-new-na regx pn rules)))
    (cond ((err-p ret) (error (err-str ret)))
          ((group-p ret) ret)
           (t (error "Result is not a group"))))
)

;;; group-new no abort (na).
(defun group-new-na (regx pn rules) ; -> group or err.
  (assert (region-p regx))
  (assert (pn-p pn))
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
         )
        (t (return-from group-new-na "unrecognized pn value")))

  (make-group :region regx :pn pn :pnc nil :rules rules :anchor nil)
)

;;; Return a string representing a group
(defun group-str (agrp)
    (assert (group-p agrp))

    (let ((str "("))
        (setf str (concatenate 'string str (format nil "group ~A" (region-str (group-region agrp)))))
        (if (< (region-number-states (group-region agrp)) 3)
          (setf str (concatenate 'string str " ")))

        (setf str (concatenate 'string str (format nil " pnc ~A" (group-pnc agrp))))

        (if (pn-eq (group-pn agrp) *pn-none*)
          (setf str (concatenate 'string str (format nil " unpredictable")))
          (setf str (concatenate 'string str (format nil " rules ~A" (rulestore-str (group-rules agrp))))))

        (if (group-anchor agrp)
          (setf str (concatenate 'string str (format nil " anchor ~A" (vertex-str (group-anchor agrp))))))
    
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

;;; Return possible steps.
;;;
;;; rule-from-to: A rule. A step should cause at least one change needed by this.
;;;
;;; within: A region. The initial region, and result region, for a step must be in the given region,
;;;                   The region may be all X.
;;;                   If an alternate rule is possible, for a two-rule group, that rule must also be within the region.
;;;
;;; The no-alt option in used by the caller, for finding an alternate plan, when needed for a two-result group,
;;; without getting into an infinte regress of alternate plans.
(defun group-get-steps (grpx rule-from-to within &optional no-alt) ; -> stepstore.
  ;(format t "~&group-get-steps: group ~A rule ~A within ~A no-alt ~A" (type-of grpx) (type-of rule-from-to) (type-of within) (type-of no-alt))
  (assert (group-p grpx))
  (assert (rule-p rule-from-to))
  (assert (region-p within))
  (assert (bool-p no-alt))
  ;(format t "~&group-get-steps: group ~A rule ~A within ~A no-alt ~A" (region-str (group-region grpx))
  ;            (rule-str rule-from-to) (region-str within) no-alt)
  (assert (rule-makes-change rule-from-to))

  (let ((ret-steps (stepstore-new nil)))

    ;; Skip unpredictable groups.
    (if (pn-eq (group-pn grpx) *pn-none*)
      (return-from group-get-steps ret-steps))

    ;; Skip group that does not apply to the within resriction.
    (if (not (region-intersects (group-region grpx) within))
      (return-from group-get-steps ret-steps))

    ;; Skip group that does not make a change.
    (if (not (group-makes-predictable-change grpx))
      (return-from group-get-steps ret-steps))

    ;; Handle *pn-one* group.
    (when (pn-eq (group-pn grpx) *pn-one*)

      (let (rulx)
        (setf rulx (rule-restrict-by (rulestore-first (group-rules grpx)) rule-from-to within))
        (if rulx
          (if (change-is-not-low (change-and (rule-changes rulx) (rule-changes rule-from-to)))
            (stepstore-push ret-steps (step-new *act-id* rulx))
          )
        )
        (return-from group-get-steps ret-steps)
      )
    )

    ;; Handle *pn-two* group.
    (let (rulx ruly initial-int)

      ;; Restrict the two rules.
      (setf rulx (rule-restrict-by (rulestore-first (group-rules grpx)) rule-from-to within))
      (setf ruly (rule-restrict-by (rulestore-second (group-rules grpx)) rule-from-to within))

      (when (and rulx ruly)
        ;; Restriction of the result region, due to the within region, may have caused
        ;; the rule initial regions to diverge.
        (when (region-intersects (rule-initial-region rulx) (rule-initial-region ruly))

          (when (not (region-eq (rule-initial-region rulx) (rule-initial-region ruly)))
            ;; Put rules back in sync.
            (setf initial-int (region-intersection (rule-initial-region rulx) (rule-initial-region ruly)))
            (setf rulx (rule-restrict-initial-region rulx initial-int))
            (setf ruly (rule-restrict-initial-region ruly initial-int))
          )

          ;; Process rulx.
          (when (change-is-not-low (change-and (rule-changes rulx) (rule-changes rule-from-to)))

            (if (rule-makes-change ruly)
              (if (null no-alt)
                (stepstore-push ret-steps (step-new *act-id* rulx ruly))) ; Caller will try to find a recovery plan.
              ; else ruly does not make a change.
              (stepstore-push ret-steps (step-new *act-id* rulx)) ; This will depend on the heuristic of trying a second time.
            )
          ) ; End process rulx.

          ;; Process ruly.
          (when (change-is-not-low (change-and (rule-changes ruly) (rule-changes rule-from-to)))

            (if (rule-makes-change rulx)
              (if (null no-alt)
                (stepstore-push ret-steps (step-new *act-id* rulx ruly))) ; Caller will try to find a recovery plan.
              ; else rulx does not make a change.
              (stepstore-push ret-steps (step-new *act-id* ruly)) ; This will depend on the heuristic of trying a second time.
            )
          ) ; end process ruly
        ) ; end when region-intersects 
      ) ; end when rulx ruly
    ) ; end let

    ret-steps
  ) ; end-let
) ; end group-get-steps.

;;; Return the number of bits used by elements withn a group.
(defun group-num-bits (grpx) ; -> integer ge 0.
  (region-num-bits (group-region grpx))
)

;;; Set the group pnc to a different value.
(defun group-set-pnc (grpx pnc) ; side-effect, group pnc changed.
  (assert (group-p grpx))
  (assert (bool-p pnc))

  (when (and (< (region-number-states (group-region grpx)) 3) (xor pnc (group-pnc grpx)))
    (format t "~&Dom: ~D Act: ~D Group ~A pnc changed from ~A to ~A" *dom-id* *act-id* (region-str (group-region grpx)) (group-pnc grpx) pnc)
    (setf (group-pnc grpx) pnc)
    (return-from group-set-pnc )
  )
)

;;; Set the group region to a equal value, with different states.
(defun group-set-region (grpx regx) ; side-effect, group region changed.
  (assert (group-p grpx))
  (assert (region-p regx))
  (assert (region-eq regx (group-region grpx)))
  (assert (or (null (group-anchor grpx)) (state-eq (region-first-state regx) (vertex-pinnacle (group-anchor grpx)))))

  (when (or (/= (region-number-states regx) (region-number-states (group-region grpx)))
            (state-ne (region-first-state regx) (region-first-state (group-region grpx))))

    (format t "~&Dom: ~D Act: ~D group ~A region changed from ~A to ~A" *dom-id* *act-id* (region-str (group-region grpx))
                                                        (statestore-str (region-states (group-region grpx)))
                                                        (statestore-str (region-states regx)))
    (if (not (null (group-anchor grpx)))
      (format t " anchor: ~A" (vertex-str (group-anchor grpx)))
    )
    (setf (group-region grpx) regx)

    (if (and (group-pnc grpx) (> (region-number-states (group-region grpx)) 2))
      (setf (group-pnc grpx) nil))

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

;; Return true if a state is needed to define a group.
(defun group-state-needed (grpx stax) ; -> bool
  (assert (group-p grpx))
  (assert (state-p stax))

  (if (region-state-needed (group-region grpx) stax)
      (return-from group-state-needed true))

  (when (not (null (group-anchor grpx)))
    (if (vertex-member (group-anchor grpx) stax)
      (return-from group-state-needed true))
    
    (when (not (group-makes-predictable-change grpx))
      (let (sta-pin x-bit-masks sta-adj)
        (setf sta-pin (vertex-pinnacle (group-anchor grpx)))
        (setf x-bit-masks (mask-split (region-x-mask (group-region grpx))))
  
        (loop for maskx in x-bit-masks do
          (setf sta-adj (state-new-xor sta-pin maskx))
          (if (state-eq sta-adj stax)
            (return-from group-state-needed true))
        )
      )
    )
  )
  false
)
