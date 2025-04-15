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
        (setf str (concatenate 'string str (format nil "group ~A" (region-str (group-region agrp)))))
        (if (< (region-number-states (group-region agrp)) 3)
          (setf str (concatenate 'string str " ")))

        (setf str (concatenate 'string str (format nil " pnc ~A" (group-pnc agrp))))

        (if (pn-eq (group-pn agrp) *pn-none*)
          (setf str (concatenate 'string str (format nil " unpredictable")))
          (setf str (concatenate 'string str (format nil " rules ~A" (rulestore-str (group-rules agrp))))))

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

; Return possible steps, given group, rule from-region to-region.
(defun group-get-steps (grpx rule-from-to within &optional no-alt) ; -> stepstore.
  ;(format t "~&group-get-steps")
  ;(format t "~&group-get-steps: group ~A" grpx)
  (assert (group-p grpx))
  (assert (rule-p rule-from-to))
  (assert (region-p within))
  ;(format t "~&group-get-steps: group ~A rule ~A within ~A no-alt ~A" (region-str (group-region grpx))
  ;            (rule-str rule-from-to) (region-str within) no-alt)

  (let ((ret-steps (stepstore-new nil)))

    ;; Skip unpredictable groups.
    (if (pn-eq (group-pn grpx) *pn-none*)
      (return-from group-get-steps ret-steps))

    ;; Skip group that does not apply to the within resriction.
    (if (not (region-intersects (group-region grpx) within))
      (return-from group-get-steps ret-steps))

    ;; Handle *pn-one* group.
    (when (pn-eq (group-pn grpx) *pn-one*)

      (let (rulx)
        (setf rulx (rulestore-first (group-rules grpx)))

        (setf rulx (rule-restrict-by rulx rule-from-to within))

        (when rulx
          (stepstore-push ret-steps (step-new 0   ; Caller to change. By convention, act 0 does not do anything.
                                              rulx))
          (return-from group-get-steps ret-steps)
        )
        (return-from group-get-steps ret-steps)
      )
    )

    ;; Handle *pn-two* group.
    (when (pn-eq (group-pn grpx) *pn-two*)

      (let (rulx)
        ;; Check first rule.
        (if (and (rule-makes-change (rulestore-first (group-rules grpx)))
                 (not (rule-makes-change (rulestore-second (group-rules grpx)))))
          (setf rulx (rulestore-first (group-rules grpx)))
        )
        ;; Check second rule.
        (if (and (rule-makes-change (rulestore-second (group-rules grpx)))
                 (not (rule-makes-change (rulestore-first (group-rules grpx)))))
          (setf rulx (rulestore-second (group-rules grpx)))
        )

        (when rulx
          (setf rulx (rule-restrict-by rulx rule-from-to within))

          (if rulx
            (stepstore-push ret-steps (step-new 0   ; Caller to change. By convention, act 0 does not do anything.
                                                rulx))
          )
          (return-from group-get-steps ret-steps)
        )
      )
    )

    ;; Handle more complicated *pn-two* groups.
    (when (and (pn-eq (group-pn grpx) *pn-two*) (null no-alt)) ; Avoid an infinite loop of using alternate rules.
      (let ((rule1 (rulestore-first (group-rules grpx)))
            (rule2 (rulestore-second (group-rules grpx)))
            initial-int
            wanted-changes
            rulex rulexs
           )

        ;; Both rules have to fit, initial-region/result-region, at least partially, into the within region.
        (if (not (region-intersects (rule-result-region rule1) within))
          (return-from group-get-steps ret-steps))

        (if (not (region-intersects (rule-result-region rule2) within))
          (return-from group-get-steps ret-steps))

        ;; Alter rules to fit within the within region, if there is a partial intersection.
        (setf rule1 (rule-restrict-by-within rule1 within))
        (when (null rule1)
          (return-from group-get-steps ret-steps)
        )

        (setf rule2 (rule-restrict-by-within rule2 within))
        (if (null rule2)
          (return-from group-get-steps ret-steps))

        ;; Check if the two, possibly altered, rules can be put in sync.
        (if (not (region-intersects (rule-initial-region rule1) (rule-initial-region rule2)))
          (return-from group-get-steps ret-steps))

        ;; Check if the two, possibly altered, rules need no be put in sync.
        (when (not (region-eq (rule-initial-region rule1) (rule-initial-region rule2)))
          (setf initial-int (region-intersection (rule-initial-region rule1) (rule-initial-region rule2)))
          (setf rule1 (rule-restrict-initial-region rule1 initial-int))
          (setf rule2 (rule-restrict-initial-region rule2 initial-int))
        )

        ;; Alter rules based on the changes needed.
        (setf wanted-changes (rule-changes rule-from-to))

        (setf rulex (rule-restrict-by-change rule1 wanted-changes))
        (when rulex

          ;; Split rule if needed, to insure a return-to-state is calculable.
          (setf rulexs (rule-split-xb rulex))

          (loop for rulez in (rulestore-rules rulexs) do
            (stepstore-push ret-steps (step-new 0   ; Caller to change. By convention, act 0 does not do anything.
                                                rulez
                                                (rule-restrict-initial-region rule2 (rule-initial-region rulez))))
          )
        )

        (return-from group-get-steps ret-steps)
      )
    )

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
            (state-ne (region-first-state regx) (region-first-state (group-region grpx))))

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

;; Return true if a state is needed to define a group.
(defun group-state-needed (grpx stax) ; -> bool
  (assert (group-p grpx))
  (assert (state-p stax))

  (if (region-state-needed (group-region grpx) stax)
      (return-from group-state-needed true))

  (let (sta-first x-bit-masks sta-adj)
    (when (not (group-makes-predictable-change grpx))
      (setf sta-first (region-first-state (group-region grpx)))
      (setf x-bit-masks (mask-split (region-x-mask (group-region grpx))))

      (loop for maskx in x-bit-masks do
        (setf sta-adj (state-new (state-xor sta-first maskx)))
        (if (state-eq sta-adj stax)
          (return-from group-state-needed true))
      )
    )
  )
  false
)
