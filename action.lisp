(defvar true t)
(defvar false nil)

;;;; Implement the Action type.
;;;;
(defstruct action
  id		  ; A number id, GE zero.
  groups	  ; A groupstore.
  squares     ; A Squarestore.
  base-rules  ; A list of rulestores to use in generating samples.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (action-<field name> <instance>) returns struct field.
;   (action-p <instance>) -> t
;
; Least used:
;   (type-of <instance>) -> action
;   (typep <instance> 'action) -> t
;
; Don't use:
;   (make-action [:<field-name> <field-value>]*), use action-new instead.
;   (copy-action <instance>) copies a action instance.

;;; Return an action.
(defun action-new (&key id rules)
  ;(format t "~&action-new: Act: ~D base-rules:" id)
  ;(loop for rulsx in rules do
  ;   (format t " ~A" (rulestore-str rulsx))
  ;)
  (assert (rulestore-list-p rules))
  (assert (>= id 0))

  (let (rulsx rulsy actx)

    ;; Check each rulestore is not empty.
    (loop for rulsx in rules do
        (assert (rulestore-is-not-empty rulsx))
    )

    ;; Check each rule, within each rulestore, has the same initial region.
    (loop for rulsx in rules do
        (when (> (rulestore-length rulsx) 1)
        )
    )

    ;; Check rules for consistency.
    (loop for inx from 0 below (1- (length rules)) do
      (setf rulsx (nth inx rules))

      (loop for iny from (1+ inx) below (length rules) do                                             
        (setf rulsy (nth iny rules))

        (when (region-intersects (rulestore-initial-region rulsx) (rulestore-initial-region rulsy))
          (if (null (rulestore-intersection rulsx rulsy))
              (error "invalid intersection of rulestores"))
        )
      )
    )

    (setf actx (make-action :id id :groups (groupstore-new nil) :squares (squarestore-new) :base-rules rules))
    ;(format t "~&returning act: ~A" (action-str actx))
    actx
  )
)

;;; Set the id.
(defun action-set-id (actx id)
    (assert (action-p actx))
    (assert (integerp id))
    (assert (>= id 0))

    (setf (action-id actx) id)
)

;;; Return the number of bits used by an action.
(defun action-num-bits (actx) ; -> number bits used
    (assert (action-p actx))

    (rulestore-num-bits (car (action-base-rules actx)))
)

;;; Return a string representing a action
(defun action-str (actx)
    (assert (action-p actx))

    (let ((str "#S(ACTION "))
        (setf str (concatenate 'string str (format nil "id ~D" (action-id actx))))
        (setf str (concatenate 'string str (format nil " groups ~A" (groupstore-str (action-groups actx)))))
        (setf str (concatenate 'string str ")"))
        str
    )
)

; Return true if the argument is a list of actions.
(defun action-list-p (actions) ; -> bool

  (if (not (listp actions))
    (return-from action-list-p false))

  ; Check for a non-state.
  (loop for actx in actions do
    (if (not (action-p actx))
      (return-from action-list-p false))
  )
  true
)

(defun action-eq (act1 act2) ; -> bool
  (assert (action-p act1))
  (assert (action-p act2))

  (= (action-id act1) (action-id act2))
)

; Return possible steps given a rule to satisfy.
(defun action-get-steps (actx rule-to-goal within) ; -> stepstore.
  (assert (action-p actx))
  (assert (rule-p rule-to-goal))
  (assert (region-p within))

  ;(format t "~&action-get-steps")
  (let ((ret-steps (stepstore-new nil)) group-steps)
    (setf group-steps (groupstore-get-steps (action-groups actx) rule-to-goal within))
    (loop for stpx in (stepstore-step-list group-steps) do
      (setf (step-act-id stpx) (action-id actx))
      (stepstore-push ret-steps stpx)
    )
    ret-steps
  )
)

;;; Return needs for sample in a region, an initial sample, or resample of existing non-pnc square.
;;; There should be no pnc square in the region.
(defun action-needs-for-region (actx regx) ; -> needstore.
  (assert (action-p actx))
  (assert (region-p regx))

  (let ((ret (needstore-new nil)) ; The return struct.
        (sqrs-in (squarestore-squares-in-region (action-squares actx) regx)) ; The squares in the given region.
        sqrs-high-num-results ; Squares in the given region with the highest number of samples.
        (sqrs-highest-num-results 0) ; Maximum number samples found for a square.
       )

    ;; Check for no squares in region.
    (when (null sqrs-in)
        (needstore-push ret (need-new :act-id (action-id actx)
                                      :kind *sample-in-region*
                                      :reason *contradictory-intersection*
                                      :target regx
                            ))
        (return-from action-needs-for-region ret)
    )

    ;; Get list of squares with the higest number of results.
    (loop for sqrx in sqrs-in do
      (when (square-pnc sqrx) 
        (format t "~&Problem: Act ~D pnc square ~A in contradictory region ~A" (action-id actx) (state-str (square-state sqrx)) (region-str regx))
        (return-from action-needs-for-region ret)
      )
      (when (> (square-results-length sqrx) sqrs-highest-num-results)
        (setf sqrs-highest-num-results (square-results-length sqrx))
        (setf sqrs-high-num-results nil)
      )
      (if (= (square-results-length sqrx) sqrs-highest-num-results)
        (push sqrx sqrs-high-num-results))
    )

    ;; Load needs for squares with the higest number of results.
    (loop for sqrx in sqrs-high-num-results do
        (needstore-push ret (need-new :act-id (action-id actx)
                                      :kind *sample-in-region*
                                      :reason *contradictory-intersection*
                                      :target (square-state sqrx) 
                            ))
    )
    ret
  )
)

;;; Return action needs to improve the understanding of the logic behind the samples, so far.
(defun action-get-needs (actx cur-state) ; -> NeedStore.
  ;(format t "~&action-get-needs: ~A ~A" (type-of actx) (type-of cur-state))
  (assert (action-p actx))
  (assert (state-p cur-state))

  (let ((needs (needstore-new nil)))
    ;; Generate need for a cur-state that is not in a group.
    (when (not (groupstore-state-in-group (action-groups actx) cur-state))
      (let ((sqrx (squarestore-find (action-squares actx) cur-state)))

        (cond (sqrx
               (if (square-pnc sqrx)  
                 (format t "~&square ~A pnc, not in a group?" cur-state) ; should not happen once group logic is set up.
                 (needstore-push needs (action-get-need-resample-state actx cur-state *state-not-in-group*)))
              )
              (t (needstore-push needs (action-get-need-sample-state actx cur-state *state-not-in-group*)))
        )
      )
    )

    ;; Recheck single-square groups.
    (let (sqrx sqrs)
      (loop for grpx in (groupstore-groups (action-groups actx)) do
        (when (and (= 1 (region-number-states (group-region grpx))) (group-pnc grpx))
            (setf sqrx (action-find-square actx (region-first-state (group-region grpx))))
            (if sqrx
              (push sqrx sqrs)
              (error "Square defining group not found?"))
        )
      )

      (loop for sqrx in sqrs do
        (when (not (groupstore-multistate-groups-state-in (action-groups actx) (square-state sqrx)))
           ;(format t "~&running expand check for group ~A" (state-str (square-state sqrx)))
           (action-make-groups-from-square actx sqrx)
        )
      )
    )

    ;; Generate needs to confirm groups.
    (let (grp-needs)
      (loop for grpx in (groupstore-groups (action-groups actx)) do
        ;(format t "~&checking group ~A" (region-str (group-region grpx)))
        (when (not (group-pnc grpx))
          ;; Get needs for group, possibly replace group region with one made of two states.
          (setf grp-needs (action-confirm-group-needs actx grpx))
          (when (needstore-is-not-empty grp-needs)
            ;(format t "~&grp-needs ~A" (needstore-str grp-needs))
            (setf needs (needstore-append needs grp-needs)))
        )
      ) ; next grpx
    )

    ;; Generate needs for resolving contradictory intersections.
    (let (grpx grpy reg-int rules-int reg-far)
      (loop for inx from 0 below (1- (groupstore-length (action-groups actx))) do

        (setf grpx (groupstore-nth (action-groups actx) inx))

        (when (group-pnc grpx)

          (loop for iny from (1+ inx) below (groupstore-length (action-groups actx)) do

            (setf grpy (groupstore-nth (action-groups actx) iny))

            (when (group-pnc grpy)

              (setf reg-int (region-intersection (group-region grpx) (group-region grpy)))

              (when reg-int
                (cond ((pn-ne (group-pn grpx) (group-pn grpy))
                        (setf needs (needstore-append needs (action-needs-for-region actx reg-int)))
                      )
                      ((pn-eq (group-pn grpx) *pn-none*) nil)
                      (t
                        ;; both pn-one or pn-two.
                        (setf rules-int (rulestore-intersection (group-rules grpx) (group-rules grpy)))
                        (cond ((null rules-int)
                                (setf needs (needstore-append needs (action-needs-for-region actx reg-int)))
                              )
                              ((region-eq reg-int (rulestore-initial-region rules-int)) nil)
                              (t
                                (setf reg-far (region-far-region reg-int (rulestore-initial-region rules-int)))
                                (setf needs (needstore-append needs (action-needs-for-region actx reg-far)))
                              )
                        )
                      )
                )
              )
            )

          ) ; next iny.
        )
      ) ; next inx.
    )
    needs
  )
)

;;; Set a group pnc slot to true, print message.
(defun action-group-set-pnc (actx grpx) ; -> side-effects, group pnc changed, message printed.
  (assert (action-p actx))
  (assert (group-p grpx))
  (assert (not (group-pnc grpx)))

  (format t "~&Act: ~D Group: ~A, pnc set to true." (action-id actx) (region-str (group-region grpx)))
  (group-set-pnc grpx true)
)

;;; Return confirm needs for a group.
;;; Resample first state in group region and/or far state, until both are pnc.
(defun action-confirm-group-needs (actx grpx) ; -> needstore.
  (assert (action-p actx))
  (assert (group-p grpx))
  ;(format t "~&action-confirm-group-needs: Act: ~D Group: ~A" (action-id actx) (region-str (group-region grpx)))

  (let ((needs (needstore-new nil)) (grp-reg (group-region grpx)))

    ;; Process a one-state group region.
    (when (= (region-number-states grp-reg) 1)

      (let (sta-first sqr-first)
        ;; Get first state.
        (setf sta-first (region-first-state grp-reg))
        ;; Get first square.
        (setf sqr-first (squarestore-find (action-squares actx) sta-first))
        (if (null sqr-first)
          (error "Region first square not found?"))

        ;(format t "~&  square ~A found" (square-str sqr-first))
        ;; Check if more samples needed.
        (if (not (square-pnc sqr-first))
          (needstore-push needs (action-get-need-resample-state actx sta-first *confirm-group*
               "" grp-reg)))

          ;(format t "~&action-confirm-group-needs: return 1 Act: ~D Group: ~A needs: ~A"
          ;(action-id actx) (region-str (group-region grpx)) (needstore-str needs))

        (return-from action-confirm-group-needs needs)
      )
    )

    ;; Process a two-state group region.
    (when (= (region-number-states grp-reg) 2)

      (let (sqr-first sqr-far)
        (setf sqr-first (squarestore-find (action-squares actx) (region-first-state grp-reg)))
        (if (null sqr-first)
          (error "Region first square not found?"))

        ;; Check if more samples needed.
        (if (not (square-pnc sqr-first))
          (needstore-push needs (action-get-need-resample-state actx (square-state sqr-first) *confirm-group*
                "" grp-reg)))

        (setf sqr-far (squarestore-find (action-squares actx) (region-second-state grp-reg)))
        (if (null sqr-far)
          (error "Region far square not found?"))

        ;; Check if more samples needed.
        (if (not (square-pnc sqr-far))
          (needstore-push needs (action-get-need-resample-state actx (square-state sqr-far) *confirm-group*
                 "" grp-reg)))

        ;(format t "~&action-confirm-group-needs: return 2 Act: ~D Group: ~A needs: ~A"
        ;    (action-id actx) (region-str (group-region grpx)) (needstore-str needs))

        (return-from action-confirm-group-needs needs)
      )
    )

    ;; Process a GT 2 state region.
    (let (sta-first sqr-first sta-far sqr-far)

      ;; Check first square.
      (setf sta-first (region-first-state grp-reg))
      (setf sqr-first (squarestore-find (action-squares actx) sta-first))
      (if (null sqr-first)
        (error "Region first square not found?"))

      ;; Check if more samples needed.
      (if (not (square-pnc sqr-first))
        (needstore-push needs (action-get-need-resample-state actx sta-first *confirm-group*
                  "" grp-reg)))

      ;; Calc far state.
      (setf sta-far (region-far-state (group-region grpx) sta-first))

      ;; Find far square, if any.
      (setf sqr-far (squarestore-find (action-squares actx) sta-far))

      ;; Generate far sample needs.
      (when sqr-far
        (if (not (square-pnc sqr-far))
          (needstore-push needs (action-get-need-resample-state actx sta-far *confirm-group*
             "" grp-reg)))

            ;(format t "~&action-confirm-group-needs: return 3 Act: ~D Group: ~A needs: ~A"
            ;  (action-id actx) (region-str (group-region grpx)) (needstore-str needs))

        (return-from action-confirm-group-needs needs)
      )

      ;; sqr-far not found.
      (needstore-push needs (action-get-need-sample-state actx sta-far *confirm-group*
          "" grp-reg))

      ;(format t "~&action-confirm-group-needs: return 4 Act: ~D Group: ~A needs: ~A"
      ;      (action-id actx) (region-str (group-region grpx)) (needstore-str needs))
      needs
    )
  )
)

;;; Return an action instance, given a list of symbols.
;;; Thi action ID defaulst to zero, the caller may need to set it.
(defun action-from (symbols) ; -> action
    ;(format t "~&action-from: ~A ~A" (type-of symbols) symbols)
    (assert (listp symbols))
    (assert (not (null symbols)))
    (assert (symbolp (car symbols)))
    (assert (eq (car symbols) 'ACT))

    (setf symbols (cdr symbols))

    (let (rulestores pos sname stax actx)
        (loop for tokx in symbols do
            (when (listp tokx)
                ;(format t "~&action-from1 ~A" tokx)
                (push (rulestore-from-str tokx) rulestores))
        )
        ;; Init action.
        (setf actx (action-new :id 0 :rules rulestores))


        ;; Check for state sample tokens.
        (loop for tokx in symbols do
            (cond ((symbolp tokx)
                   ;(format t "~&action-from2 ~A" tokx)
                   (setf sname (symbol-name tokx))
                   (setf pos (position #\/ sname))
                   (setf stax (state-from-str (subseq sname 0 pos)))
                   (if pos
                       (dotimes (i (read-from-string (subseq sname (1+ pos)))) 
                         (action-take-sample-arbitrary actx stax))
                       (action-take-sample-arbitrary actx stax))
                       ; (format t "~&state found ~A sample ~D times" (subseq sname 0 pos)
                       ;          (read-from-string (subseq sname (1+ pos))))
                       ; (format t "~&state found ~A" tokx))
                  )
            )
        )

        actx
    )
)

;;; Get a need to sample a state, after some checks.
(defun action-get-need-sample-state (actx stax reason &optional extra-info group-region) ; -> need instance.
  (assert (action-p actx))
  (assert (state-p stax))
  (assert (integerp reason))
  (assert (or (null extra-info) (stringp extra-info)))
  (assert (or (null group-region) (region-p group-region)))

  (if (null extra-info)
    (setf extra-info ""))

  ; If a square exists with this state, call action-get-need-resample-state.
  (if (squarestore-find (action-squares actx) stax)

    (action-get-need-resample-state actx stax reason)

    (need-new :act-id (action-id actx)
              :kind *first-sample-of-state*
              :reason reason
              :target stax
              :extra-info extra-info
              :group-region group-region)
  )
)

;;; Get a need to resample a state, after some checks.
(defun action-get-need-resample-state (actx stax reason &optional extra-info group-region) ; -> need instance.
  (assert (action-p actx))
  (assert (state-p stax))
  (assert (integerp reason))
  (assert (or (null extra-info) (stringp extra-info)))
  (assert (or (null group-region) (region-p group-region)))

  (if (null extra-info)
    (setf extra-info ""))

  ; A square must exist with this state, and it must be non-pnc.
  (let ((sqrx (squarestore-find (action-squares actx) stax)))

    (if (null sqrx)
      (error "action-get-need-resample-state: square not found?"))

    (if (square-pnc sqrx)
      (error "action-get-need-resample-state: square pnc is true?"))

    (need-new :act-id (action-id actx)
              :kind *resample-state*
              :reason reason
              :target stax
              :extra-info extra-info
              :group-region group-region)
  )
)

;;; Get a need to sample a region, after some checks.
(defun action-get-need-sample-region (actx regx reason &optional extra-info group-region) ; -> need instance.
  (assert (action-p actx))
  (assert (region-p regx))
  (assert (integerp reason))
  (assert (or (null extra-info) (stringp extra-info)))
  (assert (or (null group-region) (region-p group-region)))

  (if (null extra-info)
    (setf extra-info ""))

  ; There must be no square with a state in the region.
  (if (squarestore-any-in (action-squares actx) regx)
      (error "action-get-need-sample-region: squares in region?"))

  (need-new :act-id (action-id actx)
            :kind *sample-in-region*
            :reason reason
            :target regx
            :extra-info extra-info
            :group-region group-region)
)

;;; Get sample for a given state.
(defun action-get-sample (actx stax) ; -> sample
  ;(format t "~&action-get-sample: ~A ~A" (type-of actx) (type-of stax)) 
  (assert (action-p actx))
  (assert (state-p stax))

  (let ((rslt stax) ; If no rule found, default to no change.
        smpl sqrx rslt0 rslt1 rslt2)
    (loop for rulsx in (action-base-rules actx) do

        (when (region-superset-of-state (rule-initial-region (rulestore-nth rulsx 0)) stax) ; All rules in a rulestore should have the same initial region.

           (cond ((= 1 (rulestore-length rulsx))
                  (setf rslt (rule-result-from-state (rulestore-nth rulsx 0) stax)))

                 ((= 2 (rulestore-length rulsx))
                   (setf sqrx (action-find-square actx stax))
                   (cond (sqrx
                           (setf rslt0 (rule-result-from-state (rulestore-nth rulsx 0) stax))
                           (if (state-eq rslt0 (square-most-recent-result sqrx))
                               (setf rslt (rule-result-from-state (rulestore-nth rulsx 1) stax))
                               (setf rslt rslt0))
                         )
                         (t (setf rslt (rule-result-from-state (rulestore-nth rulsx (random 2)) stax)))
                   )
                 )
                 ((= 3 (rulestore-length rulsx))
                   (setf sqrx (action-find-square actx stax))
                   (cond (sqrx
                          (setf rslt0 (rule-result-from-state (rulestore-nth rulsx 0) stax))
                          (setf rslt1 (rule-result-from-state (rulestore-nth rulsx 1) stax))
                          (setf rslt2 (rule-result-from-state (rulestore-nth rulsx 2) stax))
                          (cond ((state-eq rslt0 (square-most-recent-result sqrx))
                                 (setf rslt (rule-result-from-state (rulestore-nth rulsx 1) stax)))
                                ((state-eq rslt1 (square-most-recent-result sqrx))
                                 (setf rslt (rule-result-from-state (rulestore-nth rulsx 2) stax)))
                                ((state-eq rslt2 (square-most-recent-result sqrx))
                                 (setf rslt (rule-result-from-state (rulestore-nth rulsx 0) stax)))
                           ))
                         (t
                            (setf rslt (rule-result-from-state (rulestore-nth rulsx (random 3)) stax))
                         )
                   )
                )
           )
        )
    )

    (setf smpl (sample-new :initial stax :result rslt))
    (format t "~&Act: ~D Sample: ~A" (action-id actx) (sample-str smpl))
    smpl
  )
)

;;; Check a group for pnc change.
(defun action-check-group-pnc (actx grpx) ; side-effect, group pnc may be changed.
  (assert (action-p actx))
  (assert (group-p grpx))

  (let ((pnc t) sqrx)
    (if (> (region-number-states (group-region grpx)) 2)
      (setf pnc nil)
      (progn
        (loop for stax in (statestore-states (region-states (group-region grpx))) do
          (setf sqrx (squarestore-find (action-squares actx) stax))
          (if sqrx
            (if (not (square-pnc sqrx))
              (setf pnc nil))
            (error "Group region state square not found?")
          )
        )
      )
    )
    (if (xor pnc (group-pnc grpx))
      (group-set-pnc grpx pnc))
  )
)

;;; Add a new square, from only one place in action.lisp.
;;; To support additional logic.
;;; Presumably, Pn == *pn-one*, pnc == nil.
;;; Invalidated groups may be nil, that is not yet checked for, or otherwise a groupstore, which may be empty. 
(defun action-new-square (actx sqrx invalidated-groups) ; -> side effect, action instance is changed.
  (format t "~&action-new-square: Act ~D adding ~A" (action-id actx) (square-str sqrx))
  (assert (action-p actx))
  (assert (square-p sqrx))
  (assert (or (null invalidated-groups) (groupstore-p invalidated-groups)))

  ;; Check for overwrite.
  (if (squarestore-find (action-squares actx) (square-state sqrx))
    (error "Readding a square?"))

  ;; Add the square.
  (squarestore-add (action-squares actx) sqrx)

  ;; Process added square.

  ;; Check if any groups are invalidated by the square.
  (if (null invalidated-groups)
    (setf invalidated-groups (groupstore-groups-invalidated-by-square (action-groups actx) sqrx)))

  ;; Check for invalidated groups.
  (if (groupstore-is-not-empty invalidated-groups)
    (action-process-invalidated-groups actx invalidated-groups))

  ;; Make groups from square, or at least a one-square group.
  (if (not (groupstore-state-in (action-groups actx) (square-state sqrx)))
    (action-make-groups-from-square actx sqrx))
)

;;; Process a square that changed (pn, pnc) due to a new sample.
(defun action-process-changed-square (actx sqrx) ; -> side effect, action instance is changed.
  (assert (action-p actx))
  (assert (square-p sqrx))

  ;; Check for group changes.
  (loop for grpx in (groupstore-groups (action-groups actx)) do
    (if (region-superset-of-state (group-region grpx) (square-state sqrx))
       (if (statestore-member (region-states (group-region grpx)) (square-state sqrx))
          (action-check-group-pnc actx grpx))
    )
  )

  ;; Check for invalidated groups.
  (let (invalidated-groups)
     (setf invalidated-groups (groupstore-groups-invalidated-by-square (action-groups actx) sqrx))
     (if (groupstore-is-not-empty invalidated-groups)
       (action-process-invalidated-groups actx invalidated-groups))
  )

  ;; Make groups from square, or at least a one-square group.
  (if (not (groupstore-state-in (action-groups actx) (square-state sqrx)))
    (action-make-groups-from-square actx sqrx))
)

;;; Take an action, for a given state, required for a need.
;;; An existitg square will be updated.
;;; For a need, it is assumed that a new square will be created if needed.
(defun action-take-sample-for-need (actx stax need) ; -> sample,  side effect, action instance is changed.
  ;(format t "~&action-take-sample-for-need: ~A ~A" (type-of actx) (type-of stax)) 
  (assert (action-p actx))
  (assert (state-p stax))
  (assert (need-p need))
  ;(format t "~&action-take-sample-for-need: need: ~A" (need-str need))

  (let (smpl sqrx grpx sqry)
    (setf smpl (action-get-sample actx stax))

    ;; Update, or add, square.
    (setf sqrx (action-find-square actx stax))
    (if sqrx
        (if (action-add-square-sample actx sqrx smpl)
          (action-process-changed-square actx sqrx)
        )
        (action-new-square actx (square-new smpl) nil)
    )

    ;; Check group pnc and region, if gt 2 states.
    (cond ((= (need-reason need) *confirm-group*)
             (setf sqrx (action-find-square actx stax))
             (if (null sqrx) (error "action-take-sample-for-need: sqrx not found?"))
             (when (or (pn-eq (square-pn sqrx) *pn-one*) (square-pnc sqrx))
               (setf grpx (groupstore-find (action-groups actx) (need-group-region need)))
               ;; Change group region.
               (cond ((null grpx) nil) ; Running a plan to satisfy the need, can change the groupstore.
                     ((group-pnc grpx) 
                       (format t "Problem: Group pnc: ~A" (group-str grpx))
                     )
                     ((= (region-number-states (group-region grpx)) 1)
                      ;; Group probably set to pnc before this.
                      (if (square-pnc sqrx)
                        (group-set-pnc grpx true))
                     )
                     ((= (region-number-states (group-region grpx)) 2)
                        (when (square-pnc sqrx)
                          (when (state-eq (square-state sqrx) (region-first-state (group-region grpx)))
                            (setf sqry (action-find-square actx (region-second-state (group-region grpx))))
                            (if (null sqry) (error "action-take-sample-for-need: sqry not found?"))
                          )
                          (when (state-eq (square-state sqrx) (region-second-state (group-region grpx)))
                            (setf sqry (action-find-square actx (region-first-state (group-region grpx))))
                            (if (null sqry) (error "action-take-sample-for-need: sqry not found?"))
                          )
                          (if (and (square-pnc sqry) (square-pnc sqrx))
                            (group-set-pnc grpx true))
                        )
                     )
                     (t ; Number states GT 2.
                       (when (state-eq (square-state sqrx)
                                       (region-far-state (group-region grpx) (region-first-state (group-region grpx))))

                         (group-set-region grpx (region-new (list (region-first-state (group-region grpx)) (square-state sqrx))))
                         (when (square-pnc sqrx)
                           (setf sqry (action-find-square actx (region-first-state (group-region grpx))))
                           (if (and (square-pnc sqry) (square-pnc sqrx))
                              (group-set-pnc grpx true))
                         )
                       )
                     )
                 )
             )
           )
    )
    smpl
  )
)

;;; Take an action, for a given state, arbitrarily.
;;; An existitg square will be updated.
;;; For a need, it is assumed that a new square will be created if needed.
(defun action-take-sample-arbitrary (actx stax) ; -> sample,  side effect, action instance is changed.
  ;(format t "~&action-take-sample-arbitrarily: ~A ~A" (type-of actx) (type-of stax)) 
  (assert (action-p actx))
  (assert (state-p stax))

  (let (smpl sqrx)
    (setf smpl (action-get-sample actx stax))

    ;; Update, or add, square.
    (setf sqrx (action-find-square actx stax))
    (if sqrx
        (if (action-add-square-sample actx sqrx smpl)
          (action-process-changed-square actx sqrx)
        )
        (action-new-square actx (square-new smpl) nil)
    )
    smpl
  )
)

;;; Take an action, for a given state, required for a step.
;;; An existitg square will be updated.
;;; When a step works, in most cases, its unnecessary to create a new square.
(defun action-take-sample-for-step (actx stax) ; -> sample, side effect, action instance is changed.
  ;(format t "~&action-take-sample-for-step: ~A ~A" (type-of actx) (type-of stax)) 
  (assert (action-p actx))
  (assert (state-p stax))

  (let (smpl sqrx invalidated-groups)
    (setf smpl (action-get-sample actx stax))

    ;; If a square exists, update it. If it changed, process changed square.
    ;; If a square does not exist, look for invalidated groups.
    ;;   If the sample invalidated a group, add it as a new square.
    (setf sqrx (action-find-square actx stax))
    (if sqrx
      (if (action-add-square-sample actx sqrx smpl)
        (action-process-changed-square actx sqrx))

      (progn
        (setf invalidated-groups (groupstore-groups-invalidated-by-sample (action-groups actx) smpl))
        (if (groupstore-is-not-empty invalidated-groups)
          (action-new-square actx (square-new smpl) invalidated-groups))
      )
    )
    smpl
  )
)

;;; Process groups invalidated by a square, or sample.
(defun action-process-invalidated-groups (actx invalidated-groups) ; side-effect, action changed.
  (format t "~&action-process-invalidated-groups: Act ~D groups ~A" (action-id actx) (groupstore-str invalidated-groups))
  (assert (action-p actx))
  (assert (groupstore-p invalidated-groups))

  ;; Remove the groups.
  (loop for grpx in (groupstore-groups invalidated-groups) do
    (setf (action-groups actx) (groupstore-remove-group (action-groups actx) grpx))
  )

  ;; Proccess orphaned squares.
  (let (sqrs)
    ;; Find orphaned squares.
    (loop for sqrx being the hash-values of (squarestore-squares (action-squares actx)) do
      (if (and (or (pn-eq (square-pn sqrx) *pn-one*) (square-pnc sqrx))
               (not (groupstore-state-in-group (action-groups actx) (square-state sqrx)))
               (not (member sqrx sqrs)))
          (push sqrx sqrs))
    )
    ;; Try to form groups from squares.
    (if sqrs
      (action-make-groups-from-squares actx sqrs)
    )
  )
)

;;; Process orphaned squares into groups.
(defun action-make-groups-from-squares (actx sqrs) ; side-effect, action changed.
  ;(format t "~&action-make-groups-from-squares: Act ~D ~A" (action-id actx) (mapcar #'(lambda (x) (state-str (square-state x))) sqrs))
  (assert (action-p actx))
  (assert (square-list-p sqrs))

  (loop for sqrx in sqrs do
    ;; Check if a previously processed square created a group encompassing this square.
    (if (not (groupstore-multistate-groups-state-in (action-groups actx) (square-state sqrx)))
      (action-make-groups-from-square actx sqrx)
    )
  )
)

;;; Process new, or orphaned square into groups.
(defun action-make-groups-from-square (actx sqrx) ; side-effect, action changed.

  ;; Check sample initial state square combination with other squares.
  (let ((keys (squarestore-keys (action-squares actx))) reg-t (regstr-t (regionstore-new nil)) sqr-k grpx
         (stax (square-state sqrx)))

    ;; Find compatible squares, test squares between them, add the square-pair regions to a store, no subsets.
    (loop for stay in (statestore-state-list keys) do

       (when (state-ne stay stax)

         (setf sqr-k (action-find-square actx stay))
         ;(format t "~&checking sqr ~A and ~A compatible ~A" (state-str (square-state sqrx)) (state-str (square-state sqr-k)) (square-compatible sqrx sqr-k))
           
         (when (square-compatible sqrx sqr-k)

           (setf reg-t (region-new (list stax (square-state sqr-k))))

           (if (squarestore-region-is-valid (action-squares actx) reg-t)
              (regionstore-push-nosubs regstr-t reg-t))
         )
       )
    ) ; next stay

   ; Combine regions, if needed and possible.
   (when (> (regionstore-length regstr-t) 1)
      (setf regstr-t (action-combine-regions actx regstr-t))
   )

   ; Process regions in regstr_t
   ;(format t "~&Largest regions are: ~A" (regionstore-str regstr-t))
   (loop for regx in (regionstore-regions regstr-t) do
     (setf grpx (action-make-group actx regx))
     (format t "~&Act: ~D Adding group: ~A" (action-id actx) (group-str grpx))
     (groupstore-push-nosubs (action-groups actx) grpx)
   )
    ;; Create a one-state group.
    (if (and (regionstore-is-empty regstr-t) (not (groupstore-state-in (action-groups actx) stax)))
      ;(format t "~&Act: ~D Groups: ~A" (action-id actx) (groupstore-str (action-groups actx)))
      (let ((grpx (action-make-group actx (region-new stax))))            
        (format t "~&Act: ~D Adding group: ~A" (action-id actx) (group-str grpx))
        (groupstore-add-end (action-groups actx) grpx)
        ;(format t "~&Act: ~D Groups: ~A" (action-id actx) (groupstore-str (action-groups actx)))
      )
    )
  )
)

;;; Find a square.
(defun action-find-square (actx stax) ; -> square, or nil.
  (assert (action-p actx))
  (assert (state-p stax))

  (squarestore-find (action-squares actx) stax)
)


;;; Add a sample to a square, from only one place in action.lisp.
;;; To support additional logic.
(defun action-add-square-sample (actx sqrx smpl) ; -> bool, true if a pn, or pnc, change happens.
  ;(format t "~&action-add-square-sample: Act ~D square ~A sample ~A" (action-id actx) (square-str sqrx) (sample-str smpl))
  (assert (action-p actx))
  (assert (square-p sqrx))
  (assert (sample-p smpl))

  (let (cng invalidated-groups)
    ;; Add sample to square.
    (setf cng (square-add-sample sqrx smpl))

    (when cng 
      ;; If square pn, or pnc, changed, check for invalidated groups.
      (setf invalidated-groups (groupstore-groups-invalidated-by-square (action-groups actx) sqrx))

      (if (groupstore-is-not-empty invalidated-groups)
        ;; Process invalidated groups.
        (action-process-invalidated-groups actx invalidated-groups))
    )
    cng
  )
)

;;; Combine possible regions of similar squares, if possible.
(defun action-combine-regions (actx regsx) ; -> RegionStore instance.
  ;(format t "~&action-combine-regions: Act ~D regions ~A" (action-id actx) (type-of regsx))
  (assert (action-p actx))
  (assert (regionstore-p regsx))

  (let ((cur-regs (regionstore-new nil)) ; The current regionstore.
        (nxt-regs regsx)                 ; The next regionstore, with combined regions from the current regionstore.
        regx regy                        ; Temp regions.
       )
    ;; Test possible combinations
    (loop while (and (> (regionstore-length nxt-regs) 1) (/= (regionstore-length cur-regs) (regionstore-length nxt-regs))) do

      (setf cur-regs nxt-regs)
      (setf nxt-regs (regionstore-new nil))

      ;; Tst all possible pairs of regions.
      (loop for inx from 0 below (1- (regionstore-length cur-regs)) do                                                      

        (setf regx (nth inx (regionstore-regions cur-regs)))

        (loop for iny from (1+ inx) below (regionstore-length cur-regs) do    
           
            ;(format t "~&checking reg ~A and ~A" (region-str regx) (region-str (nth iny (regionstore-regions cur-regs))))

            (setf regy (region-new (statestore-append
                (region-states regx) (region-states (nth iny (regionstore-regions cur-regs))))))

            (if (squarestore-region-is-valid (action-squares actx) regy)
               (regionstore-push nxt-regs regy))
        ) ; end loop 3, next iny.

      ) ; end loop 2, next inx.

      ;; Add uncombined regions in cur-regs to nxt-regs.
      (loop for regx in (regionstore-regions cur-regs) do
          (regionstore-push-nosubs nxt-regs regx)
      )
    ) ; end loop 1, process nxt-regs.
    nxt-regs
  )
)

;;; Return a group.
;;; The region states represent sampled, compatible, pn-equal states.
(defun action-make-group (actx region) ; -> group instance.
  ;(format t "~&action-make-group: Act ~D region ~A" (action-id actx) (region-str region))
  (assert (action-p actx))
  (assert (region-p region))

  (let (pn (pnc (< (region-number-states region) 3)) sqrx (rules (rulestore-new nil)))
    ;(format t "~&action-make-group: pnc ~A num states ~D" pnc (region-number-states region))
    ;; Check region states.
    (loop for stax in (region-state-list region) do
       ;; Get square from region state.
       (setf sqrx (action-find-square actx stax))
       (if (null sqrx) (error "Square ~A for region state not found?" (state-str stax)))

       (setf pnc (and pnc (square-pnc sqrx)))

       (if pn
          (assert (pn-eq pn (square-pn sqrx)))
          (setf pn (square-pn sqrx)))

       (when (pn-ne pn *pn-none*)
         (if (rulestore-is-empty rules)
           (setf rules (square-rules sqrx))
           (setf rules (rulestore-union rules (square-rules sqrx))))

         (if (null rules) (error "Rulestore union failed"))
       )
    )

    ;(format t "~&action-make-group: Act ~D region ~A returning ~A" (action-id actx) (region-str region) (group-str (group-new region pn pnc rules)))
    (group-new region pn pnc rules)
  )
)

;;; Print an action.
(defun action-print (actx)
  (assert (action-p actx))

  (format t "Act: ~D " (action-id actx))
  (if (groupstore-is-empty (action-groups actx))
      (format t "(no groups)") 
      (groupstore-print (action-groups actx)))
; (format t "~&   base rules: " (action-base-rules actx))
; (loop for rulsx in (action-base-rules actx) do
;   (format t " ~A" (rulestore-str rulsx))
; )
)

