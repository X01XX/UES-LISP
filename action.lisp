
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
  (assert (rulestore-list-p rules))
  (assert (>= id 0))

  (let (rulsx rulsy)

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
      (setf rulsx (nth inx (rules-region-list rules)))

      (loop for iny from (1+ inx) below (length rules) do                                             
        (setf rulsy (nth iny (rules-region-list rules)))

        (when (region-intersects (rulestore-initial-region rulsx) (rulestore-initial-region rulsy))
          (if (null (rulestore-intersection rulsx rulsy))
              (error "invalid intersection of rulestores"))
        )
      )
    )

    (make-action :id id :groups (groupstore-new nil) :squares (squarestore-new) :base-rules rules)
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

(defun action-get-needs (actx cur-state) ; -> NeedStore.
  ;(format t "~&action-get-needs: ~A ~A" (type-of actx) (type-of cur-state))
  (assert (action-p actx))
  (assert (state-p cur-state))

  (let ((needs (needstore-new nil)))
    (if (not (groupstore-state-in-group (action-groups actx) cur-state))
        (needstore-push needs (action-get-need-sample-state actx cur-state *state-not-in-group*))
    )
    ;(format t "~&action-get-needs: returning: ~A" (needstore-str needs))
    needs
  )
)

;;; Return an action instance, given a list of symbols.
;;; Thi action ID defaulst to zero, the caller may need to set it.
(defun action-from (symbols) ; -> action
    ;(format t "~&action-from: ~A" (type-of symbols))
    (assert (listp symbols))
    (assert (not (null symbols)))
    (assert (symbolp (car symbols)))
    (assert (eq (car symbols) 'ACT))

    (setf symbols (cdr symbols))

    (let (rulestores pos sname)
        (loop for tokx in symbols do
            ;(format t "~&action-from ~A ~A" (type-of tokx) tokx)
            (cond ((symbolp tokx)
                   (setf sname (symbol-name tokx))
                   (setf pos (position #\/ sname))
                   ;(if pos
                   ;    (format t "~&state found ~A sample ~D times" (subseq sname 0 pos)
                   ;             (read-from-string (subseq sname (1+ pos))))
                   ;    (format t "~&state found ~A" tokx)
                   ;)
                  )
                  (t (push (rulestore-from-str tokx) rulestores)))
        )
        (action-new :id 0 :rules (reverse rulestores))
    )
)

;;; Get a need to sample a state, after some checks.
(defun action-get-need-sample-state (actx stax reason) ; -> need instance.
  (assert (action-p actx))
  (assert (state-p stax))
  (assert (integerp reason))

  ; If a square exists with this state, call action-get-need-resample-state.
  (if (squarestore-find (action-squares actx) stax)

    (action-get-need-resample-state actx stax reason)

    (need-new :act-id (action-id actx)
              :kind *first-sample-of-state*
              :reason reason
              :target stax)
  )
)

;;; Get a need to resample a state, after some checks.
(defun action-get-need-resample-state (actx stax reason) ; -> need instance.
  (assert (action-p actx))
  (assert (state-p stax))
  (assert (integerp reason))

  ; A square must exist with this state, and it must be non-pnc.
  (let ((sqrx (squarestore-find (action-squares actx) stax)))

    (if (null sqrx)
      (error "action-get-need-resample-state: square not found?"))

    (if (square-pnc sqrx)
      (error "action-get-need-resample-state: square pnc is true?"))

    (need-new :act-id (action-id actx)
              :kind *resample-state*
              :reason reason
              :target stax)
  )
)

;;; Get a need to sample a region, after some checks.
(defun action-get-need-sample-region (actx regx reason) ; -> need instance.
  (assert (action-p actx))
  (assert (region-p regx))
  (assert (integerp reason))

  ; There must be no square with a state in the region.
  (if (squarestore-any-in (action-squares actx) regx)
      (error "action-get-need-sample-region: squares in region?"))

  (need-new :act-id (action-id actx)
            :kind *sample-in-region*
            :reason reason
            :target regx)
)

;;; Take an action, for a given state, process the result.
(defun action-take-sample (actx stax) ; -> side effoct, action changed.
  ;(format t "~&action-take-sample: ~A ~A" (type-of actx) (type-of stax)) 
  (let (rslt smpl)
    (loop for rulsx in (action-base-rules actx) do
        (when (region-superset-of-state (rule-initial-region (rulestore-nth rulsx 0)) stax)
           (when (= 1 (rulestore-length rulsx))
               (setf rslt (rule-result-from-state (rulestore-nth rulsx 0) stax))
               (setf smpl (sample-new :initial stax :result rslt))
               (format t "~&Act: ~D Sample: ~A" (action-id actx) (sample-str smpl))
               ; TODO process the sample.
               (return-from action-take-sample)
           )
        )
    )
    (setf smpl (sample-new :initial stax :result stax))
    (format t "~&Act: ~D Sample: ~A" (action-id actx) (sample-str smpl))
    
  )
)
