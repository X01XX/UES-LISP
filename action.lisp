
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
  (assert (action-p actx))
  (assert (state-p cur-state))

  (let ((needs (needstore-new nil)))
    needs
  )
)

;;; Return a action from a string.
;;; Thi action ID defaulst to zero, the caller may need to set it.
(defun action-from (symbols) ; -> action
    (format t "~&action-from: ~A" symbols)
    (assert (listp symbols))

    ;(assert (eq (car symbols) 'QUOTE))
    ;(setf symbols (second symbols))

    (assert (eq (car symbols) 'ACT))
    (setf symbols (cdr symbols))

    (let (rulestores pos sname)
        (loop for tokx in symbols do
            (format t "~&action-from ~A ~A" (type-of tokx) tokx)
            (cond ((symbolp tokx)
                   (setf sname (symbol-name tokx))
                   (setf pos (position #\/ sname))
                   (if pos
                       (format t "~&state found ~A sample ~D times" (subseq sname 0 pos)
                                (read-from-string (subseq sname (1+ pos))))
                       (format t "~&state found ~A" tokx)
                  ))
                  (t (push (rulestore-from tokx) rulestores)))
        )
        (action-new :id 0 :rules (reverse rulestores))
    )
)
