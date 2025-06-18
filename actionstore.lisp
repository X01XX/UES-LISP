; Implement a store of actions.
(defstruct actionstore
  actions  ; A list of zero, or more, non-duplicate, same number bits, actions.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (actionstore-<field name> <instance>) -> struct field.
;   (actionstore-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> actionstore
;   (typep <instance> 'actionstore) -> bool
;
; Probably shouldn't use:
;   (make-actionstore [:<field-name> <field-actionstore>]*), use actionstore-new instead.
;   (copy-actionstore <instance>) copies a actionstore instance.
(defun actionstore-new (actions) ; -> actionstore.
  ;(format t "~&actions ~A" actions)
  (assert (listp actions))

  (let ((ret (make-actionstore :actions nil)))
    (loop for actx in actions do
        (assert (action-p actx))
        (actionstore-push ret actx)
    )
    ret
  )
)

; Push a new action into a actionstore, suppress dups, subsets.
; Return true if the action has been added.
(defun actionstore-push (storex actx) ; -> sied effect, actionstore changed.
  (assert (actionstore-p storex))
  (assert (action-p actx))

  (setf (action-id actx) (length (actionstore-actions storex)))

  ; Add the new action.
  (setf (actionstore-actions storex) (append (actionstore-actions storex) (list actx)))
)

; Return the number of actions in a actionstore.
(defun actionstore-length (storex) ; -> number.
  (assert (actionstore-p storex))

  (length (actionstore-actions storex))
)

; Return true if a actionstore is empty.
(defun actionstore-is-empty (storex) ; -> bool
  (assert (actionstore-p storex))

  (zerop (actionstore-length storex))
)

; Return true if a actionstore is not empty.
(defun actionstore-is-not-empty (storex) ; -> bool
  (assert (actionstore-p storex))

  (plusp (actionstore-length storex))
)

; Return a string representing a actionstore.
(defun actionstore-str (storex) ; -> string.
  (assert (actionstore-p storex))

  (let ((ret "(AS ") (start t))

    (loop for actx in (actionstore-actions storex) do
      (if start (setf start nil) (setf ret (concatenate 'string ret " ")))

      (setf ret (concatenate 'string ret (format nil " ~&  ~A" (action-str actx))))
    )

    ret
  )
)

;  Return possible steps, given a rule.
(defun actionstore-get-steps (storex rule-from-to within &optional no-alt) ; -> stepstore.
  ;(format t "~&actionstore-get-steps")
  (assert (actionstore-p storex))
  (assert (rule-p rule-from-to))
  (assert (region-p within))

  (let ((ret-steps (stepstore-new nil)) act-steps)
    (loop for actx in (actionstore-actions storex) do
      (setf act-steps (action-get-steps actx rule-from-to within no-alt))
      (loop for stpx in (stepstore-steps act-steps) do
        (stepstore-push ret-steps stpx)
      )
    )
    ret-steps
  )
)

;;; Return needs for all actions.
(defun actionstore-get-needs (actsx cur-state reachable) ; ->  needstore.
  ;(format t "~&actionstore-get-needs: ~A ~A" (type-of actsx) (type-of cur-state))
  (assert (actionstore-p actsx))
  (assert (state-p cur-state))
  (assert (regionstore-p reachable))

  (let ((needs (needstore-new nil)))
    (loop for actx in (actionstore-actions actsx) do
      (if (not (zerop (action-id actx)))
        (setf needs (needstore-append needs (action-get-needs actx cur-state reachable))))
    )
    ;(format t "~&actionstore-get-needs: returning ~A" (needstore-str needs))
    needs
  )
)

;;; Return the nth element of a ActionStore.
(defun actionstore-nth (storex inx) ; -> action instance, or nil.
  (assert (actionstore-p storex))
  (assert (integerp inx))

  (if (>= inx (actionstore-length storex))
    (return-from actionstore-nth nil))

  (nth inx (actionstore-actions storex))
)

(defun actionstore-print (storex)
  (assert (actionstore-p storex))

  (loop for actx in (actionstore-actions storex) do
    ;(format t "~&    ")
    (action-print actx)
  )
)

;;; Return change surface.
(defun actionstore-changes (storex) ; -> change.
  (assert (actionstore-p storex))

  (let ((ret (change-new :m01 (action-mask-new-low (car (actionstore-actions storex)))
                         :m10 (action-mask-new-low (car (actionstore-actions storex))))))
    (loop for actx in (actionstore-actions storex) do
      (setf ret (change-or ret (action-changes actx)))
    )
    ;(format t "~&actionstore-changes: ~A" (regionstore-str ret))
    ret
  )
)

;;; Return true if an action id is within bounds.
(defun actionstore-valid-id (storex id) ; -> bool
  (assert (actionstore-p storex))
  (assert (integerp id))
  (assert (>= id 0))

  (< id (actionstore-length storex))
)

