; Implement a store of actions.

(defvar true t)
(defvar false nil)

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
(defun actionstore-new (actions) ; -> actionstore instance.
  ;(format t "~&actions ~A" actions)
  (assert (action-list-p actions))

  (let ((ret (make-actionstore :actions nil)))
    (loop for actx in actions do 
      (if (not (actionstore-contains ret actx))
        (actionstore-push ret actx))
    )
    ret
  )
)

; Push a new action into a actionstore, suppress dups, subsets.
; Return true if the action has been added.
(defun actionstore-push(storex actx) ; -> bool.
  (assert (actionstore-p storex))
  (assert (action-p actx))

  (let ((ret (actionstore-push-na storex actx)))
     (cond ((err-p ret) (error (err-str ret)))
           ((bool-p ret) ret)
            (t (error "Result is not a bool"))))
)

(defun actionstore-push-na(storex actx) ; -> bool, or err.
  ; Check for equal actions.
  (loop for acty in (actionstore-actions storex) do
    (if (= (action-id acty) (action-id actx))
      (return-from actionstore-push-na (err-new "duplicate action id")))
  )

  ; Add the new action to the end of the actions list.
  (if (null (actionstore-actions storex))
    (push actx (actionstore-actions storex))
    (push actx (cdr (last (actionstore-actions storex))))) 

  true
)

; Return the number of actions in a actionstore.
(defun actionstore-length (storex) ; -> number.
  (assert (actionstore-p storex))

  (length (actionstore-actions storex))
)

; Return true if a actionstore is empty.
(defun actionstore-is-empty (storex) ; -> bool
  (zerop (actionstore-length storex))
)

; Return true if a actionstore is not empty.
(defun actionstore-is-not-empty (storex) ; -> bool
  (plusp (actionstore-length storex))
)

; Return a string representing a actionstore.
(defun actionstore-str (storex) ; -> string.
  (assert (actionstore-p storex))

  (let ((ret "#S(ACTIONSTORE ") (start t))

    (loop for actx in (actionstore-actions storex) do
      (if start (setf start nil) (setf ret (concatenate 'string ret ", ")))    

      (setf ret (concatenate 'string ret (format nil " ~&  ~A" (action-str actx))))
    )

    ret
  )
)

; Return true if a actionstore contains a given action.
(defun actionstore-contains (storex actx) ; -> bool
  (assert (actionstore-p storex))
  (assert (action-p actx))

  (if (member actx (actionstore-actions storex) :test #'action-eq) true false)
)

(defun actionstore-first-action (storex) ; -> action
  (assert (actionstore-p storex))
  (assert (actionstore-is-not-empty storex))

  (car (actionstore-actions storex))
)

;  Return possible steps, given a rule.
(defun actionstore-get-steps (storex rule) ; -> stepstore.
  (assert (actionstore-p storex))
  (assert (rule-p rule))
  
  (let ((ret-steps (stepstore-new nil)) act-steps)
    (loop for actx in (actionstore-actions storex) do
      (setf act-steps (action-get-steps actx rule))
      (loop for stpx in (stepstore-steps act-steps) do
        (stepstore-push ret-steps stpx) 
      )
    )
    ret-steps
  )
)

