; Implement a store of groups.

(defvar true t)
(defvar false nil)

; Implement a store of groups.
(defstruct groupstore
  groups  ; A list of zero, or more, non-duplicate, same number bits, groups.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (groupstore-<field name> <instance>) -> struct field.
;   (groupstore-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> groupstore
;   (typep <instance> 'groupstore) -> bool
;
; Probably shouldn't use:
;   (make-groupstore [:<field-name> <field-groupstore>]*), use groupstore-new instead.
;   (copy-groupstore <instance>) copies a groupstore instance.
(defun groupstore-new (groups) ; -> groupstore instance.
  ;(format t "~&groups ~A" groups)
  (let ((ret (make-groupstore :groups nil)))
    (loop for grpx in groups do 
      (groupstore-push ret grpx)
    )
    ret
  )
)

; Push a new group into a groupstore, suppress dups, subsets.
; Return true if the group has been added.
(defun groupstore-push(storex groupx) ; -> bool.
  (assert (groupstore-p storex))
  (assert (group-p groupx))

  (if (groupstore-is-not-empty storex)
    (assert (= (group-num-bits groupx) (group-num-bits (groupstore-first storex)))))

  (if (groupstore-contains storex groupx)
    (return-from groupstore-push false))

  (let (del-grps)

    ; Check for equal, superset and subset groups.
    (loop for grpx in (groupstore-groups storex) do
      (if (region-subset :sub-reg (group-region groupx) :sup-reg (group-region grpx))
        (return-from groupstore-push false))

      (if (region-subset :sup-reg (group-region groupx) :sub-reg (group-region grpx))
        (push grpx del-grps))
    )

    ; Delete subset groups, if any.
    (loop for grpx in del-grps do
        (remove grpx (groupstore-groups storex) :test #'group-eq)
    )

    ; Add the new group to the end of the groups list, old survivors migrate to the beginning of the list.
    (if (null (groupstore-groups storex))
      (push groupx (groupstore-groups storex))
      (push groupx (cdr (last (groupstore-groups storex))))) 
  )
  true
)

; Return the number of groups in a groupstore.
(defun groupstore-length (storex) ; -> number.
  (assert (groupstore-p storex))

  (length (groupstore-groups storex))
)

; Return true if a groupstore is empty.
(defun groupstore-is-empty (storex) ; -> bool
  (zerop (groupstore-length storex))
)

; Return true if a groupstore is not empty.
(defun groupstore-is-not-empty (storex) ; -> bool
  (plusp (groupstore-length storex))
)

; Return a string representing a groupstore.
(defun groupstore-str (storex) ; -> string.
  (assert (groupstore-p storex))

  (let ((ret "#S(GROUPSTORE ") (start t))

    (loop for grpx in (groupstore-groups storex) do
      (if start (setf start nil) (setf ret (concatenate 'string ret ", ")))    

      (setf ret (concatenate 'string ret (format nil " ~&    ~A" (group-str grpx))))
    )

    ret
  )
)

; Return true if a groupstore contains a given group.
(defun groupstore-contains (storex stax) ; -> bool
  (assert (groupstore-p storex))
  (assert (group-p stax))

  (if (member stax (groupstore-groups storex) :test #'group-eq) true false)
)

(defun groupstore-first (storex) ; -> group
  (assert (groupstore-p storex))
  (assert (groupstore-is-not-empty storex))

  (car (groupstore-groups storex))
)

; Return possible steps to satisfy a rule.
(defun groupstore-get-steps (storex rule) ; -> stepstore.
  ;(format t "~&groupstore-get-steps")
  (let ((ret-steps (stepstore-new nil)) steps)
    (loop for grpx in (groupstore-groups storex) do
        (setf steps (group-get-steps grpx rule))
	(loop for stpx in (stepstore-steps steps) do
	  (if (not (stepstore-contains ret-steps stpx))
	    (stepstore-push ret-steps stpx)
	  )	
	)
    )
    ret-steps
  )
)

