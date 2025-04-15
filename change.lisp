;;;; Implement a change struct and functions.

;;; The change struct.
(defstruct change
  m01  ; 0->1 mask.
  m10  ; 1->0 mask.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (change-<field name> <instance>) -> struct field.
;   (change-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> change
;   (typep <instance> 'change) -> bool
;
; Probably shouldn't use:
;   (make-change [:<field-name> <field-change>]*), use change-new instead.
;   (copy-change <instance>) copies a change instance.

;;; Return a new change.
(defun change-new (&key m01 m10) ; -> change.
  (assert (mask-p m01))
  (assert (mask-p m10))
  (assert (= (mask-num-bits m01) (mask-num-bits m10)))

  (make-change :m01 m01 :m10 m10)
)

;;; Return a string for a change.
(defun change-str (cngx) ; -> string
  (change-p cngx)

  (let ((str "(0->1 "))
    (setf str (concatenate 'string str (mask-str (change-m01 cngx))))
    (setf str (concatenate 'string str ", 1->0 "))
    (setf str (concatenate 'string str (mask-str (change-m10 cngx))))
    (setf str (concatenate 'string str ")"))
    str
  )
)

;;; Return the number of bit positions set to one.
(defun change-num-changes (cngx) ; -> integer
  (change-p cngx)

  (+ (mask-num-ones (change-m01 cngx))
     (mask-num-ones (change-m10 cngx)))
)

;;; Return true if there is at least one bit set to one in a change.
(defun change-is-not-low (cngx) ; -> bool.
  (change-p cngx)

  (if (or (mask-is-not-low (change-m01 cngx))
          (mask-is-not-low (change-m10 cngx)))
    true
    false)
)

;;; Return true if there is no bit set to one in a change.
(defun change-is-low (cngx) ; -> bool.
  (change-p cngx)

  (if (and (mask-is-low (change-m01 cngx))
           (mask-is-low (change-m10 cngx)))
    true
    false)
)

;;; Return the number of bits used in change masks.
(defun change-num-bits (cngx) ; -> integer.
  (change-p cngx)

  (mask-num-bits (change-m01 cngx))
)

;;; Return a list of changes containing only one bit from a change.
(defun change-split (cngx) ; -> list of changes.
  (change-p cngx)

  (let (ret-lst m01 m10 (num-bits (change-num-bits cngx)))
    (setf m01 (mask-split (change-m01 cngx)))
    (loop for bitx in m01 do
      (push (change-new :m01 bitx :m10 (mask-new (value-new :num-bits num-bits :bits 0))) ret-lst)
    )

    (setf m10 (mask-split (change-m10 cngx)))
    (loop for bitx in m10 do
      (push (change-new :m10 bitx :m01 (mask-new (value-new :num-bits num-bits :bits 0))) ret-lst)
    )
    ret-lst
  )
)

;;; Return true if two changes ar equal.
(defun change-eq (cng1 cng2) ; -> bool.
  (change-p cng1)
  (change-p cng2)
  (assert (= (change-num-bits cng1) (change-num-bits cng2)))

  (and (mask-eq (change-m01 cng1) (change-m01 cng2))
       (mask-eq (change-m10 cng1) (change-m10 cng2)))
)

;;; Return the boolean and of two changes.
(defun change-and (cng1 cng2) ; -> change
  (change-p cng1)
  (change-p cng2)
  (assert (= (change-num-bits cng1) (change-num-bits cng2)))

  (change-new :m01 (mask-new-and (change-m01 cng1) (change-m01 cng2))
              :m10 (mask-new-and (change-m10 cng1) (change-m10 cng2)))
)

;;; Return the inverse of a change.
(defun change-not (cngx) ; -> change
  (change-p cngx)

  (change-new :m01 (mask-new (mask-not (change-m01 cngx)))
              :m10 (mask-new (mask-not (change-m10 cngx))))
)

;;; Return the boolean (and x (not y))
(defun change-and-not (cng1 cng2) ; -> change
  ;(format t "~&change-and-not: cng1 ~A cng2 ~A" (type-of cng1) (type-of cng2))
  (change-p cng1)
  (change-p cng2)
  ;(format t "~&change-and-not: cng1 ~A cng2 ~A" (change-str cng1) (change-str cng2))
  (assert (= (change-num-bits cng1) (change-num-bits cng2)))

  (change-and cng1 (change-not cng2))
)

;;; Return the boolean or of two changes.
(defun change-or (cng1 cng2) ; -> change
  (change-p cng1)
  (change-p cng2)
  (assert (= (change-num-bits cng1) (change-num-bits cng2)))

  (change-new :m01 (mask-new-or (change-m01 cng1) (change-m01 cng2))
              :m10 (mask-new-or (change-m10 cng1) (change-m10 cng2)))
)

;;; Return true if to changes have at least one bit, set to one, in common.
(defun change-intersects (cng1 cng2) ; -> bool
  (change-p cng1)
  (change-p cng2)
  (assert (= (change-num-bits cng1) (change-num-bits cng2)))

  (change-is-not-low (change-and cng1 cng2))
)

;;; Remove x-x-not changes from a change.
(defun change-remove-x-x-not (cngx) ; -> change
  (assert (change-p cngx))

  (let ((mask-x-x-not (mask-new-and (change-m01 cngx) (change-m10 cngx))))

    (change-new :m01 (mask-new (mask-and-not (change-m01 cngx) mask-x-x-not))
                :m10 (mask-new (mask-and-not (change-m10 cngx) mask-x-x-not)))
  )
)

