;;;; Implement a change struct and functions.

(defvar true t)
(defvar false nil)

;;; The change struct.
(defstruct (change (:print-function change-print))

  b01  ; 0->1 mask.
  b10  ; 1->0 mask.
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
(defun change-new (&key b01 b10) ; -> change.
  (assert (mask-p b01))
  (assert (mask-p b10))
  (assert (= (mask-num-bits b01) (mask-num-bits b10)))

  (make-change :b01 b01 :b10 b10)
)

;;; Print a change.
(defun change-print (instance stream depth)
  ;(assert (zerop depth))
  (format stream (change-str instance))
)

;;; Return a string for a change.
(defun change-str (cngx) ; -> string
  (change-p cngx)

  (let ((str "(0->1 "))
    (setf str (concatenate 'string str (value-str (mask-value (change-b01 cngx)))))
    (setf str (concatenate 'string str ", 1->0 "))
    (setf str (concatenate 'string str (value-str (mask-value (change-b10 cngx)))))
    (setf str (concatenate 'string str ")"))
    str
  )
)

;;; Return the number of bit positions set to one.
(defun change-num-changes (cngx) ; -> integer
  (change-p cngx)

  (+ (mask-num-ones (change-b01 cngx))
     (mask-num-ones (change-b10 cngx)))
)

;;; Return true if there is at least one bit set to one in a change.
(defun change-is-not-low (cngx) ; -> bool.
  (change-p cngx)

  (if (or (mask-is-not-low (change-b01 cngx))
          (mask-is-not-low (change-b10 cngx)))
    true
    false)
)

;;; Return true if there is no bit set to one in a change.
(defun change-is-low (cngx) ; -> bool.
  (change-p cngx)

  (if (and (mask-is-low (change-b01 cngx))
           (mask-is-low (change-b10 cngx)))
    true
    false)
)

;;; Return the number of bits used in change masks.
(defun change-num-bits (cngx) ; -> integer.
  (change-p cngx)

  (mask-num-bits (change-b01 cngx))
)

;;; Return a list of changes containing only one bit from a change.
(defun change-split (cngx) ; -> list of changes.
  (change-p cngx)

  (let (ret-lst b01 b10 (num-bits (change-num-bits cngx)))
    (setf b01 (mask-split (change-b01 cngx)))
    (loop for bitx in b01 do
      (push (change-new :b01 bitx :b10 (mask-new (value-new :num-bits num-bits :bits 0))) ret-lst)
    )

    (setf b10 (mask-split (change-b10 cngx)))
    (loop for bitx in b10 do
      (push (change-new :b10 bitx :b01 (mask-new (value-new :num-bits num-bits :bits 0))) ret-lst)
    )
    ret-lst
  )
)

;;; Return true if two changes ar equal.
(defun change-eq (cng1 cng2) ; -> bool.
  (change-p cng1)
  (change-p cng2)
  (assert (= (change-num-bits cng1) (change-num-bits cng2)))

  (and (mask-eq (change-b01 cng1) (change-b01 cng2))
       (mask-eq (change-b10 cng1) (change-b10 cng2)))
)
