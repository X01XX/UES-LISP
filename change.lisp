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
(defun change-new (&key b01 b10) ; -> change instance.
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
(defun change-str (cng) ; -> string
  (let ((str "(0->1 "))
    (setf str (concatenate 'string str (value-str (mask-value (change-b01 cng)))))
    (setf str (concatenate 'string str ", 1->0 "))
    (setf str (concatenate 'string str (value-str (mask-value (change-b10 cng)))))
    (setf str (concatenate 'string str ")"))
    str
  )
)

;;; Return the number of bit positions set to one.
(defun change-num-changes (cngx) ; -> integer
    (+ (mask-num-ones (change-b01 cngx))
       (mask-num-ones (change-b10 cngx)))
)

;;; Return true if there is at least one bit set to one in a change.
(defun change-is-not-low (cngx) ; -> bool.
  (if (or (mask-is-not-low (change-b01 cngx))
          (mask-is-not-low (change-b10 cngx)))
    true
    false)
)

;;; Return true if there is no bit set to one in a change.
(defun change-is-low (cngx) ; -> bool.
  (if (and (mask-is-low (change-b01 cngx))
           (mask-is-low (change-b10 cngx)))
    true
    false)
)

;;; Return the number of bits used in change masks.
(defun change-num-bits (cngx) ; -> integer.
  (mask-num-bits (change-b01 cngx))
)
