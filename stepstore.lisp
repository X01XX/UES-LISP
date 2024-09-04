; Implement a store of steps.

(defvar true t)
(defvar false nil)

; Implement a store of steps.
(defstruct stepstore
  step-list  ; A list of zero, or more, non-duplicate, same number bits, steps.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (stepstore-<field name> <instance>) -> struct field.
;   (stepstore-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> stepstore
;   (typep <instance> 'stepstore) -> bool
;
; Probably shouldn't use:
;   (make-stepstore [:<field-name> <field-stepstore>]*), use stepstore-new instead.
;   (copy-stepstore <instance>) copies a stepstore instance.
(defun stepstore-new (steps) ; -> stepstore.
  ;(format t "~&steps ~A" steps)
  (assert (step-list-p steps))

  (make-stepstore :step-list  steps)
)

; Push a new step into a stepstore, suppress dups, subsets.
; Return true if the step has been added.
(defun stepstore-push (storex stpx) ; -> nothing, side-effect stepstore is changed.
  ;(format t "~&stepstore-push store ~A step ~A" storex stpx)
  (assert (stepstore-p storex))
  (assert (step-p stpx))

  (push stpx (stepstore-step-list storex))
)

; Return the number of steps in a stepstore.
(defun stepstore-length (storex) ; -> number.
  (assert (stepstore-p storex))

  (length (stepstore-step-list storex))
)

; Return true if a stepstore is empty.
(defun stepstore-is-empty (storex) ; -> bool
  (assert (stepstore-p storex))

  (zerop (stepstore-length storex))
)

; Return true if a stepstore is not empty.
(defun stepstore-is-not-empty (storex) ; -> bool
  (assert (stepstore-p storex))

  (plusp (stepstore-length storex))
)

; Return a string representing a stepstore.
(defun stepstore-str (storex) ; -> string.
  (assert (stepstore-p storex))

  (let ((ret "#S(STEPSTORE ") (start t))

    (loop for stpx in (stepstore-step-list storex) do
      (if start (setf start nil) (setf ret (concatenate 'string ret ", ")))    

      (setf ret (concatenate 'string ret (format nil " ~&  ~A" (step-str stpx))))
    )

    ret
  )
)

; Return true if a stepstore contains a given step.
(defun stepstore-contains (storex stpx) ; -> bool
  (assert (stepstore-p storex))
  (assert (step-p stpx))

  (if (member stpx (stepstore-step-list storex) :test #'step-eq) true false)
)

;;; Return the first step of a non-empty stepstore.
(defun stepstore-first-step (storex) ; -> step
  (assert (stepstore-p storex))
  (assert (stepstore-is-not-empty storex))

  (car (stepstore-step-list storex))
)

;;; Return the last step of a non-empty stepstore.
(defun stepstore-last-step (storex) ; -> step
  (assert (stepstore-p storex))
  (assert (stepstore-is-not-empty storex))

  (car (last (stepstore-step-list storex)))
)

;;; Return the number of bits used is elements of a non-empty stepstore.
;;; Return the number of bits used is elements of a non-empty stepstore.
(defun stepstore-num-bits (stpstrx) ; -> integer ge 1.
  (assert (stepstore-p stpstrx))
  (assert (stepstore-is-not-empty stpstrx))

  (step-num-bits (stepstore-first-step stpstrx))
)
