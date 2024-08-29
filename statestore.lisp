;;;; Implement a statestore struct and functions.

(defvar true t)
(defvar false nil)

;;; The statestore struct.
(defstruct (statestore (:print-function statestore-print))
  states  ; A list of zero, or more, non-duplicate, same number bits, states.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (statestore-<field name> <instance>) -> struct field.
;   (statestore-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> statestore
;   (typep <instance> 'statestore) -> bool
;
; Probably shouldn't use:
;   (make-statestore [:<field-name> <field-statestore>]*), use statestore-new instead.
;   (copy-statestore <instance>) copies a statestore instance.

;;; Return a new statestore, given a list of states.
(defun statestore-new (states) ; -> statestore.
  ;(format t "~&states ~A" states)
  (assert (state-list-p states))

  (make-statestore :states states)
)

;;; Print a statestore.
(defun statestore-print (instance stream depth)
  ;(assert (zerop depth))
  (format stream (statestore-str instance))
)

;;; Push a new state into a statestore, suppress dups.
(defun statestore-push(store state) ; -> nothing, side-effect statestore is changed.
  (assert (statestore-p store))
  (assert (state-p state))

  (push state (statestore-states store))
)

;;; Return the number of states in a statestore.
(defun statestore-length (storex) ; -> number.
  (assert (statestore-p storex))

  (length (statestore-states storex)))

;;; Return true if a statestore is empty.
(defun statestore-is-empty (storex) ; -> bool
  (assert (statestore-p storex))

  (zerop (statestore-length storex))
)

;;; Return true if a statestore is not empty.
(defun statestore-is-not-empty (storex) ; -> bool
  (assert (statestore-p storex))

  (plusp (statestore-length storex))
)

;;; Return a string representing a statestore.
(defun statestore-str (storex) ; -> string.
  (assert (statestore-p storex))

  (let ((ret "#S(STATESTORE ") (start t))

    (loop for stax in (statestore-states storex) do
      (if start (setf start nil) (setf ret (concatenate 'string ret ", ")))

      (setf ret (concatenate 'string ret (state-str stax)))
    )
    (if (zerop (statestore-length storex))
      (setf ret (concatenate 'string ret "NIL)"))
      (setf ret (concatenate 'string ret ")"))
    )

    ret)
)

;;; Return true if a statestore contains a given state.
(defun statestore-contains (storex stax) ; -> bool
  (assert (statestore-p storex))
  (assert (state-p stax))

  (if (member stax (statestore-states storex) :test #'state-eq) true false)
)

;;; Return the first state of a non-empty statestore.
(defun statestore-first-state (storex) ; -> state
  (assert (statestore-p storex))
  (assert (statestore-is-not-empty storex))

  (car (statestore-states storex))
)

;;; Return the last state of a non-empty statestore.
(defun statestore-last-state (storex) ; -> state
  (assert (statestore-p storex))
  (assert (statestore-is-not-empty storex))

  (car (last (statestore-states storex)))
)

;;; Return the number of bits used by states in a non-empty statestore.
(defun statestore-num-bits (storex) ; -> number
  (assert (statestore-p storex))
  (assert (statestore-is-not-empty storex))

  (state-num-bits (statestore-first-state storex))
)

;;; Return an x-mask for states in a statestore.
(defun statestore-x-mask (storex) ; -> mask
  ;(format t "~&statestore-x-mask ~A" storex)
  (assert (statestore-p storex))
  (assert (statestore-is-not-empty storex))

  (let (ret (first-state (statestore-first-state storex)))

    (setf ret (value-new :num-bits (state-num-bits first-state) :bits 0))

    (loop for stax in (cdr (statestore-states storex)) do
       (setf ret (value-or ret (state-xor stax first-state)))
    )
    (mask-new ret)
  )
)

;;; Return the Boolean "or" of all states.
(defun statestore-or-all (storex) ; -> state
  (assert (statestore-p storex))
  (assert (statestore-is-not-empty storex))

  (let ((ret (statestore-first-state storex)))
    (loop for stax in (cdr (statestore-states storex)) do
      (state-or ret stax)
    )
    ret
  )
)

;;; Return the Boolean "and" of all states.
(defun statestore-and-all (storex) ; -> state
  (assert (statestore-p storex))
  (assert (statestore-is-not-empty storex))

  (let ((ret (statestore-first-state storex)))
    (loop for stax in (cdr (statestore-states storex)) do
      (state-and ret stax)
    )
    ret
  )
)

;;; Return true if all states in a statestore use the same number of bits.
(defun statestore-same-num-bits (storex) ; -> bool
  (assert (statestore-p storex))
  (assert (statestore-is-not-empty storex))
  
  (if (< (statestore-length storex) 2)
    (return-from statestore-same-num-bits true))

  (let ((num-bits (state-num-bits (car (statestore-states storex)))))
    (loop for stax in (cdr (statestore-states storex)) do
      (if (/= (state-num-bits stax) num-bits)
        (return-from statestore-same-num-bits false))
    )
    true
  )
)

;;; Return a statestore with states unneeded to make a region removed.
(defun statestore-remove-unneeded (storex) ; -> statestore.
  (assert (statestore-p storex))
  
  (if (< (statestore-length storex) 3)
    (return-from statestore-remove-unneeded storex))

  (let (options (targ-x (statestore-x-mask storex)) opt-x storey)

    (loop for num from 2 below (statestore-length storex) do

      (setf options (any-x-of-n num (statestore-states storex)))
      (loop for optx in options do

	(setf storey (statestore-new optx))
        (setf opt-x (statestore-x-mask storey))

	(if (mask-eq opt-x targ-x)
	  (return-from statestore-remove-unneeded storey))
      )
    )
  )
  ;; Store already at the minimum needed.
  storex
)
