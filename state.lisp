;;;; Implement the state struct and functions.
;;;; It reresents a square on a K-Map.

(defvar true t)
(defvar false nil)

;;; The state struct.
(defstruct (state (:print-function state-print))
  value  ; A value.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (state-<field name> <instance>) -> struct field.
;   (state-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> state
;   (typep <instance> 'state) -> bool
;
; Probably shouldn't use:
;   (make-state [:<field-name> <field-state>]*), use state-new instead.
;   (copy-state <instance>) copies a state instance.

;;; Return a new state.
(defun state-new (value) ; -> state.
  (assert (value-p value))

  (make-state :value value)
)

;;; Return a state from a string.
(defun state-from-str (str) ; -> state.
  (assert (stringp str))

  (state-new (value-from-str str))
)

;;; Return a string for a state.
(defun state-str (sta)  ; -> string.
  (assert (state-p sta))

  (format nil "#S(STATE ~A)" (value-str (state-value sta)))
)

;;; Print a state.
(defun state-print (instance stream depth)
  ;(assert (zerop depth))
  (format stream (state-str instance))
)

;;; Return the number of bits used by a state.
(defun state-num-bits (sta) ; -> number
  (assert (state-p sta))

  (value-num-bits (state-value sta))
)

;;; Return t if two states are equal.
(defun state-eq (sta1 sta2) ; -> bool
  (assert (state-p sta1))
  (assert (state-p sta2))
  (assert (= (state-num-bits sta1) (state-num-bits sta2)))

  (value-eq (state-value sta1) (state-value sta2))
)

;;; Return the value of a state xor another state, or, mask.
(defun state-xor (sta other) ; -> value inst.
  (assert (state-p sta))
  (assert (or (state-p other) (mask-p other)))

  (when (state-p other)
    (assert (= (state-num-bits sta) (state-num-bits other)))
 
    (return-from state-xor (value-xor (state-value sta) (state-value other)))
  )

  (assert (= (state-num-bits sta) (mask-num-bits other)))
 
  (value-xor (state-value sta) (mask-value other))
)

;;; Return true if a satate is between two others.
(defun state-between (sta1 sta2 sta3) ; -> bool
  (assert (state-p sta1))
  (assert (state-p sta2))
  (assert (state-p sta3))
  (assert (= (state-num-bits sta1) (state-num-bits sta2)))
  (assert (= (state-num-bits sta1) (state-num-bits sta3)))
  (assert (not (state-eq sta1 sta2)))
  (assert (not (state-eq sta1 sta3)))
  (assert (not (state-eq sta2 sta3)))

  (value-zerop (value-and (state-xor sta1 sta2) (state-xor sta1 sta3)))
)

;;; Return true if two states are not equal.
(defun state-neq (sta1 sta2) ; -> bool
  (assert (state-p sta1))
  (assert (state-p sta2))
  (assert (= (state-num-bits sta1) (state-num-bits sta2)))

  (not (state-eq sta1 sta2))
)

;;; Return the value of a state and another state, or, mask.
(defun state-and (sta other) ; -> value inst.
  (assert (state-p sta))
  (assert (or (state-p other) (mask-p other)))

  (when (state-p other)
    (assert (= (state-num-bits sta) (state-num-bits other)))
 
    (return-from state-and (value-and (state-value sta) (state-value other)))
  )

  (assert (= (state-num-bits sta) (mask-num-bits other)))
 
  (value-and (state-value sta) (mask-value other))
)

;;; Return the value of a state or another state, or, mask.
(defun state-or (sta other) ; -> value inst.
  (assert (state-p sta))
  (assert (or (state-p other) (mask-p other)))

  (when (state-p other)
    (assert (= (state-num-bits sta) (state-num-bits other)))
 
    (return-from state-or (value-or (state-value sta) (state-value other)))
  )

  (assert (= (state-num-bits sta) (mask-num-bits other)))
 
  (value-or (state-value sta) (mask-value other))
)

;;; Return the inverted, "not", value of a state.
(defun state-not (stax) ; -> value.
  (assert (state-p stax))

  (value-not (state-value stax))
)

;;; Return true if a list is a list of states.
;;; An empty list will return true.
(defun state-list-p (stelst) ; -> bool
  (if (not (listp stelst))
    (return-from state-list-p false))

  (loop for stax in stelst do
    (if (not (state-p stax))
      (return-from state-list-p false))
  )
  true
)

