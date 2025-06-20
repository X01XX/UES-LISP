;;;; Implement the state struct and functions.
;;;; It represents a square on a K-Map.

;;; The state struct.
(defstruct state
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

;;; Return a new state, from a value or mask.
(defun state-new (arg1) ; -> state.
  ;(format t "~&state-new: ~A" (type-of arg1))
  (cond ((value-p arg1)
         (make-state :value arg1))
        ((mask-p arg1)
         (make-state :value (mask-value arg1)))
        (t (error "Invalid argument")))
)

;;; Return a state of the same number bits, with a high value.
;;; Allows using an existing state to fill in the number of bits.
(defun state-new-high (stax) ; -> state instance.
  ;; Check argument.
  (assert (state-p stax))

  ;; Construct result.
  (make-state :value (value-new-high (state-value stax)))
)

;;; Return a state of the same number bits, with a low value.
;;; Allows using an existing state to fill in the number of bits.
(defun state-new-low (stax) ; -> state instance.
  ;; Check argument.
  (assert (state-p stax))

  ;; Construct result.
  (make-state :value (value-new-low (state-value stax)))
)

;;; Return a state instance from a symbol.
(defun state-from (symx) ; -> state.
  ;; Check argument.
  (assert (symbolp symx))

  ;; Init symbol to string name.                                                                                                 
  (let ((strx (symbol-name symx)))

    (let ((ret (state-from-str strx))) ; Process string.
      ;; Check result.
      (if (err-p ret)
        (error (err-str ret))) ; Halt with error.

      ;; Return state.
      ret
    )
  )
)

;;; Return a state instance from a string.
(defun state-from-str (strx) ; -> state.
  ;; Check argument.
  (assert (stringp strx))

  (let (strx2 val1) ; working string.

    ;; Trim spaces.
    (setf strx2 (string-left-trim '(#\Space #\Tab #\Newline) (string-right-trim '(#\Space #\Tab #\Newline) strx)))

    ;; Check for prefix.
    (if (not (string-equal (subseq strx2 0 1) "s"))
      (return-from state-from-str (err-new (format nil "State ~A should begin with an s character" strx2))))

    ;; Construct result.
    (setf val1 (value-from-str (concatenate 'string "v" (subseq strx2 1))))
    (if (value-p val1)
      (state-new val1)
      nil
    )
  )
)

;;; Return a string for a state.
(defun state-str (sta)  ; -> string.
  ;; Check argument.
  (assert (state-p sta))

  ;; Construct result.
  (format nil "s~A" (subseq (value-str (state-value sta)) 1))
)

;;; Return the number of bits used by a state.
(defun state-num-bits (sta) ; -> number
  ;; Check argument.
  (assert (state-p sta))

  ;; Construct result.
  (value-num-bits (state-value sta))
)

;;; Return t if two states are equal.
(defun state-eq (sta1 sta2) ; -> bool
  ;; Check arguments.
  (assert (state-p sta1))
  (assert (state-p sta2))

  ;; Check states num-bits.
  (if (/= (state-num-bits sta1) (state-num-bits sta2))
    (return-from state-eq false)) ; Return negative result.

  ;; Calc result.
  (value-eq (state-value sta1) (state-value sta2))
)

;;; Return a state, from a state Boolean "xor" another state.
(defun state-xor (sta1 sta2) ; -> state.
  ;; Check arguments.
  (assert (state-p sta1))
  (assert (state-p sta2))
  (assert (= (state-num-bits sta1) (state-num-bits sta2)))

  (state-new (value-xor (state-value sta1) (state-value sta2)))
)

;;; Return true if two states are not equal.
(defun state-ne (sta1 sta2) ; -> bool
  ;; Check arguments.
  (assert (state-p sta1))
  (assert (state-p sta2))
  (assert (= (state-num-bits sta1) (state-num-bits sta2)))

  ;; Calc result.
  (not (state-eq sta1 sta2))
)

;;; Return a state, from a state Boolean "and" another state.
(defun state-and (sta1 sta2) ; -> state.
  ;; Check arguments.
  (assert (state-p sta1))
  (assert (state-p sta2))
  (assert (= (state-num-bits sta1) (state-num-bits sta2)))

  (state-new (value-and (state-value sta1) (state-value sta2)))
)

;;; Return a state, from a state Boolean "or" another state.
(defun state-or (sta1 sta2) ; -> state.
  ;; Check arguments.
  (assert (state-p sta1))
  (assert (state-p sta2))
  (assert (= (state-num-bits sta1) (state-num-bits sta2)))

  (state-new (value-or (state-value sta1) (state-value sta2)))
)

;;; Return a state from the Boolean "not" of a state.
(defun state-not (sta1) ; -> state.
  ;; Check arguments.
  (assert (state-p sta1))

  (state-new (value-not (state-value sta1)))
)

;;; Return a state from the Boolean "not" of a state, or mask.
(defun state-new-not (arg1) ; -> state.
  (cond ((state-p arg1)
         (state-new (value-not (state-value arg1))))
        ((mask-p arg1)
         (state-new (value-not (mask-value arg1))))
        (t (error "Invalid argument")))
)

;;; Return a random state of a given number of bits.
(defun state-random (num-bits) ; -> state
  ;; Check argument.
  (assert (integerp num-bits))
  (assert (> num-bits 0))

  (let ((max (expt 2 num-bits)) rand-num)
    ;; Get random number.
    (setf rand-num (random max))

    ;; Construct result.
    (state-new (value-new :num-bits num-bits :bits rand-num))
  )
)

;;; Return true if two states are adjacent.
(defun state-is-adjacent (sta1 sta2) ; -> bool
  ;; Check arguments.
  (assert (state-p sta1))
  (assert (state-p sta2))
  (assert (= (state-num-bits sta1) (state-num-bits sta2)))

  ;; Calc result.
  (= 1 (value-num-ones (value-xor (state-value sta1) (state-value sta2))))
)

;;; Return the distance between two states.
(defun state-distance (sta1 sta2) ; -> integer
  ;; Check arguments.
  (assert (state-p sta1))
  (assert (state-p sta2))
  (assert (= (state-num-bits sta1) (state-num-bits sta2)))

  ;; Calc result.
  (state-num-ones (state-xor sta1 sta2))
)

;;; Return the regions implied by two dissimilar states.
;;; That is ~A + ~B, which means no possible region will contain both states.
(defun state-regions-implied-by-dissimilarity (stax stay) ; -> regionstore.
  ;; Check arguments.
  (assert (state-p stax))
  (assert (state-p stay))
  (assert (= (state-num-bits stax) (state-num-bits stay)))
  (assert (not (state-eq stax stay)))

  ;; Calc result.
  (regionstore-union (state-complement stax) (state-complement stay))
)

;;; Return the complement of a state.
(defun state-complement (stax) ; -> regionstore.
  ;; Check argument.
  (assert (state-p stax))

  (let ((max-regionstore (regionstore-new (list *max-region*))))
    ;; Calc result.
    (regionstore-subtract-state max-regionstore stax)
  )
)

;;; Return a state from a Boolean "xor" operation on any combination of state or mask.
(defun state-new-xor (arg1 arg2) ; -> state
  (let (val1 val2)
    (cond ((state-p arg1) (setf val1 (state-value arg1)))
          ((mask-p arg1)  (setf val1 (mask-value arg1)))
          (t (error "Invalid arg1")))

    (cond ((state-p arg2) (setf val2 (state-value arg2)))
          ((mask-p arg2)  (setf val2 (mask-value arg2)))
          (t (error "Invalid arg2")))

    (state-new (value-xor val1 val2))
  )
)

;;; Return a state from a Boolean "or" operation on any combination of state or mask.
(defun state-new-or (arg1 arg2) ; -> state
  (let (val1 val2)
    (cond ((state-p arg1) (setf val1 (state-value arg1)))
          ((mask-p arg1)  (setf val1 (mask-value arg1)))
          (t (error "Invalid arg1")))

    (cond ((state-p arg2) (setf val2 (state-value arg2)))
          ((mask-p arg2)  (setf val2 (mask-value arg2)))
          (t (error "Invalid arg2")))

    (state-new (value-or val1 val2))
  )
)


;;; Return a state from a Boolean "and" operation on any combination of state or mask.
(defun state-new-and (arg1 arg2) ; -> state
  (let (val1 val2)
    (cond ((state-p arg1) (setf val1 (state-value arg1)))
          ((mask-p arg1)  (setf val1 (mask-value arg1)))
          (t (error "Invalid arg1")))

    (cond ((state-p arg2) (setf val2 (state-value arg2)))
          ((mask-p arg2)  (setf val2 (mask-value arg2)))
          (t (error "Invalid arg2")))

    (state-new (value-and val1 val2))
  )
)

;;; Return a mask of matching bit positions of two states.
(defun state-eqv (sta1 sta2) ; -> mask
  ;; Check arguments.
  (assert (state-p sta1))
  (assert (state-p sta2))
  (assert (= (state-num-bits sta1) (state-num-bits sta2)))

  ;; Construct result.
  (mask-new (value-eqv (state-value sta1) (state-value sta2)))
)

;;; Return the number of one bits in a state.
(defun state-num-ones (sta1) ; -> integer.
  ;; Check arguments.
  (assert (state-p sta1))
  
  (value-num-ones (state-value sta1))
)

