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

;;; Return a new state.
(defun state-new (value) ; -> state.
  ;; Check argument.
  (assert (value-p value))

  ;; Construct result.
  (make-state :value value)
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

  (let (strx2) ; working string.

    ;; Trim spaces.
    (setf strx2 (string-left-trim '(#\Space #\Tab #\Newline) (string-right-trim '(#\Space #\Tab #\Newline) strx)))

    ;; Check for prefix.
    (if (not (string-equal (subseq strx2 0 1) "s"))
      (return-from state-from-str (err-new (format nil "State ~A should begin with an s character" strx2))))

    ;; Construct result.
    (state-new (value-from-str (concatenate 'string "v" (subseq strx2 1))))
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

;;; Return the value of a state xor another state, mask, or value.
(defun state-xor (sta other) ; -> value inst.
  ;; Check arguments.
  (assert (state-p sta))

  (cond ((state-p other)
         ;; Check arguments, continued.
         (assert (= (state-num-bits sta) (state-num-bits other)))
         ;; Calc result.
         (value-xor (state-value sta) (state-value other))
        )
        ((mask-p other)
         ;; Check arguments, continued.
         (assert (= (state-num-bits sta) (mask-num-bits other)))
         ;; Calc result.
         (value-xor (state-value sta) (mask-value other))
        )
        ((value-p other)
         ;; Check arguments, continued.
         (assert (= (state-num-bits sta) (value-num-bits other)))
         ;; Calc result.
         (value-xor (state-value sta) other)
        )
        (t (error "~&other type not expected ~A" (type-of other)))
  )
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

;;; Return the value of a state and another state, mask, or value.
(defun state-and (sta other) ; -> value inst.
  ;; Check arguments.
  (assert (state-p sta))

  (cond ((state-p other)
         ;; Check arguments, continued.
         (assert (= (state-num-bits sta) (state-num-bits other)))
         ;; Calc result.
         (value-and (state-value sta) (state-value other))
        )
        ((mask-p other)
         ;; Check arguments, continued.
         (assert (= (state-num-bits sta) (mask-num-bits other)))
         ;; Calc result.
         (value-and (state-value sta) (mask-value other))
        )
        ((value-p other)
         ;; Check arguments, continued.
         (assert (= (state-num-bits sta) (value-num-bits other)))
         ;; Calc result.
         (value-and (state-value sta) other)
        )
        (t (error "~&other type not expected ~A" (type-of other)))
  )
)

;;; Return the value of a state or another state, mask, or value.
(defun state-or (sta other) ; -> value inst.
  ;; Check arguments.
  (assert (state-p sta))

  (cond ((state-p other)
         ;; Check arguments, continued.
         (assert (= (state-num-bits sta) (state-num-bits other)))
         ;; Calc result.
         (value-or (state-value sta) (state-value other))
        )
        ((mask-p other)
         ;; Check arguments, continued.
         (assert (= (state-num-bits sta) (mask-num-bits other)))
         ;; Calc result.
         (value-or (state-value sta) (mask-value other))
        )
        ((value-p other)
         ;; Check arguments, continued.
         (assert (= (state-num-bits sta) (value-num-bits other)))
         ;; Calc result.
         (value-or (state-value sta) other)
        )
        (t (error "~&other type not expected ~A" (type-of other)))
  )
)

;;; Return the inverted, "not", value of a state.
(defun state-not (stax) ; -> value.
  ;; Check argument.
  (assert (state-p stax))

  ;; Calc result.
  (value-not (state-value stax))
)

;;; Return true if a list is a list of states.
;;; An empty list will return true.
(defun state-list-p (stelst) ; -> bool
  ;; Check argument.
  (if (not (listp stelst))
    (return-from state-list-p false))

  (loop for stax in stelst do
    ;; Check argument item.
    (if (not (state-p stax))
      (return-from state-list-p false)) ; Return negative result.
  )
  ;; Return positive result.
  true
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
  (value-num-ones (state-xor sta1 sta2))
)

;;; Return the regions implied by two dissimilar states.
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

  (let ((max-regionstore (regionstore-new (list (region-new (list (state-new-high stax) (state-new-low stax)))))))
    ;; Calc result.
    (regionstore-subtract-state max-regionstore stax)
  )
)

;; Return the bits value of a state.
(defun state-bits (stax) ; -> integer.
  ;; Check argument.
  (assert (state-p stax))

  ;; Return bits.
  (value-bits (state-value stax))
)

;;; Return a state from a state-or operation.
(defun state-new-or (sta1 sta2) ; -> state
  ;; Check arguments.
  (assert (state-p sta1))

  ;; Construct result.
  (state-new (state-or sta1 sta2))
)

;;; Return a state from a state-and operation.
(defun state-new-and (sta1 sta2) ; -> state
  ;; Check arguments.
  (assert (state-p sta1))

  ;; Construct result.
  (state-new (state-and sta1 sta2))
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

;;; Return a state from a state-xor operation.
(defun state-new-xor (sta1 sta2) ; -> state
  ;; Check arguments.
  (assert (state-p sta1))

  ;; Construct result.
  (state-new (state-xor sta1 sta2))
)

