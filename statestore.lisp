
(defvar true t)
(defvar false nil)

; Implement a store of states.
(defstruct statestore
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
(defun statestore-new (states) ; -> statestore instance.
  ;(format t "~&states ~A" states)
  (assert (state-list-p states))

  (let ((ret (make-statestore :states nil)))
    (loop for stax in (reverse states) do ; preserve order of states. 
      (if (not (statestore-contains ret stax))
        (statestore-push ret stax))
    )
    ret
  )
)

; Push a new state into a statestore, suppress dups.
(defun statestore-push(store state) ; -> nothing.
  (assert (statestore-p store))
  (assert (state-p state))

  (if (not (statestore-contains store state))
    (push state (statestore-states store)))
)

; Return the number of states in a statestore.
(defun statestore-length (storex) ; -> number.
  (assert (statestore-p storex))

  (length (statestore-states storex))
)

; Return true if a statestore is empty.
(defun statestore-is-empty (storex) ; -> bool
  (zerop (statestore-length storex))
)

; Return true if a statestore is not empty.
(defun statestore-is-not-empty (storex) ; -> bool
  (plusp (statestore-length storex))
)

; Return a string representing a statestore.
(defun statestore-str (storex) ; -> string.
  (assert (statestore-p storex))

  (let ((ret "#S(STATESTORE ") (start t))

    (loop for stax in (statestore-states storex) do
      (if start (setf start nil) (setf ret (concatenate 'string ret ", ")))    
      (setf ret (concatenate 'string ret (state-str stax)))
    )

    ret
  )
)

; Return true if a statestore contains a given state.
(defun statestore-contains (storex stax) ; -> bool
  (assert (statestore-p storex))
  (assert (state-p stax))

  (if (member stax (statestore-states storex) :test #'state-eq) true false)
)

(defun statestore-first-state (storex) ; -> state
  (assert (statestore-p storex))
  (assert (statestore-is-not-empty storex))

  (car (statestore-states storex))
)

(defun statestore-last-state (storex) ; -> state
  (assert (statestore-p storex))
  (assert (statestore-is-not-empty storex))

  (car (last (statestore-states storex)))
)


(defun statestore-num-bits (storex) ; -> number
  (assert (statestore-p storex))

  (state-num-bits (statestore-first-state storex))
)

(defun statestore-x-mask (storex) ; -> mask
  (assert (statestore-p storex))

  (state-list-x-mask (statestore-states storex))
)

; Return the Boolean "or" of all states.
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

; Return the Boolean "and" of all states.
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

; Return true if all states in a statestore use the same number of bits.
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

