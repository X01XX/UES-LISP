;;;; Implement a statestore struct and functions.

;;; The statestore struct.
(defstruct statestore
  states  ; A list of zero, or more, states.
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
(defun statestore-new (&rest states) ; -> statestore.
  (let (listx)

    (cond ((null states) nil)

          ((listp (car states)) ; A list of states.
           (setf listx (car states)))

          ((state-p (car states)) ; Enumerated states.
           (setf listx states))

          (t (error "statestore-new: invalid argumant passed")))

    ;; Check each item type.
    (eval (append (list 'and) (mapcar #'(lambda (x) (state-p x)) listx)))

    ;; Construct results.
    (make-statestore :states listx)
  )
)

;;; Push a new state into a statestore.
(defun statestore-push (store state) ; -> nothing, side-effect statestore is changed.
  ;; Check arguments.
  (assert (statestore-p store))
  (assert (state-p state))

  ;; Add state.
  (push state (statestore-states store))
)

;;; Return the number of states in a statestore.
(defun statestore-length (storex) ; -> number.
  ;; Check argument.
  (assert (statestore-p storex))

  ;; Calc result.
  (length (statestore-states storex))
)

;;; Return true if a statestore is empty.
(defun statestore-is-empty (storex) ; -> bool
  ;; Check argument.
  (assert (statestore-p storex))

  ;; Calc result.
  (zerop (statestore-length storex))
)

;;; Return true if a statestore is not empty.
(defun statestore-is-not-empty (storex) ; -> bool
  ;; Check argument.
  (assert (statestore-p storex))

  ;; Calc result.
  (plusp (statestore-length storex))
)

;;; Return a string representing a statestore.
(defun statestore-str (storex) ; -> string.
  ;; Check argument.
  (assert (statestore-p storex))

  ;; Construct result.
  (let ((ret "(") (start t))

    (loop for stax in (statestore-states storex) do
      (if start (setf start nil) (setf ret (concatenate 'string ret " ")))

      (setf ret (concatenate 'string ret (state-str stax)))
    )
    (setf ret (concatenate 'string ret ")"))

    ;; Return result.
    ret
  )
)

;;; Return true if a statestore contains a given state.
(defun statestore-member (storex stax) ; -> bool
  ;; Check arguments.
  (assert (statestore-p storex))
  (assert (state-p stax))

  ;; Return result.
  (member stax (statestore-states storex) :test #'state-eq)
)

;;; Return the first state of a non-empty statestore.
(defun statestore-first-state (storex) ; -> state
  ;; Check arguments.
  (assert (statestore-p storex))
  (assert (statestore-is-not-empty storex))

  ;; Return result.
  (car (statestore-states storex))
)

;;; Return the last state of a non-empty statestore.
(defun statestore-last-state (storex) ; -> state
  ;; Check arguments.
  (assert (statestore-p storex))
  (assert (statestore-is-not-empty storex))

  ;; Return result.
  (car (last (statestore-states storex)))
)

;;; Return an x-mask for states in a statestore.
(defun statestore-x-mask (storex) ; -> mask
  ;; Check argument.
  (assert (statestore-p storex))
  (assert (statestore-is-not-empty storex))
  (assert (statestore-same-num-bits storex))

  ;; Calc result.
  (let (ret (first-state (statestore-first-state storex)))

    (setf ret (value-new :num-bits (state-num-bits first-state) :bits 0))

    (loop for stax in (cdr (statestore-states storex)) do
       (setf ret (value-or ret (state-xor stax first-state)))
    )
    ;; Return result.
    (mask-new ret)
  )
)

;;; Return true if all states in a statestore use the same number of bits.
(defun statestore-same-num-bits (storex) ; -> bool
  ;; Check argument.
  (assert (statestore-p storex))

  (if (< (statestore-length storex) 2)
    (return-from statestore-same-num-bits true)) ; Return positive result.

  (let ((num-bits (state-num-bits (car (statestore-states storex)))))
    (loop for stax in (cdr (statestore-states storex)) do
      (if (/= (state-num-bits stax) num-bits)
        (return-from statestore-same-num-bits false)) ; Return negative result.
    )
    ;; Return positive result.
    true
  )
)

;;; Return a statestore with only states required to make a region.
(defun statestore-remove-unneeded (storex) ; -> statestore.
  ;; Check argument.
  (assert (statestore-p storex))
  (assert (statestore-same-num-bits storex))

  (if (< (statestore-length storex) 3)
    (return-from statestore-remove-unneeded storex)) ; Return empty statestore.

  (let (options (targ-x (statestore-x-mask storex)) opt-x storey)

    ;; Try combinations of successively more states.
    ;; Return first successful combination.
    (loop for num from 2 below (statestore-length storex) do

      ;; Get lists of different combinations of num states.
      (setf options (any-x-of-n num (statestore-states storex)))

      ;; Check each option list.
      (loop for optx in options do

        ;; Make statestore from state list.
        (setf storey (statestore-new optx))

        ;; Get statestore x-mask.
        (setf opt-x (statestore-x-mask storey))

        ;; Check for an x-mask equal to what is needed.
        (if (mask-eq opt-x targ-x)
          (return-from statestore-remove-unneeded storey)) ; Return statestore with fewer states.
      )
    )
  )
  ;; Store already at the minimum needed.
  storex
)

;;; Ruturn a statestore instance from a list of symbols.
;;; Like (), (s101), (s1000 s1010)
(defun statestore-from (symbols) ; -> statestore
  ;; Check argument.
  (assert (listp symbols))

  (let (states)
    ;; Construct result.
    (loop for tokx in symbols do
        (push (state-from tokx) states)
    )
    ;; Return result.
    (statestore-new (reverse states))
  )
)

;;; Return true if a statestore is congruent, by state number bits, with the domain list.
(defun statestore-congruent (statestore1) ; -> bool
  ;; Check argument.
  (assert (statestore-p statestore1))

  ;; Check length.
  (if (/= (statestore-length statestore1) (length *domain-num-bits-list*))
    (return-from statestore-congruent false)) ; Return negative result.

  ;; Check each state.
  (loop for stax in (statestore-states statestore1)
        for numx in *domain-num-bits-list* do

    ;; Compare number bits.
    (if (/= (state-num-bits stax) numx)
      (return-from statestore-congruent false)) ; Return negative result.
  )
  ;; Return positive result.
  true
)

;;; Return the number of bits used by states in a non-empty statestore.
(defun statestore-num-bits (storex) ; -> number
  ;; Check arguments.
  (assert (statestore-p storex))
  (assert (statestore-is-not-empty storex))
  (assert (statestore-same-num-bits storex))

  ;; Return result.
  (state-num-bits (statestore-first-state storex))
)

;;; Return true if two statestores have the same length and states, in any order.
(defun statestore-eq (storex storey) ; -> bool
  (assert (statestore-p storex))
  (assert (statestore-p storey))

  (if (/= (statestore-length storex) (statestore-length storey))
    (return-from statestore-eq false))

  (loop for stax in (statestore-states storex) do
    (if (not (statestore-member storey stax))
      (return-from statestore-eq false))
  )
  true
)

;;; Return the union of two statestores.
(defun statestore-union (storex storey) ; -> statestore.
  ;; Check arguments.
  (assert (statestore-p storex))
  (assert (statestore-p storey))
  (assert (or (statestore-is-empty storex) (statestore-is-empty storey)
              (= (statestore-num-bits storex) (statestore-num-bits storey))))

  (let ((ret (statestore-new nil)))
    ;; Construct result.
    (loop for stax in (statestore-states storex) do
      (if (not (statestore-member ret stax))
        (statestore-push ret stax))
    )
    (loop for stax in (statestore-states storey) do
      (if (not (statestore-member ret stax))
        (statestore-push ret stax))
    )
    ;; Return result.
    ret
  )
)

