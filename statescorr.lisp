;;;; Implement a series of states, with bit-number values corresponding to a list of domains.

; Implement a store of corresponding states.
(defstruct statescorr
  states  ; A statestore of zero, or more, states.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (statescorr-<field name> <instance>) -> struct field.
;   (statescorr-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> statescorr
;   (typep <instance> 'statescorr) -> bool
;
; Probably shouldn't use:
;   (make-statescorr [:<field-name> <field-statescorr>]*), use statescorr-new instead.
;   (copy-statescorr <instance>) copies a statescorr instance.

;;; Return a new statescorr instance, from a statestore.
;;; If this is tightly controlled, checking domain congruency of arguments to other functions is unneeded.
;;; Don't use make-statescorr anywhere else.
(defun statescorr-new (store) ; -> statescorr, or nil.
  (assert (statestore-p store))
  (assert (statestore-congruent store))

  (make-statescorr :states store)
)

;;; Return a list of states for the StatesCorr.
(defun statescorr-state-list (scx) ; -> list of states.
  (statestore-states (statescorr-states scx))
)

;;; Return the number of states in a statescorr.
(defun statescorr-length (scx) ; -> number.
  ;(format t "~&statescorr-length: ~A" (type-of scx))
  (assert (statescorr-p scx))

  (statestore-length (statescorr-states scx))
)

;;; Return true if a statescorr is empty.
(defun statescorr-is-empty (scx) ; -> bool
  ;(format t "~&statescorr-is-empty: arg ~A" (type-of scx))
  (assert (statescorr-p scx))

  (statestore-is-empty (statescorr-states scx))
)

;;; Return true if a statescorr is not empty.
(defun statescorr-is-not-empty (scx) ; -> bool
  (assert (statescorr-p scx))

  (statestore-is-not-empty (statescorr-states scx))
)

;;; Return a string representing a statescorr.
(defun statescorr-str (scx) ; -> string.
  (assert (statescorr-p scx))

  (format nil "(SC ~A)" (statestore-str (statescorr-states scx)))
)

;;; Return true if two statescorr are equal.
(defun statescorr-eq (scx1 scx2) ; -> bool
  ;(format t "~&statescorr-eq: ~A ~A" scx1 scx2)
  (assert (statescorr-p scx1))
  (assert (statescorr-p scx2))

  (loop for sta1 in (statescorr-state-list scx1)
        for sta2 in (statescorr-state-list scx2) do
    (if (not (state-eq sta1 sta2))
      (return-from statescorr-eq false))
  )
  true
)

;;; Return true if two statescorr are not equal.
(defun statescorr-ne (stascorr1 stascorr2) ; -> bool
  (assert (statescorr-p stascorr1))
  (assert (statescorr-p stascorr2))

  (not (statescorr-eq stascorr1 stascorr2))
)

;;; Translate a list of symbols into a statescorr instance.
;;; Like (SC ()), (SC (1010)), or (SC (101, 1000)).
(defun statescorr-from (symbols) ; -> statescorr or error.
    ;(format t "~&statescorr-from ~A" (type-of symbols))
    (assert (listp symbols))
    (assert (not (null symbols)))
    (assert (symbolp (car symbols)))
    (assert (eq (car symbols) 'SC))

    (setf symbols (second symbols))

    (statescorr-new (statestore-from symbols))
)
