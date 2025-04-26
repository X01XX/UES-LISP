;;;; Implement a series of states, with bit-number values corresponding to a list of domains.

; Implement a store of corresponding states.
(defstruct statescorr
  states  ; A statestore.
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
(defun statescorr-new (states) ; -> statescorr, or nil.
  (let (storex)
    ;; Check argument, convert a state list to a statestore.
    (cond ((listp states) (setf storex (statestore-new states)))
          ((statestore-p states) (setf storex states))
          (t (error "unexpected argument")))
    
    (assert (statestore-congruent storex)) ; statestore is congruent with a higer-level domain list.

    ;; Construct result.
    (make-statescorr :states storex)
  )
)

;;; Return a list of states for the StatesCorr.
(defun statescorr-state-list (scx) ; -> list of states.
  ;; Check argument.
  (assert (statescorr-p scx))

  ;; Calc result.
  (statestore-states (statescorr-states scx))
)

;;; Return a string representing a statescorr.
(defun statescorr-str (scx) ; -> string.
  ;; Check argument.
  (assert (statescorr-p scx))

  ;; Calc result.
  (format nil "(SC ~A)" (statestore-str (statescorr-states scx)))
)

;;; Return true if two statescorr are equal.
(defun statescorr-eq (scx1 scx2) ; -> bool
  ;; Check arguments.
  (assert (statescorr-p scx1))
  (assert (statescorr-p scx2))

  (loop for sta1 in (statescorr-state-list scx1)
        for sta2 in (statescorr-state-list scx2) do

    (if (not (state-eq sta1 sta2))
      (return-from statescorr-eq false)) ; Return negative result.
  )
  ;; Return positive result.
  true
)

;;; Translate a list of symbols into a statescorr instance.
;;; Like (SC ()), (SC (1010)), or (SC (101, 1000)).
(defun statescorr-from (symbols) ; -> statescorr or error.
  ;; Check arguments.
  (assert (listp symbols))
  (assert (not (null symbols)))
  (assert (symbolp (car symbols)))
  (assert (eq (car symbols) 'SC))

  (setf symbols (second symbols))

  (statescorr-new (statestore-from symbols))
)
