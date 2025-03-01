;;; Implement a squarestore struct and functions.                                                                

(defvar true t)
(defvar false nil)

;;; The squarestore struct.
(defstruct squarestore
  squares        ; A hash table of squares.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (squarestore-<field name> <instance>) -> struct field.
;   (squarestore-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> squarestore
;   (typep <instance> 'squarestore) -> bool
;
; Probably shouldn't use:
;   (make-squarestore [:<field-name> <field-squarestore>]*), use squarestore-new instead.
;   (copy-squarestore <instance>) copies a squarestore instance.

;;; Return a new squarestore, given a list of squares.
(defun squarestore-new () ; -> squarestore.
  ;(format t "~&squarestore-new")

  (make-squarestore :squares (make-hash-table))
)

;;; Add a square.
(defun squarestore-add(storex sqrx) ; -> square.
  ;(format t "~&squarestore-add")
  (assert (squarestore-p storex))
  (assert (square-p sqrx))

  (setf (gethash (square-state sqrx) (squarestore-squares storex)) sqrx) 
)

;;; Find a square, given a state.
(defun squarestore-find (storex key) ; -> square, or nil.
  (assert (squarestore-p storex))
  (assert (state-p key))

  (assert (squarestore-p storex))
  (assert (state-p key))

  (gethash key (squarestore-squares storex))
)

;;; Return the most recent result of a square.
(defun squarestore-most-recent-result (storex key) ; -> state, or nil.
  (assert (squarestore-p storex))
  (assert (state-p key))

  (let (sqrx)
    (setf sqrx (gethash key (squarestore-squares storex)))
    (if sqrx
        (square-most-recent-result sqrx)
        nil)
  )
)

;;; Return true if there is any square in a given region.
(defun squarestore-any-in (storex regx) ; -> bool
  (assert (squarestore-p storex))
  (assert (region-p regx))

  (loop for stax being the hash-keys of storex do
    (if (region-superset-of-state regx stax)
      (return-from squarestore-any-in true))
  )
  false
)

;;; Return square states in a given region.
(defun squarestore-states-in-region (storex regx) ; -> statestore instance.
  (assert (squarestore-p storex))
  (assert (region-p regx))

  (let ((ret (statestore-new nil)))
    (loop for stax being the hash-keys of (squarestore-squares storex) do
      (if (region-superset-of-state regx stax)
        (statestore-push ret stax))
    )
    ret
  )
)

;;; Return squares in a given region.
(defun squarestore-squares-in-region (storex regx) ; -> square list.
  (assert (squarestore-p storex))
  (assert (region-p regx))

  (let (ret)
    (loop for sqrx being the hash-values of (squarestore-squares storex) do
      (if (region-superset-of-state regx (square-state sqrx))
        (push sqrx ret))
    )
    ret
  )
)

;;; Return a statestore of square keys.
(defun squarestore-keys (storex) ; -> statestore instance.
  (assert (squarestore-p storex))

  (let ((ret (statestore-new nil)))
    (loop for stax being the hash-keys of (squarestore-squares storex) do
        (statestore-push ret stax)
    )
    ret
  )
)

;;; Return ntrue it a rulestore is valid, that is, not invalidated by ony square within its initial region.
(defun squarestore-rulestore-is-valid (storex regx rulstrx) ; -> bool
  ;(format t "~&squarestore-rulestore-is-valid")
  (assert (squarestore-p storex))
  (assert (region-p regx))
  (assert (rulestore-p rulstrx))

  (let ((sqrs (squarestore-squares-in-region storex regx)))

    (loop for sqrx in sqrs do
      (if (rulestore-invalidated-by-square rulstrx sqrx)
         (return-from squarestore-rulestore-is-valid false))
    )
    true
  )
)

