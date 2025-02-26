;;; Implement a squarestore struct and functions.                                                                

(defvar true t)
(defvar false nil)

;;; The squarestore struct.
(defstruct squarestore
  items        ; A hash table of squares.
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

  (make-squarestore :items (make-hash-table))
)

;;; Add a square.
(defun squarestore-add(storex sqrx) ; -> square.
  (assert (squarestore-p storex))
  (assert (square-p sqrx))

  (setf (gethash (square-state sqrx) (squarestore-items storex)) sqrx) 
)

;;; Find a square, given a state.
(defun squarestore-find (storex key) ; -> square, or nil.
  (assert (squarestore-p storex))
  (assert (state-p key))

  (assert (squarestore-p storex))
  (assert (state-p key))

  (gethash key (squarestore-items storex))
)

;;; Return the most recent result of a square.
(defun squarestore-most-recent-result (storex key) ; -> state, or nil.
  (let (sqrx)
    (setf sqrx (gethash key (squarestore-items storex)))
    (if sqrx
        (square-most-recent-result sqrx)
        nil)
  )
)

;;; Return true if there is any square in a given region.
(defun squarestore-any-in (storex regx) ; -> bool
  (loop for stax being the hash-keys of storex do
    (if (region-is-superset regx stax)
      (return-from squarestore-any-in true))
  )
  false
)

;;; Return square states in a given region.
(defun squarestore-states-in-region (storex regx) ; -> statestore instance.
  (let ((ret (statestore-new)))
    (loop for stax being the hash-keys of storex do
      (if (region-is-superset regx stax)
        (statestore-push ret stax))
    )
    ret
  )
)

