;;;; Implement a store of regionpairs.

(defstruct regionpairstore
  regionpairs  ; A list of zero, or more, regionpairs.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (regionpairstore-<field name> <instance>) -> struct field.
;   (regionpairstore-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> regionpairstore
;   (typep <instance> 'regionpairstore) -> bool
;
; Probably shouldn't use:
;   (make-regionpairstore [:<field-name> <field-regionpairstore>]*), use regionpairstore-new instead.
;   (copy-regionpairstore <instance>) copies a regionpairstore instance.

;;; Return a new regionpairstore instance, from a list of regionpairs.
(defun regionpairstore-new (regionpairs) ; -> regionpairstore.
  ;; Check argument.
  (assert (listp regionpairs))
  (loop for rpx in regionpairs do
    (assert (regionpair-p rpx))
  )

  ;; Construct result.
  (make-regionpairstore :regionpairs regionpairs)
)

;;; Return the number of regionpairs in a regionpairstore.
(defun regionpairstore-length (storex) ; -> number.
  ;; Check argument.
  (assert (regionpairstore-p storex))

  (length (regionpairstore-regionpairs storex))
)

;;; Return true if a regionpairstore is empty.
(defun regionpairstore-is-empty (storex) ; -> bool
  ;; Check argument.
  (assert (regionpairstore-p storex))

  ;; Calc result.
  (zerop (regionpairstore-length storex))
)

;;; Return true if a regionpairstore is not empty.
(defun regionpairstore-is-not-empty (storex) ; -> bool
  ;; Check argument.
  (assert (regionpairstore-p storex))

  ;; Calc result.
  (plusp (regionpairstore-length storex))
)

;;; Return a string representing a regionpairstore.
(defun regionpairstore-str (storex) ; -> string.
  ;; Check argument.
  (assert (regionpairstore-p storex))

  ;; Construct result.
  (let ((ret "(") (start t))

    (loop for plnx in (regionpairstore-regionpairs storex) do
      (if start (setf start nil) (setf ret (concatenate 'string ret " ")))

      (setf ret (concatenate 'string ret (regionpair-str plnx)))
    )
    (setf ret (concatenate 'string ret ")"))

    ;; Return result.
    ret
  )
)

;;; Return the number of bits used by regionpairs in a non-empty regionpairstore.
(defun regionpairstore-num-bits (storex) ; -> number
  ;; Check arguments.
  (assert (regionpairstore-p storex))
  (assert (regionpairstore-is-not-empty storex))

  ;; Return result.
  (regionpair-num-bits (car (regionpairstore-regionpairs storex)))
)

;;; Push regionpair into a regionpairstore.
(defun regionpairstore-push (storex rprx) ; -> nothing, side-effect regionpairstore is changed.
  ;; Check arguments.
  (assert (regionpairstore-p storex))
  (assert (regionpair-p rprx))

  ;; Add regionpair.
  (push rprx (regionpairstore-regionpairs storex))
)

;;; Add a regionpair to a regionpairstore if there are no regionpairs that are superset.
;;; Delete subsets of new regionpair.
(defun regionpairstore-push-nosubs (storex rprx) ; -> bool, true if regionpairstore is changed.
  ;; Check arguments.
  (assert (regionpairstore-p storex))
  (assert (regionpair-p rprx))
  (assert (or (regionpairstore-is-empty storex) (= (regionpairstore-num-bits storex) (regionpair-num-bits rprx))))

  ;; Check for regionpair in store that is a superset (or dup) of the new regionpair.
  (loop for regy in (regionpairstore-regionpairs storex) do
    (if (regionpair-superset-of :sup regy :sub rprx)
      (return-from regionpairstore-push-nosubs false)) ;; Return negative result.
  )

  ;; Check for regionpairs that are a subset of the new regionpair.
  (let (del-regs)
    ;; Find regionpairs that are a subset of the new regionpair.
    (loop for regy in (regionpairstore-regionpairs storex) do
      (if (regionpair-superset-of :sup rprx :sub regy)
        (push regy del-regs)
      )
    )
    ;; Remove the subset regionpairs.
    (loop for regy in del-regs do
      (setf (regionpairstore-regionpairs storex) (remove regy (regionpairstore-regionpairs storex) :test #'regionpair-eq))
    )
  )

  ;; Add the regionpair.
  (regionpairstore-push storex rprx)
  ;; Return positive result.
  true
)

;;; Return regionpairs that a state is in.
(defun regionpairstore-regionpairs-state-in (storex stax) ; -> regionpairstore
  ;; Check arguments.
  (assert (regionpairstore-p storex))
  (assert (state-p stax))

  ;; Get matching regionpairs.
  (let ((ret (regionpairstore-new nil)))
    (loop for rpx in (regionpairstore-regionpairs storex) do
      (if (regionpair-superset-of-state rpx stax)
        (regionpairstore-push ret rpx))
    )
    ;; Return result.
    ret
  )
)

;;; Return true if a regionpairstore contains a given regionpair.
(defun regionpairstore-member (storex rpx) ; -> bool
  ;; Check arguments.
  (assert (regionpairstore-p storex))
  (assert (regionpair-p rpx))
  (assert (or (regionpairstore-is-empty storex) (= (regionpairstore-num-bits storex) (regionpair-num-bits rpx))))

  ;; Calc result.
  (member rpx (regionpairstore-regionpairs storex) :test #'regionpair-eq)
)

