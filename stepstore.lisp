;;;; Implement a store of steps.

; Implement a store of steps.
(defstruct stepstore
  steps  ; A list of zero, or more, non-duplicate, same number bits, steps.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (stepstore-<field name> <instance>) -> struct field.
;   (stepstore-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> stepstore
;   (typep <instance> 'stepstore) -> bool
;
; Probably shouldn't use:
;   (make-stepstore [:<field-name> <field-stepstore>]*), use stepstore-new instead.
;   (copy-stepstore <instance>) copies a stepstore instance.
(defun stepstore-new (steps) ; -> stepstore.
  ;(format t "~&steps ~A" steps)
  (assert (step-list-p steps))

  (make-stepstore :steps  steps)
)

; Push a new step into a stepstore, suppress dups, subsets.
; Return true if the step has been added.
(defun stepstore-push (storex stpx) ; -> nothing, side-effect stepstore is changed.
  ;(format t "~&stepstore-push store ~A step ~A" storex stpx)
  (assert (stepstore-p storex))
  (assert (step-p stpx))

  (push stpx (stepstore-steps storex))
)

; Return the number of steps in a stepstore.
(defun stepstore-length (storex) ; -> number.
  (assert (stepstore-p storex))

  (length (stepstore-steps storex))
)

; Return true if a stepstore is empty.
(defun stepstore-is-empty (storex) ; -> bool
  (assert (stepstore-p storex))

  (zerop (stepstore-length storex))
)

; Return true if a stepstore is not empty.
(defun stepstore-is-not-empty (storex) ; -> bool
  (assert (stepstore-p storex))

  (plusp (stepstore-length storex))
)

; Return a string representing a stepstore.
(defun stepstore-str (storex) ; -> string.
  (assert (stepstore-p storex))

  (when (stepstore-is-empty storex)
    (return-from stepstore-str "(steps: NIL)")
  ) 
  (let ((ret "(steps: ") (start t))

    (loop for stpx in (stepstore-steps storex) do
      (if start (setf start nil) (setf ret (concatenate 'string ret ", ")))    

      (setf ret (concatenate 'string ret (format nil " ~&  ~A" (step-str stpx))))
    )
    (setf ret (concatenate 'string ret ")"))

    ret
  )
)

; Return true if a stepstore contains a given step.
(defun stepstore-member (storex stpx) ; -> bool
  (assert (stepstore-p storex))
  (assert (step-p stpx))

  (member stpx (stepstore-steps storex) :test #'step-eq)
)

;;; Return the first step of a non-empty stepstore.
(defun stepstore-first-step (storex) ; -> step
  (assert (stepstore-p storex))
  (assert (stepstore-is-not-empty storex))

  (car (stepstore-steps storex))
)

;;; Return the last step of a non-empty stepstore.
(defun stepstore-last-step (storex) ; -> step
  (assert (stepstore-p storex))
  (assert (stepstore-is-not-empty storex))

  (car (last (stepstore-steps storex)))
)

;;; Return the number of bits used is elements of a non-empty stepstore.
(defun stepstore-num-bits (stpstrx) ; -> integer ge 1.
  (assert (stepstore-p stpstrx))
  (assert (stepstore-is-not-empty stpstrx))

  (step-num-bits (stepstore-first-step stpstrx))
)

;;; Return steps that have an initial region intersecting a given region.
(defun stepstore-initial-region-intersects (storex regx) ; -> stepstore
  (assert (stepstore-p storex))
  (assert (region-p regx))

  (let ((ret (stepstore-new nil)))
    (loop for stpx in (stepstore-steps storex) do
      (if (region-intersects (step-initial-region stpx) regx)
        (stepstore-push ret stpx))
    )
    ret
  )
)

;;; Return steps that have a result region intersecting a given region.
(defun stepstore-result-region-intersects (storex regx) ; -> stepstore
  (assert (stepstore-p storex))
  (assert (region-p regx))

  (let ((ret (stepstore-new nil)))
    (loop for stpx in (stepstore-steps storex) do
      (if (region-intersects (step-result-region stpx) regx)
        (stepstore-push ret stpx))
    )
    ret
  )
)

;;; Return steps that have a change that intersects a given change.
(defun stepstore-change-intersects (storex cngx) ; -> stepstore.
  (assert (stepstore-p storex))
  (assert (change-p cngx))
  (assert (= (stepstore-num-bits storex) (change-num-bits cngx)))

  (let ((ret (stepstore-new nil)))
    (loop for stpx in (stepstore-steps storex) do
      (if (change-intersects (step-changes stpx) cngx)
        (stepstore-push ret stpx))
    )
    ret
  )
)

;;; Return steps not equal steps in a second store.
(defun stepstore-difference (storex storey) ; -> stepstore.
  (assert (stepstore-p storex))
  (assert (stepstore-p storey))
  (assert (or (or (stepstore-is-empty storex) (stepstore-is-empty storey))
              (= (stepstore-num-bits storex) (stepstore-num-bits storey))))

  (let ((ret (stepstore-new nil)))
    (loop for stpx in (stepstore-steps storex) do
      (if (not (stepstore-member storey stpx))
        (stepstore-push ret stpx))
    )
    ret
  )
)

;;; Return the union of two stepstores.
(defun stepstore-union (storex storey) ; -> stepstore.
  (assert (stepstore-p storex))
  (assert (stepstore-p storey))
  (assert (or (or (stepstore-is-empty storex) (stepstore-is-empty storey))
              (= (stepstore-num-bits storex) (stepstore-num-bits storey))))

  (let ((ret (stepstore-new nil)))
    (loop for stpx in (stepstore-steps storex) do
      (if (not (stepstore-member ret stpx))
        (stepstore-push ret stpx))
    )
    (loop for stpx in (stepstore-steps storey) do
      (if (not (stepstore-member ret stpx))
        (stepstore-push ret stpx))
    )
    ret
  )
)

;;; Return the nth element of a StepStore.
(defun stepstore-nth (storex inx) ; -> step instance, or nil.
  (assert (stepstore-p storex))
  (assert (integerp inx))

  (if (>= inx (stepstore-length storex))
    (return-from stepstore-nth nil))

  (nth inx (stepstore-steps storex))
)

;;; Return steps in both stepstores.
(defun stepstore-intersection (storex storey) ; -> stepstore.
  (assert (stepstore-p storex))
  (assert (stepstore-p storey))
  (assert (or (or (stepstore-is-empty storex) (stepstore-is-empty storey))
              (= (stepstore-num-bits storex) (stepstore-num-bits storey))))

  (let ((ret (stepstore-new nil)))
    (loop for stpx in (stepstore-steps storex) do
      (if (stepstore-member storey stpx)
        (stepstore-push ret stpx))
    )
    ret
  )
)

;;; Return all changes in steps of a non-empty stepstore.
(defun stepstore-aggregate-changes (storex) ; -> change
  (assert (stepstore-p storex))
  (assert (stepstore-is-not-empty storex))

  (let ((cng (step-changes (stepstore-first-step storex))))
    (loop for stpx in (cdr (stepstore-steps storex)) do
      (setf cng (change-or cng (step-changes stpx)))
    )
    cng
  )
)

;;; Return a psuedo-random step from a non-empty store.
(defun stepstore-select-step (storex glide-path) ; -> step
  (assert (stepstore-p storex))
  (assert (stepstore-is-not-empty storex))
  (assert (or (stepstore-is-empty storex)
              (= (stepstore-num-bits storex) (region-num-bits glide-path))))

  (if (= 1 (stepstore-length storex))
    (return-from stepstore-select-step (stepstore-first-step storex)))

  (let (rate (min-rate 99999) ret-step stepy)
    ;; Check three randomly selected steps, choose one based on minimum excursion from glide-path.
    (loop for i from 0 to 2 do

      (setf stepy (stepstore-nth storex (random (stepstore-length storex))))

      (setf rate (+ (region-distance (step-initial-region stepy) glide-path)
                    (region-distance (step-result-region stepy) glide-path)))

      (when (< rate min-rate)
        (setf min-rate rate)
        (setf ret-step stepy)
      )
    )
    ret-step
  )
)
