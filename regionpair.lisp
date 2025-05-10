;;;; Implement a structure to hold two adjacent, symmetric, regionpair pairs.
(defstruct regionpair
  regions    ; A list of two regions.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (regionpair-<field name> <instance>) -> struct field.
;   (regionpair-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> regionpair
;   (typep <instance> 'regionpair) -> bool
;
; Probably shouldn't use:
;   (make-regionpair [:<field-name> <field-regionpair>]*), use regionpair-new instead.
;   (copy-regionpair <instance>) copies a regionpair instance.

;;; Return a new regionpair.
(defun regionpair-new (regions) ; -> regionpair.
  ;; Check argument.
  (assert (listp regions))
  (assert (= (length regions) 2))
  (assert (region-p (car regions)))
  (assert (region-p (second regions)))
  (assert (= (region-num-bits (car regions)) (region-num-bits (second regions))))
  (assert (region-is-adjacent (car regions) (second regions)))
  (assert (mask-eq (region-x-mask (car regions)) (region-x-mask (second regions))))

  ;; Construct result.
  (make-regionpair :regions regions)
)

;;; Return the number of bits used by a regionpair.
(defun regionpair-num-bits (rpx) ; -> integer GT 0.
  ;; Check argument.
  (assert (regionpair-p rpx))

  ;; Return result.
  (region-num-bits (car (regionpair-regions rpx)))
)

;;; Return true if a regionpair is equal to another.
(defun regionpair-eq (rpx rpy) ; -> bool
  ;; Check arguments.
  (assert (regionpair-p rpx))
  (assert (regionpair-p rpy))
  (assert (= (regionpair-num-bits rpx) (regionpair-num-bits rpy)))

  ;; Calc result.
  (or (and (region-eq (car (regionpair-regions rpx)) (car (regionpair-regions rpy)))
           (region-eq (second (regionpair-regions rpx)) (second (regionpair-regions rpy))))
      (and (region-eq (car (regionpair-regions rpx)) (second (regionpair-regions rpy)))
           (region-eq (second (regionpair-regions rpx)) (car (regionpair-regions rpy)))))
)

;;; Return true if a regionpair is a superset of another, order of internal regions does not matter.
(defun regionpair-superset-of (&key sup sub) ; -> bool
  ;; Check arguments.
  (assert (regionpair-p sup))
  (assert (regionpair-p sub))
  (assert (= (regionpair-num-bits sup) (regionpair-num-bits sub)))

  ;; Calc result.
  (or (and (region-superset-of :sup (car (regionpair-regions sup)) :sub (car (regionpair-regions sub)))
           (region-superset-of :sup (second (regionpair-regions sup)) :sub (second (regionpair-regions sub))))
      (and (region-superset-of :sup (car (regionpair-regions sup)) :sub (second (regionpair-regions sub)))
           (region-superset-of :sup (second (regionpair-regions sup)) :sub (car (regionpair-regions sub)))))
)

;;; Return a string representing a regionpair.
(defun regionpair-str (rpx) ; -> string
  ;; Check argument.
  (assert (regionpair-p rpx))

  ;; Return result.
  (format nil "(~A ~A)" (region-str (car (regionpair-regions rpx))) (region-str (second (regionpair-regions rpx))))
)

;;; Return true if a regionpair is superset of a state.
(defun regionpair-superset-of-state (rpx stax) ; -> bool
  ;; Check arguments.
  (assert (regionpair-p rpx))
  (assert (state-p stax))

  (if (region-superset-of-state (car (regionpair-regions rpx)) stax)
    (return-from regionpair-superset-of-state true)) ; Return positive result.

  (if (region-superset-of-state (second (regionpair-regions rpx)) stax)
    (return-from regionpair-superset-of-state true)) ; Return positive result.

  ;; Return negative result.
  false
)

;;; Return the symmetric state of a subset state.
(defun regionpair-symmetric-state (rpx stax) ; -> state
  ;; Check arguments.
  (assert (regionpair-p rpx))
  (assert (state-p stax))

  (assert (or (region-superset-of-state (car (regionpair-regions rpx)) stax)
              (region-superset-of-state (second (regionpair-regions rpx)) stax)))

  (let ((change-mask (region-edge-dif-mask (car (regionpair-regions rpx)) (second (regionpair-regions rpx)))))
    ;; Return result
    (state-new-xor stax change-mask)
  )
)

;;; Return the difference mask of the region pair.
(defun regionpair-dif-mask (rpx) ; -> mask
  ;; Check argument.
  (assert (regionpair-p rpx))

  ;; Return result.
  (region-edge-dif-mask (car (regionpair-regions rpx)) (second (regionpair-regions rpx)))
)

