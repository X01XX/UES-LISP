;;;; Implement the mask struct and functions.

;;; The mask struct.
(defstruct (mask (:print-function mask-print))
  value ; A value, where bits set to one have some meaning.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (mask-<field name> <instance>) -> struct field.
;   (mask-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> mask
;   (typep <instance> 'mask) -> bool
;
; Probably shouldn't use:
;   (make-mask [:<field-name> <field-mask>]*), use mask-new instead.
;   (copy-mask <instance>) copies a mask instance.

;;; Return a new mask.
(defun mask-new (value) ; -> mask.
  (assert (value-p value))

  (make-mask :value value)
)

;;; Return a string for a mask.
(defun mask-str (msk)  ; -> string.
  (assert (mask-p msk))

  (format nil "#S(MASK ~A)" (value-str (mask-value msk)))
)

;;; Print a mask.
(defun mask-print (instance stream depth)
  ;(assert (zerop depth))
  (format stream (mask-str instance))
)

;;; Return the number of bits used by a mask.
(defun mask-num-bits (msk) ; -> number
  (assert (mask-p msk))

  (value-num-bits (mask-value msk))
)

;;; Return t if two masks are equal.
(defun mask-eq (msk1 msk2) ; -> bool
  (assert (mask-p msk1))
  (assert (mask-p msk2))
  (assert (= (mask-num-bits msk1) (mask-num-bits msk2)))

  (value-eq (mask-value msk1) (mask-value msk2))
)

;;; Return a mask from a string.
(defun mask-from-str (str) ; -> string
  (assert (stringp str))
  
  (mask-new (value-from-str str))
)

;;; Return a mask with the most significant bit set to one.
(defun mask-msb (msk) ; -> mask.
  (assert (mask-p msk))

  (mask-new (value-msb (mask-value msk)))
)

;;; Return a mask with bits shifted by a given value.
;;; A positive integer shifts left.
;;; A negative integer shifts right.
(defun mask-shift (msk num) ; -> mask.
  (assert (mask-p msk))
  (assert (integerp num))
  (assert (<= (abs num) (mask-num-bits msk)))

  (mask-new (value-shift (mask-value msk) num))
)

;;; Return true if a given mask is zero.
(defun mask-zerop (msk) ; -> bool.
  (assert (mask-p msk))

  (value-zerop (mask-value msk))
)

;;; Return the "and" bit mask of a mask or a state.
(defun mask-and (msk1 other) ; -> value.
  (assert (mask-p msk1))
  (assert (or (mask-p other) (state-p other)))

  (when (state-p other)
    (assert (= (mask-num-bits msk1) (state-num-bits other)))

    (return-from mask-and (value-and (mask-value msk1) (state-value other)))
  )

  (assert (= (mask-num-bits msk1) (mask-num-bits other)))

  (value-and (mask-value msk1) (mask-value other))
)

;;; Return the "and" bit mask of the "not" of a mask or a state.
(defun mask-and-not (msk1 other) ; -> value.
  (assert (mask-p msk1))
  (assert (or (mask-p other) (state-p other)))

  (when (state-p other)
    (assert (= (mask-num-bits msk1) (state-num-bits other)))

    (return-from mask-and-not (value-and (mask-value msk1) (state-not other)))
  )

  (assert (= (mask-num-bits msk1) (mask-num-bits other)))

  (mask-and msk1 (mask-not other))
)

;;; Return the Boolean "or" of two masks.
(defun mask-or(msk1 msk2) ; -> value.
  (assert (mask-p msk1))
  (assert (mask-p msk2))
  (assert (= (mask-num-bits msk1) (mask-num-bits msk2)))

  ; Create mask to return.
  (value-or (mask-value msk1) (mask-value msk2))
)

;;; Return the "not" bit mask of a given mask.
(defun mask-not (msk) ; -> mask.
  (assert (mask-p msk))

  ; Create mask to return.
  (mask-new (value-not (mask-value msk)))
)

;;; Return the number of bits set to one in a mask.
(defun mask-num-ones (mskx) ; -> integer.
  (assert (mask-p mskx))

  (value-num-ones (mask-value mskx))
)

;;; Return true if a mask is a ones-subset of another.
(defun mask-subset-of (&key sub-mask sup-mask) ; -> bool
  (assert (mask-p sub-mask))
  (assert (mask-p sup-mask))
  (assert (= (mask-num-bits sub-mask) (mask-num-bits sup-mask)))

  (value-eq (mask-and sub-mask sup-mask) (mask-value sub-mask))
)

;;; Return true if a mask is a ones-superset of another.
(defun mask-superset-of (&key sub-mask sup-mask) ; -> bool
  (assert (mask-p sub-mask))
  (assert (mask-p sup-mask))
  (assert (= (mask-num-bits sub-mask) (mask-num-bits sup-mask)))

  (value-eq (mask-and sub-mask sup-mask) (mask-value sub-mask))
)

;;; Return true if a mask is zero.
;;; Return true if a mask is zero.
(defun mask-is-low (mskx) ; -> bool
  (assert (mask-p mskx))

  (value-is-low (mask-value mskx))
)

;;; Return true if a mask is at its highest value.
(defun mask-is-high (mskx) ; -> bool
  (assert (mask-p mskx))

  (value-is-high (mask-value mskx))
)

;;; Return true if a mask is not zero.
(defun mask-is-not-low (mskx) ; -> bool
  (plusp (value-bits (mask-value mskx)))
)

;;; Return a mask from a mask-or operation.
(defun mask-new-or (msk1 msk2) ; -> mask
  (assert (mask-p msk1))
  (assert (mask-p msk2))
  (assert (= (mask-num-bits msk1) (mask-num-bits msk2)))

  (mask-new (mask-or msk1 msk2))
)

;;; Return a mask from a mask-and operation.
(defun mask-new-and (msk1 msk2) ; -> mask
  (assert (mask-p msk1))
  (assert (mask-p msk2))
  (assert (= (mask-num-bits msk1) (mask-num-bits msk2)))

  (mask-new (mask-and msk1 msk2))
)

;;; Return a list of masks, each one having one bit from a given mask.
(defun mask-split (mskx) ; -> list of masks.
  (assert (mask-p mskx))

  (let (ret-msks)
      (loop for bitx in (value-split (mask-value mskx)) do
	(push (mask-new bitx) ret-msks)
      )
      ret-msks
  )
)


