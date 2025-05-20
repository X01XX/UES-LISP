;;;; Implement the mask struct and functions.

;;; The mask struct, where bits set to one have some meaning.
(defstruct mask
  value ; A value instance.
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
  ;; Check argument.
  (assert (value-p value))

  ;; Construct return value.
  (make-mask :value value)
)

;;; Return a string for a mask.
(defun mask-str (msk)  ; -> string.
  ;; Check argument.
  (assert (mask-p msk))

  ;; Construct return value.
  (format nil "~A" (concatenate 'string "m" (subseq (value-str (mask-value msk)) 1)))
)

;;; Return the number of bits used by a mask.
(defun mask-num-bits (msk) ; -> number
  ;; Check argument.
  (assert (mask-p msk))

  ;; Return struct field value.
  (value-num-bits (mask-value msk))
)

;;; Return true if two masks are equal.
(defun mask-eq (msk1 msk2) ; -> bool
  ;; Check arguments.
  (assert (mask-p msk1))
  (assert (mask-p msk2))
  (assert (= (mask-num-bits msk1) (mask-num-bits msk2)))

  ;; Calc return value.
  (value-eq (mask-value msk1) (mask-value msk2))
)

;;; Return a mask instance from a symbol.
(defun mask-from (symx) ; -> mask.
  ;; Check argument.
  (assert (symbolp symx))

  ;; Init symbol to string name.                                                                                                 
  (let ((strx (symbol-name symx)))

    (let ((ret (mask-from-str strx))) ; Process string.
      ;; Check result.
      (if (err-p ret)
        (error (err-str ret))) ; Halt with error.

      ;; Return mask.
      ret
    )
  )
)

;;; Return a mask instance from a string.
(defun mask-from-str (strx) ; -> mask, or nil.
  ;; Check argument.
  (assert (stringp strx))

  (let (strx2)

    ;; Trim spaces.
    (setf strx2 (string-left-trim '(#\Space #\Tab #\Newline) (string-right-trim '(#\Space #\Tab #\Newline) strx)))

    ;; Check prefix.
    (if (not (string-equal (subseq strx2 0 1) "m"))
      (return-from mask-from-str (err-new (format nil "mask ~A should begin with an m character" strx2))))

    ;; Construct result.
    (let (val1)
      (setf val1 (value-from-str (concatenate 'string "v" (subseq strx2 1))))
      (if (value-p val1)
        (mask-new val1)
        nil
      )
    )
  )
)

;;; Return a mask with the most significant bit set to one.
(defun mask-msb (msk) ; -> mask.
  ;; Check argument.
  (assert (mask-p msk))

  ;; Construct result.
  (mask-new (value-msb (mask-value msk)))
)

;;; Return a mask with the least significant bit set to one.
(defun mask-lsb (msk) ; -> mask.
  ;; Check argument.
  (assert (mask-p msk))

  ;; Construct result.
  (mask-new (value-lsb (mask-value msk)))
)

;;; Return a mask with bits shifted right by one bit.
(defun mask-shift-right (msk) ; -> mask.
  ;; Check arguments.
  (assert (mask-p msk))

  ;; Construct result.
  (mask-new (value-shift-right (mask-value msk)))
)

;;; Return true if a given mask is zero.
(defun mask-zerop (msk) ; -> bool.
  ;; Check argument.
  (assert (mask-p msk))

  ;; Calc result
  (value-zerop (mask-value msk))
)

;;; Return the Boolean "and" mask of a mask and a mask, state, or value.
(defun mask-and (msk1 other) ; -> value.
  ;; Check arguments.
  (assert (mask-p msk1))

  (cond ((mask-p other)
         ;; Check arguments, continued.
         (assert (= (mask-num-bits msk1) (mask-num-bits other)))
         ;; Calc result
         (value-and (mask-value msk1) (mask-value other))
        )
        ((state-p other)
         ;; Check arguments, continued.
         (assert (= (mask-num-bits msk1) (state-num-bits other)))
         ;; Calc result
         (value-and (mask-value msk1) (state-value other))
        )
        ((value-p other)
         ;; Check arguments, continued.
         (assert (= (mask-num-bits msk1) (value-num-bits other)))
         ;; Calc result
         (value-and (mask-value msk1) other)
        )
        (t (error "~&other type ~A not expected" (type-of other)))
  )
)

;;; Return the Boolean "xor" mask of a mask and a mask, state, or value.
(defun mask-xor (msk1 other) ; -> value.
  ;; Check arguments.
  (assert (mask-p msk1))

  (cond ((mask-p other)
         ;; Check arguments, continued.
         (assert (= (mask-num-bits msk1) (mask-num-bits other)))
         ;; Calc result
         (value-xor (mask-value msk1) (mask-value other))
        )
        ((state-p other)
         ;; Check arguments, continued.
         (assert (= (mask-num-bits msk1) (state-num-bits other)))
         ;; Calc result
         (value-xor (mask-value msk1) (state-value other))
        )
        ((value-p other)
         ;; Check arguments, continued.
         (assert (= (mask-num-bits msk1) (value-num-bits other)))
         ;; Calc result
         (value-xor (mask-value msk1) other)
        )
        (t (error "~&other type ~A not expected" (type-of other)))
  )
)

;;; Return the "and" mask of a mask and the "not" of a mask, state, or value.
(defun mask-and-not (msk1 other) ; -> value.
  ;; Check arguments.
  (assert (mask-p msk1))

  (cond ((mask-p other)
         ;; Check arguments, continued.
         (assert (= (mask-num-bits msk1) (mask-num-bits other)))
         ;; Calc result
         (value-and (mask-value msk1) (mask-not other))
        )
        ((state-p other)
         ;; Check arguments, continued.
         (assert (= (mask-num-bits msk1) (state-num-bits other)))
         ;; Calc result
         (value-and (mask-value msk1) (state-not other))
        )
        ((value-p other)
         ;; Check arguments, continued.
         (assert (= (mask-num-bits msk1) (value-num-bits other)))
         ;; Calc result
         (value-and (mask-value msk1) (value-not other))
        )
        (t (error "~&other type ~A not expected" (type-of other)))
  )
)

;;; Return the Boolean "or" of a mask list.
(defun mask-list-or (msks) ; -> mask.
  ;; Check argument.
  (assert (listp msks))
  (assert (not (null msks)))
  (assert (> (length msks) 0))

  ;; Init result with first item.
  (let ((ret (car msks)))
    (loop for mskx in (cdr msks) do
      (setf ret (mask-new (mask-or ret mskx)))
    )
    ;; Return result.
    ret
  )
)

;;; Return the Boolean "or" of a mask, and a mask, state, or value.
(defun mask-or (msk1 other) ; -> value.
  ;; Check arguments.
  (assert (mask-p msk1))

  ; Create value to return.
  (cond ((mask-p other)
         ;; Check arguments, continued.
         (assert (= (mask-num-bits msk1) (mask-num-bits other)))
         ;; Calc result
         (value-or (mask-value msk1) (mask-value other))
        )
        ((state-p other)
         ;; Check arguments, continued.
         (assert (= (mask-num-bits msk1) (state-num-bits other)))
         ;; Calc result
         (value-or (mask-value msk1) (state-value other))
        )
        ((value-p other)
         ;; Check arguments, continued.
         (assert (= (mask-num-bits msk1) (value-num-bits other)))
         ;; Calc result
         (value-or (mask-value msk1) other)
        )
        (t (error "~&other type not expected ~A" (type-of other)))
  )
)

;;; Return the "not" bit mask of a given mask.
(defun mask-not (msk) ; -> value.
  ;; Check argument.
  (assert (mask-p msk))

  ;; Construct result.
  (value-not (mask-value msk))
)

;;; Return the number of bits set to one in a mask.
(defun mask-num-ones (mskx) ; -> integer.
  ;; Check argument.
  (assert (mask-p mskx))

  ;; Construct result.
  (value-num-ones (mask-value mskx))
)

;;; Return true if a mask is a ones-subset of another.
(defun mask-subset-of (&key sub-mask sup-mask) ; -> bool
  ;; Check arguments.
  (assert (mask-p sub-mask))
  (assert (mask-p sup-mask))
  (assert (= (mask-num-bits sub-mask) (mask-num-bits sup-mask)))

  ;; Calc result.
  (value-eq (mask-and sub-mask sup-mask) (mask-value sub-mask))
)

;;; Return true if a mask is a ones-superset of another.
(defun mask-superset-of (&key sub-mask sup-mask) ; -> bool
  ;; Check arguments.
  (assert (mask-p sub-mask))
  (assert (mask-p sup-mask))
  (assert (= (mask-num-bits sub-mask) (mask-num-bits sup-mask)))

  ;; Calc result.
  (value-eq (mask-and sub-mask sup-mask) (mask-value sub-mask))
)

;;; Return true if a mask is zero.
(defun mask-is-low (mskx) ; -> bool
  ;; Check argument.
  (assert (mask-p mskx))

  ;; Calc result.
  (value-is-low (mask-value mskx))
)

;;; Return true if a mask is at its highest value.
(defun mask-is-high (mskx) ; -> bool
  ;; Check argument.
  (assert (mask-p mskx))

  ;; Calc result.
  (value-is-high (mask-value mskx))
)

;;; Return true if a mask is not zero.
(defun mask-is-not-low (mskx) ; -> bool
  ;; Check argument.
  (assert (mask-p mskx))

  ;; Calc result.
  (plusp (value-bits (mask-value mskx)))
)

;;; Return a mask from a mask-or operation.
(defun mask-new-or (msk1 msk2) ; -> mask
  ;; Check arguments.
  (assert (mask-p msk1))

  ;; Construct result.
  (mask-new (mask-or msk1 msk2))
)

;;; Return a mask from a mask-and operation.
(defun mask-new-and (msk1 msk2) ; -> mask
  ;; Check arguments.
  (assert (mask-p msk1))

  ;; Construct result.
  (mask-new (mask-and msk1 msk2))
)

;;; Return a list of masks, each one having one bit from a given mask.
(defun mask-split (mskx) ; -> list of masks.
  ;; Check argument.
  (assert (mask-p mskx))

  (let (ret-msks)
    ;; Isolate each bit, save as mask.
    (loop for bitx in (value-split (mask-value mskx)) do
      (push (mask-new bitx) ret-msks)
    )
    ;; Return results.
    ret-msks
  )
)

;;; Return true if a list is a list of masks.
;;; An empty list will return true.
(defun mask-list-p (msklst) ; -> bool
  ;; Check argument.
  (if (not (listp msklst))
    (return-from mask-list-p false))

  ;; Check each item.
  (loop for mskx in msklst do
    (if (not (mask-p mskx))
      (return-from mask-list-p false)) ; Return negative result.
  )
  ;; Return positive result.
  true
)

;; Return the bits value of a mask.
(defun mask-bits (mskx) ; -> integer.
  ;; Check argument.
  (assert (mask-p mskx))

  ;; Return bits.
  (value-bits (mask-value mskx))
)
