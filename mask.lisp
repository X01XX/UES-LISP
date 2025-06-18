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

;;; Return a new mask, from a value or state.
(defun mask-new (arg1) ; -> mask.
  ;(format t "~&mask-new ~A" (type-of arg1))

  (cond ((value-p arg1)
         (make-mask :value arg1))
        ((state-p arg1)
         (make-mask :value (state-value arg1)))
        (t (error "invalid argument")))
)

;;; Return a low mask, given a mask.
(defun mask-new-low (msk1) ; -> mask.
  (mask-new (value-new-low (mask-value msk1)))
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

;;; Return a mask with the most significant bit set to one,
;;; given a mask or state.
(defun mask-msb (arg1) ; -> mask.
  (let (val1)
    (cond ((mask-p arg1)  (setf val1 (mask-value arg1)))
          ((state-p arg1) (setf val1 (state-value arg1)))
          (t (error "invalid arg1")))

    ;; Construct result.
    (mask-new (value-msb val1))
  )
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

;;; Return a mask from the Boolean "and" of two masks.
(defun mask-and (msk1 msk2) ; -> mask.
  ;; Check arguments.
  (assert (mask-p msk1))
  (assert (mask-p msk2))
  (assert (= (mask-num-bits msk1) (mask-num-bits msk2)))

  (mask-new (value-and (mask-value msk1) (mask-value msk2)))
)

;;; Return the "and" mask of a mask and the "not" of a mask, state.
(defun mask-and-not (msk1 other) ; -> mask.
  ;; Check arguments.
  (assert (mask-p msk1))

  (cond ((mask-p other)
         ;; Check arguments, continued.
         (assert (= (mask-num-bits msk1) (mask-num-bits other)))
         ;; Calc result
         (mask-and msk1 (mask-not other))
        )
        ((state-p other)
         ;; Check arguments, continued.
         (assert (= (mask-num-bits msk1) (state-num-bits other)))
         ;; Calc result
         (mask-new-and msk1 (state-not other))
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

;;; Return a mask from the Boolean "or" of two masks.
(defun mask-or (msk1 msk2) ; -> mask.
  ;; Check arguments.
  (assert (mask-p msk1))
  (assert (mask-p msk2))
  (assert (= (mask-num-bits msk1) (mask-num-bits msk2)))

  (mask-new (value-or (mask-value msk1) (mask-value msk2)))
)

;;; Return a mask from the "not" bit mask of a given mask.
(defun mask-not (msk) ; -> mask.
  ;; Check argument.
  (assert (mask-p msk))

  ;; Construct result.
  (mask-new (value-not (mask-value msk)))
)

;;; Return a mask from the "not" bit mask of a given mask or state.
(defun mask-new-not (arg1) ; -> mask.
  (cond ((mask-p arg1)
         (mask-new (value-not (mask-value arg1))))
        ((state-p arg1)
         (mask-new (value-not (state-value arg1))))
        (t (error "Invalid argument")))
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
  (mask-eq (mask-and sub-mask sup-mask) sub-mask)
)

;;; Return true if a mask is a ones-superset of another.
(defun mask-superset-of (&key sub-mask sup-mask) ; -> bool
  ;; Check arguments.
  (assert (mask-p sub-mask))
  (assert (mask-p sup-mask))
  (assert (= (mask-num-bits sub-mask) (mask-num-bits sup-mask)))

  ;; Calc result.
  (mask-eq (mask-and sub-mask sup-mask) sub-mask)
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

;;; Return a mask from a Boolean "xor" operation on any combination of state or mask.
(defun mask-new-xor (arg1 arg2) ; -> mask
  (let (val1 val2)
    (cond ((mask-p arg1)  (setf val1 (mask-value arg1)))
          ((state-p arg1) (setf val1 (state-value arg1)))
          (t (error "Invalid arg1")))

    (cond ((mask-p arg2)  (setf val2 (mask-value arg2)))
          ((state-p arg2) (setf val2 (state-value arg2)))
          (t (error "Invalid arg2")))

    (mask-new (value-xor val1 val2))
  )
)

;;; Return a mask from a Boolean "or" operation on any combination of state or mask.
(defun mask-new-or (arg1 arg2) ; -> mask
  (let (val1 val2)
    (cond ((mask-p arg1)  (setf val1 (mask-value arg1)))
          ((state-p arg1) (setf val1 (state-value arg1)))
          (t (error "Invalid arg1")))

    (cond ((mask-p arg2)  (setf val2 (mask-value arg2)))
          ((state-p arg2) (setf val2 (state-value arg2)))
          (t (error "Invalid arg2")))

    (mask-new (value-or val1 val2))
  )
)

;;; Return a mask from a Boolean "and" operation on any combination of state or mask.
(defun mask-new-and (arg1 arg2) ; -> mask
  ;(format t "~&mask-new-and: ~A ~A" (type-of arg1) (type-of arg2))
  (let (val1 val2)
    (cond ((mask-p arg1)  (setf val1 (mask-value arg1)))
          ((state-p arg1) (setf val1 (state-value arg1)))
          (t (error "Invalid arg1")))

    (cond ((mask-p arg2)  (setf val2 (mask-value arg2)))
          ((state-p arg2) (setf val2 (state-value arg2)))
          (t (error "Invalid arg2")))

    (mask-new (value-and val1 val2))
  )
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

;; Return the bits value of a mask.
(defun mask-bits (mskx) ; -> integer.
  ;; Check argument.
  (assert (mask-p mskx))

  ;; Return bits.
  (value-bits (mask-value mskx))
)
