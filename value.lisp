;;;; Implement the value struct and functions.

;;; The value struct holds a given number of bits, GT zero.
(defstruct value
  num-bits  ; Number of bits used.
  bits      ; Bits value, zero to 2^num-bits - 1.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (value-<field name> <instance>) -> struct field.
;   (value-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> value
;   (typep <instance> 'value) -> bool
;
; Probably shouldn't use:
;   (make-value [:<field-name> <field-value>]*), use value-new instead.
;   (copy-value <instance>) copies a value instance.

;;; Return a new value instasnce.
(defun value-new (&key num-bits bits) ; -> value.
  ;; Check arguments.
  (assert (integerp num-bits))
  (assert (plusp num-bits))
  (assert (integerp bits))
  (assert (>= bits 0))
  (assert (< bits (expt 2 num-bits)))

  ;; Construct return value.
  (make-value :num-bits num-bits :bits bits)
)

;;; Given a symbol, like 'v10, 'v11_1100, 'v101, return a valid value.
;;; Underscore characters, which can be used as spacers, are ignored.
;;; All bits must be specified, since the number of bits is kept in the num-bits field in the struct.
(defun value-from (symx) ; -> value.
  ;; Check argument.
  (assert (symbolp symx))

  ;; Init symbol to string name.
  (let ((strx (symbol-name symx)))
    ;; Check argument, continued.
    (assert (> (length strx) 1))

    (let ((ret (value-from-str strx))) ; Process string.
      ;; Check result.
      (if (err-p ret)
        (error (err-str ret))) ; Halt on error.

      ;; Return value.
      ret
    )
  )
)

;;; Get a value from a string.
(defun value-from-str (strx) ; -> value, or err.
  ;; Check argument.
  (assert (stringp strx))

  (let ((str2 "#b")     ; Init binary number string representation.
        (num-bits 0)    ; Init number bit counter.
         valx           ; Init integer to hold binary number.
         strx2)         ; Work string.

    ;; Trim spaces.
    (setf strx2 (string-left-trim '(#\Space #\Tab #\Newline) (string-right-trim '(#\Space #\Tab #\Newline) strx)))

    ;; Check for v prefix.
    (if (not (string-equal (subseq strx2 0 1) "v"))
      (return-from value-from-str (err-new (format nil "value-from-str: Value ~A Should begin with a v character" strx2))))

    ;; Accumulate digits, count digits, skip underscores.
    (loop for chr across (subseq strx2 1) do
      ;; Check one character.
      (cond ((or (char= chr #\0) (char= chr #\1))
             (incf num-bits)
             (setf str2 (concatenate 'string str2 (princ-to-string chr))))
            ((char/= chr #\_)
             (return-from value-from-str (err-new (format nil "value-from-str: Invalid binary digit ~A in ~A" chr strx2))))
      )
    )

    ;; Check number bits GT zero.
    (if (zerop num-bits)
      (return-from value-from-str (err-new (format nil "value-from-str: No valid bit character found in ~A" strx2))))

    ;; Translate string to integer.
    (setf valx (read-from-string str2))

    ;; Construct value to return.
    (value-new :num-bits num-bits :bits valx)
  )
)

;;; Return a string representation of a value.
(defun value-str (val) ; -> string.
  ;; Check argument.
  (assert (value-p val))

  ; Init  binary string representation.
  (let ((bstr (format nil (write-to-string (value-bits val) :base 2))))

    ;; Add zeros to the beginning of the string, to reach the correct number of bits, as needed.
    (loop repeat (- (value-num-bits val) (length bstr)) do
      (setf bstr (concatenate 'string "0" bstr))
    )
    ;; Add prefix and underscores, return result.
    (concatenate 'string "v" (string-add-underscores bstr))
  )
)

;;; Return true if a given value is zero.
(defun value-zerop (val) ; -> bool.
  ;; Check argument.
  (assert (value-p val))

  ;; Calc return value.
  (zerop (value-bits val))
)

;;; Return the number of ones in a given value.
(defun value-num-ones (val) ; -> integer.
  ;; Check argument.
  (assert (value-p val))

  ;; Calc return value.
  (logcount (value-bits val))
)

;;; Return the "not" bit value of a given value.
(defun value-not (val) ; -> value.
  ;; Check argument.
  (assert (value-p val))

  ;; Construct return value.
  (value-new :num-bits (value-num-bits val) :bits (logxor (1- (expt 2 (value-num-bits val))) (value-bits val)))
)

;;; Return true if two given values are equal.
(defun value-eq (val1 val2) ; -> bool.
  ;; Check arguments.
  (assert (value-p val1))
  (assert (value-p val2))
  (assert (= (value-num-bits val1) (value-num-bits val2)))

  ;; Calc return value.
  (= (value-bits val1) (value-bits val2))
)

;;; Return the Booluean OR of two, or more, values.
(defun value-or (&rest vals) ; -> value.
  ;; Check arguments.
  (assert (> (length vals) 1))
  (assert (value-p (car vals)))

  ;; Init working values based on the first value.
  (let* ((first-val (car vals))
         (num-bits (value-num-bits first-val))
         (ret-bits (value-bits first-val)))

    ;; Boolean OR the initial value with subsequent values.
    (loop for valx in (cdr vals) do
      ;; Check arguments, continued.
      (assert (value-p valx))
      (assert (= (value-num-bits valx) num-bits))

      ;; OR the value.
      (setf ret-bits (logior ret-bits (value-bits valx)))
    )
    ;; Construct return value.
    (value-new :num-bits num-bits :bits ret-bits)
  )
)

;;; Return the Boolean AND of two, or more, values.
(defun value-and (&rest vals) ; -> value.
  ;; Check arguments.
  (assert (> (length vals) 1))
  (assert (value-p (car vals)))

  ;; Init working values based on the first value.
  (let* ((first-val (car vals))
         (num-bits (value-num-bits first-val))
         (ret-bits (value-bits first-val)))

    ;; Boolean AND the initial value with subsequent values.
    (loop for valx in (cdr vals) do
      ;; Check arguments, continued.
      (assert (value-p valx))
      (assert (= (value-num-bits valx) num-bits))

      ;; AND the value.
      (setf ret-bits (logand ret-bits (value-bits valx)))
    )
    ;; Construct return value.
    (value-new :num-bits num-bits :bits ret-bits)
  )
)

;;; Return the Boolean XOR of two values.
(defun value-xor (val1 val2) ; -> value.
  ;; Check arguments.
  (assert (value-p val1))
  (assert (value-p val2))
  (assert (= (value-num-bits val1) (value-num-bits val2)))

  ;; Construct return value.
  (value-new :num-bits (value-num-bits val1) :bits (logxor (value-bits val1) (value-bits val2)))
)

;;; Return the Boolean NOT-XOR of two values.
(defun value-eqv (val1 val2) ; -> value.
  ;; Check arguments.
  (assert (value-p val1))
  (assert (value-p val2))
  (assert (= (value-num-bits val1) (value-num-bits val2)))

  ;; Calc return value.
  (value-not (value-xor val1 val2))
)

;;; Return a list of single-bit 1 values from a given value.
(defun value-split (val) ; -> list of values with only one bit, from input, set each
  ;; Check argument.
  (assert (value-p val))

  (let (ret                     ; Return list.
        (val2 (value-bits val)) ; Value bits, to be split.
        val3                    ; Holder for current val2 -1, like 100 - 1 = 011.
        tmp)                    ; Holder for current isolated bit.

    ;; Split bits until none left.
    (loop while (> val2 0) do

       (setf val3 (1- val2))    ; Subtract from the least significant bit.

       (setf tmp (logand (lognot val3) val2)) ; Isolate the least significant bit.

       (push (value-new :num-bits (value-num-bits val) :bits tmp) ret) ; Add the single bit value to the return list.

       (setf val2 (logxor val2 tmp)) ; Erase the current least significant bit.
    )
    ;; Return the value list.
    ret
  )
)

;;; Return a value with the most significant bit set to one.
(defun value-msb (val) ; -> value.
  ;; Check argument.
  (assert (value-p val))

  ;; Construct return value.
  (value-new :num-bits (value-num-bits val) :bits (expt 2 (1- (value-num-bits val))))
)

;;; Return a value with the least significant bit set to one.
(defun value-lsb (val) ; -> value.
  ;; Check argument.
  (assert (value-p val))

  ;; Construct return value.
  (value-new :num-bits (value-num-bits val) :bits 1)
)

;;; Return a value with bits shifted right by one bit.
(defun value-shift-right (val) ; -> value.
  ;; Check arguments.
  (assert (value-p val))

  ;; Construct return value.
  (value-new :num-bits (value-num-bits val) :bits (ash (value-bits val) -1))
)

;;; Return true if a value is low.
(defun value-is-low (val) ; -> bool.
  ;; Check argument.
  (assert (value-p val))

  ;; Calc return value.
  (zerop (value-bits val))
)

;;; Return true if a value is not low.
(defun value-is-not-low (val) ; -> bool.
  ;; Check argument.
  (assert (value-p val))

  ;; Calc return value.
  (not (zerop (value-bits val)))
)

;;; Return true if a value is high.
;;; That is, all bits, within num-bits value, are set to one.
(defun value-is-high (val) ; -> bool.
  ;; Check argument.
  (assert (value-p val))

  ;; Calc return value.
  (= (value-bits val) (1- (expt 2 (value-num-bits val))))
)

;;; Return a value of the same number bits, with a high value.
;;; Reuses an existing value num-bits field.
(defun value-new-high (valx) ; -> value instance.
  ;; Check argument.
  (assert (value-p valx))

  (value-new :num-bits (value-num-bits valx) :bits (1- (expt 2 (value-num-bits valx))))
)

;;; Return a value of the same number bits, with a low value.
;;; Reuses an existing value num-bits field.
(defun value-new-low (valx) ; -> value instance.
  ;; Check argument.
  (assert (value-p valx))

  ;; Construct return value.
  (value-new :num-bits (value-num-bits valx) :bits 0)
)

;;; Return a string for a value list;
(defun value-list-str (vlist) ; -> string
  ;; Check argument.
  (assert (listp vlist))

  (let ((ret "(")   ; Init return string.
        (first t))  ; Init first item flag.

    (loop for valx in vlist do
      ;; Check argument, continued.
      (assert (value-p valx))

      ;; Add separator, if not the first item.
      (if first
        (setf first nil)
        (setf ret (concatenate 'string ret " ")))

      ;; Add item string to list string.
      (setf ret (concatenate 'string ret (value-str valx)))
    )
    ;; Close list string.
    (setf ret (concatenate 'string ret ")"))

    ;; Return result.
    ret
  )
)
