;;;; Implement the value struct and functions.

;;; The value struct.
;;; It holds a given number of bits.
(defstruct (value (:print-function value-print))
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
  (assert (integerp num-bits))
  (assert (plusp num-bits))
  (assert (integerp bits))
  (assert (>= bits 0))
  (assert (< bits (expt 2 num-bits)))

  (make-value :num-bits num-bits :bits bits)
)

;;; Given a string like "#x123", "#b1100", return a valid value.
;;; Underscore characters, which can be used as spacers, are ignored.
(defun value-from-str (str) ; -> value.
  (assert (stringp str))
  (assert (> (length str) 2))

  (let ((ret (value-from-str-na str)))
    (cond ((err-p ret) (error (err-str ret)))
          ((value-p ret) ret)
           (t (error "Result is not a value"))))
)
;;; value-from-str no-abort (na).
(defun value-from-str-na (str) ; -> value, or err.
  ;(format t "~&value-from-str-na: ~A" str)

  (let (valx bin hex (digit-num 0) str2 num-bits)
    ; Check for base indicators.
    (if (string= (subseq str 0 1) "#") 
      (cond ((or (string= (subseq str 1 2) "b") (string= (subseq str 1 2) "B")) (setf bin t))
            ((or (string= (subseq str 1 2) "x") (string= (subseq str 1 2) "X")) (setf hex t))
	     (t (return-from value-from-str-na (err-new "Second character is not b, B, x or X")))
      )
      (return-from value-from-str-na (err-new "String does not begin with the # character"))
    )
    ;; Init second string.
    (setf str2 (subseq str 0 2))

    ;; Count digits, skip underscores.
    (loop for chr across (subseq str 2) do
      (cond ((char= chr #\_) nil)
             (t (incf digit-num)
		(if bin
		  (if (not (and (char>= chr #\0) (char<= chr #\1)))
		    (return-from value-from-str-na (err-new "Invalid binary digit")))
		  (if (not (or
		    (and (char>= chr #\0) (char<= chr #\9))
		    (and (char>= chr #\a) (char<= chr #\f))
		    (and (char>= chr #\A) (char<= chr #\F))))
		      (return-from value-from-str-na (err-new "Invalid hexadecimal digit")))
		)
	        (setf str2 (concatenate 'string str2 (princ-to-string chr))))
      )
    ) ; end loop

    ;; Calc number bits.
    (if bin (setf num-bits digit-num) (setf num-bits (* digit-num 4)))

    ;; Translate string to integer.
    (setf valx (read-from-string str2))

    ;; Create value to return.
    (value-new :num-bits num-bits :bits valx)
  )
)

;;; Add underscores for each 4 characters of a string, from right to left.
(defun string-add-underscores (str) ; -> string.
  (assert (stringp str))

  (let ((ret (string-add-underscores-na str)))
    (cond ((err-p ret) (error (err-str ret)))
	  ((stringp ret) ret)
	  (t (error "Result is not a string"))))
)
;;; Add underscores no-abort (na).
(defun string-add-underscores-na (str) ; -> string, or err.

  (let ((str2 "") cnt (str-len (length str)))
     (setf cnt str-len)
     (loop for chr across str do
       (if (char= chr #\_)
           (return-from string-add-underscores-na (err-new "Argument contains underscores")))

       (if (and (/= cnt str-len) (zerop (mod cnt 4)))
           (setf str2 (concatenate 'string str2 "_"))
       )
       (setf str2 (concatenate 'string str2 (princ-to-string chr)))
       (decf cnt)
     )
     str2
  )
)

;;; Return a string representation of a value.
;;; Use hexadecimal in preference to binary, if possible.
(defun value-str (val) ; -> string.
  (assert (value-p val))

  (if (zerop (mod (value-num-bits val) 4))
    (value-str-hex val)
    (value-str-bin val)
  )
)

;;; Print a value.
(defun value-print (instance stream depth)
  ;(assert (zerop depth))
  (format stream (value-str instance))
)

;;; Return a string representing a value in binary
(defun value-str-bin (val) ; -> string.
  (assert (value-p val))

  (let (str str-len val-len)

    (setf str (format nil (write-to-string (value-bits val) :base 2)))
    (setf str-len (length str))
    (setf val-len (value-num-bits val))

    (while (< str-len val-len)
      (setf str (concatenate 'string "0" str))
      (incf str-len)
    )
    (concatenate 'string "#b" (string-add-underscores str))
  )
)

;;; Return a string representing a value in hexadecimal
(defun value-str-hex (val) ; -> string.
  (assert (value-p val))
  (assert (zerop (mod (value-num-bits val) 4)))

  (let (str str-len val-len)

    (setf str (format nil (write-to-string (value-bits val) :base 16)))
    (setf str-len (length str))
    (setf val-len (/ (value-num-bits val) 4))

    (while (< str-len val-len)
      (setf str (concatenate 'string "0" str))
      (incf str-len)
    )
    (concatenate 'string "#x" (string-add-underscores str))
  )
)

;;; Return true if a given value is zero.
(defun value-zerop (val) ; -> bool.
  (assert (value-p val))

  (zerop (value-bits val))
)

;;; Return the number of ones in a given value.
(defun value-num-ones (val) ; -> integer.
  (assert (value-p val))

  (logcount (value-bits val))
)

;;; Return the "not" bit value of a given value.
(defun value-not (val) ; -> value.
  (assert (value-p val))

  ; Create value to return.
  (value-new :num-bits (value-num-bits val) :bits (logxor (- (expt 2 (value-num-bits val)) 1) (value-bits val)))
)

;;; Return true if two given values are adjacent.
(defun value-is-adjacent (val1 val2) ; -> bool.
  (let ((ret (value-is-adjacent-na val1 val2)))
    (cond ((err-p ret) (error (err-str ret)))
	  ((bool-p ret) ret)
	  (t (error "Result is not a bool"))))
)
;;; value-is-adjacent no-abort (na).
(defun value-is-adjacent-na (val1 val2) ; -> bool, or err.
  (if (not (value-p val1))
    (return-from value-is-adjacent-na (err-new "Argument 1 is not a value")))

  (if (not (value-p val2))
    (return-from value-is-adjacent-na (err-new "Argument 2 is not a value")))

  (if (not (= (value-num-bits val1) (value-num-bits val2)))
    (return-from value-is-adjacent-na
		 (err-new "Argument 1 and 2 use a different number of bits")))

  (= 1 (value-num-ones (value-xor val1 val2)))
)

;;; Return true if two given values are equal.
(defun value-eq (val1 val2) ; -> bool.
  (assert (value-p val1))
  (assert (value-p val2))
  (assert (= (value-num-bits val1) (value-num-bits val2)))

  (= (value-bits val1) (value-bits val2))
)

;;; Return the "or" bit value of two, or more, values.
(defun value-or (&rest vals) ; -> value.
  (assert (> (length vals) 1))
  (assert (value-p (car vals)))

  (let* ((first-val (car vals))
	 (num-bits (value-num-bits first-val))
	 (ret-bits (value-bits first-val)))

    (loop for valx in (cdr vals) do
        (assert (value-p valx))
	(assert (= (value-num-bits valx) num-bits))

	(setf ret-bits (logior ret-bits (value-bits valx)))
    )
    (value-new :num-bits num-bits :bits ret-bits)
  )
)

;;; Return the "and" bit value of two, or more, given values.
(defun value-and (&rest vals) ; -> value.
  (assert (> (length vals) 1))
  (assert (value-p (car vals)))

  (let* ((first-val (car vals))
	 (num-bits (value-num-bits first-val))
	 (ret-bits (value-bits first-val)))

    (loop for valx in (cdr vals) do
        (assert (value-p valx))
	(assert (= (value-num-bits valx) num-bits))

	(setf ret-bits (logand ret-bits (value-bits valx)))
    )
    (value-new :num-bits num-bits :bits ret-bits)
  )
)

;;; Return the "xor" bit value of two given values.
(defun value-xor (val1 val2) ; -> value.
  (assert (value-p val1))
  (assert (value-p val2))
  (assert (= (value-num-bits val1) (value-num-bits val2)))

  ; Create value to return.
  (value-new :num-bits (value-num-bits val1) :bits (logxor (value-bits val1) (value-bits val2)))
)

;;; Return the "not xor" bit value of two given values.
(defun value-eqv (val1 val2) ; -> value.
  (assert (value-p val1))
  (assert (value-p val2))
  (assert (= (value-num-bits val1) (value-num-bits val2)))

  ; Create value to return.
  (value-not (value-xor val1 val2))
)

;;; Return a list of single-bit 1 values from a given value.
(defun value-split (val) ; -> list of values with only one bit, from input, set each
  (assert (value-p val))

  (let (ret (val2 (value-bits val)) val3 tmp (all-bits (1- (expt 2 (value-num-bits val)))))

    (loop while (> val2 0) do

       (setf val3 (1- val2))

       (setf tmp (logand (logxor val3 all-bits) val2))

       (push (value-new :num-bits (value-num-bits val) :bits tmp) ret)

       (setf val2 (logxor val2 tmp))

    ) ; end-loop
    ret
  )
)

;;; Given a value, and two others, return true if the first value is between the other two.
(defun value-between (&key target from to) ; -> bool
  (assert (value-p target))
  (assert (value-p from))
  (assert (value-p to))
  (assert (= (value-num-bits target) (value-num-bits from)))
  (assert (= (value-num-bits target) (value-num-bits to)))

  (value-zerop (value-and (value-xor target from) (value-xor target to)))
)

;;; Return a value with the most significant bit set to one.
(defun value-msb (val) ; -> value.
  (assert (value-p val))

  (value-new :num-bits (value-num-bits val) :bits (expt 2 (1- (value-num-bits val))))
)

;;; Return a value with bits shifted by a given value.
;;; A positive integer shifts left.
;;; A negative integer shifts right.
(defun value-shift (val num) ; -> value.
  (assert (value-p val))
  (assert (integerp num))
  (assert (<= (abs num) (value-num-bits val)))

  (let ((new-bits (ash (value-bits val) num)))
      (if (plusp new-bits)
	(setf new-bits (logand new-bits (1- (expt 2 (value-num-bits val))))) ; Some zero bits may have shifted too far left.
      )
      (value-new :num-bits (value-num-bits val) :bits new-bits)
  )
)

;;; Return true if a value is low.
(defun value-is-low (val) ; -> bool.
  (assert (value-p val))

  (zerop (value-bits val))
)

;;; Return true if a value is not low.
(defun value-is-not-low (val) ; -> bool.
  (assert (value-p val))

  (not (zerop (value-bits val)))
)

;;; Return true if a value is high.
(defun value-is-high (val) ; -> bool.
  (assert (value-p val))

  (= (- (expt 2 (value-num-bits val)) 1) (value-bits val))
)

