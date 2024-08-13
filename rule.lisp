
(defvar true t)
(defvar false nil)


;;;; Implement the rule struct.
(defstruct (rule (:print-function rule-print))
  b00  ; A mask instance, where each bit set to one represents a 0->0 bit position before/after for a sample.
  b01  ; A mask instance, where each bit set to one represents a 0->1 bit position before/after for a sample.
  b11  ; A mask instance, where each bit set to one represents a 1->1 bit position before/after for a sample.
  b10  ; A mask instance, where each bit set to one represents a 1->0 bit position before/after for a sample.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (rule-<field name> <instance>) -> struct field.
;   (rule-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> rule
;   (typep <instance> 'rule) -> bool
;
; Probably shouldn't use:
;   (make-rule [:<field-name> <field-rule>]*), use rule-new instead.
;   (copy-rule <instance>) copies a rule instance.
(defun rule-new (smpl) ; -> rule instance.
  (assert (sample-p smpl))

  (let ((b00 (mask-new (value-and (value-not (state-value (sample-initial smpl))) (value-not (state-value (sample-result smpl))))))
        (b01 (mask-new (value-and (value-not (state-value (sample-initial smpl))) (state-value (sample-result smpl)))))
        (b11 (mask-new (value-and (state-value (sample-initial smpl)) (state-value (sample-result smpl)))))
        (b10 (mask-new (value-and (state-value (sample-initial smpl)) (value-not (state-value (sample-result smpl))))))
       )
    (make-rule :b00 b00 :b01 b01 :b11 b11 :b10 b10)
  )
)

;;; Return a rule from a token, like "[00/01/11/10/x0/X0/x1/X1/XX/xx/Xx/xX]"
;;; Do not end a string with the slash character since it shifts the bits to the left.
;;; Keying a bit-position change on the slash character allows testing the output of
;;; disallowed combinations.
;;; Undeclared bit positions to the left, due to more positions in the Value-mask, become 0->0.
(defun rule-from-str (str) ; -> rule instance.
  (assert (stringp str))

  (let ((ret (rule-from-str-na str)))
    (cond ((err-p ret) (error (err-str ret)))
          ((rule-p ret) ret)
           (t (error "Result is not a rule")))
  )
)
(defun rule-from-str-na (strx) ; -> rule instance or error string
    (if (< (length strx) 4)
        (return-from rule-from-str-na (err-new "String is too short")))

    (if (not (string-equal (subseq strx 0 1) "["))
        (return-from rule-from-str-na (err-new "String should start with a right bracket")))

    (let ((b00 "#b") (b01 "#b") (b11 "#b") (b10 "#b") bit-i bit-j b00-i b01-i b11-i b10-i)
        (loop for chr across (subseq strx 1) do

            (cond ((or (char= chr #\/) (char= chr #\_) (char= chr #\])) ; Check for separator or end-of-rule.
	            
		    (if (or (null bit-i) (null bit-j)) 
                       (return-from rule-from-str-na (err-new "Malformed bit position")))

		    ; Set mask strings.
		    (setf b00-i "0" b01-i "0" b11-i "0" b10-i "0")
                    (cond
		      ((and (char= bit-i #\0) (char= bit-j #\0)) (setf b00-i "1"))
		      ((and (char= bit-i #\0) (char= bit-j #\1)) (setf b01-i "1"))
		      ((and (char= bit-i #\1) (char= bit-j #\1)) (setf b11-i "1"))
		      ((and (char= bit-i #\1) (char= bit-j #\0)) (setf b10-i "1"))
		      ((and (or (char= bit-i #\X) (char= bit-i #\x)) (char= bit-j #\0)) (setf b10-i "1") (setf b00-i "1"))
		      ((and (or (char= bit-i #\X) (char= bit-i #\x)) (char= bit-j #\1)) (setf b01-i "1") (setf b11-i "1"))
		      ((and (char= bit-i #\X) (char= bit-j #\X)) (setf b00-i "1") (setf b11-i "1"))
		      ((and (char= bit-i #\x) (char= bit-j #\x)) (setf b00-i "1") (setf b11-i "1"))
		      ((and (char= bit-i #\X) (char= bit-j #\x)) (setf b01-i "1") (setf b10-i "1"))
		      ((and (char= bit-i #\x) (char= bit-j #\X)) (setf b01-i "1") (setf b10-i "1"))
		      (t (return-from rule-from-str-na (err-new "Invalid character or combination")))
		    )
		    ; Add a bit position to the mask strings.
		    (setf b00 (concatenate 'string b00 b00-i))
		    (setf b01 (concatenate 'string b01 b01-i))
		    (setf b11 (concatenate 'string b11 b11-i))
		    (setf b10 (concatenate 'string b10 b10-i))

                    (setf bit-i nil bit-j nil) ; Init for next bit position.

                  (when (char= chr #\]) ; End-of-rule.

		     ; Return new rule.
                     (return-from rule-from-str-na
                                      (make-rule :b00 (mask-from-str b00)
                                                 :b01 (mask-from-str b01)
                                                 :b11 (mask-from-str b11)
                                                 :b10 (mask-from-str b10)))
                   ))
                  ((null bit-i) (setf bit-i chr)) ; Set first char of bit position.
                  ((null bit-j) (setf bit-j chr)) ; Set second char of bit position.
                  (t 
                     (return-from rule-from-str-na (err-new "Too many characters in a bit position"))))

        ) ;; end loop
        (err-new "Missing closing bracket?")
    ) ;; end let
)

;;; Return a string representation of a rule
(defun rule-str (rulx) ; -> string.
  (assert (rule-p rulx))

    (let (
          (strs "[")
          (b00 (rule-b00 rulx))
          (b01 (rule-b01 rulx))
          (b11 (rule-b11 rulx))
          (b10 (rule-b10 rulx))
          bitval
          (bit-pos (mask-msb (rule-b00 rulx))) 
          (not-start nil)
	  (cnt (mask-num-bits (rule-b00 rulx)))
         )

         (loop while (not (mask-zerop bit-pos)) do
	     (setf bitval 0)
             (if (not (value-zerop (mask-and bit-pos b00)))
                 (setf bitval 1))
             (if (not (value-zerop (mask-and bit-pos b01)))
                 (incf bitval 2))
             (if (not (value-zerop (mask-and bit-pos b11)))
                 (incf bitval 4))
             (if (not (value-zerop (mask-and bit-pos b10)))
                 (incf bitval 8))

             (if not-start (if (zerop (mod cnt 4))
			     (setf strs (concatenate 'string strs "_"))
			     (setf strs (concatenate 'string strs "/"))))

	     (setf not-start t)
	     (decf cnt)

             (cond ((= bitval  0) (setf strs (concatenate 'string strs "..")))
                   ((= bitval  1) (setf strs (concatenate 'string strs "00")))
                   ((= bitval  2) (setf strs (concatenate 'string strs "01")))
                   ((= bitval  3) (setf strs (concatenate 'string strs "0X?")))
                   ((= bitval  4) (setf strs (concatenate 'string strs "11")))
                   ((= bitval  5) (setf strs (concatenate 'string strs "XX")))
                   ((= bitval  6) (setf strs (concatenate 'string strs "X1")))
                   ((= bitval  7) (setf strs (concatenate 'string strs "0X?11")))
                   ((= bitval  8) (setf strs (concatenate 'string strs "10")))
                   ((= bitval  9) (setf strs (concatenate 'string strs "X0")))
                   ((= bitval 10) (setf strs (concatenate 'string strs "Xx")))
                   ((= bitval 11) (setf strs (concatenate 'string strs "0X?10")))
                   ((= bitval 12) (setf strs (concatenate 'string strs "1X?")))
                   ((= bitval 13) (setf strs (concatenate 'string strs "1X?00")))
                   ((= bitval 14) (setf strs (concatenate 'string strs "1X?01")))
                   ((= bitval 15) (setf strs (concatenate 'string strs "1X?0X?")))
                   (t (setf strs (concatenate 'string strs "..")))
             )
             (setf bit-pos (mask-shift bit-pos -1))
         ) ; end-while

    (setf strs (concatenate 'string strs "]"))
    strs
    )
)

; Print a rule.
(defun rule-print (instance stream depth)
  ;(assert (zerop depth))
  (format stream (rule-str instance))
)

; Return the Boolean "or", or union, of two rules.
(defun rule-union (rul1 rul2) ; -> rule instance.
  (assert (rule-p rul1))
  (assert (rule-p rul2))
  (assert (= (rule-num-bits rul1) (rule-num-bits rul2)))

  (make-rule :b00 (mask-new (mask-or (rule-b00 rul1) (rule-b00 rul2)))
             :b01 (mask-new (mask-or (rule-b01 rul1) (rule-b01 rul2)))
             :b11 (mask-new (mask-or (rule-b11 rul1) (rule-b11 rul2)))
             :b10 (mask-new (mask-or (rule-b10 rul1) (rule-b10 rul2))))
)

; Return true if a rule is a valid union, that is no 1X, or 0X, bit positions.
(defun rule-is-valid-union (rul) ; -> bool
  (assert (rule-p rul))

  (and
    (value-zerop (mask-and (rule-b00 rul) (rule-b01 rul)))
    (value-zerop (mask-and (rule-b11 rul) (rule-b10 rul)))
  )
)

; Return the Boolean "and", or intersection, of two rules.
(defun rule-intersection (rul1 rul2) ; -> rule instance.
  (assert (rule-p rul1))
  (assert (rule-p rul2))
  (assert (= (rule-num-bits rul1) (rule-num-bits rul2)))

  (make-rule :b00 (mask-new (mask-and (rule-b00 rul1) (rule-b00 rul2)))
             :b01 (mask-new (mask-and (rule-b01 rul1) (rule-b01 rul2)))
             :b11 (mask-new (mask-and (rule-b11 rul1) (rule-b11 rul2)))
             :b10 (mask-new (mask-and (rule-b10 rul1) (rule-b10 rul2))))
)

; Return true if a rule is a valid intersection, that is no bit position is zero for all four masks.
(defun rule-is-valid-intersection (rul) ; -> bool.
  (assert (rule-p rul))

    (value-zerop (value-not (value-or
			      (mask-value (rule-b00 rul))
			      (value-or (mask-value (rule-b01 rul))
					(mask-or (rule-b11 rul) (rule-b10 rul))))))
)

; Return true if two rules are equal.
(defun rule-eq (rul1 rul2) ; -> bool
  (assert (rule-p rul1))
  (assert (rule-p rul2))
  (assert (= (rule-num-bits rul1) (rule-num-bits rul2)))

  (and (mask-eq (rule-b00 rul1) (rule-b00 rul2))
       (mask-eq (rule-b01 rul1) (rule-b01 rul2))
       (mask-eq (rule-b11 rul1) (rule-b11 rul2))
       (mask-eq (rule-b10 rul1) (rule-b10 rul2)))
)

; Ruturn the number oy bits used by a rules masks.
(defun rule-num-bits (rulx) ; -> a number
  (mask-num-bits (rule-b00 rulx))
)

; Return the initial region of a rule.
(defun rule-initial-region (rulx) ; -> region
  (assert (rule-p rulx))

  (let (
    (sta1 (state-new (mask-or (rule-b10 rulx) (rule-b11 rulx))))
    (sta2 (state-new (value-not (mask-or (rule-b01 rulx) (rule-b00 rulx))))))

    (if (state-eq sta1 sta2)
      (region-new (statestore-new (list sta1)))
      (region-new (statestore-new (list sta1 sta2))))
  )
)

; Return the result region of a rule.
(defun rule-result-region (rulx) ; -> region
  (assert (rule-p rulx))

  (let (
    (sta1 (state-new (mask-or (rule-b11 rulx) (rule-b01 rulx))))
    (sta2 (state-new (value-not (mask-or (rule-b00 rulx) (rule-b10 rulx))))))

    (if (state-eq sta1 sta2)
      (region-new (statestore-new (list sta1)))
      (region-new (statestore-new (list sta1 sta2))))
  )
)

; Return true if a rule is a subset of another.
(defun rule-subset-of (&key sub-rule sup-rule) ; -> bool
  (assert (rule-p sub-rule))
  (assert (rule-p sup-rule))
  (assert (= (rule-num-bits sub-rule) (rule-num-bits sup-rule)))

  (if (not (mask-subset-of :sub-mask (rule-b00 sub-rule) :sup-mask (rule-b00 sup-rule)))
    (return-from rule-subset-of false))

  (if (not (mask-subset-of :sub-mask (rule-b01 sub-rule) :sup-mask (rule-b01 sup-rule)))
    (return-from rule-subset-of false))

  (if (not (mask-subset-of :sub-mask (rule-b11 sub-rule) :sup-mask (rule-b11 sup-rule)))
    (return-from rule-subset-of false))

  (if (not (mask-subset-of :sub-mask (rule-b10 sub-rule) :sup-mask (rule-b10 sup-rule)))
    (return-from rule-subset-of false))

  true
)

; Return a rule to translate from one region to another.
(defun rule-new-region-to-region (reg1 reg2)
  (assert (region-p reg1))
  (assert (region-p reg2))
  (assert (= (region-num-bits reg1) (region-num-bits reg2)))

  (let (b00 b0x bxx bx0 b01 bx1 b11 b1x b10)

    (setf b00 (mask-and (region-0-mask reg1) (region-0-mask reg2)))
    (setf b0x (mask-and (region-0-mask reg1) (region-x-mask reg2)))
    (setf bxx (mask-and (region-x-mask reg1) (region-x-mask reg2)))
    (setf bx0 (mask-and (region-x-mask reg1) (region-0-mask reg2)))
    (setf b01 (mask-and (region-0-mask reg1) (region-1-mask reg2)))
    (setf bx1 (mask-and (region-x-mask reg1) (region-1-mask reg2)))
    (setf b11 (mask-and (region-1-mask reg1) (region-1-mask reg2)))
    (setf b1x (mask-and (region-1-mask reg1) (region-x-mask reg2)))
    (setf b10 (mask-and (region-1-mask reg1) (region-0-mask reg2)))

    (make-rule :b00 (mask-new (value-or b00 b0x bxx bx0))
               :b01 (mask-new (value-or b01 bx1))
               :b11 (mask-new (value-or b11 b1x bxx bx1))
               :b10 (mask-new (value-or b10 bx0)))
  )
)

; Mask off one positions in a rule, that has 0->0, or 0->1, in the same positions.
(defun rule-mask-off-ones (rulex maskx) ; -> rule instance.
  (assert (rule-p rulex))
  (assert (mask-p maskx))
  (assert (= (rule-num-bits rulex) (mask-num-bits maskx)))

  (let (msk-not rulz)
    (setf msk-not (mask-new (mask-not maskx)))
 
    (setf rulz (make-rule :b00 (rule-b00 rulex)
                          :b01 (rule-b01 rulex)
                          :b11 (mask-new (mask-and (rule-b11 rulex) msk-not))
                          :b10 (mask-new (mask-and (rule-b10 rulex) msk-not))))

    (assert (rule-is-valid-intersection rulz))
    rulz
  )
)

; Mask off zero positions in a rule, that has 1->1, or 1->0, in the same positions.
(defun rule-mask-off-zeros (rulex maskx) ; -> rule instance.
  (assert (rule-p rulex))
  (assert (mask-p maskx))
  (assert (= (rule-num-bits rulex) (mask-num-bits maskx)))

  (let (msk-not rulz)
    (setf msk-not (mask-new (mask-not maskx)))
 
    (setf rulz (make-rule :b00 (mask-new (mask-and (rule-b00 rulex) msk-not))
                          :b01 (mask-new (mask-and (rule-b01 rulex) msk-not))
                          :b11 (rule-b11 rulex)
                          :b10 (rule-b10 rulex)))

    (assert (rule-is-valid-intersection rulz))
    rulz
  )
)

; Return the combination of two rules where the result region of the first rule
; intersects the initial region of the second rule.
(defun rule-combine-sequence2 (rul1 rul2) ; -> rule instance.
  (assert (rule-p rul1))
  (assert (rule-p rul2))
  (assert (= (rule-num-bits rul1) (rule-num-bits rul2)))
  (assert (region-intersects (rule-result-region rul1) (rule-initial-region rul2)))

  (make-rule :b00 (mask-new (value-or (mask-and (rule-b00 rul1) (rule-b00 rul2)) (mask-and (rule-b01 rul1) (rule-b10 rul2)))) 
             :b01 (mask-new (value-or (mask-and (rule-b01 rul1) (rule-b11 rul2)) (mask-and (rule-b00 rul1) (rule-b01 rul2)))) 
             :b11 (mask-new (value-or (mask-and (rule-b11 rul1) (rule-b11 rul2)) (mask-and (rule-b10 rul1) (rule-b01 rul2)))) 
             :b10 (mask-new (value-or (mask-and (rule-b10 rul1) (rule-b00 rul2)) (mask-and (rule-b11 rul1) (rule-b10 rul2))))) 
)

; Return the combination of two rules.
; The result region of the first rule may, or may not,  intersect the initial region of the second rule.
(defun rule-combine-sequence (rul1 rul2) ; -> rule instance.
  (assert (rule-p rul1))
  (assert (rule-p rul2))
  (assert (= (rule-num-bits rul1) (rule-num-bits rul2)))

  (if (region-intersects (rule-result-region rul1) (rule-initial-region rul2))
    (return-from rule-combine-sequence (rule-combine-sequence2 rul1 rul2)))

  (let ((rule-between (rule-new-region-to-region (rule-result-region rul1) (rule-initial-region rul2))))
    (rule-combine-sequence (rule-combine-sequence rul1 rule-between) rul2)
  )
)
