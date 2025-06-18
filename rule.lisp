;;;; Implement the rule struct and functions.
;;;;
;;;; The rule struct is a representation of a change, from region to region, like X01X -> 100X.
;;;;
;;;; The rule struct can be manipulated in a number of ways, like union and intersection.

;;; The rule struct.
;;;
;;; Single before/after samples can be directly set, for 0->0, 0->1, 1->1 and 1->0.
;;;
;;; Combinations of single changes can be combined to make
;;; X->X = (0->0, 1->1)
;;; X->0 = (0->0, 1->0)
;;; X->1 = (1->1, 0->1)
;;; X->x = (1->0, 1->0)
;;;
;;; 0->X, 1->X are disallowed.
;;;   Something has to be disallowed, or one rule, with all 0-X,1->X positions, is the end result.
;;;   These are not predictive in forward-chaining.
;;;
;;;   Backward-chaining would seem to disallow X->0 and X->1, but successful backward-chaining
;;;   is converted to forward-chaining to run a plan.
(defstruct rule
  m00  ; A mask, where each bit set to one represents a 0->0 bit position before/after for a sample.
  m01  ; A mask, where each bit set to one represents a 0->1 bit position before/after for a sample.
  m11  ; A mask, where each bit set to one represents a 1->1 bit position before/after for a sample.
  m10  ; A mask, where each bit set to one represents a 1->0 bit position before/after for a sample.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (rule-<field name> <instance>) -> struct field.
;   (rule-p <instance>) -> bool.
;
; Least used:
;   (type-of <instance>) -> rule symbol.
;   (typep <instance> 'rule) -> bool.
;
; Probably shouldn't use:
;   (make-rule [:<field-name> <field-rule>]*), use rule-new if possible..
;   (copy-rule <instance>) copies a rule instance.

;;; Return a new rule, given a sample.
(defun rule-new (smpl) ; -> rule.
  ;; Check argument.
  (assert (sample-p smpl))

  (let ((m00 (mask-new-and (state-not (sample-initial smpl)) (state-not (sample-result smpl))))
        (m01 (mask-new-and (sample-result smpl) (state-not (sample-initial smpl))))
        (m11 (mask-new-and (sample-initial smpl) (sample-result smpl)))
        (m10 (mask-new-and (sample-initial smpl) (state-not (sample-result smpl))))
       )
    ;; Construct result.
    (make-rule :m00 m00 :m01 m01 :m11 m11 :m10 m10)
  )
)

;;; Return a rule from a string, like '[01/10], or a string like "[00/01/11/10/x0/X0/x1/X1/XX/xx/Xx/xX]"
;;; XX == xx, x to x.
;;; Xx == xX, x to x-not.
(defun rule-from-str (strx) ; -> rule.
  ;; Check argument.
  (assert (stringp strx))

  (let (ret strx2)
    ;; Trim white space.
    (setf strx2 (string-left-trim '(#\Space #\Tab #\Newline) (string-right-trim '(#\Space  #\Tab #\Newline) strx)))

    ;; Get result of parsing string.
    (setf ret (rule-from-str2 strx2))

    ;; Check result.
    (cond ((rule-p ret) ret)  ; Return result.
          ((err-p ret) (error (err-str ret)))
           (t (error "Result is not a rule")))
  )
)

;;; Return a rule from a string, or an err instance.
(defun rule-from-str2 (strx) ; -> rule or err.
  ;; Check argument.
  (assert (stringp strx))

  ;; Check for no bit positions given.
  (if (< (length strx) 3)
      (return-from rule-from-str2 (err-new "String is too short")))

  ;; Check for prefix.
  (if (not (string-equal (subseq strx 0 1) "["))
      (return-from rule-from-str2 (err-new "String must begin with a [")))

  ;; Check for suffix.
  (if (not (string-equal (subseq strx (1- (length strx))) "]"))
      (return-from rule-from-str2 (err-new "String must end with a ]")))

  ;; Init four mask strings, working variables.
  (let ((m00 "m") (m01 "m") (m11 "m") (m10 "m") bit-i bit-j m00-i m01-i m11-i m10-i continue-loop)

    ;; For each character, after the prefix.
    (setf continue-loop true)
    (loop for chr across (subseq strx 1)
          while continue-loop do
  
      ;; Check for bit position character, or delimiting character.
      (cond ((or (char= chr #\]) (char= chr #\_) (char= chr #\/))
              ;; Process a delimiting character.
              (if (or (null bit-i) (null bit-j))
                (return-from rule-from-str2 (err-new "Too few characters given for a bit position")))
  
              ;; Set mask strings.
              (setf m00-i "0" m01-i "0" m11-i "0" m10-i "0")
              (cond ((and (char= bit-i #\0) (char= bit-j #\0)) (setf m00-i "1"))
                    ((and (char= bit-i #\0) (char= bit-j #\1)) (setf m01-i "1"))
                    ((and (char= bit-i #\1) (char= bit-j #\1)) (setf m11-i "1"))
                    ((and (char= bit-i #\1) (char= bit-j #\0)) (setf m10-i "1"))
                    ((and (or (char= bit-i #\X) (char= bit-i #\x)) (char= bit-j #\0)) (setf m10-i "1") (setf m00-i "1"))
                    ((and (or (char= bit-i #\X) (char= bit-i #\x)) (char= bit-j #\1)) (setf m01-i "1") (setf m11-i "1"))
                    ((and (char= bit-i #\X) (char= bit-j #\X)) (setf m00-i "1") (setf m11-i "1"))
                    ((and (char= bit-i #\x) (char= bit-j #\x)) (setf m00-i "1") (setf m11-i "1"))
                    ((and (char= bit-i #\X) (char= bit-j #\x)) (setf m01-i "1") (setf m10-i "1"))
                    ((and (char= bit-i #\x) (char= bit-j #\X)) (setf m01-i "1") (setf m10-i "1"))
                    ((and (char= bit-i #\0) (char= bit-j #\X)) (setf m01-i "1") (setf m00-i "1"))
                    ((and (char= bit-i #\0) (char= bit-j #\x)) (setf m01-i "1") (setf m00-i "1"))
                    ((and (char= bit-i #\1) (char= bit-j #\X)) (setf m10-i "1") (setf m11-i "1"))
                    ((and (char= bit-i #\1) (char= bit-j #\x)) (setf m10-i "1") (setf m11-i "1"))
                    (t (return-from rule-from-str2 (err-new "Invalid character or combination")))
              ) ; end cond
  
              ;; Add a bit position to the mask strings.
              (setf m00 (concatenate 'string m00 m00-i))
              (setf m01 (concatenate 'string m01 m01-i))
              (setf m11 (concatenate 'string m11 m11-i))
              (setf m10 (concatenate 'string m10 m10-i))
  
              ;; Init for next bit position.
              (setf bit-i nil bit-j nil)

              (if (char= chr #\])
                (setf continue-loop false))
            )
            ((or (char= chr #\0) (char= chr #\1) (char= chr #\X) (char= chr #\x))
              ;; Process a bit position character.
              (cond ((null bit-i)
                     (setf bit-i chr))
                    ((null bit-j)
                     (setf bit-j chr))
                    (t (return-from rule-from-str2 (err-new "Too many characters in a bit position")))
              )
            )
           (t (return-from rule-from-str2 (err-new (format nil "Invalid character ~A" chr))))
      )
    ) ; end loop

    ;; Return new rule.
    (make-rule :m00 (mask-from-str m00)
               :m01 (mask-from-str m01)
               :m11 (mask-from-str m11)
               :m10 (mask-from-str m10))
  ) ; end let
)

;;; Return a string representation of a rule, like [01/10/XX].
(defun rule-str (rulx) ; -> string.
  (assert (rule-p rulx))

  (let ((strs "[")
        (m00 (rule-m00 rulx))
        (m01 (rule-m01 rulx))
        (m11 (rule-m11 rulx))
        (m10 (rule-m10 rulx))
        bitval
        (bit-pos (mask-msb (rule-m00 rulx)))
        (not-start nil)
        (cnt (mask-num-bits (rule-m00 rulx)))
       )

       (loop while (not (mask-zerop bit-pos)) do

         ;; Assign value to bitval according to the rule masks.
         (setf bitval 0)
         (if (not (mask-is-low (mask-and bit-pos m00)))
             (setf bitval 1))
         (if (not (mask-is-low (mask-and bit-pos m01)))
             (incf bitval 2))
         (if (not (mask-is-low (mask-and bit-pos m11)))
             (incf bitval 4))
         (if (not (mask-is-low (mask-and bit-pos m10)))
             (incf bitval 8))

         ;; Add separator, if needed.
         (if not-start (if (zerop (mod cnt 4))
           (setf strs (concatenate 'string strs "_"))
           (setf strs (concatenate 'string strs "/"))))

         (setf not-start t)

         (decf cnt) ; Decrement cnt.

         ; Transate bitval to string.
         (cond ((= bitval  0) (setf strs (concatenate 'string strs "..")))
               ((= bitval  1) (setf strs (concatenate 'string strs "00")))
               ((= bitval  2) (setf strs (concatenate 'string strs "01")))
               ((= bitval  3) (setf strs (concatenate 'string strs "0X")))
               ((= bitval  4) (setf strs (concatenate 'string strs "11")))
               ((= bitval  5) (setf strs (concatenate 'string strs "XX")))
               ((= bitval  6) (setf strs (concatenate 'string strs "X1")))
               ((= bitval  7) (setf strs (concatenate 'string strs "0X?11")))
               ((= bitval  8) (setf strs (concatenate 'string strs "10")))
               ((= bitval  9) (setf strs (concatenate 'string strs "X0")))
               ((= bitval 10) (setf strs (concatenate 'string strs "Xx")))
               ((= bitval 11) (setf strs (concatenate 'string strs "0X?10")))
               ((= bitval 12) (setf strs (concatenate 'string strs "1X")))
               ((= bitval 13) (setf strs (concatenate 'string strs "1X?00")))
               ((= bitval 14) (setf strs (concatenate 'string strs "1X?01")))
               (t             (setf strs (concatenate 'string strs "1X?0X?")))
         )
         (setf bit-pos (mask-shift-right bit-pos))
       ) ; end-while

    ;; Add suffix.
    (setf strs (concatenate 'string strs "]"))

    ;; Return result.
    strs
    )
)

;;; Return a string representation of a rule, like 01X->10x.
(defun rule-str2 (rulx) ; -> string.
  ;; Check argument.
  (assert (rule-p rulx))

  (let ((initial (rule-initial-region rulx))
        (result  (rule-result-region rulx))
        (ret-str ""))

    ;; Construct result.
    (setf ret-str (concatenate 'string ret-str (region-str-bits initial)))
    (setf ret-str (concatenate 'string ret-str "->"))
    (setf ret-str (concatenate 'string ret-str (region-str-bits result)))

    ;; Return result.
    ret-str
  )
)

;;; Return the Boolean "or", or union, of two rules.
(defun rule-union (rul1 rul2) ; -> rule or nil.
  ;; Check arguments.
  (assert (rule-p rul1))
  (assert (rule-p rul2))
  (assert (= (rule-num-bits rul1) (rule-num-bits rul2)))

  (let (rulx)
    ;; Construct rule union.
    (setf rulx (make-rule :m00 (mask-or (rule-m00 rul1) (rule-m00 rul2))
                          :m01 (mask-or (rule-m01 rul1) (rule-m01 rul2))
                          :m11 (mask-or (rule-m11 rul1) (rule-m11 rul2))
                          :m10 (mask-or (rule-m10 rul1) (rule-m10 rul2))))

    ;; Return result.
    (if (rule-is-valid-union rulx) rulx nil)
  )
)

;;; Return true if a rule is a valid union, that is no 1X, or 0X, bit positions.
(defun rule-is-valid-union (rulx) ; -> bool
  ;; Check argument.
  (assert (rule-p rulx))

  ;; Calc result.
  (and
    (mask-is-low (mask-and (rule-m00 rulx) (rule-m01 rulx)))
    (mask-is-low (mask-and (rule-m11 rulx) (rule-m10 rulx)))
  )
)

;;; Return the Boolean "and", or intersection, of two rules.
(defun rule-intersection (rul1 rul2) ; -> rule, or nil.
  ;; Check arguments.
  (assert (rule-p rul1))
  (assert (rule-p rul2))
  (assert (= (rule-num-bits rul1) (rule-num-bits rul2)))

  (let (rulx)
    ;; Construct rule intersection.
    (setf rulx (make-rule :m00 (mask-and (rule-m00 rul1) (rule-m00 rul2))
                          :m01 (mask-and (rule-m01 rul1) (rule-m01 rul2))
                          :m11 (mask-and (rule-m11 rul1) (rule-m11 rul2))
                          :m10 (mask-and (rule-m10 rul1) (rule-m10 rul2))))

    ;; Return result.
    (if (rule-is-valid-intersection rulx) rulx nil)
  )
)

;;; Return true if a rule is a valid intersection, that is no bit position is zero for all four masks.
(defun rule-is-valid-intersection (rul) ; -> bool.
  ;; Check argument.
  (assert (rule-p rul))

  ;; Calc result.
  (mask-is-high (mask-or
                  (rule-m00 rul)
                  (mask-or (rule-m01 rul)
                    (mask-or (rule-m11 rul) (rule-m10 rul)))))
)

;;; Return true if two rules are equal.
(defun rule-eq (rul1 rul2) ; -> bool.
  ;; Check arguments.
  (assert (rule-p rul1))
  (assert (rule-p rul2))
  (assert (= (rule-num-bits rul1) (rule-num-bits rul2)))

  ;; Calc result.
  (and (mask-eq (rule-m00 rul1) (rule-m00 rul2))
       (mask-eq (rule-m01 rul1) (rule-m01 rul2))
       (mask-eq (rule-m11 rul1) (rule-m11 rul2))
       (mask-eq (rule-m10 rul1) (rule-m10 rul2)))
)

;;; Ruturn the number of bits used by a rules masks.
(defun rule-num-bits (rulx) ; -> a number.
  ;; Check argument.
  (assert (rule-p rulx))

  ;; Calc result.
  (mask-num-bits (rule-m00 rulx))
)

;;; Return the initial region of a rule.
(defun rule-initial-region (rulx) ; -> region.
  ;; Check argument.
  (assert (rule-p rulx))

  (let (
    ;; Calc result.
    (sta1 (state-new-or (rule-m10 rulx) (rule-m11 rulx)))
    (sta2 (state-new-not (mask-or (rule-m01 rulx) (rule-m00 rulx)))))

    ;; Return result.
    (if (state-eq sta1 sta2)
      (region-new sta1)
      (region-new (list sta1 sta2)))
  )
)

;;; Return the result region of a rule.
(defun rule-result-region (rulx) ; -> region.
  ;; Check argument.
  (assert (rule-p rulx))

  (let (
    (sta1 (state-new-or (rule-m11 rulx) (rule-m01 rulx)))
    (sta2 (state-new-not (mask-or (rule-m00 rulx) (rule-m10 rulx))))
    (x-not-x (mask-and (rule-m01 rulx) (rule-m10 rulx))))

    ;; The initial region for a rule will have all X positions represented by a capitol X.
    ;; To indicate X->x, the result region position needs to be changed to lower-case.
    (setf sta1 (state-new-xor x-not-x sta1))
    (setf sta2 (state-new-xor x-not-x sta2))

    ;; Return result.
    (if (state-eq sta1 sta2)
      (region-new sta1)
      (region-new (list sta1 sta2)))
  )
)

;;; Return true if a rule is a subset of another.
(defun rule-subset-of (&key sub sup) ; -> bool.
  ;; Check arguments.
  (assert (rule-p sub))
  (assert (rule-p sup))
  (assert (= (rule-num-bits sub) (rule-num-bits sup)))

  ;; Check each rule mask for subset, ruturn a negative result if oany test fails.
  (if (not (mask-subset-of :sub-mask (rule-m00 sub) :sup-mask (rule-m00 sup)))
    (return-from rule-subset-of false))

  (if (not (mask-subset-of :sub-mask (rule-m01 sub) :sup-mask (rule-m01 sup)))
    (return-from rule-subset-of false))

  (if (not (mask-subset-of :sub-mask (rule-m11 sub) :sup-mask (rule-m11 sup)))
    (return-from rule-subset-of false))

  (if (not (mask-subset-of :sub-mask (rule-m10 sub) :sup-mask (rule-m10 sup)))
    (return-from rule-subset-of false))

  ;; Return posative result.
  true
)

;;; Return a rule that has the minimun changes, to translate from one region to intersect another.
;;; A rule made this way will never have a X->x (0->1, 1->0) bit position.
;;; The X->x bit position can result from the union of two rules.
(defun rule-region-to-region (reg1 reg2) ; -> rule.
  ;; Check arguments.
  (assert (region-p reg1))
  (assert (region-p reg2))
  (assert (= (region-num-bits reg1) (region-num-bits reg2)))

  (let (m00 mxx mx0 m01 mx1 m11 m10 m0x m1x)

    ; Make masks for each possible bit position, (0, 1, X) to (0, 1, X), 3 X 3 = 9 possibilities.
    (setf m00 (mask-and (region-0-mask reg1) (region-0-mask reg2)))
    (setf m0x (mask-and (region-0-mask reg1) (region-x-mask reg2)))
    (setf mxx (mask-and (region-x-mask reg1) (region-x-mask reg2)))

    (setf mx0 (mask-and (region-x-mask reg1) (region-0-mask reg2)))
    (setf m01 (mask-and (region-0-mask reg1) (region-1-mask reg2)))
    (setf mx1 (mask-and (region-x-mask reg1) (region-1-mask reg2)))
    (setf m11 (mask-and (region-1-mask reg1) (region-1-mask reg2)))
    (setf m10 (mask-and (region-1-mask reg1) (region-0-mask reg2)))
    (setf m1x (mask-and (region-1-mask reg1) (region-x-mask reg2)))

    ;; Construct result.
    (make-rule :m00 (mask-or m00 (mask-or mxx (mask-or mx0 m0x)))
               :m01 (mask-or m01 mx1)
               :m11 (mask-or m11 (mask-or mxx (mask-or mx1 m1x)))
               :m10 (mask-or m10 mx0))
  )
)

;;; Mask off one positions in a rule, that has 0->0, or 0->1, in the same positions.
(defun rule-mask-off-ones (rulex msk-out) ; -> rule.
  ;; Check arguments.
  (assert (rule-p rulex))
  (assert (mask-p msk-out))
  (assert (= (rule-num-bits rulex) (mask-num-bits msk-out)))

  (let (msk-in rulz)
    (setf msk-in (mask-not msk-out))

    ;; Construct modified rule.
    (setf rulz (make-rule :m00 (rule-m00 rulex)
                          :m01 (rule-m01 rulex)
                          :m11 (mask-and (rule-m11 rulex) msk-in)
                          :m10 (mask-and (rule-m10 rulex) msk-in)))

    (assert (rule-is-valid-intersection rulz))
    ;; Return result.
    rulz
  )
)

;;; Mask off zero positions in a rule, that has 1->1, or 1->0, in the same positions.
(defun rule-mask-off-zeros (rulex msk-out) ; -> rule.
  ;; Check arguments.
  (assert (rule-p rulex))
  (assert (mask-p msk-out))
  (assert (= (rule-num-bits rulex) (mask-num-bits msk-out)))

  (let (msk-in rulz)
    (setf msk-in (mask-not msk-out))

    ;; Construct modified rule.
    (setf rulz (make-rule :m00 (mask-and (rule-m00 rulex) msk-in)
                          :m01 (mask-and (rule-m01 rulex) msk-in)
                          :m11 (rule-m11 rulex)
                          :m10 (rule-m10 rulex)))

    (assert (rule-is-valid-intersection rulz))
    ;; Return result.
    rulz
  )
)

;;; Return the combination of two rules where the result region of the first rule
;;; intersects the initial region of the second rule.
(defun rule-combine-sequence2 (rul1 rul2) ; -> rule.
  ;; Check arguments.
  (assert (rule-p rul1))
  (assert (rule-p rul2))
  (assert (= (rule-num-bits rul1) (rule-num-bits rul2)))
  (assert (region-intersects (rule-result-region rul1) (rule-initial-region rul2)))

  ;; Consrtuct result.
  (make-rule :m00 (mask-or (mask-and (rule-m00 rul1) (rule-m00 rul2)) (mask-and (rule-m01 rul1) (rule-m10 rul2)))
             :m01 (mask-or (mask-and (rule-m01 rul1) (rule-m11 rul2)) (mask-and (rule-m00 rul1) (rule-m01 rul2)))
             :m11 (mask-or (mask-and (rule-m11 rul1) (rule-m11 rul2)) (mask-and (rule-m10 rul1) (rule-m01 rul2)))
             :m10 (mask-or (mask-and (rule-m10 rul1) (rule-m00 rul2)) (mask-and (rule-m11 rul1) (rule-m10 rul2))))
)

;;; Return the combination of two rules.
;;; The result region of the first rule may, or may not,  intersect the initial region of the second rule.
(defun rule-combine-sequence (rul1 rul2) ; -> rule.
  ;; Check arguments.
  (assert (rule-p rul1))
  (assert (rule-p rul2))
  (assert (= (rule-num-bits rul1) (rule-num-bits rul2)))

  ;; Result if the rules intersect.
  (if (region-intersects (rule-result-region rul1) (rule-initial-region rul2))
    (return-from rule-combine-sequence (rule-combine-sequence2 rul1 rul2)))

  ;; If the rules intersect, fabricate a rule between them, and combine the three.
  (let ((rule-between (rule-region-to-region (rule-result-region rul1) (rule-initial-region rul2))))
    (rule-combine-sequence2 (rule-combine-sequence2 rul1 rule-between) rul2)
  )
)

;;; Return a rule that has an initial region restricted by a given region.
(defun rule-restrict-initial-region (rulx regx) ; -> rule.
  ;; Check arguments.
  (assert (rule-p rulx))
  (assert (region-p regx))
  (assert (= (rule-num-bits rulx) (region-num-bits regx)))
  (assert (region-intersects (rule-initial-region rulx) regx))

  (let* ((regint (region-intersection (rule-initial-region rulx) regx))
         (zeros (mask-new-not (region-low-state regint)))
         (ones  (mask-new (region-high-state regint))))

    ;; Construct result.
    (make-rule :m00 (mask-and (rule-m00 rulx) zeros)
               :m01 (mask-and (rule-m01 rulx) zeros)
               :m11 (mask-and (rule-m11 rulx) ones)
               :m10 (mask-and (rule-m10 rulx) ones))
  )
)

;;; Return a rule that has an result region restricted by a given region.
(defun rule-restrict-result-region (rulx regx) ; -> rule.
  ;; Check arguments.
  (assert (rule-p rulx))
  (assert (region-p regx))
  (assert (= (rule-num-bits rulx) (region-num-bits regx)))
  (assert (region-intersects (rule-result-region rulx) regx))

  (let* ((regint (region-intersection (rule-result-region rulx) regx))
         (zeros (mask-new-not (region-low-state regint)))
         (ones  (mask-new (region-high-state regint))))

    ;; Construct result.
    (make-rule :m00 (mask-and (rule-m00 rulx) zeros)
               :m01 (mask-and (rule-m01 rulx) ones)
               :m11 (mask-and (rule-m11 rulx) ones)
               :m10 (mask-and (rule-m10 rulx) zeros))
  )
)

;;; Return a change from a rule.
(defun rule-changes (rulx) ; -> change
  ;; Check argument.
  (assert (rule-p rulx))

  ;; Construct result.
  (change-new :m01 (rule-m01 rulx)
              :m10 (rule-m10 rulx))
)

;;; Return the number of changes.
(defun rule-num-changes (rulx) ; -> integer.
  ;; Check argument.
  (assert (rule-p rulx))

  ;; Calc result.
  (change-num-changes (rule-changes rulx))
)

;;; Return the intersection of a rule and a change, as a change.
(defun rule-intersection-change (rulx cngx) ; -> change
  ;; Check arguments.
  (assert (rule-p rulx))
  (assert (change-p cngx))

  ;; Calc result.
  (change-and cngx (rule-changes rulx))
)

;;; Return true if two rules run in a given order results in
;;; all needed changes in the first rule being reversed.
(defun rule-sequence-blocks-changes (&key first next wanted) ; -> bool
  ;; Check arguments.
  (assert (rule-p first))
  (assert (rule-p next))
  (assert (change-p wanted))
  (assert (= (rule-num-bits first) (rule-num-bits next)))
  (assert (= (rule-num-bits first) (change-num-bits wanted)))
  (assert (mask-is-low (mask-and (change-m01 wanted) (change-m10 wanted)))) ; 0->1 and 1->0 is never needed for the same bit position.
  (assert (mask-is-not-low (mask-or (change-m01 wanted) (change-m10 wanted)))) ; At least one change should be needed.

  (let ((rule-comb (rule-combine-sequence first next))
        (msk01 (mask-and (rule-m01 first) (change-m01 wanted)))
        (msk10 (mask-and (rule-m10 first) (change-m10 wanted)))
       )

    ;; Check that 0->1 changes are preserved.
    (if (mask-is-not-low (mask-and (rule-m01 rule-comb) msk01))
      (return-from rule-sequence-blocks-changes false)) ; Return negative result.

    ;; Check that 1->0 changes are preserved.
    (if (mask-is-not-low (mask-and (rule-m10 rule-comb) msk10))
      (return-from rule-sequence-blocks-changes false)) ; Return negative result.

    ;; Return positive result.
    true
  )
)

;;; Return true if two rules are mutually exclusive, for a given wanted change.
(defun rule-mutually-exclusive (rul1 rul2 wanted) ; -> bool
  ;; Check arguments.
  (assert (rule-p rul1))
  (assert (rule-p rul2))
  (assert (change-p wanted))
  (assert (= (rule-num-bits rul1) (rule-num-bits rul2)))
  (assert (= (rule-num-bits rul1) (change-num-bits wanted)))
  (assert (mask-is-low (mask-and (change-m01 wanted) (change-m10 wanted))))    ; 0->1 and 1->0 is never needed for the same bit position.
  (assert (change-is-not-low wanted)) ; At least one change should be needed.
  (assert (change-is-not-low (rule-intersection-change rul1 wanted))) ; rul1 should have a wanted change.
  (assert (change-is-not-low (rule-intersection-change rul2 wanted))) ; rul2 should have a wanted change.

  ;; Calc result.
  (and (rule-sequence-blocks-changes :first rul1 :next rul2 :wanted wanted)
       (rule-sequence-blocks-changes :first rul2 :next rul1 :wanted wanted))
)

;;; Return the result of applying a rule to a state.
(defun rule-result-from-state (rulx stax) ; -> state instance.
  ;; Check arguments.
  (assert (rule-p rulx))
  (assert (state-p stax))
  (assert (= (rule-num-bits rulx) (state-num-bits stax)))
  (assert (region-superset-of-state (rule-initial-region rulx) stax))

  (let (cng1s cng0s)
    ;; Find one bits that should change.
    (setf cng1s (mask-new-and stax (rule-m10 rulx)))
    ;; Find zero bits that should change.
    (setf cng0s (mask-new-and (state-not stax) (rule-m01 rulx)))

    ;; Calc result.
    (state-new-xor stax (mask-or cng1s cng0s))
  )
)

;;; Return true if a rule makes a change.
(defun rule-makes-change (rulx) ; -> bool
  ;; Check argument.
  (assert (rule-p rulx))

  (or (mask-is-not-low (rule-m01 rulx)) (mask-is-not-low (rule-m10 rulx)))
)

;;;; Return a rule based on a number or restrictions.
;;;; The rule may be restricted by the within region.
;;;; X-bit positions may be restricted,
;;;; Xx to 1->0 or 0->1, if one of those changes is needed.
;;;; X0 to 1->0, if that change is needed.
;;;; X1 to 0->1, if that change is needed.
;;;; Seeking a region with the zero. or one, edge is needed te get the desired change.
(defun rule-restrict-by (rulx rule-from-to within) ; -> rule, or nil.
  ;; Check argument.
  (assert (rule-p rulx))
  (assert (rule-p rule-from-to))
  (assert (region-p within))
  (assert (= (rule-num-bits rulx) (rule-num-bits rule-from-to)))
  (assert (= (rule-num-bits rulx) (region-num-bits within)))

  (let ((ruly rulx) wanted-changes)

    ;; Restrict rule by the within region.
    (setf ruly (rule-restrict-by-within ruly within))

    (when ruly
      (setf wanted-changes (rule-changes rule-from-to))
      (setf ruly (rule-restrict-by-change ruly wanted-changes))
    )
    ;; Return result.
    ruly
  )
)

;;;; Return a rule based a region it must stay within.
(defun rule-restrict-by-within (rulx within) ; -> rule, or nil.
  ;; Check argument.
  (assert (rule-p rulx))
  (assert (region-p within))
  (assert (= (rule-num-bits rulx) (region-num-bits within)))

  (let ((ruly rulx))

    ;; Restrict ruly initial region to the within region.
    (if (not (region-intersects (rule-initial-region ruly) within))
      (return-from rule-restrict-by-within nil)
    )
    (if (not (region-superset-of :sup within :sub (rule-initial-region ruly)))
      (setf ruly (rule-restrict-initial-region ruly within))
    )

    ;; Restrict ruly result region to the within region.
    (if (not (region-intersects (rule-result-region ruly) within))
      (return-from rule-restrict-by-within nil)
    )
    (if (not (region-superset-of :sup within :sub (rule-result-region ruly)))
      (setf ruly (rule-restrict-result-region ruly within)))

    ;; Return result.
    ruly
  )
)

;;;; Return a rule based needed changes.
(defun rule-restrict-by-change (rulx wanted-changes) ; -> rule, or nil.
  ;; Check argument.
  (assert (rule-p rulx))
  (assert (change-p wanted-changes))
  (assert (= (rule-num-bits rulx) (change-num-bits wanted-changes)))

  (let ((ruly rulx) rule-wanted-changes)

    (setf rule-wanted-changes (change-and (rule-changes rulx) wanted-changes))

    ;; Calc wanted changes in ruly.
    (if (change-is-low rule-wanted-changes)
      (return-from rule-restrict-by-change nil))

    ;; Alt-rules in steps for two-result groups may have Xx bit positions,
    (setf rule-wanted-changes (change-remove-x-x-not rule-wanted-changes))

    ;; Restrict rule by 0->1 wanted changes.
    (if (mask-is-not-low (change-m01 rule-wanted-changes))
      (setf ruly (rule-mask-off-ones ruly (change-m01 rule-wanted-changes))) ; X->x, X->1, to 0->1.
    )

    ;; Restrict rule by 1->0 wanted changes.
    (if (mask-is-not-low (change-m10 rule-wanted-changes))
      (setf ruly (rule-mask-off-zeros ruly (change-m10 rule-wanted-changes))) ; X->x, X->0, to 1->0.
    )

    ;; Return result.
    ruly
  )
)

;;; Return a rule that reverses a rules' changes, except X->0 and X->1.
;;; So applying the result of a rule to its reverse, will result in a region equal, or subset, to the
;;; the original region that the rule was applied to.
;;; Alt-rules in steps for two-result groups needs to reverse the effects of an unwanted change.
(defun rule-reverse (rulx) ; -> rule
  ;; Check argument.
  (assert (rule-p rulx))

  (let (initial result i-0 i-1 i-x r-0 r-1 xx x-not-x)

    (setf initial (rule-initial-region rulx))
    (setf result  (rule-result-region rulx))

    (setf i-0 (region-0-mask initial))
    (setf i-1 (region-1-mask initial))
    (setf i-x (region-x-mask initial))

    (setf r-0 (region-0-mask result))
    (setf r-1 (region-1-mask result))

    (setf xx (mask-and (rule-m00 rulx) (rule-m11 rulx)))
    (setf x-not-x (mask-and (rule-m10 rulx) (rule-m01 rulx)))

    ;; Check for X->0.
    (if (mask-is-not-low (mask-and i-x r-0))
      (error "Rule with x->0 bit position cannot be reversed"))

    ;; Check for X->1.
    (if (mask-is-not-low (mask-and i-x r-1))
      (error "Rule with x->1 bit position cannot be reversed"))

    ;; Construct result.
    (make-rule :m00 (mask-or (mask-and i-0 r-0) xx)
               :m01 (mask-or (mask-and i-1 r-0) x-not-x)
               :m11 (mask-or (mask-and i-1 r-1) xx)
               :m10 (mask-or (mask-and i-0 r-1) x-not-x)
    )
  )
)

