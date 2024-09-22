;;;; Implement a series of rules, with bit-number values corresponding to a list of domains.

(defvar true t)
(defvar false nil)

; Implement a store of corresponding rules.
(defstruct (rulescorr (:print-function rulescorr-print))
  rulestore  ; A rulestore of zero, or more, rules.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (rulescorr-<field name> <instance>) -> struct field.
;   (rulescorr-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> rulescorr
;   (typep <instance> 'rulescorr) -> bool
;
; Probably shouldn't use:
;   (make-rulescorr [:<field-name> <field-rulescorr>]*), use rulescorr-new instead.
;   (copy-rulescorr <instance>) copies a rulescorr instance.

;;; Return a new rulescorr instance, from a list of rules.
(defun rulescorr-new (rule-list) ; -> rulescorr, or nil.
  ;(format t "~&rulescorr-new: rules ~A" rules)
  (assert (rule-list-p rule-list))

  (make-rulescorr :rulestore (rulestore-new rule-list))
)

;;; Create a new rulescorr instance from four maskcorrs.
(defun rulescorr-new-from-maskscorrs (&key b00 b01 b11 b10) ; -> rulescorr
  (assert (maskscorr-p b00))
  (assert (maskscorr-p b01))
  (assert (maskscorr-p b11))
  (assert (maskscorr-p b10))
  (assert (maskscorr-congruent b00 b01))
  (assert (maskscorr-congruent b00 b11))
  (assert (maskscorr-congruent b00 b10))

  (let (rules-list)
    (loop for b00x in (maskscorr-mask-list b00)
          for b01x in (maskscorr-mask-list b01)
          for b11x in (maskscorr-mask-list b11)
          for b10x in (maskscorr-mask-list b10) do 
      (setf rules-list (append rules-list (list (make-rule :b00 b00x :b01 b01x :b11 b11x :b10 b10x))))
    )
    (rulescorr-new rules-list)
  )
)

;;; Return a list of rules from a rulescorr.
(defun rulescorr-rule-list (rulscx) ; -> list of rules.
  (assert (rulescorr-p rulscx))

  (rulestore-rule-list (rulescorr-rulestore rulscx))
)

;;; Print a rulescorr.
(defun rulescorr-print (instance stream depth)
  ;(assert (zerop depth))
  (format stream (rulescorr-str instance))
)

;;; Return a string representing a rulescorr.
(defun rulescorr-str (rulscx) ; -> string.
  ;(format t "~&rulescorr-str")
  (assert (rulescorr-p rulscx))

  (format nil "#S(RULESCORR ~A )" (rulescorr-rulestore rulscx))
)

;;; Return the number of rules in a rulescorr.
(defun rulescorr-length (rulscx) ; -> number, ge 0.
  ;(format t "~&rulescorr-length: ~A" (type-of rulscx))
  (assert (rulescorr-p rulscx))

  (rulestore-length (rulescorr-rulestore rulscx))
)

;;; Return true if a rulescorr is empty.
(defun rulescorr-is-empty (rulescorrx) ; -> bool
  ;(format t "~&rulescorr-is-empty: arg ~A" (type-of rulescorrx))
  (assert (rulescorr-p rulescorrx))

  (zerop (rulescorr-length rulescorrx))
)

;;; Return true if a rulescorr is not empty.
(defun rulescorr-is-not-empty (rulescorrx) ; -> bool
  (assert (rulescorr-p rulescorrx))

  (plusp (rulescorr-length rulescorrx))
)

;;; Return true is two rulescorr have similar format.
(defun rulescorr-congruent (rulsc1 rulsc2) ; -> bool
  (assert (rulescorr-p rulsc1))
  (assert (rulescorr-p rulsc2))

  (loop for rul1 in (rulescorr-rule-list rulsc1)
        for rul2 in (rulescorr-rule-list rulsc2) do
	  (if (/= (rule-num-bits rul1) (rule-num-bits rul2))
	    (return-from rulescorr-congruent false))
  )
  true
)

;;; Return true if two rulescorr are equal.
(defun rulescorr-eq (rulsc1 rulsc2) ; -> bool
  ;(format t "~&rulescorr-eq: ~A ~A" rulsc1 rulsc2)
  (assert (rulescorr-p rulsc1))
  (assert (rulescorr-p rulsc2))
  (assert (rulescorr-congruent rulsc1 rulsc2))

  (loop for rul1 in (rulescorr-rule-list rulsc1)
        for rul2 in (rulescorr-rule-list rulsc2) do
    (if (not (rule-eq rul1 rul2))
      (return-from rulescorr-eq false))
  )
  true
)

;;; Return the Boolean "OR" of two rulescorrs.
(defun rulescorr-union (rulsc1 rulsc2) ; -> rulescorr.
  (assert (rulescorr-p rulsc1))
  (assert (rulescorr-p rulsc2))
  (assert (rulescorr-congruent rulsc1 rulsc2))

  (let (rule-list)
    (loop for rulx in (rulescorr-rule-list rulsc1)
          for ruly in (rulescorr-rule-list rulsc2) do
      (setf rule-list (append rule-list (list (rule-new (rule-or rulx ruly)))))
    )

    (rulescorr-new rule-list)
  )
)

;;; Return the Boolean "AND" of two rulescorrs.
(defun rulescorr-intersection (rulsc1 rulsc2) ; -> rulescorr.
  (assert (rulescorr-p rulsc1))
  (assert (rulescorr-p rulsc2))
  (assert (rulescorr-congruent rulsc1 rulsc2))

  (let (rule-list)
    (loop for rulx in (rulescorr-rule-list rulsc1)
          for ruly in (rulescorr-rule-list rulsc2) do
      (setf rule-list (append rule-list (list (rule-new (rule-and rulx ruly)))))
    )

    (rulescorr-new rule-list)
  )
)

;;; Return the first rule in a rulescorr.
(defun rulescorr-first-rule (rulscx) ; -> rule
  ;(format t "~&rulescorr-first-rule: ~A" rulscx)
  (assert (rulescorr-p rulscx))
  (assert (rulescorr-is-not-empty rulscx))

  (car (rulescorr-rule-list rulscx))
)

;;; Return the last rule in a rulescorr.
(defun rulescorr-last-rule (rulscx) ; -> rule
  (assert (rulescorr-p rulscx))
  (assert (rulescorr-is-not-empty rulscx))

  (car (last (rulescorr-rule-list rulscx)))
)

;;; Return a rule that has the minimun changes, to translate from one regionscorr to intersect another.
;;; A rule made this way will never have a X->x (0->1, 1->0) bit position.
;;; The X->x bit position can result from the union of two rules.
(defun rulescorr-new-regionscorr-to-regionscorr (regsc1 regsc2) ; -> rulescorr.
  (assert (regionscorr-p regsc1))
  (assert (regionscorr-p regsc2))
  (assert (regionscorr-congruent regsc1 regsc2))

  (let (b00 b0x bxx bx0 b01 bx1 b11 b1x b10)

    ; Make maskscorrs for each possible bit position, (0, 1, X) to (0, 1, X), 3 X 3 = 9 possibilities.
    (setf b00 (maskscorr-and (regionscorr-0-maskscorr regsc1) (regionscorr-0-maskscorr regsc2)))
    (setf b0x (maskscorr-and (regionscorr-0-maskscorr regsc1) (regionscorr-x-maskscorr regsc2)))
    (setf bxx (maskscorr-and (regionscorr-x-maskscorr regsc1) (regionscorr-x-maskscorr regsc2)))
    (setf bx0 (maskscorr-and (regionscorr-x-maskscorr regsc1) (regionscorr-0-maskscorr regsc2)))
    (setf b01 (maskscorr-and (regionscorr-0-maskscorr regsc1) (regionscorr-1-maskscorr regsc2)))
    (setf bx1 (maskscorr-and (regionscorr-x-maskscorr regsc1) (regionscorr-1-maskscorr regsc2)))
    (setf b11 (maskscorr-and (regionscorr-1-maskscorr regsc1) (regionscorr-1-maskscorr regsc2)))
    (setf b1x (maskscorr-and (regionscorr-1-maskscorr regsc1) (regionscorr-x-maskscorr regsc2)))
    (setf b10 (maskscorr-and (regionscorr-1-maskscorr regsc1) (regionscorr-0-maskscorr regsc2)))

    (rulescorr-new-from-maskscorrs :b00 (maskscorr-or b00 (maskscorr-or bxx (maskscorr-or bx0 b0x)))
                                   :b01 (maskscorr-or b01 bx1)
                                   :b11 (maskscorr-or b11 (maskscorr-or bxx (maskscorr-or bx1 b1x)))
                                   :b10 (maskscorr-or b10 bx0))
  )
)

