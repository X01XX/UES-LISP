;;;; Implement a series of rules, with bit-number values corresponding to a list of domains.

(defvar true t)
(defvar false nil)

; Implement a store of corresponding rules.
(defstruct rulescorr
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
(defun rulescorr-new (rules) ; -> rulescorr, or nil.
  ;(format t "~&rulescorr-new: rules ~A" rules)
  (assert (rules-list-p rules))

  (make-rulescorr :rulestore (rulestore-new rules))
)

;;; Create a new rulescorr instance from four maskcorrs.
(defun rulescorr-new-from-maskscorrs (&key m00 m01 m11 m10) ; -> rulescorr
  (assert (maskscorr-p m00))
  (assert (maskscorr-p m01))
  (assert (maskscorr-p m11))
  (assert (maskscorr-p m10))
  (assert (maskscorr-congruent m00 m01))
  (assert (maskscorr-congruent m00 m11))
  (assert (maskscorr-congruent m00 m10))

  (let (rules-list)
    (loop for m00x in (maskscorr-mask-list m00)
          for m01x in (maskscorr-mask-list m01)
          for m11x in (maskscorr-mask-list m11)
          for m10x in (maskscorr-mask-list m10) do 
      (setf rules-list (append rules-list (list (make-rule :m00 m00x :m01 m01x :m11 m11x :m10 m10x))))
    )
    (rulescorr-new rules-list)
  )
)

;;; Return a list of rules from a rulescorr.
(defun rulescorr-rules (rulscx) ; -> list of rules.
  (assert (rulescorr-p rulscx))

  (rulestore-rules (rulescorr-rulestore rulscx))
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

  (loop for rul1 in (rulescorr-rules rulsc1)
        for rul2 in (rulescorr-rules rulsc2) do
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

  (loop for rul1 in (rulescorr-rules rulsc1)
        for rul2 in (rulescorr-rules rulsc2) do
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

  (let (rules)
    (loop for rulx in (rulescorr-rules rulsc1)
          for ruly in (rulescorr-rules rulsc2) do
      (setf rules (append rules (list (rule-new (rule-or rulx ruly)))))
    )

    (rulescorr-new rules)
  )
)

;;; Return the Boolean "AND" of two rulescorrs.
(defun rulescorr-intersection (rulsc1 rulsc2) ; -> rulescorr.
  (assert (rulescorr-p rulsc1))
  (assert (rulescorr-p rulsc2))
  (assert (rulescorr-congruent rulsc1 rulsc2))

  (let (rules)
    (loop for rulx in (rulescorr-rules rulsc1)
          for ruly in (rulescorr-rules rulsc2) do
      (setf rules (append rules (list (rule-new (rule-and rulx ruly)))))
    )

    (rulescorr-new rules)
  )
)

;;; Return the first rule in a rulescorr.
(defun rulescorr-first-rule (rulscx) ; -> rule
  ;(format t "~&rulescorr-first-rule: ~A" rulscx)
  (assert (rulescorr-p rulscx))
  (assert (rulescorr-is-not-empty rulscx))

  (car (rulescorr-rules rulscx))
)

;;; Return the last rule in a rulescorr.
(defun rulescorr-last-rule (rulscx) ; -> rule
  (assert (rulescorr-p rulscx))
  (assert (rulescorr-is-not-empty rulscx))

  (car (last (rulescorr-rules rulscx)))
)

;;; Return a rule that has the minimun changes, to translate from one regionscorr to intersect another.
;;; A rule made this way will never have a X->x (0->1, 1->0) bit position.
;;; The X->x bit position can result from the union of two rules.
(defun rulescorr-new-regionscorr-to-regionscorr (regsc1 regsc2) ; -> rulescorr.
  (assert (regionscorr-p regsc1))
  (assert (regionscorr-p regsc2))
  (assert (regionscorr-congruent regsc1 regsc2))

  (let (m00 m0x mxx mx0 m01 mx1 m11 m1x m10)

    ; Make maskscorrs for each possible bit position, (0, 1, X) to (0, 1, X), 3 X 3 = 9 possibilities.
    (setf m00 (maskscorr-and (regionscorr-0-maskscorr regsc1) (regionscorr-0-maskscorr regsc2)))
    (setf m0x (maskscorr-and (regionscorr-0-maskscorr regsc1) (regionscorr-x-maskscorr regsc2)))
    (setf mxx (maskscorr-and (regionscorr-x-maskscorr regsc1) (regionscorr-x-maskscorr regsc2)))
    (setf mx0 (maskscorr-and (regionscorr-x-maskscorr regsc1) (regionscorr-0-maskscorr regsc2)))
    (setf m01 (maskscorr-and (regionscorr-0-maskscorr regsc1) (regionscorr-1-maskscorr regsc2)))
    (setf mx1 (maskscorr-and (regionscorr-x-maskscorr regsc1) (regionscorr-1-maskscorr regsc2)))
    (setf m11 (maskscorr-and (regionscorr-1-maskscorr regsc1) (regionscorr-1-maskscorr regsc2)))
    (setf m1x (maskscorr-and (regionscorr-1-maskscorr regsc1) (regionscorr-x-maskscorr regsc2)))
    (setf m10 (maskscorr-and (regionscorr-1-maskscorr regsc1) (regionscorr-0-maskscorr regsc2)))

    (rulescorr-new-from-maskscorrs :m00 (maskscorr-or m00 (maskscorr-or mxx (maskscorr-or mx0 m0x)))
                                   :m01 (maskscorr-or m01 mx1)
                                   :m11 (maskscorr-or m11 (maskscorr-or mxx (maskscorr-or mx1 m1x)))
                                   :m10 (maskscorr-or m10 mx0))
  )
)

