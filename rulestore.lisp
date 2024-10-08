
(defvar true t)
(defvar false nil)

; Implement a store of rules.
(defstruct rulestore
  rule-list  ; A list of zero, or two, non-duplicate, same number bits, rules.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (rulestore-<field name> <instance>) -> struct field.
;   (rulestore-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> rulestore
;   (typep <instance> 'rulestore) -> bool
;
; Probably shouldn't use:
;   (make-rulestore [:<field-name> <field-rulestore>]*), use rulestore-new instead.
;   (copy-rulestore <instance>) copies a rulestore instance.

; Return a rulestore given one, or two, rules.
(defun rulestore-new (rules) ; -> rulestore.
  (assert (listp rules))
  (assert (not (null rules)))
  (assert (rule-list-p rules))

  (make-rulestore :rule-list rules)
)

(defun rulestore-length (storex) ; -> integer
  (assert (rulestore-p storex))

  (length (rulestore-rule-list storex))
)

(defun rulestore-initial-region (storex) ; -> region
  (assert (rulestore-p storex))

  (rule-initial-region (car (rulestore-rule-list storex)))
)

(defun rulestore-str (storex) ; -> string
  ;(format t "~&rulestore-str")
  (assert (rulestore-p storex))

  (let ((ret "#S(RULESTORE ") (start t))

    (loop for rulx in (rulestore-rule-list storex) do
      (if start (setf start false)
        (setf ret (concatenate 'string ret ", ")))

      (setf ret (concatenate 'string ret (rule-str rulx)))
    )
    (setf ret (concatenate 'string ret ")"))
    ret
  )
)

; Return true if a two rulestores are equal.
(defun rulestore-eq (store1 store2) ; -> bool
  (assert (rulestore-p store1))
  (assert (rulestore-p store2))

  (if (/= (rulestore-length store1) (rulestore-length store2))
    (return-from rulestore-eq false))

  (let (found-eq)
    (loop for rulx in (rulestore-rule-list store1) do
      (setf found-eq false)
      (loop for ruly in (rulestore-rule-list store2) do
	(if (rule-eq rulx ruly)
	  (setf found-eq true))
      )
      (if (not found-eq)
        (return-from rulestore-eq false))
    )
    true
  )
)

; Return true if a rulestore is a subset of another.
; The subset store may have fewer rules that the suberset store.
(defun rulestore-subset-of (&key sub-store sup-store) ; -> bool
  (assert (rulestore-p sub-store))
  (assert (rulestore-p sup-store))

  (if (> (rulestore-length sub-store) (rulestore-length sup-store))
    (return-from rulestore-subset-of false))

  (let (found-sup)
    (loop for rulx in (rulestore-rule-list sub-store) do
      (setf found-sup false)
      (loop for ruly in (rulestore-rule-list sup-store) do
	(if (rule-subset-of :sub-rule rulx :sup-rule ruly)
	  (setf found-sup true))
      )
      (if (not found-sup)
        (return-from rulestore-subset-of false))
    )
    true
  )
)

;;; Return the first rule of a non-empty rulestore.
(defun rulestore-first (storex) ; -> rule.
  (assert (rulestore-p storex))
  (assert (> (rulestore-length storex) 0))

  (car (rulestore-rule-list storex))
)

;;; Return the second rule of a rulestore that has at least two rules.
(defun rulestore-second (storex) ; -> rule.
  (assert (rulestore-p storex))
  (assert (> (rulestore-length storex) 1))

  (second (rulestore-rule-list storex))
)

