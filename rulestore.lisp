;;;; Implement a store of rules.
(defstruct rulestore
  rules  ; A list of zero, or two, non-duplicate, same number bits, rules.
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

;;; Return a rulestore given one, or two, rules.
(defun rulestore-new (rules) ; -> rulestore.
  ;; Check argument.
  (assert (rules-list-p rules))
  (assert (< (length rules) 4)) ;; Defining an unpredictable region, for action-base-rules, requires three rules.
  (if (> (length rules) 1)
    (loop for rulx in (cdr rules) do
      (assert (region-eq (rule-initial-region (car rules)) (rule-initial-region rulx)))))

  ;; Construct result.
  (make-rulestore :rules rules)
)

;;; Return the length of a rulestore.
(defun rulestore-length (storex) ; -> integer
  ;; Check argument.
  (assert (rulestore-p storex))

  ;; Calc result.
  (length (rulestore-rules storex))
)

;;; Return the initial region of rules in a rulestore.
(defun rulestore-initial-region (storex) ; -> region
  ;; Check argument.
  (assert (rulestore-p storex))

  ;; Calc result.
  (rule-initial-region (car (rulestore-rules storex)))
)

;;; Return a string representation of a rulestore.
(defun rulestore-str (storex) ; -> string
  ;; Check argument.
  (assert (rulestore-p storex))

  (let ((ret "(") (start t)) ; Init prefix and start flag.

    ;; Process each rule.
    (loop for rulx in (rulestore-rules storex) do
      ;; Add separator, if not at start.
      (if start
        (setf start false)
        (setf ret (concatenate 'string ret " ")))

      ;; Add rule string.
      (setf ret (concatenate 'string ret (rule-str rulx)))
    )
    ;; Add suffix.
    (setf ret (concatenate 'string ret ")"))
    ;; Return result.
    ret
  )
)

;;; Return true if a two rulestores are equal.
(defun rulestore-eq (store1 store2) ; -> bool
  ;; Check arguments.
  (assert (rulestore-p store1))
  (assert (rulestore-p store2))

  ;; Check for length equality.
  (if (/= (rulestore-length store1) (rulestore-length store2))
    (return-from rulestore-eq false)) ; Return negative result.

  (let (found-eq)
    ;; For each rule in store1,
    (loop for rulx in (rulestore-rules store1) do

      ;; Look for equal rule in store2.
      (setf found-eq false)
      (loop for ruly in (rulestore-rules store2) do
        (if (rule-eq rulx ruly)
          (setf found-eq true))
      )
      (if (not found-eq)
        (return-from rulestore-eq false)) ; Return negative result.
    )
    ;; Return positive result.
    true
  )
)

;;; Return true if a rulestore is a subset of another.
;;; The subset store may have fewer rules that the suberset store.
(defun rulestore-subset-of (&key sub sup) ; -> bool
  ;; Check arguments.
  (assert (rulestore-p sub))
  (assert (rulestore-p sup))

  ;; Check length.
  (if (> (rulestore-length sub) (rulestore-length sup))
    (return-from rulestore-subset-of false)) ; Return negative result.

  (let (found-sup)
    (loop for rulx in (rulestore-rules sub) do
      ;; Check for a superset rule for each subset rule.
      (setf found-sup false)
      (loop for ruly in (rulestore-rules sup) do

        (if (rule-subset-of :sub rulx :sup ruly)
          (setf found-sup true))
      )
      (if (not found-sup)
        (return-from rulestore-subset-of false)) ; Return negative result.
    )
    ;; Return positive result.
    true
  )
)

;;; Return the first rule of a non-empty rulestore.
(defun rulestore-first (storex) ; -> rule.
  ;; Check argument.
  (assert (rulestore-p storex))
  (assert (> (rulestore-length storex) 0))

  ;; Return result.
  (car (rulestore-rules storex))
)

;;; Return the second rule of a rulestore that has at least two rules.
(defun rulestore-second (storex) ; -> rule.
  ;; Check argument.
  (assert (rulestore-p storex))
  (assert (> (rulestore-length storex) 1))

  ;; Return result.
  (second (rulestore-rules storex))
)

;;; Return true if a list is a list of rulestores.
;;; An empty list will return true.
(defun rulestore-list-p (rullst) ; -> bool
  ;; Check argument.
  (if (not (listp rullst))
    (return-from rulestore-list-p false))

  ;; Check each list item type.
  (loop for rulx in rullst do
    (if (not (rulestore-p rulx))
      (return-from rulestore-list-p false))
  )
  ;; Return positive result.
  true
)

;;; Translate a string, or list of token, into a rulestore.
;;; Like [], [[01/10]], or [[01/10], [00/11/11]].
(defun rulestore-from-str (rsx) ; -> rulestore
  ;; Check argument.
  (assert (or (listp rsx) (stringp rsx)))
  
  ;; If argument is a string, convert it to a list of tokens.
  (when (stringp rsx)
    (if (not (string-equal (subseq rsx 0 1) "["))
      (return-from rulestore-from-str (err-new "String must begin with a [")))

    (if (not (string-equal (subseq rsx (1- (length rsx))) "]"))
      (return-from rulestore-from-str (err-new "String must end with a ]")))

    (if (= (length rsx) 2)
      (return-from rulestore-from-str (make-rulestore :rules nil)))

    (setf rsx (parse-str (subseq rsx 1 (1- (length rsx)))))
  )

  (assert (listp rsx))

  (let (rules)
    ;; For each rule token, convert it to a rule instance, store instance into rules var.
    (loop for tokx in rsx do
      (push (rule-from-str tokx) rules)
    )

    ;; Construct result.
    (rulestore-new (reverse rules))
  )
)

;;; Return the number of bits used in a rulestore.
(defun rulestore-num-bits (storex) ; -> number bits used.
  ;; Check argument.
  (assert (rulestore-p storex))
  (assert (> (rulestore-length storex) 0))

  ;; Return result.
  (rule-num-bits (rulestore-first storex))
)

;;; Return a valid intersection of two rulestares, or nil.
(defun rulestore-intersection (storex storey) ; -> rulestore, or nil.
  ;; Check arguments.
  (assert (rulestore-p storex))
  (assert (rulestore-p storey))

  ;; Check for intersection.
  (if (not (region-intersects (rulestore-initial-region storex) (rulestore-initial-region storey)))
    (return-from rulestore-intersection nil))

  ;; Check for equal lengths.
  (if (/= (rulestore-length storex) (rulestore-length storey))
    (return-from rulestore-intersection nil))

  ;; Process one-rule rulestore.
  (when (= (rulestore-length storex) 1)
    (let (ret)
      (setf ret (rule-intersection (rulestore-first storex) (rulestore-first storey)))
      (if (rule-p ret)
        (return-from rulestore-intersection (rulestore-new (list ret)))
        (return-from rulestore-intersection nil))
    )
  )

  ;; Process two-rule rulestore.
  (when (= (rulestore-length storex) 2)
    (let (rul1 rul2)
      ;; Check a possible order of intersection.
      (setf rul1 (rule-intersection (rulestore-first storex) (rulestore-first storey)))
      (setf rul2 (rule-intersection (rulestore-second storex) (rulestore-second storey)))

      (when (and (rule-p rul1) (rule-p rul2))
          (return-from rulestore-intersection (rulestore-new (list rul1 rul2))) ; Return intersection.
      )

      ;; Check another possible order of intersection.
      (setf rul1 (rule-intersection (rulestore-first storex) (rulestore-second storey)))
      (setf rul2 (rule-intersection (rulestore-second storex) (rulestore-first storey)))

      (when (and (rule-p rul1) (rule-p rul2))
          (return-from rulestore-intersection (rulestore-new (list rul1 rul2))) ; Return intersection.
      )
    )
  )
  ;; Return negative result.
  nil
)

;;; Return t if a rulestore is empty.
(defun rulestore-is-empty (storex) ; -> bool
  ;; Check argument.
  (assert (rulestore-p storex))

  ;; Calc result.
  (null (rulestore-rules storex))
)

;;; Return t if a rulestore is not empty.
(defun rulestore-is-not-empty (storex) ; -> bool
  ;; Check argument.
  (assert (rulestore-p storex))

  ;; Calc result.
  (not (null (rulestore-rules storex)))
)

;;; Return the nth element of a RuleStore.
(defun rulestore-nth (storex inx) ; -> rule instance, or nil.
  ;; Check arguments.
  (assert (rulestore-p storex))
  (assert (integerp inx))
  (assert (>= inx 0))
  (assert (< inx (rulestore-length storex)))

  ;; Calc result.
  (nth inx (rulestore-rules storex))
)

(defun rulestore-union (storex storey) ; -> rulestore instance, or nil.
  ;; Check arguments.
  (assert (rulestore-p storex))
  (assert (rulestore-p storey))
  (assert (= (rulestore-length storex) (rulestore-length storey)))
  (assert (> (rulestore-length storex) 0))
  (assert (< (rulestore-length storex) 3))

  ;; Process one-rule store.
  (when (= 1 (rulestore-length storex))
    (let (unx)
      (setf unx (rule-union (rulestore-first storex) (rulestore-first storey)))
      (if unx
        (return-from rulestore-union (rulestore-new (list unx)))    ; Return union.
        (return-from rulestore-union nil))                          ; Return negative result.
    )
  )

  ;; Process two-rule store.
  (when (= 2 (rulestore-length storex))
    (let (unx uny rul1 rul2)
      (setf rul1 (rule-union (rulestore-first storex) (rulestore-first storey)))
      (setf rul2 (rule-union (rulestore-second storex) (rulestore-second storey)))
      (if (and rul1 rul2)
        (setf unx (rulestore-new (list rul1 rul2))))

      (setf rul1 (rule-union (rulestore-first storex) (rulestore-second storey)))
      (setf rul2 (rule-union (rulestore-second storex) (rulestore-first storey)))
      (if (and rul1 rul2)
        (setf uny (rulestore-new (list rul1 rul2))))

      ;; If no union ordec worked, return negative result.
      (if (and (null unx) (null uny))
        (return-from rulestore-union nil))

      ;; If uny worked and unx did not, return uny.
      (if (null unx)
        (return-from rulestore-union uny))

      ;; If unx worked and uny did not, return unx.
      (if (null uny)
        (return-from rulestore-union unx))

      ;; Return negative result.
      nil
    )
  )
)

;;; Return a list of rules.
(defun rulestore-rules-list (storex) ; -> list of rules.
  ;; Check argument.
  (assert (rulestore-p storex))

  ;; Return rules.
  (rulestore-rules storex)
)

;;; Return true if a rulestore contains a given rule.
(defun rulestore-member (storex rulx) ; -> bool
  ;; Check arguments.
  (assert (rulestore-p storex))
  (assert (rule-p rulx))

  ;; Calc result.
  (member rulx (rulestore-rules storex) :test #'rule-eq)
)

;;; Push a new rule into a rulestore, suppress dups.
(defun rulestore-push (store rulx) ; -> side-effect rulestore is changed.
  ;; Check arguments.
  (assert (rulestore-p store))
  (assert (rule-p rulx))

  ;; Push rule, if its not a duplicate.
  (if (not (rulestore-member store rulx))
    (push rulx (rulestore-rules store)))
)

