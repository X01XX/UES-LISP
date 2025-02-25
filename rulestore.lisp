
(defvar true t)
(defvar false nil)

; Implement a store of rules.
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

; Return a rulestore given one, or two, rules.
(defun rulestore-new (rules) ; -> rulestore.
  (assert (rules-list-p rules))

  (make-rulestore :rules rules)
)

(defun rulestore-length (storex) ; -> integer
  ;(format t "~&rulestore-length: ~A" storex)
  (assert (rulestore-p storex))

  (length (rulestore-rules storex))
)

(defun rulestore-initial-region (storex) ; -> region
  (assert (rulestore-p storex))

  (rule-initial-region (car (rulestore-rules storex)))
)

(defun rulestore-str (storex) ; -> string
  ;(format t "~&rulestore-str")
  (assert (rulestore-p storex))

  (let ((ret "#S(RULESTORE ") (start t))

    (loop for rulx in (rulestore-rules storex) do
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
    (loop for rulx in (rulestore-rules store1) do
      (setf found-eq false)
      (loop for ruly in (rulestore-rules store2) do
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
    (loop for rulx in (rulestore-rules sub-store) do
      (setf found-sup false)
      (loop for ruly in (rulestore-rules sup-store) do
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

  (car (rulestore-rules storex))
)

;;; Return the second rule of a rulestore that has at least two rules.
(defun rulestore-second (storex) ; -> rule.
  (assert (rulestore-p storex))
  (assert (> (rulestore-length storex) 1))

  (second (rulestore-rules storex))
)

;;; Return true if a list is a list of rulestores.
;;; An empty list will return true.
(defun rulestore-list-p (rullst) ; -> bool
  ;(format t "~&rulestore-list-p: ~A" rullst)
  (if (not (listp rullst))
    (return-from rulestore-list-p false))

  (loop for rulx in rullst do
    (if (not (rulestore-p rulx))
      (return-from rulestore-list-p false))
  )
  true
)

;;; Translate a string into a rulestore.
;;; Like [], [[01/10]], or [[01/10], [00/11/11]].
(defun rulestore-from-str (rsx) ; -> rulestore
   ;(format t "~&rulestore-from-str1 ~A" rsx)
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

    ;(assert (eq (car symbols) 'QUOTE))
    ;(setf symbols (second symbols))

    (let (rules)
        (loop for tokx in rsx do
            (push (rule-from tokx) rules)
        )
       (rulestore-new (reverse rules))
  )
)

;;; Return the number of bits used in a rulestore.
(defun rulestore-num-bits (storex) ; -> number bits used.
  (assert (rulestore-p storex))
  (assert (> (rulestore-length storex) 0))

  (rule-num-bits (rulestore-first storex))
)

;;; Return a valid intersection of two rulestares, or nil.
(defun rulestore-intersection (storex storey) ; -> rulestore, or nil.
  (assert (rulestore-p storex))
  (assert (rulestore-p storey))

  (if (not (region-intersects (rulestore-initial-region storex) (rulestore-initial-region storey)))
    (return-from rulestore-intersection nil))

  (when (= (rulestore-length storex) 1)
    (let (ret)
      (setf ret (rule-intersection (rulestore-first storex) (rulestore-first storey)))
      (if (rule-is-valid-intersection ret)
        (return-from rulestore-intersection ret)
        (return-from rulestore-intersection nil))
    )
  )
  (when (= (rulestore-length storex) 2)
    (let (rul1 rul2 rul3 rul4)
      (setf rul1 (rule-intersection (rulestore-first storex) (rulestore-first storey)))
      (setf rul2 (rule-intersection (rulestore-second storex) (rulestore-second storey)))
      (setf rul3 (rule-intersection (rulestore-first storex) (rulestore-second storey)))
      (setf rul4 (rule-intersection (rulestore-second storex) (rulestore-first storey)))

      (when (and (rule-is-valid-intersection rul1) (rule-is-valid-intersection rul2))
          (return-from rulestore-intersection (rulestore-new (list rul1 rul2)))
      )

      (when (and (rule-is-valid-intersection rul3) (rule-is-valid-intersection rul4))
          (return-from rulestore-intersection (rulestore-new (list rul3 rul4)))
      )
    )
  )
  nil
)

;;; Return t if a rulestore is empty.
(defun rulestore-is-empty (storex) ; -> bool
  (assert (rulestore-p storex))

  (null (rulestore-rules storex))
)
  
;;; Return t if a rulestore is not empty.
(defun rulestore-is-not-empty (storex) ; -> bool
  (assert (rulestore-p storex))

  (not (null (rulestore-rules storex)))
)
  
