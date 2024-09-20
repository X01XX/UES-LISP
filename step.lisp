
(defvar true t)
(defvar false nil)

;;;; Implement the Step type.
;;;;
(defstruct (step (:print-function step-print))
  act-id	; An action ID, GE zero.
  rule		; A rule.
  kind		; 'a = Asymmetrical.
                ; 'b = Backward chaining.
                ; 'f = Forward chaining.
		; 's = Spans gap.
  wanted-changes    ; number of wanted changes, GE 1.
  unwanted-changes  ; number of unwanted changes, GE 0.
  net-changes       ; wanted - unwanted changes. 
)
; Functions automatically created by defstruct:
;
; Most used:
;   (step-<field name> <instance>) returns struct field.
;   (step-p <instance>) -> t
;
; Least used:
;   (type-of <instance>) -> step
;   (typep <instance> 'step) -> t
;
; Don't use:
;   (make-step [:<field-name> <field-value>]*), use step-new instead.
;   (copy-step <instance>) copies a step instance.

;;; Return a new step.
;;; A nil act-id indicates it will be assigned later.
(defun step-new (&key act-id rule kind w u)
  (assert (rule-p rule))
  (assert (or (null act-id) (>= act-id 0)))
  (assert (or (eq kind 'a) (eq kind 'b) (eq kind 'f) (eq kind 's)))
  (assert (and (integerp w) (plusp w)))
  (assert (and (integerp u) (>= u 0)))

  (make-step :act-id act-id :rule rule :kind kind :wanted-changes w :unwanted-changes u :net-changes (- w u))
)

;;; Print a step.
(defun step-print (instance stream depth)
    (format stream (step-str instance))
)

;;; Return a string representing a step
(defun step-str (stpx)
    (assert (step-p stpx))

    (let ((str "#S(STEP "))
        (setf str (concatenate 'string str (format nil "act-id ~D" (step-act-id stpx))))
        (setf str (concatenate 'string str (format nil " rule ~A" (rule-str (step-rule stpx)))))
        (setf str (concatenate 'string str (format nil " kind ~A" (step-kind stpx))))
        (setf str (concatenate 'string str (format nil " w ~D" (step-wanted-changes stpx))))
        (setf str (concatenate 'string str (format nil " u ~D" (step-unwanted-changes stpx))))
        (setf str (concatenate 'string str (format nil " n ~D" (step-net-changes stpx))))
        (setf str (concatenate 'string str ")"))
        str
    )
)

; Return true if the argument is a list of steps.
(defun step-list-p (steps) ; -> bool

  (if (not (listp steps))
    (return-from step-list-p false))

  ; Check for a non-step.
  (loop for stpx in steps do
    (if (not (step-p stpx))
      (return-from step-list-p false))
  )
  true
)

(defun step-eq (stp1 stp2) ; -> bool
  (assert (step-p stp1))
  (assert (step-p stp2))

  (and (= (step-act-id stp1) (step-act-id stp2))
       (rule-eq (step-rule stp1) (step-rule stp2)))
)

;;; Return the number of bits used by elements of a step.
(defun step-num-bits (stpx) ; -> integer, ge 1.
  (assert (step-p stpx))

  (rule-num-bits (step-rule stpx))
)

;;; Return the initial region of a step.
(defun step-initial-region (stepx) ; -> region.
  (assert (step-p stepx))

  (rule-initial-region (step-rule stepx))
)

;;; Return the result region of a step.
(defun step-result-region (stepx) ; -> region.
  (assert (step-p stepx))

  (rule-result-region (step-rule stepx))
)

;;; Return a step with its rule initial region restricted bf a given region.
(defun step-restrict-initial-region (stepx regx) ; -> step
  (let* ((new-rule (rule-restrict-initial-region (step-rule stepx) regx))
	 (w (rule-num-wanted-changes new-rule))
	 (u (rule-num-unwanted-changes new-rule)))

    (make-step :act-id (step-act-id stepx) 
  	       :rule new-rule 
	       :kind (step-kind stepx)
	       :wanted-changes w
	       :unwanted-changes u
	       :net-changes (- w u))
  )
)

;;; Return a step with its rule result region restricted bf a given region.
(defun step-restrict-result-region (stepx regx) ; -> step
  (let* ((new-rule (rule-restrict-result-region (step-rule stepx) regx))
	 (w (rule-num-wanted-changes new-rule))
	 (u (rule-num-unwanted-changes new-rule)))

    (make-step :act-id (step-act-id stepx) 
  	       :rule new-rule 
	       :kind (step-kind stepx)
	       :wanted-changes w
	       :unwanted-changes u
	       :net-changes (- w u))
  )
)

