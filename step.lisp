;;;; Implement the Step type.
;;;;
(defstruct step
  act-id	    ; An action ID, GE zero.
  rule		    ; A rule.
  alt-rule      ; A second result is possible, based on this rule.
  alt-plan      ; Plan to go from alt result back to the beginning region to rerun the action.
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
(defun step-new (act-id rule &optional alt-rule alt-plan)
  (assert (rule-p rule))
  (assert (and (integerp act-id) (>= act-id 0)))
  (assert (or (null alt-rule) (rule-p alt-rule)))
  (assert (or (null alt-rule) (= (rule-num-bits rule) (rule-num-bits alt-rule))))

  (make-step :act-id act-id :rule rule :alt-rule alt-rule :alt-plan alt-plan)
)

;;; Return a string representing a step
(defun step-str (stpx)
    (assert (step-p stpx))

    (let ((str "#S(STEP "))
        (setf str (concatenate 'string str (format nil "act-id ~D" (step-act-id stpx))))
        (setf str (concatenate 'string str (format nil " rule ~A" (rule-str (step-rule stpx)))))

        (when (not (null (step-alt-rule stpx)))
          (setf str (concatenate 'string str (format nil " alt-rule ~A" (rule-str (step-alt-rule stpx)))))
          (if (not (null (step-alt-plan stpx)))
            (setf str (concatenate 'string str (format nil " alt-plan ~A" (plan-str (step-alt-plan stpx))))))
        )

        (setf str (concatenate 'string str ")"))
        str
    )
)

;;; Return true if the argument is a list of steps, or nil.
(defun step-list-p (steps) ; -> bool
  ;(format t "~&step-list-p: ~A ~A" (type-of steps) steps)
  (if (not (listp steps))
    (return-from step-list-p false))

  (if (null steps)
    (return-from step-list-p true))

  ;; Check items in the list.
  (if (not (step-p (car steps)))
    (return-from step-list-p false))

  (let ((num-bits (step-num-bits (car steps))))

    (loop for stpx in (cdr steps) do
      ;; Check item type.
      (if (not (step-p stpx))
        (return-from step-list-p false))
      ;; Check item number bits.
      (if (/= (step-num-bits stpx) num-bits)
        (return-from step-list-p false))
    )
  )
  true
)

;;; Return true if two steps ar equal.
(defun step-eq (stp1 stp2) ; -> bool
  (assert (step-p stp1))
  (assert (step-p stp2))
  (assert (= (step-num-bits stp1) (step-num-bits stp2)))

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
  (assert (step-p stepx))
  (assert (region-p regx))
  (assert (= (step-num-bits stepx) (region-num-bits regx)))
  (assert (region-intersects regx (step-initial-region stepx)))

  (if (region-superset-of :sup regx :sub (step-initial-region stepx))
    (return-from step-restrict-initial-region stepx))

  (let ((new-rule (rule-restrict-initial-region (step-rule stepx) regx)) alt-rule alt-plan)

    (if (step-alt-rule stepx)
       (setf alt-rule (rule-restrict-initial-region (step-alt-rule stepx) regx))
       (if (step-alt-plan stepx)
         (setf alt-plan (plan-restrict-initial-region (step-alt-plan stepx) regx))))

    (step-new (step-act-id stepx) new-rule alt-rule alt-plan)
  )
)

;;; Return a step with its rule result region restricted bf a given region.
(defun step-restrict-result-region (stepx regx) ; -> step
  (assert (step-p stepx))
  (assert (region-p regx))
  (assert (= (step-num-bits stepx) (region-num-bits regx)))
  (assert (region-intersects regx (step-result-region stepx)))

  (if (region-superset-of :sup regx :sub (step-result-region stepx))
    (return-from step-restrict-result-region stepx))

  (let ((new-rule (rule-restrict-result-region (step-rule stepx) regx)) alt-rule alt-plan)

    (if (step-alt-rule stepx)
       (setf alt-rule (rule-restrict-result-region (step-alt-rule stepx) regx))
       (if (step-alt-plan stepx)
         (setf alt-plan (plan-restrict-result-region (step-alt-plan stepx) regx))))

    (step-new (step-act-id stepx) new-rule alt-rule alt-plan)
  )
)

;;; Return the changes made by a step.
(defun step-changes (stepx) ; -> change
  (assert (step-p stepx))

  (rule-changes (step-rule stepx))
)

