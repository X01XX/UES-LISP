;;;; Implement the Action type.
;;;;
(defstruct domain
  id		; A number id, GE zero.
  actions	; A actionstore.
  current-state	; The current state of the domain. Actions change this.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (domain-<field name> <instance>) returns struct field.
;   (domain-p <instance>) -> t
;
; Least used:
;   (type-of <instance>) -> domain
;   (typep <instance> 'domain) -> t
;
; Don't use:
;   (make-domain [:<field-name> <field-value>]*), use domain-new instead.
;   (copy-domain <instance>) copies a domain instance.

;;; Return a new domain.
(defun domain-new (&key id initial-state)
  (assert (state-p initial-state))
  (assert (and (integerp id) (>= id 0)))

  (let (act0 high-state low-state sample1 sample2)
    ;; Create a no-op action as action 0.
    (setf high-state (state-new-high initial-state))
    (setf low-state (state-new-low initial-state))
    (setf sample1 (sample-new :initial low-state :result low-state))
    (setf sample2 (sample-new :initial high-state :result high-state))

    (setf act0 (action-new :id 0 :rules (list (rulestore-new (list (rule-union  (rule-new sample1) (rule-new sample2)))))))
    (action-take-sample-arbitrary act0 high-state)
    (action-take-sample-arbitrary act0 low-state)
    (action-take-sample-arbitrary act0 high-state)
    (action-take-sample-arbitrary act0 low-state)
    (action-take-sample-arbitrary act0 high-state)
    (action-take-sample-arbitrary act0 low-state)

    (make-domain :id id :actions (actionstore-new (list act0)) :current-state initial-state)
  )
)

;;; Set a domain id.
(defun domain-set-id (domx id) ; -> nothing.  Side effect, domain id is changed.
  (assert (domain-p domx))
  (assert (and (integerp id) (>= id 0)))

  (setf (domain-id domx) id)
)

;;; Set the domain state.
(defun domain-set-state (domx stax) ; -> nothing.  Side effect, domain id is changed.
  (assert (domain-p domx))
  (assert (state-p stax))

  (setf (domain-current-state domx) stax)
)

;;; Return a string representing a domain
(defun domain-str (domx)
    (assert (domain-p domx))

    (let ((str "#S(DOMAIN "))
        (setf str (concatenate 'string str (format nil "id ~D current-state ~A" (domain-id domx) (state-str (domain-current-state domx)))))
        (setf str (concatenate 'string str (format nil " actions ~A" (actionstore-str (domain-actions domx)))))
        (setf str (concatenate 'string str ")"))
        str
    )
)

;;; Print a domain.
(defun domain-print (domx)
    (assert (domain-p domx))

    (format t "~&Domain ~D current-state ~A change-surface: ~A" (domain-id domx) (state-str (domain-current-state domx))
      (regionstore-str (domain-change-surface domx)))

    (actionstore-print (domain-actions domx))
)

;;; Return possible steps, given a rule.
(defun domain-get-steps (domx from-reg to-reg within) ; -> stepstore.
  (assert (domain-p domx))
  (assert (region-p from-reg))
  (assert (region-p to-reg))
  (assert (region-p within))
  (assert (= (domain-num-bits domx) (region-num-bits from-reg)))
  (assert (= (domain-num-bits domx) (region-num-bits to-reg)))
  (assert (= (domain-num-bits domx) (region-num-bits within)))

  (actionstore-get-steps (domain-actions domx) from-reg to-reg within)
)

;;; Return the number of bits used by a domain.
(defun domain-num-bits (domx) ; -> integer, gt zero.
  (assert (domain-p domx))

  (state-num-bits (domain-current-state domx))
)

;;; Return true if a list is a list of domains.
;;; An empty list will return true.
(defun domain-list-p (domlst) ; -> bool
  ;(format t "~&domain-list-p: ~A" domlst)
  (if (not (listp domlst))
    (return-from domain-list-p false))

  (loop for domx in domlst do
    (if (not (domain-p domx))
      (return-from domain-list-p false))
  )
  true
)

;;; Return the maximum region for a domain.
(defun domain-max-region (domx) ; -> region.
  (region-new (list (domain-current-state domx) (state-new (state-not (domain-current-state domx)))))
)

;;; Return a plan to change a current region to a goal region.
;;; Since a random-depth-first process is used, try more than once, if needed.
(defun domain-get-plan (domx from-reg to-reg with-reg) ; -> plan, or nil.
  ;(format t "~&domain-get-plan: domx ~D from ~A to ~A within ~A depth ~D" (domain-id domx) (region-str from-reg) (region-str to-reg) (region-str with-reg) depth)
  (assert (domain-p domx))
  (assert (region-p from-reg))
  (assert (region-p to-reg))
  (assert (region-p with-reg))
  (assert (= (domain-num-bits domx) (region-num-bits from-reg)))
  (assert (= (domain-num-bits domx) (region-num-bits to-reg)))
  (assert (= (domain-num-bits domx) (region-num-bits with-reg)))
  (assert (region-superset-of :sup with-reg :sub from-reg))
  (assert (region-superset-of :sup with-reg :sub to-reg))

  (let (plan
       (num-changes (rule-num-changes (rule-region-to-region from-reg to-reg)))) ; adjust depth limit by number chnages needed.
    (loop for i from 0 to 2
          while (null plan) do
      (setf plan (domain-get-plan2 domx from-reg to-reg with-reg (* 2 num-changes)))
    )
    plan
  )
)

;;; Return a plan to change a current region to a goal region.
;;; Choose one step randomly, then recurse.
;;; So random forward-chaining, backward-chaining, with each step.
(defun domain-get-plan2 (domx from-reg to-reg with-reg depth) ; -> plan, or nil.
  ;(format t "~&domain-get-plan2: domx ~D from ~A to ~A within ~A depth ~D" (domain-id domx) (region-str from-reg) (region-str to-reg) (region-str with-reg) depth)

  (if (region-intersects to-reg from-reg)
    (let ((int-reg (region-intersection to-reg from-reg)))
      ;(format t "~&domain-get-plan2: returning 1 act 0 plan")
      (return-from domain-get-plan2 (plan-new (list (step-new :act-id 0 :rule (rule-region-to-region int-reg int-reg)))))))

  (when (zerop depth)
    ;(format t "~&domain-get-plan2: returning 2 nil")
    (return-from domain-get-plan2 nil))

  (let ((steps (domain-get-steps domx from-reg to-reg with-reg))
        (wanted-changes (rule-changes (rule-region-to-region from-reg to-reg)))
        steps-from steps-to steps-both agg-changes)

    (when (stepstore-is-empty steps)
      (return-from domain-get-plan2 nil))

    ;; Check if steps contain all wanted changes.
    (if (not (change-eq (change-and wanted-changes (stepstore-aggregate-changes steps)) wanted-changes))
      (return-from domain-get-plan2 nil))

    ;(format t "~&steps found ~A" (stepstore-str steps))
    (setf steps-from (stepstore-initial-region-intersects steps from-reg))
    (setf steps-to   (stepstore-initial-region-intersects steps to-reg))

    (setf steps-both (stepstore-intersection steps-from steps-to))

    ;; Check for one step that spans the gap.
    (let (span-steps stepy)
      (loop for stepx in (stepstore-steps steps-both) do
        (setf stepy (step-restrict-initial-region stepx from-reg))
        (when (region-intersects (step-result-region stepy) to-reg)
           (setf stepy (step-restrict-result-region stepy to-reg))
           (push stepx span-steps)
        )
      )
      (when span-steps
	    (setf stepy (nth (random (length span-steps)) span-steps))
        ;(format t "~&domain-get-plan2: returning 3 plan")
	    (return-from domain-get-plan2 (plan-new (list stepy)))
      )
    )

    ;; Gather steps that intersect the from-reg or to-reg.
    (let (step-list stepy planx step-list-intermediate plan1 plan2 plan3 plan4)

      (setf step-list (stepstore-union steps-from steps-to))

      (setf step-list-intermediate (stepstore-difference steps step-list))

      ;; Check for intermediate steps.
      ;; TODO better selection logic.
      (when (and (stepstore-is-not-empty step-list-intermediate) (= 1 (random 3)))
	    ;; Choose a random step.
	    (setf stepy (stepstore-nth step-list-intermediate (random (stepstore-length step-list-intermediate))))

        (setf plan1 (domain-get-plan2 domx from-reg (step-initial-region stepy) with-reg (1- depth)))
        (if (null plan1) (return-from domain-get-plan2 nil))
        (setf plan2 (plan-link plan1 (plan-new (list stepy))))
        (if (null plan2) (return-from domain-get-plan2 nil))
        (setf plan3 (domain-get-plan2 domx (plan-result-region plan2) to-reg with-reg (1- depth)))
        (if (null plan3) (return-from domain-get-plan2 nil))
        (setf plan4 (plan-link plan2 plan3))
        ;(if plan4
        ;   (format t "~&intermediate step plan ~A" (plan-str plan4))
        ;   (format t "~&intermediate step ~A failed" (step-str stepy)))
        (return-from domain-get-plan2 plan4)
      )

	  (when (stepstore-is-not-empty step-list)
	    ;; Choose a random step.
	    (setf stepy (stepstore-nth step-list (random (stepstore-length step-list))))

	    ;; Recurse to build the rest of the plan.
	    ;(format t "~&rule initial ~A intersects ~A = ~A" (region-str (rule-initial-region (step-rule stepy))) (region-str from-reg)
		;                                                 (region-intersects (rule-initial-region (step-rule stepy)) from-reg))
	    (when (region-intersects (rule-initial-region (step-rule stepy)) from-reg)
	      (setf stepy (step-restrict-initial-region stepy from-reg))
	      (if stepy
	        (progn
	          (setf planx (domain-get-plan2 domx (step-result-region stepy) to-reg with-reg (1- depth)))
              ;(format t "~&domain-get-plan2: returning 4 plan/nil")
	          (if planx
                (return-from domain-get-plan2 (plan-link (plan-new (list stepy)) planx))
                (return-from domain-get-plan2 nil)
	          )
	        )
            (progn
              ;(format t "~&domain-get-plan2: returning 5 nil")
              (return-from domain-get-plan2 nil)
            )
	      )
        )
	    ;(format t "~&rule result ~A intersects ~A = ~A" (region-str (rule-result-region (step-rule stepy))) (region-str to-reg)
		;                                                (region-intersects (rule-result-region (step-rule stepy)) to-reg))
	    (when (region-intersects (rule-result-region (step-rule stepy)) to-reg)
	      (setf stepy (step-restrict-result-region stepy to-reg))
          (setf planx (domain-get-plan2 domx from-reg (step-initial-region stepy) with-reg (1- depth)))
          ;(format t "~&domain-get-plan2: returning 6 plan/nil")
	      (if planx
            (return-from domain-get-plan2 (plan-link planx (plan-new (list stepy)))) 
            (return-from domain-get-plan2 nil))
        )
	  )
      ;(format t "~&domain-get-plan2: returning 7 nil")
	  (return-from domain-get-plan2 nil)
    ) ; end-let
  ) ; end-let
)

(defun domain-get-needs (domx) ; ->  needstore.
  ;(format t "~&domain-get-needs: ~A" (type-of domx))
  (assert (domain-p domx))

  (let ((needs (actionstore-get-needs (domain-actions domx) (domain-current-state domx) (domain-change-surface domx))))

    (needstore-set-dom-id needs (domain-id domx)) ; set needs domain-id

    ;; Find plan for each need.
    (loop for needx in (needstore-needs needs) do
        (cond ((state-p (need-target needx))
                (if (state-eq (need-target needx) (domain-current-state domx))
                  (setf (need-plan needx) (plan-new nil))
                  (setf (need-plan needx) (domain-get-plan domx (region-new (domain-current-state domx))
                                                               (region-new (need-target needx))
                                                               (domain-max-region domx))))
              )
              ((region-p (need-target needx))
                (if (region-superset-of-state (need-target needx) (domain-current-state domx))
                  (setf (need-plan needx) (plan-new nil))
                  (setf (need-plan needx) (domain-get-plan domx (region-new (domain-current-state domx))
                                                               (need-target needx)
                                                               (domain-max-region domx))))
              )
              (t (error "Unrecognized target type"))
        )
    )
    needs
  )
)

(defun domain-add-action (domx actx)
  (assert (domain-p domx))
  (assert (action-p actx))
  (assert (= (domain-num-bits domx) (action-num-bits actx)))

  (action-set-id actx (actionstore-length (domain-actions domx)))
  (actionstore-push (domain-actions domx) actx)    
)

;;; Return a domain instance, given a list of symbols.
(defun domain-from (symbols) ; -> domain instance.
    ;(format t "~&domain-from: ~A" (type-of symbols))
    (assert (listp symbols))
    (assert (not (null symbols)))
    (assert (symbolp (car symbols)))
    (assert (eq (car symbols) 'DOM))

    (setf symbols (cdr symbols))

    (let (actions domx actx)
        (loop for tokx in symbols do
            ;(format t "~&domain-from ~A ~A" (type-of tokx) tokx)
            (setf actx (action-from tokx))
            (action-set-id actx (length actions))
            (push actx actions)
        )
        (assert (not (null actions)))

        (setf domx (domain-new :id 0 :initial-state (state-random (action-num-bits (car actions)))))
        (loop for actx in (reverse actions) do
             (domain-add-action domx actx)
        )
        domx
    )
)

;;; Run a plan.
;;; Return false as soon as an unexpected result happens.
;;; Otherwise return true.
(defun domain-run-plan (domx planx) ; -> bool. side effect, domain may be changed.
  (assert (domain-p domx))
  (assert (plan-p planx))
  (assert (= (domain-num-bits domx) (plan-num-bits planx)))

  (if (zerop (step-act-id (plan-first-step planx)))
     (return-from domain-run-plan true))

  (let (smpl)
    (format t "~&Domain: ~D, running plan: ~A" (domain-id domx) (plan-str planx))
    (loop for stepx in (plan-step-list planx) do

      (if (region-superset-of-state (step-initial-region stepx) (domain-current-state domx))
        (progn
          (setf smpl (action-take-sample-for-step (actionstore-nth (domain-actions domx) (step-act-id stepx)) (domain-current-state domx)))
          (setf (domain-current-state domx) (sample-result smpl))
          (when (not (region-superset-of-state (step-result-region stepx) (sample-result smpl)))
            (format t "~&step result region unexpected.")
            (return-from domain-run-plan false)
          )
        )
        (progn
          (format t "~&step initial region is not a superset of the current state")
          (return-from domain-run-plan false)
        )
      ) 
    ) ; next stepx
    true
  )
)

;;; Process a need.
(defun domain-process-need (domx needx) ; -> sample instance.
   ;(format t "~&domain-process-need: ~A ~A" (type-of domx) (type-of needx)) 
   (assert (domain-p domx))
   (assert (need-p needx))
   (assert (= (domain-id domx) (need-dom-id needx)))

   (let ((act-id (need-act-id needx)) smpl)
      (if (plan-is-not-empty (need-plan needx))
        (domain-run-plan domx (need-plan needx))
      )
      (if (or
           (and (state-p (need-target needx)) (state-eq (domain-current-state domx) (need-target needx)))
           (and (region-p (need-target needx)) (region-superset-of-state (need-target needx) (domain-current-state domx)))
          )
        (progn
          (setf smpl (action-take-sample-for-need (actionstore-nth (domain-actions domx) act-id) (domain-current-state domx) needx))
          (setf (domain-current-state domx) (sample-result smpl))
        )
        (format t "~&need action not taken")
      )
   )
)

;;; Return the domain change surface.
;;; The aggregation of all action group rule initial-regions that allow a predictable change to be made.
(defun domain-change-surface (domx) ; -> regionstore.
  (assert (domain-p domx))

  (let (ret
        (max-region (region-new (list (state-new-high (domain-current-state domx)) (state-new-low (domain-current-state domx)))))
       )
    ;; Get change surface.
    (setf ret (actionstore-change-surface (domain-actions domx)))

    ;; Combine regions, like (1xxx, 0xxx) or (01x1, 11x1, x101, x111).
    (setf ret (regionstore-subtract :min-store (regionstore-new (list max-region)) :sub-store ret))
    (setf ret (regionstore-subtract :min-store (regionstore-new (list max-region)) :sub-store ret))

    ret
  )
)

