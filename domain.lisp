;;;; Implement the Action type.
;;;;
(defstruct domain
  id		; A number id, GE zero.
  actions	; A actionstore.
  current-state	; The current state of the domain. Actions change this.
  max-region    ; The maximum region for the domain.
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

  (let (act0 high-state low-state sample-low sample-high (*dom-id* id) max-region)
    ;; Create highest state, lowest state, max-region.
    (setf high-state (state-new-high initial-state))
    (setf low-state (state-new-low initial-state))
    (setf max-region (region-new (list high-state low-state)))

    (let ((*max-region* max-region))
      ;; Create a no-op action as action 0.
  
      ;; Create no-op samples for highest state.
      (setf sample-high (sample-new :initial high-state :result high-state))
  
      ;; Create no-op samples for lowest state.
      (setf sample-low (sample-new :initial low-state :result low-state))
  
      ;; Create act 0 with no-op base rules.
      (setf act0 (action-new :id 0 :rules (list (rulestore-new (list (rule-union  (rule-new sample-low) (rule-new sample-high)))))))
  
      ;; Generate act 0 rules-by-experiance..
      (action-take-sample-arbitrary act0 high-state)
      (action-take-sample-arbitrary act0 low-state)
      (action-take-sample-arbitrary act0 high-state)
      (action-take-sample-arbitrary act0 low-state)
      (action-take-sample-arbitrary act0 high-state)
      (action-take-sample-arbitrary act0 low-state)
  
      ;; Construct result.
      (make-domain :id id :actions (actionstore-new (list act0)) :current-state initial-state
            :max-region max-region)
    )
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

    (format t "~&Domain ~D current-state ~A reachable: ~A" (domain-id domx) (state-str (domain-current-state domx))
      (regionstore-str (domain-reachable domx)))

    (actionstore-print (domain-actions domx))
)

;;; Return possible steps, given a rule.
(defun domain-get-steps (domx rule-from-to within &optional no-alt) ; -> stepstore.
  (assert (domain-p domx))
  (assert (rule-p rule-from-to))
  (assert (region-p within))
  (assert (= (domain-num-bits domx) (rule-num-bits rule-from-to)))
  (assert (= (domain-num-bits domx) (region-num-bits within)))

  (let  ((*dom-id* (domain-id domx)) (*max-region* (domain-max-region domx)))
    (actionstore-get-steps (domain-actions domx) rule-from-to within no-alt)
  )
)

;;; Return the number of bits used by a domain.
(defun domain-num-bits (domx) ; -> integer, gt zero.
  (assert (domain-p domx))

  (state-num-bits (domain-current-state domx))
)

;;; Return a plan to change a current region to a goal region.
;;; Since a random-depth-first process is used, try more than once, if needed.
(defun domain-get-plan (domx rule-from-to with-reg &optional no-alt) ; -> plan, or nil.
  ;(format t "~&domain-get-plan: domx ~D from ~A to ~A within ~A depth ~D" (domain-id domx) (region-str from-reg) (region-str to-reg) (region-str with-reg) depth)
  (assert (domain-p domx))
  (assert (rule-p rule-from-to))
  (assert (region-p with-reg))
  (assert (= (domain-num-bits domx) (rule-num-bits rule-from-to)))
  (assert (= (domain-num-bits domx) (region-num-bits with-reg)))
  (assert (region-superset-of :sup with-reg :sub (rule-initial-region rule-from-to)))
  (assert (region-superset-of :sup with-reg :sub (rule-result-region rule-from-to)))

  (let (plan
       (*dom-id* (domain-id domx)) (*max-region* (domain-max-region domx))
       (num-changes (rule-num-changes rule-from-to))) ; adjust depth limit by number chnages needed.

    (loop for i from 0 to 2
          while (null plan) do
      (setf plan (domain-get-plan2 domx rule-from-to with-reg (* 2 num-changes) no-alt))
    )
    (when plan
      (if (not (and (region-superset-of :sub (plan-initial-region plan) :sup (rule-initial-region rule-from-to))
                    (region-superset-of :sub (plan-result-region plan)  :sup (rule-result-region rule-from-to))))
        (error "~&plan ~A NOT correct for rule ~A" (plan-str plan) (rule-str rule-from-to))
      )
    )
    plan
  )
)

;;; Return a plan to change from a from-region to a to-region.
;;; An asymmetric step requires changes to get to the step, not within the expected changes between the from-region and to-region.
;;;
;;; Check for a one, or two, step solution.
;;; Else, check for asymmetric steps.
;;; Else, choose a forward-chaining step, or a backward-chaining step. randomly, then recurse.
;;;
;;; Wanted changes:     (rule-changes rule-from-to)
;;; Don't care changes: (change-new :m01 (region-x-mask to-region) :m10 (region-x-mask to-region))
;;; Unwanted changes:   (change-new :m01 (mask-and (region-0-mask from-region) (region-0-mask to-region))
;;;                                 :m10 (mask-and (region-1-mask from-region) (region-1-mask to-region)))
(defun domain-get-plan2 (domx rule-from-to with-reg depth &optional no-alt) ; -> plan, or nil.
  (assert (domain-p domx))
  (assert (rule-p rule-from-to))
  (assert (region-p with-reg))
 ; (format t "~&domain-get-plan2: domx ~D rule ~A within ~A depth ~D no-alt ~A" (domain-id domx) (rule-str rule-from-to)
 ;    (region-str with-reg) depth no-alt)

  ;; If no changes are needed, return a act zero plan.
  (if (change-is-low (rule-changes rule-from-to))
    (return-from domain-get-plan2 (plan-new (list (step-new 0 rule-from-to)))))

  ;; Check for recursion limit.
  (if (not (plusp depth))
    (return-from domain-get-plan2 nil))

  (let (steps
       (wanted-changes (rule-changes rule-from-to))
        steps-from steps-to
        (from-reg (rule-initial-region rule-from-to))
        (to-reg (rule-result-region rule-from-to))
       )

    ;; Get steps that contain at least one wanted bit change.
    (setf steps (domain-get-steps domx rule-from-to with-reg no-alt))

    ;; For steps with an alt-rule, if any.
    ;; Develop a return plan restricted to within, else skip using the step.
    ;; In the return plan, disallow using steps with alt-rules, to avoid an infinite regress of alt-rules.
    (let ((steps2 (stepstore-new nil)) planx plan-result)

      (loop for stpx in (stepstore-steps steps) do
        (if (step-alt-rule stpx)
          (progn
            ;; Get plan to return to the step initial region.
            (setf planx (domain-get-plan domx (rule-reverse (step-alt-rule stpx)) with-reg true)) ; true means no alt-rules.
            (when planx
              ;(format t "~&step ~A alt plan found ~A" (step-str stpx) (plan-str planx))
              (setf plan-result (plan-result-region planx))

              (when (not (region-eq (step-initial-region stpx) plan-result))
                (setf (step-rule stpx) (rule-restrict-initial-region (step-rule stpx) plan-result))
                (setf (step-alt-rule stpx) (rule-restrict-initial-region (step-alt-rule stpx) plan-result))
              )
              (setf (step-alt-plan stpx) planx)
              ;(format t "~&pushing step with alt plan ~A" (step-str stpx))
              (stepstore-push steps2 stpx)
            )
          )
          (stepstore-push steps2 stpx)
        )
      ) ; next stpx
      (setf steps steps2)
    )

    (if (stepstore-is-empty steps)
      (return-from domain-get-plan2 nil))

    ;; Check if steps contain all wanted changes.
    (if (not (change-eq (change-and wanted-changes (stepstore-aggregate-changes steps)) wanted-changes))
      (return-from domain-get-plan2 nil))

    ;; Get steps that intersect the from-region.
    (setf steps-from (stepstore-initial-region-intersects steps from-reg))

    ;; Get steps that intersect the to-region.
    (setf steps-to (stepstore-result-region-intersects steps to-reg))

    ;; Check for one step that spans the gap.
    (let (span-steps stepy planx)
      (loop for stepx in (stepstore-steps steps-from) do
        (when (region-intersects (step-result-region stepx) to-reg)
           (setf stepy (step-restrict-result-region stepx to-reg))
           (push stepy span-steps)
        )
      )
      (when span-steps
	    (setf planx (plan-new (list (nth (random (length span-steps)) span-steps))))
        ;(format t "~&one step span found: from: ~A to: ~A plan: ~A" (region-str from-reg) (region-str to-reg) (plan-str planx))
	    (return-from domain-get-plan2 planx)
      )
    )

    ;; Check for two steps that span the gap.
    (let (planx)
      (loop for step-f in (stepstore-steps steps-from) do

        (loop for step-t in (stepstore-steps steps-to) do
  
          (when (region-intersects (step-result-region step-f) (step-initial-region step-t))

            (setf planx (plan-new (list
              (step-restrict-result-region  step-f (step-initial-region step-t))
              (step-restrict-initial-region step-t (step-result-region step-f))))
            )
            ;(format t "~&two step span found: from: ~A to: ~A plan: ~A" (region-str from-reg) (region-str to-reg) (plan-str planx))
            (return-from domain-get-plan2 planx)
          )
        ) ; next step-t
      ) ; next step-f
    )

    ;; Check for asymmetric, required, steps.
    (let ((asym-steps (stepstore-asymmetric-steps steps rule-from-to)) stepx plan1 plan2 plan3 plan4)
      (when (stepstore-is-not-empty asym-steps)
        ;; Choose a step.
        (setf stepx (stepstore-nth asym-steps (random (stepstore-length asym-steps))))

        ;; Get first leg of a plan.
        (setf plan1 (domain-get-plan2 domx (rule-region-to-region from-reg (step-initial-region stepx)) with-reg (1- depth) no-alt))
        (if plan1
          (progn
            ;; Add stepx to plan, possibly restricting stepx.
            (setf plan2 (plan-link plan1 (plan-new (list stepx))))

            ;; Get second leg of plan. 
            (setf plan3 (domain-get-plan2 domx (rule-region-to-region (plan-result-region plan2) to-reg) with-reg (1- depth) no-alt))
            (if plan3
              (progn
                (setf plan4 (plan-link plan2 plan3))
                (if plan4
                  (progn
                    ;(format t "~&domain-get-plan2: Dom: ~D plan: ~A from: ~A to: ~A asym step ~A" *dom-id* (plan-str plan4)
                    ;   (region-str from-reg) (region-str to-reg) (step-str stepx))
                    (return-from domain-get-plan2 plan4)
                  )
                  (return-from domain-get-plan2 nil)
                )
              )
              (return-from domain-get-plan2 nil)
            )
          )
          (return-from domain-get-plan2 nil)
        )
      )
    )

    (if (and (stepstore-is-empty steps-from) (stepstore-is-empty steps-to))
      (return-from domain-get-plan2 nil))

    ;; Choose a step to continue.
    (let (choice step-f step-t rest-of-plan)

      (setf choice (random 2))

      (if (stepstore-is-empty steps-from)
        (setf choice 0))

      (if (stepstore-is-empty steps-to)
        (setf choice 1))

      ;; Choose a from step, or to-step.
      (if (= choice 1)
        (progn
          (setf step-f (stepstore-nth steps-from (random (stepstore-length steps-from))))
          (setf rest-of-plan (domain-get-plan2 domx (rule-region-to-region (step-result-region step-f) to-reg) with-reg (1- depth) no-alt))
          (if rest-of-plan
            (return-from domain-get-plan2 (plan-link (plan-new (list step-f)) rest-of-plan))
            (return-from domain-get-plan2 nil)
          )
        )
        (progn
          (setf step-t (stepstore-nth steps-to (random (stepstore-length steps-to))))
          (setf rest-of-plan (domain-get-plan2 domx (rule-region-to-region from-reg (step-initial-region step-t)) with-reg (1- depth) no-alt))
          (if rest-of-plan
            (return-from domain-get-plan2 (plan-link rest-of-plan (plan-new (list step-t))))
            (return-from domain-get-plan2 nil)
          )
        )
      )
    ) ; end-let
  ) ; end-let
)

(defun domain-get-needs (domx) ; ->  needstore.
  ;(format t "~&domain-get-needs: ~A" (type-of domx))
  (assert (domain-p domx))

  (let (needs (*dom-id* (domain-id domx)) (*max-region* (domain-max-region domx)) (needs2 (needstore-new nil)))

    (setf needs (actionstore-get-needs (domain-actions domx) (domain-current-state domx) (domain-reachable domx)))

    ;(needstore-set-dom-id needs (domain-id domx)) ; set needs domain-id

    ;; Find needs satisfied by the current state.
    ;; Its easier than making plans.
    (loop for needx in (needstore-needs needs) do
        (cond ((state-p (need-target needx))
                (when (state-eq (need-target needx) (domain-current-state domx))
                  (setf (need-plan needx) (plan-new nil))
                  (needstore-push needs2 needx)
                )
              )
              ((region-p (need-target needx))
                (when (region-superset-of-state (need-target needx) (domain-current-state domx))
                  (setf (need-plan needx) (plan-new nil))
                  (needstore-push needs2 needx)
                )
              )
              (t (error "Unrecognized target type"))
        )
    )

    (if (needstore-is-not-empty needs2)
      (return-from domain-get-needs needs2))
;-------------------------------------------------------------------------------
;    ;; Find plan for each need.
;    (loop for needx in (needstore-needs needs) do
;        (cond ((state-p (need-target needx))
;                (if (state-eq (need-target needx) (domain-current-state domx))
;                  (setf (need-plan needx) (plan-new nil))
;                  (setf (need-plan needx) (domain-get-plan domx
;                                                   (rule-region-to-region (region-new (domain-current-state domx)) (region-new (need-target needx)))
;                                                   (domain-max-region domx))))
;              )
;              ((region-p (need-target needx))
;                (if (region-superset-of-state (need-target needx) (domain-current-state domx))
;                  (setf (need-plan needx) (plan-new nil))
;                  (setf (need-plan needx) (domain-get-plan domx
;                                                  (rule-region-to-region  (region-new (domain-current-state domx)) (need-target needx))
;                                                  (domain-max-region domx))))
;              )
;              (t (error "Unrecognized target type"))
;        )
;    )
    needs
  )
)

(defun domain-add-action (domx actx)
  (assert (domain-p domx))
  (assert (action-p actx))
  (assert (= (domain-num-bits domx) (action-num-bits actx)))

  (let ((*dom-id* (domain-id domx)) (*max-region* (domain-max-region domx)))
    (action-set-id actx (actionstore-length (domain-actions domx)))
    (actionstore-push (domain-actions domx) actx)
  )
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

  (let (smpl (*dom-id* (domain-id domx)) (*max-region* (domain-max-region domx)))
    (format t "~&Dom: ~D Running plan: ~A" (domain-id domx) (plan-str planx))
    (loop for stepx in (plan-step-list planx) do

      (when (> (step-act-id stepx) 0) ; Skip no change, other steps are expected to make a change.
        (if (region-superset-of-state (step-initial-region stepx) (domain-current-state domx))
          (progn
            (setf smpl (action-take-sample-for-step (actionstore-nth (domain-actions domx) (step-act-id stepx)) (domain-current-state domx)))

            ;; Resample-on-no-change heuristic.
            (when (sample-no-change smpl)
              (format t "~&Dom: ~D Step ~A result ~A unexpected, retrying." (domain-id domx) (step-str stepx) (state-str (sample-result smpl)))
              (setf smpl (action-take-sample-for-step (actionstore-nth (domain-actions domx) (step-act-id stepx)) (domain-current-state domx)))
            )
            (setf (domain-current-state domx) (sample-result smpl))
            (when (not (region-superset-of-state (step-result-region stepx) (sample-result smpl)))

              (when (step-alt-rule stepx)

                (when (region-superset-of-state (rule-result-region (step-alt-rule stepx)) (sample-result smpl))

                  (format t "~&Dom: ~D step ~A result ~A unwanted, running alt plan." (domain-id domx) (step-str stepx) (state-str (sample-result smpl)))
                  (if (domain-run-plan domx (step-alt-plan stepx))
                    (progn
                      (setf smpl (action-take-sample-for-step (actionstore-nth (domain-actions domx) (step-act-id stepx)) (domain-current-state domx)))
                      (setf (domain-current-state domx) (sample-result smpl))
                    )
                    (progn
                      (format t "~&Dom: ~D Step ~A result ~A unexpected, Alt plan failed, learned something."  (domain-id domx) (step-str stepx) (state-str (sample-result smpl)))
                      (return-from domain-run-plan false)
                    )
                  )
                )
              )

              (when (not (region-superset-of-state (step-result-region stepx) (sample-result smpl)))
                (format t "~&Dom: ~D Step ~A result ~A unexpected, plan failed, learned something." (domain-id domx) (step-str stepx) (state-str (sample-result smpl)))
                (return-from domain-run-plan false)
              )
            )
          )
          (progn
            (format t "~&step initial region is not a superset of the current state")
            (return-from domain-run-plan false)
          )
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

   (let ((act-id (need-act-id needx)) smpl  (*dom-id* (domain-id domx)) (*max-region* (domain-max-region domx)))
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

;;; Return a regionstore of a region of appalently reachable states from the current state.
;;; The aggregation of all changes that are possible to the current state.
(defun domain-reachable (domx) ; -> regionstore.
  (assert (domain-p domx))

  (let ((domain-changes (actionstore-changes (domain-actions domx)))
        (max-region (region-new (domain-current-state domx)))
        xmask
       )

    ;; Apply possible changes to the current state.
    ;; Combine regions, like (1xxx, 0xxx) or (01x1, 11x1, x101, x111).
    (setf xmask (mask-new-or
                  (mask-new-and (state-not (domain-current-state domx)) (change-m01 domain-changes))
                  (mask-new-and (domain-current-state domx) (change-m10 domain-changes))
                )
    )
    (setf max-region (region-set-to-x max-region xmask))

    (regionstore-new (list max-region))
  )
)

;;; Take an arbitrary sample, given act-id and state.
(defun domain-take-sample-arbitrary (domx act-id state) ; -> ?
  (assert (domain-p domx))
  (assert (actionstore-valid-id (domain-actions domx) act-id))
  (assert (state-p state))

  (let ((*dom-id* (domain-id domx)) (*max-region* (domain-max-region domx)))
    (action-take-sample-arbitrary (actionstore-nth (domain-actions domx) act-id) state)
  )
)

;;; Take a sample for a need.
(defun domain-take-sample-for-need (domx nedx) ; -> ?
  (assert (domain-p domx))
  (assert (need-p nedx))

  (let ((*dom-id* (domain-id domx)) (*max-region* (domain-max-region domx)))
    (action-take-sample-for-need (actionstore-nth (domain-actions domx) (need-act-id nedx)) (domain-current-state domx) nedx)
  )
)

