;;;; Implement a need struct

;;; Define need kinds
(defvar *get-first-sample-of-state* 1009)
(defvar *resample-state*            1013)
(defvar *get-sample-in-region*      1019)
(defvar *sample-state* 1023)
(defvar *kinds* (list *get-first-sample-of-state* *resample-state* *get-sample-in-region* *sample-state*))

;;; Define need reasons
(defvar *state-not-in-group* 2003)
(defvar *group-set-pnc*      2011)
(defvar *form-group*         2017)
(defvar *limit-group*        2027)
(defvar *test-region*        2039)
(defvar *change-defining-squares* 2053)
(defvar *reasons* (list *state-not-in-group* *group-set-pnc* *form-group* *limit-group* *test-region* *change-defining-squares*))

(defstruct need
    (dom-id 0)
    (act-id 0)
    (kind 0)
    (priority 0)
    (reason 0)
    target
    info
    (region nil)
    (plan nil)
)

; Functions automatically created by defstruct:
;
; Most used:
; (need-<field name> <instance>) returns struct field.
; (need-p <instance>) -> t
;
; Least used:
; (make-need [:<field-name> <field-value>]*), use need-new instead.
; (copy-need <instance>) copies a need instance, don't use.
; (type-of <instance>) -> need
; (typep <instance> 'need) -> t

; Return a new need instance.
(defun need-new (&key (dom-id 0) act-id kind reason target (region nil) (info "No info"))
    (assert (integerp dom-id))
    (assert (integerp act-id))
    (assert (numberp kind))
    (assert (member kind *kinds*))
    (assert (member reason *reasons*))
    (assert (or (null info) (stringp info)))

    (let ((pri 0))
        ; Calc priority
        (cond ((= kind *get-first-sample-of-state*)
            (cond ((= reason *state-not-in-group*)
                      (setf pri 900)
                  )
                  ((= reason *form-group*)
                      (setf pri 100)
                  )
                  ((= reason *limit-group*)
                      (setf pri 700)
                  )
                  ((= reason *change-defining-squares*)
                      (setf pri 200)
                  )
                  (t (error "~&Need kind ~D reason ~D not found" kind reason)))
            )
            ((= kind *resample-state*)
                (cond ((= reason *group-set-pnc*)
                          (setf pri (- 490 (region-num-x region)))
                      )
                      ((= reason *limit-group*)
                          (setf pri (- 390 (region-num-x region)))
                      )
                      ((= reason *change-defining-squares*)
                          (setf pri 600)
                      )
                      ((= reason *form-group*)
                          (setf pri (- 590 (region-num-x region)))
                      )
                      (t (error "~&Need kind ~D reason ~D not found" kind reason)))
            )
            ((= kind *get-sample-in-region*)
                (cond ((= reason *test-region*)
                          (setf pri 0)
                      )
                      (t (error "~&Need kind ~D reason ~D not found" kind reason)))
            )
            ((= kind *sample-state*)
                (cond ((= reason *state-not-in-group*)
                          (setf pri 0)
                      )
                      (t (error "~&Need kind ~D reason ~D not found (1)" kind reason)))
            )
            (t (error "~&Need kind ~D reason ~D not found (2)" kind reason))
	) ; end cond

        (make-need :dom-id dom-id :act-id act-id :kind kind :priority pri :reason reason :target target
                   :region region :info info)
    )
)

;;; Return a string representing a need.
(defun need-str (needx)
    (assert (need-p needx))

    (let ((str "#S[NEED "))
        (setf str (concatenate 'string str (format nil ":dom ~D " (need-dom-id needx))))
        (setf str (concatenate 'string str (format nil ":act ~D " (need-act-id needx))))

        (setf str (concatenate 'string str (format nil ":pri ~D " (need-priority needx))))

        (cond ((= (need-kind needx) *get-first-sample-of-state*)
                (setf str (concatenate 'string str ":kind Get first sample of state ")))
              ((= (need-kind needx) *resample-state*)
                (setf str (concatenate 'string str ":kind Resample state ")))
              ((= (need-kind needx) *get-sample-in-region*)
                (setf str (concatenate 'string str ":kind Get sample in region ")))
        )

        (cond ((= (need-reason needx) *state-not-in-group*)
                (setf str (concatenate 'string str ":reason State not in a group ")))
              ((= (need-reason needx) *group-set-pnc*)
                (setf str (concatenate 'string str ":reason Group set pnc ")))
              ((= (need-reason needx) *form-group*)
                (setf str (concatenate 'string str ":reason Form group ")))
              ((= (need-reason needx) *limit-group*)
                (setf str (concatenate 'string str ":reason Limit group ")))
              ((= (need-reason needx) *change-defining-squares*)
                (setf str (concatenate 'string str ":reason Change defining squares ")))
              ((= (need-reason needx) *test-region*)
                (setf str (concatenate 'string str ":reason Test region ")))
        )

        (if (state-p (need-target needx))
            (setf str (concatenate 'string str (format nil ":target ~A " (state-str (need-target needx)))))
            (setf str (concatenate 'string str (format nil ":target ~A " (region-str (need-target needx))))))

        (if (not (null (need-info needx)))
            (setf str (concatenate 'string str (format nil ":info ~A " (need-info needx)))))

        (when (not (null (need-plan needx)))
            (setf str (concatenate 'string str "P["))
            (loop for stepx in (plan-steps (need-plan needx)) do
                (setf str (concatenate 'string str (format nil "~D" (astep-act-id stepx))))
            )
            (setf str (concatenate 'string str "]"))
        )
  
        (setf str (concatenate 'string str "]"))
        str
    )
)

;;; Return true if a list is a list of needs.
;;; An empty list will return true.
(defun need-list-p (nedlst) ; -> bool
  (if (not (listp nedlst))
    (return-from need-list-p false))

  (loop for nedx in nedlst do
    (if (not (need-p nedx))
      (return-from need-list-p false))
  )
  true
)

