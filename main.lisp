;;;; The Unorthodox-Expert-System, in LISP.
;;;;
;;;; The obvious question is: What is it an expert of?
;;;;
;;;; It is an expert of its own state.
;;;;
;;;; For use outside of the GPL-3.0 license, contact the Wisconsin Alumni Research Foundation (WARF).
;;;;

(load #p "err.lisp")
(load #p "tools.lisp")

(load #p "value.lisp")
(load #p "value_t.lisp")

(load #p "state.lisp")
(load #p "state_t.lisp")
(load #p "statestore.lisp")
(load #p "statestore_t.lisp")

(load #p "mask.lisp")
(load #p "mask_t.lisp")
(load #p "maskstore.lisp")
(load #p "maskstore_t.lisp")
(load #p "maskscorr.lisp")
(load #p "maskscorr_t.lisp")

(load #p "region.lisp")
(load #p "region_t.lisp")
(load #p "regionstore.lisp")
(load #p "regionstore_t.lisp")

(load #p "sample.lisp")
(load #p "sample_t.lisp")

(load #p "rule.lisp")
(load #p "rule_t.lisp")

(load #p "rulestore.lisp")
(load #p "rulestore_t.lisp")

(load #p "rulescorr.lisp")
(load #p "rulescorr_t.lisp")

(load #p "group.lisp")
(load #p "group_t.lisp")
(load #p "groupstore.lisp")
(load #p "groupstore_t.lisp")

(load #p "action.lisp")
(load #p "action_t.lisp")
(load #p "actionstore.lisp")
(load #p "actionstore_t.lisp")

(load #p "step.lisp")
(load #p "step_t.lisp")
(load #p "stepstore.lisp")
(load #p "stepstore_t.lisp")

(load #p "change.lisp")
(load #p "change_t.lisp")

(load #p "domain.lisp")
(load #p "domain_t.lisp")

(load #p "anyxofn.lisp")

(load #p "cngstps.lisp")
(load #p "cngstps_t.lisp")

(load #p "cngstpsstore.lisp")
(load #p "cngstpsstore_t.lisp")

(load #p "regionscorr.lisp")
(load #p "regionscorr_t.lisp")

(load #p "pathscorr.lisp")
(load #p "pathscorr_t.lisp")

(load #p "regionscorrstore.lisp")
(load #p "regionscorrstore_t.lisp")

(load #p "domainstore.lisp")
(load #p "domainstore_t.lisp")

(load #p "plan.lisp")
(load #p "plan_t.lisp")

(load #p "planstore.lisp")
(load #p "planstore_t.lisp")

(load #p "selectregions.lisp")
(load #p "selectregions_t.lisp")

(load #p "selectregionsstore.lisp")
(load #p "selectregionsstore_t.lisp")

(load #p "pn.lisp")

(load #p "square.lisp")
(load #p "square_t.lisp")

(load #p "planscorr.lisp")
(load #p "planscorr_t.lisp")

(load #p "planscorrstore.lisp")
(load #p "planscorrstore_t.lisp")

(load #p "need.lisp")
(load #p "need_t.lisp")

(load #p "needstore.lisp")
(load #p "needstore_t.lisp")

(load #p "sessiondata.lisp")
(load #p "statescorr.lisp")
(load #p "squarestore.lisp")

(defvar true t)
(defvar false nil)

(defun main ()
  (run)
)

;;; (do-interactive-session nil)
(defun default-session ()
  (let (dmxs)
    (setf dmxs (domainstore-new)) ; Init domainstore.
    (domainstore-add-domain (state-from "v0000")) ; Add a domain.
    (domainstore-add-domain (state-from "v00"))   ; Add a domain.

    (do-interactive-session dmxs)
  )
)

;;; Do commands against a given DomainList instance.
(defun do-interactive-session (dmxs)
  (multiple-value-bind (needs can-do cant-do) (generate-and-display-needs dmxs)
    (command-loop dmxs needs can-do cant-do)
  )
)

(defun generate-and-display-needs (dmxs) ; -> (values needs can-do cant-do)
  (format t "~&generate-and-display-needs")
  (assert (domainstore-p dmxs))

  (let (needs can-do cant-do)
    (multivalue-bind (needs can-do cant-do) (domainstore-get-needs dmxs)
      (display-needs dmxs needs can-do cant-do)
      (values needs can-do cant-do)
    )
  )
)

(defun command-loop (sessx needs can-do)
  (format t "~&command-loop")
  (assert (domainstore-p sessx))
  (let (inp tokens token)
    (loop 
      (format t "~&Press Enter or type a command: ")
      (setf inp (read-line *STANDARD-INPUT*))

      ; Parse tokens from the input string
      (setf tokens nil token nil)
      (loop for char across inp do
          ;(format t "c ~A" char)
          (when (char= char #\ )
              (if (not (null token))
                  (push token tokens))
              (setf token nil)
          )
          (when (char/= char #\ )
              (if token
                  (setf token (format nil "~A~A" token char))
                  (setf token (format nil "~A" char)))
          )
      )
      (when token
        (push token tokens))

      (setf tokens (reverse tokens))

      (format t "~&tokens: ~A" tokens)

      (if (or (string= (car tokens) #\q) (string= (car tokens) #\Q))
	(return-from command-loop))

      (if (null tokens)
	;; Process needs.
	(if can-do
	  (do-any-need sessx needs can-do)
	)
      )
    )
  )
) ; end command-loop

(defun do-any-need (sessx needs can-do) 
  (format t "~&do-any-needs")
  (assert (sessiondata-p sessx))
  (assert (needstore-p needs))
  (assert (needstore-p can-do))
)

;;; Run a new session.
(defun run (&optional fname) 
    (if (null fname) (setf fname "default.kmp"))

    (let ((in (open fname :if-does-not-exist nil)) (str "") sdx sdx-in)
        (when in
            (loop for line = (read-line in nil)
                while line do
                   (setf str (concatenate 'string str line))
                   (setf str (concatenate 'string str (coerce (list #\NewLine) 'string)))
            )
            (close in)
            ;(setf str (remove-comments str))
            ;(format t "~&final: ~A" str)
            (setf sdx-in (read-from-string str)) ; read in data, check that parentheses are balanced.
            (when sdx-in
                (pprint sdx-in)
                (setf sdx (eval sdx-in))
                ;(setf sdx (sessiondata-from sdx-in))
                (format t "~&type sdx ~A" (type-of sdx))
 ;              (format t "~&sdx ~A" sdx)
            )
        )
        ;tokens
    )
)

(defun all-tests ()
  (format t "~&All tests beginning")
  (value-tests)

  (state-tests)
  (statestore-tests)

  (mask-tests)
  (maskstore-tests)

  (rule-tests)
  (rulestore-tests)

  (region-tests)

  (sample-tests)

  (group-tests)
  (groupstore-tests)

  (action-tests)
  (actionstore-tests)

  (step-tests)
  (stepstore-tests)

  (change-tests)
  (cngstps-tests)

  (cngstpsstore-tests)
  (regionstore-tests)

  (pathscorr-tests)

  (regionscorr-tests)
  (regionscorrstore-tests)

  (square-tests)

  (domain-tests)
  (domainstore-tests)

  (plan-tests)
  (planstore-tests)

  (selectregions-tests)
  (selectregionsstore-tests)

  (maskscorr-tests)
  (rulescorr-tests)
  (planscorr-tests)
  (planscorrstore-tests)

  (format t "~&All tests done")
  t
)
