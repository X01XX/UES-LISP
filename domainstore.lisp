;;;; Implement a store of domains.

(defvar true t)
(defvar false nil)

; Implement a store of domains.
(defstruct domainstore
  domain-list  ; A list of zero, or more, domains with unique id values.
  initial-selectregionsstore      ; Initial select regions.
  selectregionsstore-fragments    ; intersections-of-intersections of the initial-selectregionsstore.
  non-negative-regionscorrstore   ; Non-negative select regions.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (domainstore-<field name> <instance>) -> struct field.
;   (domainstore-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> domainstore
;   (typep <instance> 'domainstore) -> bool
;
; Probably shouldn't use:
;   (make-domainstore [:<field-name> <field-domainstore>]*), use domainstore-new instead.
;   (copy-domainstore <instance>) copies a domainstore instance.

;;; Return a new domainstore instance.
;;; Domains cannot have duplicate ids.
(defun domainstore-new (domains) ; -> domainstore.
  ;(format t "~&domains ~A" domains)
  (assert (domain-list-p domains))

  (let ((ret (make-domainstore
	       :domain-list nil
	       :initial-selectregionsstore (selectregionsstore-new nil)
	       :selectregionsstore-fragments (selectregionsstore-new nil)
	       :non-negative-regionscorrstore (regionscorrstore-new nil)
	     )
	 )
       )
    (loop for domx in domains do 
      (if (not (domainstore-contains ret domx))
        (domainstore-push ret domx))
    )
    ret
  )
)

;;; Add a selectregions instance.
;;; Do not call this until all domains are added.
;;; If a selectregions instance has zero values, it will be ignored.
(defun domainstore-add-selectregions (storex sregsx) ; nothing, side effect domainstore changed.
  (assert (domainstore-p storex))
  (assert (selectregions-p sregsx))

  ;; Check values.
  (if (and (zerop (selectregions-positive sregsx))
           (zerop (selectregions-negative sregsx)))
      (return-from domainstore-add-selectregions))

  (assert (= (domainstore-length storex) (selectregions-length sregsx)))

  ;; Check that selectregions are congruent with domains.
  (loop for domx in (domainstore-domain-list storex)
        for regsx in (selectregions-region-list sregsx) do
    (assert (= (domain-num-bits domx) (region-num-bits regsx)))
  )

  ;; Check for duplicate selectregions.
  (loop for sregsy in (selectregionsstore-selectregions-list (domainstore-initial-selectregionsstore storex)) do

    (when (regionscorr-eq (selectregions-regionscorr sregsx) (selectregions-regionscorr sregsy))

      (setf (selectregions-positive sregsy) (+ (selectregions-positive sregsy) (selectregions-positive sregsx)))
      (setf (selectregions-negative sregsy) (+ (selectregions-negative sregsy) (selectregions-negative sregsx)))
      (return-from domainstore-add-selectregions)
    )
  )
  (selectregionsstore-push (domainstore-initial-selectregionsstore storex) sregsx)
)

;;; Push a new domain into a domainstore, suppress dups, subsets.
;;; Return true if the domain has been added.
(defun domainstore-push (storex domx) ; -> bool, true if added.
  (assert (domainstore-p storex))
  (assert (domain-p domx))

  ; Check for equal domains.
  (loop for acty in (domainstore-domain-list storex) do
    (if (= (domain-id acty) (domain-id domx))
      (return-from domainstore-push false))
  )

  (push domx (domainstore-domain-list storex))
  true
)

;;; Return the number of domains in a domainstore.
(defun domainstore-length (storex) ; -> number.
  (assert (domainstore-p storex))

  (length (domainstore-domain-list storex))
)

;;; Return true if a domainstore is empty.
(defun domainstore-is-empty (storex) ; -> bool
  (zerop (domainstore-length storex))
)

;;; Return true if a domainstore is not empty.
(defun domainstore-is-not-empty (storex) ; -> bool
  (plusp (domainstore-length storex))
)

;;; Return a string representing a domainstore.
(defun domainstore-str (storex) ; -> string.
  (assert (domainstore-p storex))

  (let ((ret "#S(ACTIONSTORE ") (start t))

    (loop for domx in (domainstore-domain-list storex) do
      (if start (setf start nil) (setf ret (concatenate 'string ret ", ")))    
      (setf ret (concatenate 'string ret (domain-str domx)))
    )
    (setf ret (concatenate 'string ret "~& initial-selectregions:"))

    (loop for sregsx in (domainstore-initial-selectregions storex) do
      (setf ret (concatenate 'string ret (selectregions-str sregsx)))
    )

    (setf ret (concatenate 'string ret ")"))

    ret
  )
)

;;; Return true if a domainstore contains a given domain.
(defun domainstore-contains (storex domx) ; -> bool
  ;(format t "domainstore-contains storex ~A domx ~A" storex domx)
  (assert (domainstore-p storex))
  (assert (domain-p domx))

  (if (member domx (domainstore-domain-list storex) :test #'domain-eq) true false)
)

;;; Calculate selectregions, aftec all domains are added, after all selectrecions are added.
(defun domainstore-calc-select (storex) ; -> nothing, side-effect, domainstore select regions changed.
  (assert (domainstore-p storex))

  (let ((regionscorrstorex (regionscorrstore-new nil)) fragments (selectregionfragments (selectregionsstore-new nil)) vals)

    ;; Gather selectregions regionscorr into a regionscorrstore.
    (loop for selectregionsx in (selectregionsstore-selectregions-list (domainstore-initial-selectregionsstore storex)) do
      (regionscorrstore-push regionscorrstorex (selectregions-regionscorr selectregionsx))
    )
    ;; Generate fragments.
    (setf fragments (regionscorrstore-intersections-of-intersections regionscorrstorex))

    ;; Generate selectregions with values.
    (loop for regionscorrx in (regionscorrstore-regionscorr-list fragments) do
        (setf vals (selectregionsstore-values (domainstore-initial-selectregionsstore storex) regionscorrx))
	(selectregionsstore-push selectregionfragments (selectregions-new regionscorrx (car vals) (second vals)))
    )
    (setf (domainstore-selectregionsstore-fragments storex) selectregionfragments)
    ;(format t "~&fragment selectregions: ~A" selectregionfragments)
  )

  ;; Calc non-negative regionscorrs.
  (let ((regionscorrstore-non-negative (regionscorrstore-new (list (domainstore-max-regions storex)))))

    ;; Subtract non-negative selectregions regionscorr from max domain regions.
    (loop for selectregionsx in (selectregionsstore-selectregions-list (domainstore-initial-selectregionsstore storex)) do

      (when (< (selectregions-negative selectregionsx) 0)

	(setf regionscorrstore-non-negative (regionscorrstore-subtract-regionscorr regionscorrstore-non-negative 
		(selectregions-regionscorr selectregionsx)))
      )
    )

    ;; Double check.
    (loop for regionscorrx in (regionscorrstore-regionscorr-list regionscorrstore-non-negative) do

      (loop for selectregionsx in (selectregionsstore-selectregions-list (domainstore-initial-selectregionsstore storex)) do
	 (when (and (< (selectregions-negative selectregionsx) 0) 
	            (regionscorr-intersects regionscorrx (selectregions-regionscorr selectregionsx)))
	   (format t "~&~A intersects ~A" regionscorrx selectregionsx)
	   (assert (not (regionscorr-intersects regionscorrx (selectregions-regionscorr selectregionsx))))
	 )
      )
    )

    ;; Store non-negative selectregions.
    (setf (domainstore-non-negative-regionscorrstore storex) regionscorrstore-non-negative)
    ;(format t "~&non-negative selectregions: ~A" (domainstore-non-negative-regionscorrstore storex))
  )
)

;;; Return a regionscorrstore of the maximum regions of each domain, in order.
(defun domainstore-max-regions (storex) ; -> regionscorr
  (assert (domainstore-p storex))

  (let (regions)
    (loop for domx in (domainstore-domain-list storex) do
	(push (domain-max-region domx) regions)
    )
    (regionscorr-new (reverse regions))
  )
)

;;; Return true if a domainstore is congruent with a regionstorecorr.
(defun domainstore-congruent (storex regionscorrx) ; -> bool
  (assert (domainstore-p storex))
  (assert (regionscorr-p regionscorrx))

  (loop for domx in (domainstore-domain-list storex)
        for regx in (regionscorr-region-list regionscorrx) do

    (if (/= (domain-num-bits domx) (region-num-bits regx))
      (return-from domainstore-congruent false))
  )
  true
)

;;; Return plans to go from one region to another.
(defun domainstore-get-plans (storex from-regs to-regs)
  (assert (domainstore-p storex))
  (assert (regionscorr-p from-regs))
  (assert (regionscorr-p to-regs))
  (assert (domainstore-congruent storex from-regs))
  (assert (domainstore-congruent storex to-regs))
  (assert (not (regionscorr-intersects from-regs to-regs)))

  (let (last-regs from-to superset aplan pathx (all-plans (planstore-new nil))) 

    (setf pathx (regionscorrstore-find-path (domainstore-non-negative-regionscorrstore storex) from-regs to-regs))
    (format t "~&pathx ~A" pathx)

    (loop for regsx in (pathcorr-regionscorr-list pathx) do
      (if last-regs
	(progn
	  (setf from-to (append from-to (list (regionscorr-intersection last-regs regsx))))
	)
      )
      (setf last-regs regsx)
    )
    (format t "~&from-to ~A" from-to)

    (setf last-regs from-regs)
    
    (loop for regsx in (cdr from-to) 
	  for inx from 0 below (length (cdr from-to)) do

	(loop for regsy in (pathcorr-regionscorr-list pathx) do
	  (if (and (regionscorr-superset-of :sup-regcorr regsy :sub-regcorr last-regs)
	           (regionscorr-superset-of :sup-regcorr regsy :sub-regcorr regsx))
	    (setf superset regsy)
	  )
	)
	(format t "~&from ~A to ~A within ~A" last-regs regsx superset)
	(setf aplan (domainstore-get-plan storex last-regs regsx superset))
	(if aplan
	  (progn
	    (format t "~&plan is ~A" aplan)
	    (setf (nth inx (regionscorr-region-list last-regs))
		      (step-result-region (plan-last-step (car (planstore-plan-list aplan)))))

	    (planstore-add-end-link all-plans (car (planstore-plan-list aplan)))
	  )
	  (progn
	    (format t "~&No plan found" aplan)
	    (return-from domainstore-get-plans nil)
	  )
	)
    )
  )
)

;;; Return plans to go from one region to another, within a given region.
(defun domainstore-get-plan (storex from-regs to-regs within) ; -> planstore, or nil.
  ;(format t "~&domainstore-get-plan: from ~A to ~A within ~A" from-regs to-regs within)
  (assert (domainstore-p storex))
  (assert (regionscorr-p from-regs))
  (assert (regionscorr-p to-regs))
  (assert (regionscorr-p within))
  (assert (domainstore-congruent storex from-regs))
  (assert (domainstore-congruent storex to-regs))
  (assert (domainstore-congruent storex within))
  (assert (not (regionscorr-intersects from-regs to-regs)))
  (assert (regionscorr-superset-of :sup-regcorr within :sub-regcorr from-regs))
  (assert (regionscorr-superset-of :sup-regcorr within :sub-regcorr to-regs))

  (let ((plans (planstore-new nil)) domx-plan from-next)

    (setf from-next (car (regionscorr-region-list from-regs)))

    (loop for domx in (domainstore-domain-list storex)
          for from-regx in (regionscorr-region-list from-regs)
          for to-regx   in (regionscorr-region-list to-regs)
          for with-regx in (regionscorr-region-list within) do
      ;(format t "~&from-next: ~A" from-next)
      (when (not (region-superset-of :sup to-regx :sub from-next))

	(assert (region-superset-of :sup from-regx :sub from-next))

        (setf domx-plan (domain-get-plan domx from-next to-regx with-regx))
	(format t "~&domx-plan ~A" domx-plan)

	(if (null domx-plan)
	  (return-from domainstore-get-plan nil))

        (if (plan-is-not-empty domx-plan)
	  (progn
	    (setf from-next (step-result-region (plan-last-step domx-plan)))
	    (planstore-add-end-link plans domx-plan)
	  )
	  (return-from domainstore-get-plan nil))
      )
    )
    plans
  )
)
