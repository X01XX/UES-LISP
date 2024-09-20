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

  (let (last-regs from-to superset aplanstr pathx (all-plans (planstore-new nil)) from-to-int last-int cur-int) 

    (setf pathx (regionscorrstore-find-path (domainstore-non-negative-regionscorrstore storex) from-regs to-regs))
    (format t "~&pathx ~A" pathx)

    (setf last-regs (car (pathcorr-regionscorr-list pathx)))
    (setf last-int (car (pathcorr-regionscorr-list pathx)))
    (loop for regsx in (cdr (pathcorr-regionscorr-list pathx)) do
      ;(format t "~&last regs ~A regs ~A" last-regs regsx)
      (when (not (regionscorr-superset-of :sup-regscorr regsx :sub-regscorr last-regs))
	(setf cur-int (regionscorr-intersection last-regs regsx))
	(format t "~&from ~A to ~A within ~A" last-int cur-int last-regs)
	(setf last-int (regionscorr-translate-to last-int cur-int)) ; todo change from regionscorr-translate-to to get-plan.
      )

      (setf last-regs regsx)
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
  
  (if (not (regionscorr-superset-of :sup-regscorr within :sub-regscorr from-regs))
    (format t "~&domainstore-get-plan: within ~A not superset from regs ~A" within from-regs)
    )
  (assert (regionscorr-superset-of :sup-regscorr within :sub-regscorr from-regs))

  (if (not (regionscorr-superset-of :sup-regscorr within :sub-regscorr to-regs))
    (format t "~&domainstore-get-plan: within ~A not superset from regs ~A" within to-regs)
    )
  (assert (regionscorr-superset-of :sup-regscorr within :sub-regscorr to-regs))

  (let ((ret-store (planstore-new nil)) domx-plan from-next)

    (setf from-next (car (regionscorr-region-list from-regs)))

    (loop for domx in (domainstore-domain-list storex)
          for from-regx in (regionscorr-region-list from-regs)
          for to-regx   in (regionscorr-region-list to-regs)
          for with-regx in (regionscorr-region-list within) do

      (when (not (region-superset-of :sup to-regx :sub from-next))

	(assert (region-superset-of :sup from-regx :sub from-next))

        (setf domx-plan (domain-get-plan domx from-next to-regx with-regx 5))

	(if (null domx-plan)
	  (return-from domainstore-get-plan nil))

        (if (plan-is-not-empty domx-plan)
	  (planstore-add-end-link ret-store domx-plan))
      )
    )
    (format t "~&domainstore-get-plan: returning ~A" ret-store)
    ret-store
  )
)
