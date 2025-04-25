;;;; Implement a series of regions, with bit-number values corresponding to a list of domains.

; Implement a store of regions, corresponding to a higher-level list of domains.
(defstruct regionscorr
  regionstore   ; A store of regions, the same number of regions, and bits used in each region,
                ; for all regionscorr instances.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (regionscorr-<field name> <instance>) -> struct field.
;   (regionscorr-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> regionscorr
;   (typep <instance> 'regionscorr) -> bool
;
; Probably shouldn't use:
;   (make-regionscorr [:<field-name> <field-regionscorr>]*), use regionscorr-new instead.
;   (copy-regionscorr <instance>) copies a regionscorr instance.

;;; Return a new regionscorr instance, from a list of regions, or a regionstore.
;;; If this is tightly controlled, checking domain congruency of arguments to other functions is unneeded.
;;; Don't use make-regionscorr anywhere else.
(defun regionscorr-new (regions) ; -> regionscorr, or nil.
  (let (storex)
    ;; Check argument, convert a region list to a regionstore.
    (cond ((listp regions) (setf storex (regionstore-new regions)))
          ((regionstore-p regions) (setf storex regions))
          (t (error "unexpected argument")))
         
    (assert (regionstore-congruent storex)) ; regionstore is congruent with a higer-level domain list.

    ;; Construct result.
    (make-regionscorr :regionstore storex)
  )
)

;;; Return a list of regions from a regionscorr.
(defun regionscorr-region-list (regionscorrx) ; -> list of regions.
  ;; Check argument.
  (assert (regionscorr-p regionscorrx))

  ;; Construct result.
  (regionstore-regions (regionscorr-regionstore regionscorrx))
)

;;; Return a string representing a regionscorr.
(defun regionscorr-str (regionscorrx) ; -> string.
  ;; Check argument.
  (assert (regionscorr-p regionscorrx))

  ;; Construct result.
  (format nil "(RC ~A)" (regionstore-str2 (regionscorr-regionstore regionscorrx)))
)

;;; Return true if two regionscorrs intersect.
(defun regionscorr-intersects (regscorr1 regscorr2) ; -> bool
  ;; Check arguments.
  (assert (regionscorr-p regscorr1))
  (assert (regionscorr-p regscorr2))

  (loop for reg1 in (regionscorr-region-list regscorr1)
        for reg2 in (regionscorr-region-list regscorr2) do

    (if (not (region-intersects reg1 reg2))
      (return-from regionscorr-intersects false)) ; Return negative result.
  )
  ;; Return positive result.
  true
)

;;; Return the intersection of two regionscorr, or nil.
(defun regionscorr-intersection (regscorr1 regscorr2) ; -> regionscorr, or nil.
  ;; Check arguments.
  (assert (regionscorr-p regscorr1))
  (assert (regionscorr-p regscorr2))

  (let (regs regx)
    (loop for reg1 in (regionscorr-region-list regscorr1)
          for reg2 in (regionscorr-region-list regscorr2) do

      (setf regx (region-intersection reg1 reg2))
      (if regx
        (push regx regs)
        (return-from regionscorr-intersection nil)) ; Return negative result.
   )
   ;; Construct result.
   (regionscorr-new (regionstore-new (reverse regs)))
  )
)

;;; Return the union of two regionscorr.
(defun regionscorr-union (regscorr1 regscorr2) ; -> regionscorr.
  ;; Check arguments.
  (assert (regionscorr-p regscorr1))
  (assert (regionscorr-p regscorr2))

  (let (regs)
    ;; Aggregate unions of correspnding items.
    (loop for reg1 in (regionscorr-region-list regscorr1)
          for reg2 in (regionscorr-region-list regscorr2) do

       (push (region-union reg1 reg2) regs)
    )
    ;; Construct result.
    (regionscorr-new (regionstore-new (reverse regs)))
  )
)

;;; Return true if two regionscorr are equal.
(defun regionscorr-eq (regscorr1 regscorr2) ; -> bool
  ;; Check arguments.
  (assert (regionscorr-p regscorr1))
  (assert (regionscorr-p regscorr2))

  (loop for reg1 in (regionscorr-region-list regscorr1)
        for reg2 in (regionscorr-region-list regscorr2) do
    (if (not (region-eq reg1 reg2))
      (return-from regionscorr-eq false)) ; Return negative result.
  )
  ;; Return positive result.
  true
)

;;; Return true if two regionscorr are not equal.
(defun regionscorr-ne (regscorr1 regscorr2) ; -> bool
  ;; Check arguments.
  (assert (regionscorr-p regscorr1))
  (assert (regionscorr-p regscorr2))

  ;; Calc result.
  (not (regionscorr-eq regscorr1 regscorr2))
)

;;;; Return true if a regionscorr is a superset of another.
(defun regionscorr-superset-of (&key sub sup) ; -> bool
  ;; Check arguments.
  (assert (regionscorr-p sub))
  (assert (regionscorr-p sup))

  (loop for reg1 in (regionscorr-region-list sup)
        for reg2 in (regionscorr-region-list sub) do
    (if (not (region-superset-of :sup reg1 :sub reg2))
      (return-from regionscorr-superset-of false)) ; Return negative result.
  )
  ;; Return positive result.
  true
)

;;;; Return true if a regionscorr is a superset of a statescorr.
(defun regionscorr-superset-of-states (rcx scx) ; -> bool
  ;; Check arguments.
  (assert (regionscorr-p rcx))
  (assert (statescorr-p scx))

  (loop for regx in (regionscorr-region-list rcx)
        for stax in (statescorr-state-list scx) do
    (if (not (region-superset-of-state regx stax))
      (return-from regionscorr-superset-of-states false)) ; Return negative result.
  )
  ;; Return positive result.
  true
)

;;; Return a list of regionscorr from subtracting two regionscorr.
(defun regionscorr-subtract (&key min sub) ; -> regionscorrstore.
  ;; Check arguments.
  (assert (regionscorr-p min))
  (assert (regionscorr-p sub))

  ;; Check for nothing to subtract.
  (if (not (regionscorr-intersects min sub))
    (return-from regionscorr-subtract (regionscorrstore-new (list min))))

  ;; Check for nothing left after subtraction.
  (if (regionscorr-superset-of :sub min :sup sub)
    (return-from regionscorr-subtract (regionscorrstore-new nil)))

  ;; All corresponding regions intersect.
  (let (ret tmp-regs regs)

    ;; Process each corresponding region pair.
    (loop for regx in (regionscorr-region-list  min)
          for regy in (regionscorr-region-list  sub)
          counting regx into inx do

      ;; Subtract two regions.
      (setf tmp-regs (region-subtract :min-reg regx :sub-reg regy))

      ;; Produce a new regionscorr for each remainder region.
      (loop for regz in (regionstore-regions tmp-regs) do

        (setf regs nil)

        ;; Copy regionscorr except for the remainder.
        (loop for regw in (regionscorr-region-list min)
              counting regw into iny do

          (if (= inx iny)
            (push regz regs)
            (push regw regs)
          )
        )
        ;; Save new regionscorr.
        (push (regionscorr-new (regionstore-new (reverse regs))) ret)
      )
    )
    ;; Construct result.
    (regionscorrstore-new ret)
  )
)

;;; Return true if a list is a list of regionscorr.
;;; An empty list will return true.
(defun regionscorr-list-p (region-list) ; -> bool
  ;; Check argument.
  (if (not (listp region-list))
    (return-from regionscorr-list-p false))

  (loop for regx in region-list do

    ;; Check item type.
    (if (not (regionscorr-p regx))
      (return-from regionscorr-list-p false)) ; Return negative result.
  )
  ;; Return positive result.
  true
)

;;; Return a regionscorr instance, given a list of symbols.
;;; Like (RC (()), (RC (1010)), or (RC (101, 1000)).
(defun regionscorr-from (symbols) ; -> regionscorr instance.
  ;; Check arguments.
  (assert (listp symbols))
  (assert (not (null symbols)))
  (assert (symbolp (car symbols)))
  (assert (eq (car symbols) 'RC))

  ;; Calc result.
  (regionscorr-new (regionstore-from (second symbols)))
)

;;; Return the distance between two regionscorrs.
(defun regionscorr-distance (regscorr1 regscorr2) ; -> integer
  ;; Check arguments.
  (assert (regionscorr-p regscorr1))
  (assert (regionscorr-p regscorr2))

  (let ((cnt 0))
    ;; Add up distance of each corresponding region pair.
    (loop for reg1 in (regionscorr-region-list regscorr1)
          for reg2 in (regionscorr-region-list regscorr2) do

      (setf cnt (+ cnt (region-distance reg1 reg2)))
    )
    ;; Return result.
    cnt
  )
)
