;;;; Implement a series of regions that correspond to a particular order.
;;;;
;;;; The regions may use different numbers of bits, depending on their position in a list.
;;;;
;;;; See the Domain concept in the Rust version of the project.

(defvar true t)
(defvar false nil)

; Implement a store of correspeonding regions.
(defstruct (regionscorr (:print-function regionscorr-print))
  regionstore  ; A regionstore of zero, or more, regions.
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

;;; Return a new regionscorr instance, from a regionstore.
(defun regionscorr-new (regions) ; -> regionscorr, or nil.
  ;(format t "~&regionscorr-new: regions ~A" regions)
  (assert (region-list-p regions))

  (make-regionscorr :regionstore (regionstore-new regions))
)

;;; Retirn a list of regions from a regionscorr.
(defun regionscorr-region-list (regionscorrx) ; -> list of regions.
  (assert (regionscorr-p regionscorrx))

  (regionstore-region-list (regionscorr-regionstore regionscorrx))
)

;;; Print a regionscorr.
(defun regionscorr-print (instance stream depth)
  ;(assert (zerop depth))
  (format stream (regionscorr-str instance))
)

;;; Add region to the end of a regionscorr.
(defun regionscorr-add-end (regionscorrx regx) ; -> nothing, side-effect regionscorr changed.
  (assert (regionscorr-p regionscorrx))
  (assert (region-p regx))

  (regionstore-add-end (regionscorr-regionstore regionscorrx) regx)
)

;;; Return the number of regions in a regionscorr.
(defun regionscorr-length (regionscorrx) ; -> number.
  ;(format t "~&regionscorr-length: ~A" (type-of regionscorrx))
  (assert (regionscorr-p regionscorrx))

  (regionstore-length (regionscorr-regionstore regionscorrx))
)

;;; Return true if a regionscorr is empty.
(defun regionscorr-is-empty (regionscorrx) ; -> bool
  ;(format t "~&regionscorr-is-empty: arg ~A" (type-of regionscorrx))
  (assert (regionscorr-p regionscorrx))

  (zerop (regionscorr-length regionscorrx))
)

;;; Return true if a regionscorr is not empty.
(defun regionscorr-is-not-empty (regionscorrx) ; -> bool
  (assert (regionscorr-p regionscorrx))

  (plusp (regionscorr-length regionscorrx))
)

;;; Return a string representing a regionscorr.
(defun regionscorr-str (regionscorrx) ; -> string.
  (assert (regionscorr-p regionscorrx))

  (if (regionscorr-is-empty regionscorrx)
    (return-from regionscorr-str "#S(REGIONSCORR REGIONS NIL"))

  (let ((ret "#S(REGIONSCORR REGIONS ") last-reg)

    (loop for regx in (regionscorr-region-list regionscorrx) do
      (if last-reg (setf ret (concatenate 'string ret ", ")))
      (setf last-reg regx)
      (setf ret (concatenate 'string ret (region-str regx)))
    )
    (setf ret (concatenate 'string ret ")"))
    ret
  )
)

;;; Return true if a regionscorr contains a given region.
(defun regionscorr-contains (regionscorrx regx) ; -> bool
  ;(format t "regionscorr-contains regionscorrx ~A regx ~A" regionscorrx regx)
  (assert (regionscorr-p regionscorrx))
  (assert (region-p regx))

  (regionstore-contains (regionscorr-regionstore regionscorrx) regx)
)

;;; Return true if two regionscorrs intersect.
(defun regionscorr-intersects (regcorr1 regcorr2) ; -> bool
  (assert (regionscorr-p regcorr1))
  (assert (regionscorr-p regcorr2))
  (assert (regionscorr-congruent regcorr1 regcorr2))

  (loop for reg1 in (regionscorr-region-list regcorr1)
	for reg2 in (regionscorr-region-list regcorr2) do

    (if (not (region-intersects reg1 reg2))
      (return-from regionscorr-intersects false))
  )
  true
)

;;; Return the intersection of two regionscorr, or nil.
(defun regionscorr-intersection (regcorr1 regcorr2) ; -> regionscorr, or nil.
  (assert (regionscorr-p regcorr1))
  (assert (regionscorr-p regcorr2))
  (assert (regionscorr-congruent regcorr1 regcorr2))

  (let ((ret (regionscorr-new nil)) regx)
    (loop for reg1 in (regionscorr-region-list regcorr1)
          for reg2 in (regionscorr-region-list regcorr2) do
      
      (setf regx (region-intersection reg1 reg2))
      (if regx
        (regionscorr-add-end ret regx)
        (return-from regionscorr-intersection nil))
   )
   ret
  )
)

;;; Return the union of two regionscorr.
(defun regionscorr-union (regcorr1 regcorr2) ; -> regionscorr.
  (assert (regionscorr-p regcorr1))
  (assert (regionscorr-p regcorr2))
  (assert (regionscorr-congruent regcorr1 regcorr2))

  (let ((ret (regionscorr-new nil)))
    (loop for reg1 in (regionscorr-region-list regcorr1)
          for reg2 in (regionscorr-region-list regcorr2) do
      
       (regionscorr-add-end ret (region-union reg1 reg2))
    )
    ret
  )
)

;;; Return true if two regionscorr are equal.
(defun regionscorr-eq (regcorr1 regcorr2) ; -> bool
  ;(format t "~&regionscorr-eq: ~A ~A" regcorr1 regcorr2)
  (assert (regionscorr-p regcorr1))
  (assert (regionscorr-p regcorr2))
  (assert (regionscorr-congruent regcorr1 regcorr2))

  (loop for reg1 in (regionscorr-region-list regcorr1)
        for reg2 in (regionscorr-region-list regcorr2) do
    (if (not (region-eq reg1 reg2))
      (return-from regionscorr-eq false))
  )
  true
)

;;; Return true if two regionscorr are not equal.
(defun regionscorr-neq (regcorr1 regcorr2) ; -> bool
  (assert (regionscorr-p regcorr1))
  (assert (regionscorr-p regcorr2))
  (assert (regionscorr-congruent regcorr1 regcorr2))

  (not (regionscorr-eq regcorr1 regcorr2))
)

;;;; Return true if a regionscorr is a superset of another.
(defun regionscorr-superset-of (&key sub-regcorr sup-regcorr) ; -> bool
  (assert (regionscorr-p sub-regcorr))
  (assert (regionscorr-p sup-regcorr))
  (assert (regionscorr-congruent sup-regcorr sub-regcorr))

  (loop for reg1 in (regionscorr-region-list sup-regcorr)
        for reg2 in (regionscorr-region-list sub-regcorr) do
    (if (not (region-superset-of :sup reg1 :sub reg2))
      (return-from regionscorr-superset-of false))
  )
  true
)

;;; Return a list of regionscorr from subtracting two regionscorr.
(defun regionscorr-subtract (&key min-regcorr sub-regcorr) ; -> regionscorrstore.
  ;(format t "~&regionscorr-subtract: ~A ~A" min-regcorr sub-regcorr)
  (assert (regionscorr-p min-regcorr))
  (assert (regionscorr-p sub-regcorr))
  (assert (= (regionscorr-length min-regcorr) (regionscorr-length sub-regcorr)))

  (if (not (regionscorr-intersects min-regcorr sub-regcorr))
    (return-from regionscorr-subtract (regionscorrstore-new (list min-regcorr))))

  (if (regionscorr-superset-of :sub-regcorr min-regcorr :sup-regcorr sub-regcorr)
    (return-from regionscorr-subtract (regionscorrstore-new nil)))

  (let (ret tmp-regs new-regs)

    (loop for regx in (regionscorr-region-list  min-regcorr)
          for regy in (regionscorr-region-list  sub-regcorr)
	  for inx from 0 below (regionscorr-length min-regcorr) do
	     
      ; Subtract two regions.
      (setf tmp-regs (region-subtract :min-reg regx :sub-reg regy))

      ; Produce a new regionscorr for each remainder region.
      (loop for regz in (regionstore-region-list tmp-regs) do

	(setf new-regs (regionscorr-new nil))

        (loop for regw in (regionscorr-region-list min-regcorr)
	      for iny from 0 below (regionscorr-length min-regcorr) do

	  (if (= inx iny)
	    (regionscorr-add-end new-regs regz)
	    (regionscorr-add-end new-regs regw)
	  )
        )
	;; Save new regionscorr.
	(push new-regs ret)
      )
    )
    (regionscorrstore-new ret)
  )
)

;;; Return true if a list is a list of regionscorr.
;;; An empty list will return true.
(defun regionscorr-list-p (reglst) ; -> bool
  ;; Check arg type.
  (if (not (listp reglst))
    (return-from regionscorr-list-p false))

  (let (first-item)
    (loop for regx in reglst do

      ;; Check item type.
      (if (not (regionscorr-p regx))
        (return-from regionscorr-list-p false))

      ;; Check item characteristics.
      (if first-item
	(if (not (regionscorr-congruent regx first-item))
          (return-from regionscorr-list-p false))
	(setf first-item regx))
    )
    true
  )
)
;;; Return true if two regionscorr have the same length and corresponding region-num-bits values.
(defun regionscorr-congruent (regs1 regs2)
  ;(format t "~&regionscorr-congruent: ~A ~A" regs1 regs2)
  (assert (regionscorr-p regs1))
  (assert (regionscorr-p regs2))

  (if (/= (regionscorr-length regs1) (regionscorr-length regs2))
    (return-from regionscorr-congruent false))

  (loop for reg1 in (regionscorr-region-list regs1)
	for reg2 in (regionscorr-region-list regs2) do

    (if (/= (region-num-bits reg1) (region-num-bits reg2))
      (return-from regionscorr-congruent false))
  )
  true
)

