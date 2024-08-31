;;;; Implement a series of regions that correspond to a particular order.
;;;;
;;;; The regions may use different numbers of bits, depending on their position in a list.

(defvar true t)
(defvar false nil)

; Implement a store of correspeonding regions.
(defstruct (regionscorr (:print-function regionscorr-print))
  regions  ; A regionstore of zero, or more, regions.
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
  ;(format t "~&regions ~A" regions)
  (assert (regionstore-p regions))

  (make-regionscorr :regions regions)
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

  (regionstore-add-end (regionscorr-regions regionscorrx) regx)
)

;;; Return the number of regions in a regionscorr.
(defun regionscorr-length (regionscorrx) ; -> number.
  (assert (regionscorr-p regionscorrx))

  (regionstore-length (regionscorr-regions regionscorrx))
)

;;; Return true if a regionscorr is empty.
(defun regionscorr-is-empty (regionscorrx) ; -> bool
  (assert (regionscorr-p regionscorrx))

  (zerop (regionscorr-length regionscorrx))
)

;;; Return true if a regionscorr is not empty.
(defun regionscorr-is-not-empty (regionscorrx) ; -> bool
  (assert (regionscorr-p regionscorrx))

  (plusp (regionscorr-length regionscorrx))
)

;;; R(regionscorr-new (regionstore-new (list (region-from-str "001X") (region-from-str "X0")))eturn a string representing a regionscorr.
(defun regionscorr-str (regionscorrx) ; -> string.
  (assert (regionscorr-p regionscorrx))

  (let ((ret "#S(regionscorr REGIONS ") last-reg)

    (loop for regx in (regionstore-regions (regionscorr-regions regionscorrx)) do
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

  (regionstore-contains (regionscorr-regions regionscorrx) regx)
)

;;; Return true if two regionscorrs intersect.
(defun regionscorr-intersect (regcorr1 regcorr2) ; -> bool
  (assert (regionscorr-p regcorr1))
  (assert (regionscorr-p regcorr2))
  (assert (= (regionscorr-length regcorr1) (regionscorr-length regcorr2)))

  (loop for reg1 in (regionstore-regions (regionscorr-regions regcorr1))
	for reg2 in (regionstore-regions (regionscorr-regions regcorr2)) do

    (assert (= (region-num-bits reg1) (region-num-bits reg2)))

    (if (not (region-intersects reg1 reg2))
      (return-from regionscorr-intersect false))
  )
  true
)

;;; Return the intersection of two regionscorr, or nil.
(defun regionscorr-intersection (regcorr1 regcorr2) ; -> regionscorr, or nil.
  (assert (regionscorr-p regcorr1))
  (assert (regionscorr-p regcorr2))
  (assert (= (regionscorr-length regcorr1) (regionscorr-length regcorr2)))

  (let ((ret (regionscorr-new (regionstore-new nil))) regx)
    (loop for reg1 in (regionstore-regions (regionscorr-regions regcorr1))
          for reg2 in (regionstore-regions (regionscorr-regions regcorr2)) do
      
      (assert (= (region-num-bits reg1) (region-num-bits reg2)))
      (setf regx (region-intersection reg1 reg2))
      (if regx
        (regionscorr-add-end ret regx)
        (return-from regionscorr-intersection nil))
   )
   ret
  )
)

;;; Return true if two regionscorr are equal.
(defun regionscorr-eq (regcorr1 regcorr2) ; -> bool
  (assert (regionscorr-p regcorr1))
  (assert (regionscorr-p regcorr2))
  (assert (= (regionscorr-length regcorr1) (regionscorr-length regcorr2)))

  (loop for reg1 in (regionstore-regions (regionscorr-regions regcorr1))
        for reg2 in (regionstore-regions (regionscorr-regions regcorr2)) do
    (if (not (region-eq reg1 reg2))
      (return-from regionscorr-eq false))
  )
  true
)

;;;; Return true if a regionscorr is a superset of another.
(defun regionscorr-superset-of (&key sub-regcorr sup-regcorr) ; -> bool
  (assert (regionscorr-p sub-regcorr))
  (assert (regionscorr-p sup-regcorr))
  (assert (= (regionscorr-length sup-regcorr) (regionscorr-length sub-regcorr)))

  (loop for reg1 in (regionstore-regions (regionscorr-regions sup-regcorr))
        for reg2 in (regionstore-regions (regionscorr-regions sub-regcorr)) do
    (if (not (region-superset-of :sup reg1 :sub reg2))
      (return-from regionscorr-superset-of false))
  )
  true
)

;;; Return a list of regionscorr from subtracting two regionscorr.
(defun regionscorr-subtract (&key min-regcorr sub-regcorr) ; -> list of regionscorr.
  (assert (regionscorr-p min-regcorr))
  (assert (regionscorr-p sub-regcorr))
  (assert (= (regionscorr-length min-regcorr) (regionscorr-length sub-regcorr)))

  (if (not (regionscorr-intersect min-regcorr sub-regcorr))
    (return-from regionscorr-subtract (list min-regcorr)))

  (if (regionscorr-superset-of :sub-regcorr min-regcorr :sup-regcorr sub-regcorr)
    (return-from regionscorr-subtract nil))

  (let (ret tmp-regs new-regs)

    (loop for regx in (regionstore-regions (regionscorr-regions min-regcorr))
          for regy in (regionstore-regions (regionscorr-regions sub-regcorr))
	  for inx from 0 below (regionscorr-length min-regcorr) do
	     
      ; Subtract two regions.
      (setf tmp-regs (region-subtract :min-reg regx :sub-reg regy))

      ; Produce a new regionscorr for each remainder region.
      (loop for regz in (regionstore-regions tmp-regs) do

	(setf new-regs (regionscorr-new (regionstore-new nil)))

        (loop for regw in (regionstore-regions (regionscorr-regions min-regcorr))
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
    ret
  )
)


