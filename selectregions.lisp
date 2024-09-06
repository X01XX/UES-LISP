;;;; Implement the selectregions struct and functions.

(defvar true t)
(defvar false nil)

;;; The selectregions struct.
(defstruct (selectregions (:print-function selectregions-print))
  regionscorr   ; A store of correspondung regions.
  positive      ; A number, zero or positive.
  negative      ; A number, zero or negative.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (selectregions-<field name> <instance>) -> struct field.
;   (selectregions-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> selectregions
;   (typep <instance> 'selectregions) -> bool
;
; Probably shouldn't use:
;   (make-selectregions [:<field-name> <field-selectregions>]*), use selectregions-new instead.
;   (copy-selectregions <instance>) copies a selectregions instance.

;;; Return a new selectregions, made up of corresponding regions and a value.
(defun selectregions-new (regions pos neg) ; -> selectregions.
  (assert (regionscorr-p regions))
  (assert (> (regionscorr-length regions) 0))

  (make-selectregions :regionscorr regions :positive pos :negative neg)
)

;;; Return a list of regions.
(defun selectregions-region-list (sregsx) ; -> A list ogf regions.
  (regionscorr-region-list (selectregions-regionscorr sregsx))
)

;;; Return a string for a selectregions.
(defun selectregions-str (sregsx)  ; -> string.
  (assert (selectregions-p sregsx))

  (let ((ret "#S(SELECTREGIONS "))
    (setf ret (concatenate 'string ret (format nil "~A" (selectregions-regionscorr sregsx))))
    (setf ret (concatenate 'string ret " POSITIVE "))
    (setf ret (concatenate 'string ret (format nil "~D" (selectregions-positive sregsx))))
    (setf ret (concatenate 'string ret " NEGATIVE "))
    (setf ret (concatenate 'string ret (format nil "~D" (selectregions-negative sregsx))))
    (setf ret (concatenate 'string ret ")"))
    ret
  )
)

;;; Print a selectregions.
(defun selectregions-print (instance stream depth)
  ;(assert (zerop depth))
  (format stream (selectregions-str instance))
)

;;; Return true if two selectregionss are equal.
(defun selectregions-eq (sregs1 sregs2) ; -> bool
  ;(format t "~&selectregions-eq: ~A ? ~A" sregs1 sregs2)
  (assert (selectregions-p sregs1))
  (assert (selectregions-p sregs2))

  (and (regionscorr-eq (selectregions-regionscorr sregs1) (selectregions-regionscorr sregs1))
    (= (selectregions-positive sregs1) (selectregions-positive sregs2))
    (= (selectregions-negative sregs1) (selectregions-negative sregs2)))))
)

;;; Return true if two selectregionss are not equal.
(defun selectregions-neq (sregs1 sregs2) ; -> bool
  (assert (selectregions-p sregs1))
  (assert (selectregions-p sregs2))

  (not (selectregions-eq sregs1 sregs2))
)

;;; Return true if a list is a list of selectregionss.
;;; An empty list will return true.
(defun selectregions-list-p (sregslst) ; -> bool
  ;(format t "~&selectregions-list-p: ~A" sregslst)
  (if (not (listp sregslst))
    (return-from selectregions-list-p false))

  (let (last-item)
    (loop for sregsx in sregslst do
      (if (not (selectregions-p sregsx))
        (return-from selectregions-list-p false))

      (if last-item
	(if (not (regionscorr-congruent (selectregions-regionscorr sregsx) (selectregions-regionscorr last-item)))
	  (return-from selectregions-list-p false))
        (setf last-item sregsx)
      )
    )
  )
  true
)

;;; Return the number of regions in seleectregions-regionscorr.
(defun selectregions-length (sregsx) ; -> a number gt 0.
  (assert (selectregions-p sregsx))

  (regionscorr-length (selectregions-regionscorr sregsx))
)
