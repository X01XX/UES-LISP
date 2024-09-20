; Implement a store of selectregionss.

(defvar true t)
(defvar false nil)

; Implement a store of selectregionss.
(defstruct selectregionsstore
  selectregions-list  ; A list of zero, or more, non-duplicate, same number bits, selectregionss.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (selectregionsstore-<field name> <instance>) -> struct field.
;   (selectregionsstore-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> selectregionsstore
;   (typep <instance> 'selectregionsstore) -> bool
;
; Probably shouldn't use:
;   (make-selectregionsstore [:<field-name> <field-selectregionsstore>]*), use selectregionsstore-new instead.
;   (copy-selectregionsstore <instance>) copies a selectregionsstore instance.

;;; Return a new selectregionsstore instance.
(defun selectregionsstore-new (selectregionss) ; -> selectregionsstore.
  ;(format t "~&selectregionsstore-new ~A" selectregionss)
  (assert (selectregions-list-p selectregionss))

  (make-selectregionsstore :selectregions-list selectregionss)
)

; Push a new selectregions into a selectregionsstore.
(defun selectregionsstore-push (storex selectregionsx) ; -> nothing. Side-effect selectregionsstore is changed.
  (assert (selectregionsstore-p storex))
  (assert (selectregions-p selectregionsx))

  (push selectregionsx (selectregionsstore-selectregions-list storex))
)

; Return the number of selectregionss in a selectregionsstore.
(defun selectregionsstore-length (storex) ; -> number.
  (assert (selectregionsstore-p storex))

  (length (selectregionsstore-selectregions-list storex))
)

; Return true if a selectregionsstore is empty.
(defun selectregionsstore-is-empty (storex) ; -> bool
  (zerop (selectregionsstore-length storex))
)

; Return true if a selectregionsstore is not empty.
(defun selectregionsstore-is-not-empty (storex) ; -> bool
  (plusp (selectregionsstore-length storex))
)

; Return a string representing a selectregionsstore.
(defun selectregionsstore-str (storex) ; -> string.
  (assert (selectregionsstore-p storex))

  (let ((ret "#S(SELECTREGIONSSTORE ") (start t))

    (loop for sregsx in (selectregionsstore-selectregions-list storex) do
      (if start (setf start nil) (setf ret (concatenate 'string ret ", ")))    

      (setf ret (concatenate 'string ret (selectregions-str sregsx)))
    )

    ret
  )
)

; Return true if a selectregionsstore contains a given selectregions.
(defun selectregionsstore-contains (storex sregsx) ; -> bool
  (assert (selectregionsstore-p storex))
  (assert (selectregions-p sregsx))

  (if (member sregsx (selectregionsstore-selectregions-list storex) :test #'selectregions-eq) true false)
)

(defun selectregionsstore-first-selectregions (storex) ; -> selectregions
  (assert (selectregionsstore-p storex))
  (assert (selectregionsstore-is-not-empty storex))

  (car (selectregionsstore-selectregions-list storex))
)

;;; Return the positive and negative values for a regionscorr,
;;; relative to a selectregios in a given selectregionsstore.
;;; The given regionscorr must be a subset of every selectregions that it intersects.
(defun selectregionsstore-values (storex regionscorrx) ; -> list of positive value, negative value.
  (assert (selectregionsstore-p storex))
  (assert (regionscorr-p regionscorrx))

  (let ((pos 0) (neg 0))

    (loop for selectregionsx in (selectregionsstore-selectregions-list storex) do
      (when (regionscorr-intersects regionscorrx (selectregions-regionscorr selectregionsx))
	(if (regionscorr-superset-of :sup-regscorr (selectregions-regionscorr selectregionsx) :sub-regscorr regionscorrx)
	  (setf pos (+ pos (selectregions-positive selectregionsx))
	        neg (+ neg (selectregions-negative selectregionsx))
	  )
	  (error "selectregionsstore-values: intersecting, non-superset found"))
      )
    )
    ;; Return list of values.
    (list pos neg)
  )
)
