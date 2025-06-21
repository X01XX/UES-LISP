;;;; Implement the RCSVal struct.
;;;; A value and associated RegionCorrs.

(defstruct rcsval
  value     ; A value LE 0.
  regions   ; A RegionsCorrStore, of one, or more, RegionsCorr.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (rcsval-<field name> <instance>) -> struct field.
;   (rcsval-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> rcsval
;   (typep <instance> 'rcsval) -> bool
;
; Probably shouldn't use:
;   (make-rcsval [:<field-name> <field-rcsval>]*), use rcsval-new instead.
;   (copy-rcsval <instance>) copies a rcsval instance.

;;; Return a new rcsval, made up of corresponding regions and a value.
(defun rcsval-new (regions value) ; -> rcsval.
  (assert (regionscorrstore-p regions))
  (assert (integerp value))

  (make-rcsval :regions regions :value value)
)

;;; Return a string representation for a rcsval instance.
(defun rcsval-str (rcsvalx) ; -> String.
  (format nil "(~D ~A)" (rcsval-value rcsvalx) (regionscorrstore-str (rcsval-regions rcsvalx)))
)
