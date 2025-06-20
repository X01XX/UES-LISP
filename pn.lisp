;;;; Implement a Pattern Number struct
;;;;
;;;; This is implemented as a struct to support serialization.

(defstruct pn
  value
)

; Functions automatically created by defstruct:
;
; Most used:
;   (pn-<field name> <instance>) returns struct field.
;   (pn-p <instance>) -> t
;
; Least used:
;   (type-of <instance>) -> pn
;   (typep <instance> 'pn) -> t
;
; Don't use:
;   (make-pn [:<field-name> <field-value>]*).
;   (copy-pn <instance>) copies a pn instance.

(defvar *pn-one*  1)
(defvar *pn-two*  2)
(defvar *pn-none* 3)

(defun pn-new (val) ; -> pn
  ;; Check argument.
  (assert (integerp val))
  (assert (and (> val 0) (< val 4)))

  ;; Return result.
  (make-pn :value val)
)

(defun pn-str (pnx)
  (assert (pn-p pnx))

  (if (pn-eq pnx *pn-one*) "One"
      (if (pn-eq pnx *pn-two*) "Two"
 	 (if (pn-eq pnx *pn-none*) "None")))
)

(defun pn-gt (pnx pny)
  (assert (pn-p pnx))
  (assert (or (pn-p pny) (integerp pny)))

  (if (pn-p pny)
    (> (pn-value pnx) (pn-value pny))
    (> (pn-value pnx) pny))
)

(defun pn-lt (pnx pny)
  (assert (pn-p pnx))
  (assert (or (pn-p pny) (integerp pny)))

  (if (pn-p pny)
    (< (pn-value pnx) (pn-value pny))
    (< (pn-value pnx) pny))
)

(defun pn-eq (pnx pny)
  (assert (pn-p pnx))
  (assert (or (pn-p pny) (integerp pny)))

  (if (pn-p pny)
    (= (pn-value pnx) (pn-value pny))
    (= (pn-value pnx) pny))
)

(defun pn-ne (pnx pny)
  (assert (pn-p pnx))
  (assert (or (pn-p pny) (integerp pny)))

  (if (pn-p pny)
    (/= (pn-value pnx) (pn-value pny))
    (/= (pn-value pnx) pny))
)

