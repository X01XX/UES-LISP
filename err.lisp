; Implement an error struct.
(defstruct err
  message  ; Text message.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (err-<field name> <instance>) -> struct field.
;   (err-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> err
;   (typep <instance> 'err) -> bool
;
; Probably shouldn't use:
;   (make-err [:<field-name> <field-err>]*), use err-new instead.
;   (copy-err <instance>) copies a err instance.
(defun err-new (msg) ; -> err instance.
  (assert (stringp msg))
  (assert (> (length msg) 0))

  (make-err :message msg)
)

; Return the err message string.
(defun err-str (errx) ; -> string.
  (assert (err-p errx))

  (err-message errx)
)
