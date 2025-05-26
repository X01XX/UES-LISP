;;;; Implement a store of defining instances.

(defstruct definingstore
  defining  ; A list of zero, or more, defining struct.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (definingstore-<field name> <instance>) -> struct field.
;   (definingstore-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> definingstore
;   (typep <instance> 'definingstore) -> bool
;
; Probably shouldn't use:
;   (make-definingstore [:<field-name> <field-definingstore>]*), use definingstore-new instead.
;   (copy-definingstore <instance>) copies a definingstore instance.

;;; Return a new definingstore instance, from a list of definings.
(defun definingstore-new (definings) ; -> definingstore.
  ;; Check argument.
  (loop for dfx in definings do
    (assert (defining-p dfx))
  )

  ;; Construct result.
  (make-definingstore :defining definings)
)

;;; Return the number of defining instances in a definingstore.
(defun definingstore-length (storex) ; -> number.
  ;; Check argument.
  (assert (definingstore-p storex))

  (length (definingstore-defining storex))
)

;;; Return true if a definingstore is empty.
(defun definingstore-is-empty (storex) ; -> bool
  ;; Check argument.
  (assert (definingstore-p storex))

  ;; Calc result.
  (zerop (definingstore-length storex))
)

;;; Return true if a definingstore is not empty.
(defun definingstore-is-not-empty (storex) ; -> bool
  ;; Check argument.
  (assert (definingstore-p storex))

  ;; Calc result.
  (plusp (definingstore-length storex))
)

;;; Return a string representing a definingstore.
(defun definingstore-str (storex) ; -> string.
  ;; Check argument.
  (assert (definingstore-p storex))

  ;; Construct result.
  (let ((ret "(") (start t))

    (loop for dfnx in (definingstore-defining storex) do
      (if start (setf start nil) (setf ret (concatenate 'string ret " ")))

      (setf ret (concatenate 'string ret (defining-str dfnx)))
    )
    (setf ret (concatenate 'string ret ")"))

    ;; Return result.
    ret
  )
)

;;; Push a defining instance into a definingstore.
(defun definingstore-push (storex defx) ; -> nothing, side-effect definingstore is changed.
  ;; Check arguments.
  (assert (definingstore-p storex))
  (assert (defining-p defx))

  ;; Add defining.
  (push defx (definingstore-defining storex))
)

;; Return true if a state is needed 
(defun defingstore-state-needed (storex stax) ; -> bool
  ;; Check arguments.
  (assert (definingstore-p storex))
  (assert (state-p stax))

  ;; Check each defining instatnce.
  (loop for dfnx in (definingstore-defining storex) do
    (if (defining-state-needed dfnx stax)
      (return-from defingstore-state-needed true)) ; Return positive result.
  )
  ;; Return negative result.
  false
)

;; Return all regions for a definingstore.
(defun definingstore-regions (storex) ; -> regionstore.
  ;; Check argument.
  (assert (definingstore-p storex))

  (let ((ret (regionstore-new nil)))

    (loop for dfnx in (definingstore-defining storex) do
      (regionstore-push ret (defining-region dfnx))
    )

    ;; Return result.
    ret
  )
)
