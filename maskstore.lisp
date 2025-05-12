;;;; Implement a store of masks.
(defstruct maskstore
  masks  ; A list of zero, or more, non-duplicate, same number bits, masks.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (maskstore-<field name> <instance>) -> struct field.
;   (maskstore-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> maskstore
;   (typep <instance> 'maskstore) -> bool
;
; Probably shouldn't use:
;   (make-maskstore [:<field-name> <field-maskstore>]*), use maskstore-new instead.
;   (copy-maskstore <instance>) copies a maskstore instance.

;;; Return a new maskstore instance.
(defun maskstore-new (&rest masks) ; -> maskstore.
  ;; Check argument.
  (let (listx)
    ;; Check argument, convert list of list to list.
    (if (listp (car masks))
      (setf listx (car masks))
      (setf listx masks))

    ;; Check each item type.
    (loop for mskx in listx do
      (assert (mask-p mskx))
    )
    ;; Construct result.
    (make-maskstore :masks listx)
  )
)

;;; Push a new mask into a maskstore.
(defun maskstore-push (storex maskx) ; -> nothing. Side-effect maskstore is changed.
  ;; Check argument.
  (assert (maskstore-p storex))
  (assert (mask-p maskx))

  ;; Add item.
  (push maskx (maskstore-masks storex))
)

;;; Return the number of masks in a maskstore.
(defun maskstore-length (storex) ; -> number.
  ;; Check argument.
  (assert (maskstore-p storex))

  ;; Return result.
  (length (maskstore-masks storex))
)

;;; Return true if a maskstore is empty.
(defun maskstore-is-empty (storex) ; -> bool
  ;; Check argument.
  (assert (maskstore-p storex))

  ;; Return result.
  (zerop (maskstore-length storex))
)

;;; Return true if a maskstore is not empty.
(defun maskstore-is-not-empty (storex) ; -> bool
  ;; Check argument.
  (assert (maskstore-p storex))

  ;; Return result.
  (plusp (maskstore-length storex))
)

;;; Return a string representing a maskstore.
(defun maskstore-str (storex) ; -> string.
  ;; Check argument.
  (assert (maskstore-p storex))

  (let ((ret "(") (start t))

    ;; Add each mask string.
    (loop for mskx in (maskstore-masks storex) do
      (if start (setf start nil) (setf ret (concatenate 'string ret " ")))

      (setf ret (concatenate 'string ret (mask-str mskx)))
    )
    (setf ret (concatenate 'string ret ")"))

    ;; Return result.
    ret
  )
)

;;; Return true if a maskstore contains a given mask.
(defun maskstore-member (storex mskx) ; -> bool
  ;; Check arguments.
  (assert (maskstore-p storex))
  (assert (mask-p mskx))

  ;; Return result.
  (member mskx (maskstore-masks storex) :test #'mask-eq)
)

;;; Return the first mask in a store.
(defun maskstore-first-mask (storex) ; -> mask
  ;; Check arguments.
  (assert (maskstore-p storex))
  (assert (maskstore-is-not-empty storex))

  ;; Return result.
  (car (maskstore-masks storex))
)

;;; Return true if twe maskstores are equal.
;;; Order of masks does not matter.
(defun maskstore-eq (storex storey) ; -> bool
  ;; Check arguments.
  (assert (maskstore-p storex))
  (assert (maskstore-p storey))

  (if (/= (maskstore-length storex) (maskstore-length storey))
    (return-from maskstore-eq false)) ; Return negative result.

  ;; Check each mask in storey.
  (loop for msky in (maskstore-masks storey) do
    (if (not (maskstore-member storex msky))
      (return-from maskstore-eq false)) ; Return negative result.
  )
  ;; Return positive result.
  true
)

;;; Return true if a maskstore is a subset of another.
(defun maskstore-subset-of (&key sub sup) ; -> bool
  ;; Check arguments.
  (assert (maskstore-p sub))
  (assert (maskstore-p sup))

  ;; Check each mask in sub.
  (loop for msky in (maskstore-masks sub) do
    (if (not (maskstore-member sup msky))
      (return-from maskstore-subset-of false)) ; Return negative result.
  )
  ;; Return positive result.
  true
)

