;;; Implement a store of masksvertices.                                                                                          

; Implement a store of masksvertices.
(defstruct masksverticesstore
  masksvertices  ; A list of zero, or more, masksvertices.
)
; Automatically created by defstruct:
;
; Most used:
;   (masksverticesstore-<field name> <instance>) -> struct field.
;   (masksverticesstore-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> masksverticesstore
;   (typep <instance> 'masksverticesstore) -> bool
;
; Probably shouldn't use:
;   (make-masksverticesstore [:<field-name> <field-masksverticesstore>]*), use masksverticesstore-new instead.
;   (copy-masksverticesstore <instance>) copies a masksverticesstore instance.

;;; Return a new masksverticesstore instance, from a list of masksverticess.
(defun masksverticesstore-new (&rest masksvertices) ; -> masksverticesstore.
  (let (listx)
   ;; Check argument, convert list of list to list.
    (if (listp (car masksvertices))
      (setf listx (car masksvertices))
      (setf listx masksvertices))

    ;; Check each item type.
    (loop for regx in listx do
      (assert (masksvertices-p regx))
    )   

    ;; Construct results.
    (make-masksverticesstore :masksvertices listx)
  )
)

;;; Push masksvertices into a masksverticesstore.
(defun masksverticesstore-push (storex regx) ; -> nothing, side-effect masksverticesstore is changed.
  ;; Check arguments.
  (assert (masksverticesstore-p storex))
  (assert (masksvertices-p regx))

  ;; Add masksvertices.
  (push regx (masksverticesstore-masksvertices storex))
)

;;; Return the number of masksvertices in a masksverticesstore.
(defun masksverticesstore-length (storex) ; -> number.
  ;; Check argument.
  (assert (masksverticesstore-p storex))

  ;; Calc result.
  (length (masksverticesstore-masksvertices storex))
)

;;; Return true if a masksverticesstore is empty.
(defun masksverticesstore-is-empty (storex) ; -> bool
  ;; Check argument.
  (assert (masksverticesstore-p storex))

  ;; Calc result.
  (zerop (masksverticesstore-length storex))
)

;;; Return true if a masksverticesstore is not empty.
(defun masksverticesstore-is-not-empty (storex) ; -> bool
  ;; Check argument.
  (assert (masksverticesstore-p storex))

  ;; Calc result.
  (plusp (masksverticesstore-length storex))
)

