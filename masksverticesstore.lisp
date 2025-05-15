;;; Implement a store of masksvertices.                                                                                          

; Implement a store of masksvertices.
(defstruct masksverticesstore
  masksvertices  ; A list of zero, or more, masksvertices. masksvertices-masks is a key.
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
    (loop for mskvtcx in listx do
      (assert (masksvertices-p mskvtcx))
    )

    ;; Construct results.
    (make-masksverticesstore :masksvertices listx)
  )
)

;;; Push masksvertices into a masksverticesstore.
(defun masksverticesstore-push (storex mskvirx) ; -> nothing, side-effect masksverticesstore is changed.
  ;; Check arguments.
  (assert (masksverticesstore-p storex))
  (assert (masksvertices-p mskvirx))

  ;; Add masksvertices.
  (push mskvirx (masksverticesstore-masksvertices storex))
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

;;; Remove a masksvertices with matching masks.
(defun masksverticesstore-remove (storex masks) ; -> masksverticesstore.
  ;; Check arguments.
  (assert (masksverticesstore-p storex))
  (assert (maskstore-p masks))

  (let ((ret (masksverticesstore-new nil)))
    (loop for mskviry in (masksverticesstore-masksvertices storex) do
      (if (not (maskstore-eq masks (masksvertices-masks mskviry)))
        (masksverticesstore-push ret mskviry))
    )
    ;; Return result.
    ret
  )
)

;;; Add a masksvertices to a masksverticesstore, if there are no masksvertices that are masks-superset.
;;; Delete masks-subsets of new masksvertices.
(defun masksverticesstore-push-nosubs (storex mskvirx) ; -> bool, true if masksverticesstore is changed.
  ;; Check arguments.
  (assert (masksverticesstore-p storex))
  (assert (masksvertices-p mskvirx))

  ;; Check for masksvertices in store that is a masks-superset (or dup) of the new region.
  (loop for mskviry in (masksverticesstore-masksvertices storex) do
    (if (maskstore-superset-of :sup (masksvertices-masks mskviry) :sub (masksvertices-masks mskvirx))
      (return-from masksverticesstore-push-nosubs false)) ;; Return negative result.
  )

  ;; Check for masksvertices that are a subset of the new masksvertices.
  (let (del-items)
    ;; Find masksvertices that are a subset of the new masksvertices.
    (loop for mskviry in (masksverticesstore-masksvertices storex) do
      (if (maskstore-superset-of :sup (masksvertices-masks mskvirx) :sub (masksvertices-masks mskviry))
        (push mskviry del-items)
      )
    )
    ;; Remove the subset masksvertices.
    (loop for mskviry in del-items do
      (setf (masksverticesstore-masksvertices storex)
        (remove mskviry (masksverticesstore-masksvertices storex) :test #'masksvertices-eq))
    )

    ;; Add the masksvertices.
    (masksverticesstore-push storex mskvirx)
    ;; Return positive result.
    true
  )
)

;;; Return vertices that contain a given maskstore.
(defun masksverticesstore-find (storex mskstx) ; -> masksvertices, or nil.
  ;; Check arguments.
  (assert (masksverticesstore-p storex))
  (assert (maskstore-p mskstx))

  ;; Check each vertex.
  (loop for mskvirx in (masksverticesstore-masksvertices storex) do
  
    (if (maskstore-eq (masksvertices-masks mskvirx) mskstx)
      (return-from masksverticesstore-find mskvirx)) ; Return positive result.
  )
  ;; Return negative result.
  nil 
)

;;; Return a string representing a masksverticesstore.
(defun masksverticesstore-str (storex) ; -> string.
  ;; Check argument.
  (assert (masksverticesstore-p storex))

  (let ((ret "(") (start t)) 
    (loop for mvx in (masksverticesstore-masksvertices storex) do
      (if start (setf start nil) (setf ret (concatenate 'string ret " ")))

      (setf ret (concatenate 'string ret (masksvertices-str mvx)))
    )   
    (setf ret (concatenate 'string ret ")"))

    ;; Return result.
    ret 
  )
)

