; Implement a store of regions.

(defvar true t)
(defvar false nil)

; Implement a store of regions.
(defstruct (regionstore (:print-function regionstore-print))
  region-list  ; A list of zero, or more, regions.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (regionstore-<field name> <instance>) -> struct field.
;   (regionstore-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> regionstore
;   (typep <instance> 'regionstore) -> bool
;
; Probably shouldn't use:
;   (make-regionstore [:<field-name> <field-regionstore>]*), use regionstore-new instead.
;   (copy-regionstore <instance>) copies a regionstore instance.

;;; Return a new regionstore instance, from a list of regions.
(defun regionstore-new (regions) ; -> regionstore.
  ;(format t "~&regions ~A" regions)
  (assert (region-list-p regions))

  (make-regionstore :region-list regions)
)

;;; Print a regionstore.
(defun regionstore-print (instance stream depth)
  ;(assert (zerop depth))
  (format stream (regionstore-str instance))
)

;;; Push region into a regionstore.
(defun regionstore-push (storex regx) ; -> nothing, side-effect regionstore is changed.
  (assert (regionstore-p storex))
  (assert (region-p regx))

  (push regx (regionstore-region-list storex))
)

;;; Add region to the end of a regionstore.
(defun regionstore-add-end (storex regx) ; -> nothing, side-effect regionstore changed.
  (assert (regionstore-p storex))
  (assert (region-p regx))

  (setf (regionstore-region-list storex) (append (regionstore-region-list storex) (list regx)))
)

;;; Return a regionstore, suppressing subsets.
;;; Preserve region order.
(defun regionstore-push-nosubs (storex regx) ; -> bool, side-effect regionstore is changed.
  ;(format t "~&regionstore-push-nosubs ~A ~A" storex regx)
  (assert (regionstore-p storex))
  (assert (region-p regx))

  ;; Check for region in store that is a superset (or dup) of the new region.
  (loop for regy in (regionstore-region-list storex) do
    (if (region-superset-of :sup regy :sub regx)
      (return-from regionstore-push-nosubs false))
  )

  ;; Check for regions that are a subset of the new region.
  (let (del-regs)
    ;; Find regions that are a subset of the new region.
    (loop for regy in (regionstore-region-list storex) do
      (if (region-superset-of :sup regx :sub regy)
        (push regy del-regs)
      )
    )
    ;; Remove the subset regions.
    (loop for regy in del-regs do
      (setf (regionstore-region-list storex) (remove regy (regionstore-region-list storex) :test #'region-eq))
    )
  )

  ;; Add the region.
  (regionstore-push storex regx)
  true
)

;;; Return the number of regions in a regionstore.
(defun regionstore-length (storex) ; -> number.
  (assert (regionstore-p storex))

  (length (regionstore-region-list storex))
)

;;; Return true if a regionstore is empty.
(defun regionstore-is-empty (storex) ; -> bool
  (assert (regionstore-p storex))

  (zerop (regionstore-length storex))
)

;;; Return true if a regionstore is not empty.
(defun regionstore-is-not-empty (storex) ; -> bool
  (assert (regionstore-p storex))

  (plusp (regionstore-length storex))
)

;;; Return a string representing a regionstore.
(defun regionstore-str (storex) ; -> string.
  (assert (regionstore-p storex))

  (let ((ret "#S(REGIONSTORE ") (start t))

    (loop for regx in (regionstore-region-list storex) do
      (if start (setf start nil) (setf ret (concatenate 'string ret ", ")))    

      (setf ret (concatenate 'string ret (region-str regx)))
    )
    (if (zerop (regionstore-length storex))
      (setf ret (concatenate 'string ret "NIL)"))
      (setf ret (concatenate 'string ret ")"))
    )
    ret
  )
)

;;; Return true if a regionstore contains a given region.
(defun regionstore-contains (storex regx) ; -> bool
  ;(format t "regionstore-contains storex ~A regx ~A" storex regx)
  (assert (regionstore-p storex))
  (assert (region-p regx))

  (if (member regx (regionstore-region-list storex) :test #'region-eq) true false)
)

;;; Return the first region in a non-empty regionstore.
(defun regionstore-first-region (storex) ; -> region
  (assert (regionstore-p storex))
  (assert (regionstore-is-not-empty storex))

  (car (regionstore-region-list storex))
)

;;; Return the last region in a non-empty regionstore.
(defun regionstore-last-region (storex) ; -> region
  (assert (regionstore-p storex))
  (assert (regionstore-is-not-empty storex))

  (car (last (regionstore-region-list storex)))
)

;;; Return the cdr of a non-empty regionstore.
(defun regionstore-cdr (storex) ; -> regionstore.
  (assert (regionstore-p storex))
  (assert (regionstore-is-not-empty storex))

  (make-regionstore :region-list (cdr (regionstore-region-list storex)))
)

;;; Return a regionstore minus a region.
(defun regionstore-subtract-region (storex regx) ; -> regionstore.
  (assert (regionstore-p storex))
  (assert (region-p regx))

  (let ((ret (regionstore-new nil))
	tmpstore
       )

    (loop for regy in (regionstore-region-list storex) do
        (cond ((region-superset-of :sup regx :sub regy) nil)
	      ((region-intersects regy regx)
	         (setf tmpstore (region-subtract :min-reg regy :sub-reg regx))
		 (loop for regz in (regionstore-region-list tmpstore) do
		   (regionstore-push-nosubs ret regz)
		 )
	       )
	      (t (regionstore-push-nosubs ret regy))
	)
    )
    ret
  )
)

;;; Return true if there is any region in a regionstore that intersects a given region.
(defun regionstore-any-intersection (storex regx) ; -> bool.
  (assert (regionstore-p storex))
  (assert (region-p regx))

  (loop for regy in (regionstore-region-list storex) do
    (if (region-intersects regy regx)
      (return-from regionstore-any-intersection true))
  )
  false 
)

;;; Append two regionstores.
;;; Preserve order.
(defun regionstore-append (store1 store2) ; -> regionstore
  (assert (regionstore-p store1))
  (assert (regionstore-p store2))

  (let ((ret (make-regionstore :region-list (regionstore-region-list store1))))

    ;; Add store2 regions.
    (loop for regx in (regionstore-region-list store2) do
      (regionstore-add-end ret regx)
    )
    ret
  )
)

