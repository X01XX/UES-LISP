; Implement a store of regions.

(defvar true t)
(defvar false nil)

; Implement a store of regions.
(defstruct (regionstore (:print-function regionstore-print))
  regions  ; A list of zero, or more, regions.
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

;;; Return a new regionstore instance.
(defun regionstore-new (regions) ; -> regionstore.
  ;(format t "~&regions ~A" regions)
  (assert (listp regions))

  (let ((ret (make-regionstore :regions nil)))
    (loop for regx in regions do 
      (assert (region-p regx))
      (if (not (regionstore-contains ret regx))
        (regionstore-push-nosubs ret regx))
    )
    ret
  )
)

;;; Print a regionstore.
(defun regionstore-print (instance stream depth)
  ;(assert (zerop depth))
  (format stream (regionstore-str instance))
)

;;; Push a new region into a regionstore, suppress dups.
;;; Return true if the region has been added.
(defun regionstore-push(storex regx) ; -> bool, true if added.
  (assert (regionstore-p storex))
  (assert (region-p regx))

  (if (regionstore-contains storex regx)
    (return-from regionstore-push false))

  (push regx (regionstore-regions storex)) ; Add new region.
  true
)

;;; Push a new region into a regionstore, suppress dups, subsets.
;;; Return true if the region has been added.
(defun regionstore-push-nosubs (storex regx) ; -> bool, true if added.
  ;(format t "~&regionstore-push-nosubs")
  (assert (regionstore-p storex))
  (assert (region-p regx))

  ;; Add the new region to the end of the regions list, old survivors migrate to the beginning of the list.
  (if (regionstore-contains storex regx)
    (return-from regionstore-push-nosubs false))

  ;; Check for region in store that is a superset of the new region.
  (loop for regy in (regionstore-regions storex) do
    (if (region-superset-of :sup regy :sub regx)
      (return-from regionstore-push-nosubs false))
  )

  ;; Check for regions that are a subset of the new region.
  (let (del-regs)
    ;; Find regions that are a subset of the new region.
    (loop for regy in (regionstore-regions storex) do
      (if (region-superset-of :sup regx :sub regy)
	(push regy del-regs)
      )
    )
    ;; Remove the subset regions.
    (loop for regy in del-regs do
      (remove regy (regionstore-regions storex) :test #'region-eq)
    )
  )
  ;; Add the region.
  (push regx (regionstore-regions storex))
  true
)

;;; Return the number of regions in a regionstore.
(defun regionstore-length (storex) ; -> number.
  (assert (regionstore-p storex))

  (length (regionstore-regions storex))
)

;;; Return true if a regionstore is empty.
(defun regionstore-is-empty (storex) ; -> bool
  (zerop (regionstore-length storex))
)

;;; Return true if a regionstore is not empty.
(defun regionstore-is-not-empty (storex) ; -> bool
  (plusp (regionstore-length storex))
)

;;; Return a string representing a regionstore.
(defun regionstore-str (storex) ; -> string.
  (assert (regionstore-p storex))

  (let ((ret "#S(REGIONSTORE ") (start t))

    (loop for regx in (regionstore-regions storex) do
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

  (if (member regx (regionstore-regions storex) :test #'region-eq) true false)
)

(defun regionstore-first-region (storex) ; -> region
  (assert (regionstore-p storex))
  (assert (regionstore-is-not-empty storex))

  (car (regionstore-regions storex))
)

;;; Return a regionstore minus a region.
(defun regionstore-subtract-region (storex regx) ; -> regionstore.
  (assert (regionstore-p storex))
  (assert (region-p regx))

  (let ((ret (regionstore-new nil))
	tmpstore
       )

    (loop for regy in (regionstore-regions storex) do
        (cond ((region-superset-of :sup regx :sub regy) nil)
	      ((region-intersects regy regx)
	         (setf tmpstore (region-subtract :min-reg regy :sub-reg regx))
		 (loop for regz in (regionstore-regions tmpstore) do
		   (regionstore-push-nosubs ret regz)
		 )
	       )
	      (t (regionstore-push-nosubs ret regy))
	)
    )
    ret
  )
)
