; Implement a store of regions.

(defvar true t)
(defvar false nil)

; Implement a store of regions.
(defstruct regionstore
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
(defun regionstore-new (regions) ; -> regionstore instance.
  ;(format t "~&regions ~A" regions)
  (assert (region-list-p regions))

  (let ((ret (make-regionstore :regions nil)))
    (loop for regx in regions do 
      (if (not (regionstore-contains ret regx))
        (regionstore-push ret regx))
    )
    ret
  )
)

; Push a new region into a regionstore, suppress dups, subsets.
; Return true if the region has been added.
(defun regionstore-push(storex regx) ; -> bool.
  (assert (regionstore-p storex))
  (assert (region-p regx))

  ; Add the new region to the end of the regions list, old survivors migrate to the beginning of the list.
  (if (null (regionstore-regions storex))
    (push regx (regionstore-regions storex))
    (push regx (cdr (last (regionstore-regions storex))))
  )
)

; Return the number of regions in a regionstore.
(defun regionstore-length (storex) ; -> number.
  (assert (regionstore-p storex))

  (length (regionstore-regions storex))
)

; Return true if a regionstore is empty.
(defun regionstore-is-empty (storex) ; -> bool
  (zerop (regionstore-length storex))
)

; Return true if a regionstore is not empty.
(defun regionstore-is-not-empty (storex) ; -> bool
  (plusp (regionstore-length storex))
)

; Return a string representing a regionstore.
(defun regionstore-str (storex) ; -> string.
  (assert (regionstore-p storex))

  (let ((ret "#S(REGIONSTORE ") (start t))

    (loop for regx in (regionstore-regions storex) do
      (if start (setf start nil) (setf ret (concatenate 'string ret ", ")))    

      (setf ret (concatenate 'string ret (region-str regx)))
    )
    ret
  )
)

; Return true if a regionstore contains a given region.
(defun regionstore-contains (storex regx) ; -> bool
  (assert (regionstore-p storex))
  (assert (region-p regx))

  (if (member regx (regionstore-regions storex) :test #'region-eq) true false)
)

(defun regionstore-first-region (storex) ; -> region
  (assert (regionstore-p storex))
  (assert (regionstore-is-not-empty storex))

  (car (regionstore-regions storex))
)


