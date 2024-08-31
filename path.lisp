; Implement a series of regions that intersect.

(defvar true t)
(defvar false nil)

; Implement a store of regions.
(defstruct (path (:print-function path-print))
  regions  ; A regionstore of zero, or more, regions.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (path-<field name> <instance>) -> struct field.
;   (path-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> path
;   (typep <instance> 'path) -> bool
;
; Probably shouldn't use:
;   (make-path [:<field-name> <field-path>]*), use path-new instead.
;   (copy-path <instance>) copies a path instance.

;;; Return a new path instance, from a list of regions.
(defun path-new (regions) ; -> path, or nil.
  ;(format t "~&regions ~A" regions)
  (assert (regionstore-p regions))

  (let (ret)
    (setf ret (make-path :regions regions))
    (assert (path-is-valid ret))
    ret
  )
)

;;; Print a path.
(defun path-print (instance stream depth)
  ;(assert (zerop depth))
  (format stream (path-str instance))
)

;;; Push region into a path.
(defun path-add-start (pathx regx) ; -> nothing, side-effect path is changed.
  (assert (path-p pathx))
  (assert (region-p regx))

  (assert (or (path-is-empty pathx) (region-intersects regx (path-first-region pathx))))

  (regionstore-push (path-regions pathx) regx)
)

;;; Add region to the end of a path.
(defun path-add-end (pathx regx) ; -> nothing, side-effect path changed.
  (assert (path-p pathx))
  (assert (region-p regx))

  (assert (or (path-is-empty pathx) (region-intersects regx (path-last-region pathx))))

  (regionstore-add-end (path-regions pathx) regx)
)

;;; Return the number of regions in a path.
(defun path-length (pathx) ; -> number.
  (assert (path-p pathx))

  (regionstore-length (path-regions pathx))
)

;;; Return true if a path is empty.
(defun path-is-empty (pathx) ; -> bool
  (assert (path-p pathx))

  (zerop (path-length pathx))
)

;;; Return true if a path is not empty.
(defun path-is-not-empty (pathx) ; -> bool
  (assert (path-p pathx))

  (plusp (path-length pathx))
)

;;; Return a string representing a path.
(defun path-str (pathx) ; -> string.
  (assert (path-p pathx))

  (if (regionstore-is-empty (path-regions pathx))
    (return-from path-str "#S(PATH REGIONS NIL)")
  )

  (let ((ret "#S(PATH REGIONS ") (last-reg))

    (loop for regx in (regionstore-regions (path-regions pathx)) do
      (when last-reg
	(if (or (region-superset-of :sup regx :sub last-reg)
	        (region-superset-of :sub regx :sup last-reg))
	  (setf ret (concatenate 'string ret "-"))
	  (setf ret (concatenate 'string ret (format nil "-~A-" (region-intersection last-reg regx))))
	)
      )
      (setf last-reg regx)
      (setf ret (concatenate 'string ret (region-str regx)))
    )
    (setf ret (concatenate 'string ret ")"))
    ret
  )
)

;;; Return true if a path contains a given region.
(defun path-contains (pathx regx) ; -> bool
  ;(format t "path-contains pathx ~A regx ~A" pathx regx)
  (assert (path-p pathx))
  (assert (region-p regx))

  (regionstore-contains (path-regions pathx) regx)
)

;;; Return the first region in a non-empty path.
(defun path-first-region (pathx) ; -> region
  (assert (path-p pathx))
  (assert (path-is-not-empty pathx))

  (regionstore-first-region (path-regions pathx))
)

;;; Return the last region in a non-empty path.
(defun path-last-region (pathx) ; -> region
  (assert (path-p pathx))
  (assert (path-is-not-empty pathx))

  (regionstore-last-region (path-regions pathx))
)

;;; Return the cdr of a non-empty path.
(defun path-cdr (pathx) ; -> path.
  (assert (path-p pathx))
  (assert (path-is-not-empty pathx))

  (make-path :regions (regionstore-cdr (path-regions pathx)))
)

;;; Append two paths.
;;; Preserve order.
(defun path-append (store1 store2) ; -> path
  (assert (path-p store1))
  (assert (path-p store2))

  (let ((ret (make-path :regions (path-regions store1))))

    ;; Add store2 regions.
    (loop for regx in (path-regions store2) do
      (regionstore-add-end (path-regions ret) regx)
    )
    ret
  )
)

;;; Return true if a path contains a series of intersecting regions.
(defun path-is-valid (pathx) ; -> bool
  (assert (path-p pathx))

  (if (< (path-length pathx) 2)
    (return-from path-is-valid true))

  (let ((last-reg (regionstore-first-region (path-regions pathx))))

    (loop for regx in (regionstore-regions (regionstore-cdr (path-regions pathx))) do
      ;; Each two successive regions must intersect.
      (when (not (region-intersects regx last-reg))
	(format t "~&~A does not intersect ~A" regx last-reg)
	(return-from path-is-valid false))
 
      ;; Each two successive regions cannot be the same.
      (when (region-eq regx last-reg)
	(format t "~&~A eq ~A" regx last-reg)
	(return-from path-is-valid false))

      (setf last-reg regx)
    )
    true
  )
)

