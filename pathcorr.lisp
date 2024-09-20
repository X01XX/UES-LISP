; Implement a series of corresponding regions that intersect.

(defvar true t)
(defvar false nil)

; Implement a store of regions.
(defstruct (pathcorr (:print-function pathcorr-print))
  regionscorrstore  ; A store of of zero, or more, regionscorr.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (pathcorr-<field name> <instance>) -> struct field.
;   (pathcorr-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> pathcorr
;   (typep <instance> 'pathcorr) -> bool
;
; Probably shouldn't use:
;   (make-pathcorr [:<field-name> <field-pathcorr>]*), use pathcorr-new instead.
;   (copy-pathcorr <instance>) copies a pathcorr instance.

;;; Return a new pathcorr instance, from a regionscorr.
(defun pathcorr-new (regions) ; -> pathcorr, or nil.
  ;(format t "~&pathcorr-new: regions ~A" regions)
  (assert (regionscorr-list-p regions))

  (let (ret)
    (setf ret (make-pathcorr :regionscorrstore (regionscorrstore-new regions)))
    (assert (pathcorr-is-valid ret))
    ret
  )
)

;;; Return a list of regionscorr.
(defun pathcorr-regionscorr-list (pathcorrx) ; -> A list of regionscorr.
  (regionscorrstore-regionscorr-list (pathcorr-regionscorrstore pathcorrx))
)

;;; Print a pathcorr.
(defun pathcorr-print (instance stream depth)
  ;(assert (zerop depth))
  (format stream (pathcorr-str instance))
)

;;; Push region into a pathcorr.
(defun pathcorr-add-start (pathcorrx regx) ; -> nothing, side-effect pathcorr is changed.
  ;(format t "~&pathcorr-add-start: ~A add-start ~A" pathcorrx regx) ; nothing, side effect changed pathcorr.
  (assert (pathcorr-p pathcorrx))
  (assert (regionscorr-p regx))

  (assert (or (pathcorr-is-empty pathcorrx) (regionscorr-intersects regx (pathcorr-first-region pathcorrx))))

  (regionscorrstore-push (pathcorr-regionscorrstore pathcorrx) regx)
)

;;; Add region to the end of a pathcorr.
(defun pathcorr-add-end (pathcorrx regx) ; -> nothing, side-effect pathcorr changed.
  (assert (pathcorr-p pathcorrx))
  (assert (regionscorr-p regx))

  (assert (or (pathcorr-is-empty pathcorrx) (regionscorr-intersects regx (pathcorr-last-region pathcorrx))))

  (regionscorrstore-add-end (pathcorr-regionscorrstore pathcorrx) regx)
)

;;; Return the number of regions in a pathcorr.
(defun pathcorr-length (pathcorrx) ; -> number.
  (assert (pathcorr-p pathcorrx))

  (regionscorrstore-length (pathcorr-regionscorrstore pathcorrx))
)

;;; Return true if a pathcorr is empty.
(defun pathcorr-is-empty (pathcorrx) ; -> bool
  (assert (pathcorr-p pathcorrx))

  (zerop (pathcorr-length pathcorrx))
)

;;; Return true if a pathcorr is not empty.
(defun pathcorr-is-not-empty (pathcorrx) ; -> bool
  (assert (pathcorr-p pathcorrx))

  (plusp (pathcorr-length pathcorrx))
)

;;; Return a string representing a pathcorr.
(defun pathcorr-str (pathcorrx) ; -> string.
  (assert (pathcorr-p pathcorrx))

  (if  (pathcorr-is-empty pathcorrx)
    (return-from pathcorr-str "#S(pathcorr REGIONS NIL)")
  )

  (let ((ret "#S(pathcorr REGIONSCORR ") (last-reg))

    (loop for regx in (pathcorr-regionscorr-list pathcorrx) do
      (when last-reg
	(if (or (regionscorr-superset-of :sup-regscorr regx :sub-regscorr last-reg)
	        (regionscorr-superset-of :sub-regscorr regx :sup-regscorr last-reg))
	  (setf ret (concatenate 'string ret "-"))
	  (setf ret (concatenate 'string ret (format nil "-~A-" (regionscorr-intersection last-reg regx))))
	)
      )
      (setf last-reg regx)
      (setf ret (concatenate 'string ret (regionscorr-str regx)))
    )
    (setf ret (concatenate 'string ret ")"))
    ret
  )
)

;;; Return true if a pathcorr contains a given region.
(defun pathcorr-contains (pathcorrx regx) ; -> bool
  ;(format t "pathcorr-contains pathcorrx ~A regx ~A" pathcorrx regx)
  (assert (pathcorr-p pathcorrx))
  (assert (regionscorr-p regx))

  (regionscorrstore-contains (pathcorr-regionscorrstore pathcorrx) regx)
)

;;; Return the first regionscorr in a non-empty pathcorr.
(defun pathcorr-first-region (pathcorrx) ; -> regioncorr
  (assert (pathcorr-p pathcorrx))
  (assert (pathcorr-is-not-empty pathcorrx))

  (regionscorrstore-first-region (pathcorr-regionscorrstore pathcorrx))
)

;;; Return the last regionscorr in a non-empty pathcorr.
(defun pathcorr-last-region (pathcorrx) ; -> region
  (assert (pathcorr-p pathcorrx))
  (assert (pathcorr-is-not-empty pathcorrx))

  (regionscorrstore-last-region (pathcorr-regionscorrstore pathcorrx))
)

;;; Return the cdr of a non-empty pathcorr.
(defun pathcorr-cdr (pathcorrx) ; -> pathcorr.
  (assert (pathcorr-p pathcorrx))
  (assert (pathcorr-is-not-empty pathcorrx))

  (make-pathcorr :regionscorrstore (regionscorrstore-cdr (pathcorr-regionscorrstore pathcorrx)))
)

;;; Append two pathcorrs.
;;; Preserve order.
(defun pathcorr-append (store1 store2) ; -> pathcorr
  (assert (pathcorr-p store1))
  (assert (pathcorr-p store2))

  (make-pathcorr :regionscorrstore (regionscorrstore-append (pathcorr-regioncorrstore store1)
                                                            (pathcorr-regioncorrstore store2)))
)

;;; Return true if a pathcorr contains a series of intersecting regions.
(defun pathcorr-is-valid (pathcorrx) ; -> bool
  (assert (pathcorr-p pathcorrx))

  (if (< (pathcorr-length pathcorrx) 2)
    (return-from pathcorr-is-valid true))

  (let ((last-reg (pathcorr-first-region pathcorrx)))

    (loop for regx in (cdr (pathcorr-regionscorr-list pathcorrx)) do
      ;; Each two successive regions must intersect.
      (when (not (regionscorr-intersects regx last-reg))
	(format t "~&~A does not intersect ~A" regx last-reg)
	(return-from pathcorr-is-valid false))
 
      ;; Each two successive regions cannot be the same.
      (when (regionscorr-eq regx last-reg)
	(format t "~&~A eq ~A" regx last-reg)
	(return-from pathcorr-is-valid false))

      (setf last-reg regx)
    )
    true
  )
)

