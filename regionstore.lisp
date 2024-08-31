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

;;; Return a new regionstore instance, from a list of regions.
(defun regionstore-new (regions) ; -> regionstore.
  ;(format t "~&regions ~A" regions)
  (assert (region-list-p regions))

  (make-regionstore :regions regions)
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

  (push regx (regionstore-regions storex))
)

;;; Add region to the end of a regionstore.
(defun regionstore-add-end (storex regx) ; -> nothing, side-effect regionstore changed.
  (assert (regionstore-p storex))
  (assert (region-p regx))

  (setf (regionstore-regions storex) (append (regionstore-regions storex) (list regx)))
)

;;; Return a regionstore, suppressing subsets.
;;; Preserve region order.
(defun regionstore-push-nosubs (storex regx) ; -> bool, side-effect regionstore is changed.
  ;(format t "~&regionstore-push-nosubs ~A ~A" storex regx)
  (assert (regionstore-p storex))
  (assert (region-p regx))

  ;; Check for region in store that is a superset (or dup) of the new region.
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
      (setf (regionstore-regions storex) (remove regy (regionstore-regions storex) :test #'region-eq))
    )
  )

  ;; Add the region.
  (regionstore-push storex regx)
  true
)

;;; Return the number of regions in a regionstore.
(defun regionstore-length (storex) ; -> number.
  (assert (regionstore-p storex))

  (length (regionstore-regions storex))
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

;;; Return the first region in a non-empty regionstore.
(defun regionstore-first-region (storex) ; -> region
  (assert (regionstore-p storex))
  (assert (regionstore-is-not-empty storex))

  (car (regionstore-regions storex))
)

;;; Return the last region in a non-empty regionstore.
(defun regionstore-last-region (storex) ; -> region
  (assert (regionstore-p storex))
  (assert (regionstore-is-not-empty storex))

  (car (last (regionstore-regions storex)))
)

;;; Return the cdr of a non-empty regionstore.
(defun regionstore-cdr (storex) ; -> regionstore.
  (assert (regionstore-p storex))
  (assert (regionstore-is-not-empty storex))

  (make-regionstore :regions (cdr (regionstore-regions storex)))
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
;;; Return true if there is any region in a regionstore that intersects a given region.
(defun regionstore-any-intersection (storex regx) ; -> bool.
  (assert (regionstore-p storex))
  (assert (region-p regx))

  (loop for regy in (regionstore-regions storex) do
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

  (let ((ret (make-regionstore :regions (regionstore-regions store1))))

    ;; Add store2 regions.
    (loop for regx in (regionstore-regions store2) do
      (regionstore-add-end ret regx)
    )
    ret
  )
)

;;; Return true if a given region intersects regions that another region does not intersect.
(defun regionstore-other-intersections (&key store int-reg not-reg) ; -> bool
  (assert (regionstore-p store))
  (assert (region-p int-reg))
  (assert (region-p not-reg))

  (loop for regx in (regionstore-regions store) do
    (if (and (region-intersects regx int-reg) (not (region-intersects regx not-reg)))
      (return-from regionstore-other-intersections true))
  )
  false
)

;;; Find a path of intersecting regions between two given regions.
;;; Return a regionstore of reg1 + series-of-intersecting-regions + reg2.
;;; Failure to find a path returns nil.
;;; The strategy is to keep dividing the problem into two smaller problems.
;;; Later, a path can be calculated from intersection to intersection.
(defun regionstore-find-path (path-options left-reg right-reg) ; -> path, or nil.
  ;(format t "~&regionstore-find-path ~A and ~A" left-reg right-reg)
  (assert (regionstore-p path-options))
  (assert (region-p left-reg))
  (assert (region-p right-reg))
  (assert (not (region-intersects left-reg right-reg)))

  ;; No point without at least one intersectionu of the left region.
  (if (not (regionstore-any-intersection path-options left-reg))
    (return-from regionstore-find-path nil))

  ;; No point without at least one intersection of the right region.
  (if (not (regionstore-any-intersection path-options right-reg))
    (return-from regionstore-find-path nil))

  ;; Regions should not intersect already.
  (if (region-intersects left-reg right-reg)
    (return-from regionstore-find-path nil))

  ;; Try to find a path between the regions.
  (regionstore-find-path2 path-options left-reg right-reg)
)
(defun regionstore-find-path2 (path-options left-reg right-reg) ; -> path, or nil. Probably should not call this function directly.
  ;(format t "~&regionstore-find-path2 ~A and ~A" left-reg right-reg)
  ;(assert (regionstore-p path-options))
  ;(assert (region-p left-reg))
  ;(assert (region-p right-reg))

  ;; Check for the successful end of a search, or sub-search.
  ;; Look for one region that intersects both regions.
  (let (links ; Store of regions that intersect both given regions.
       )
    (loop for regx in (regionstore-regions path-options) do
      (if (and (and (region-neq regx left-reg) (region-intersects regx left-reg))
	       (and (region-neq regx right-reg) (region-intersects regx right-reg)))
	(push regx links)
      )
    )
    (if links
      (return-from regionstore-find-path2 (path-new (regionstore-new (list left-reg (nth (random (length links)) links) right-reg))))
    )
  )

  ;; Look for regions that do not intersect either region.
  ;; These split the search into two smaller searches.
  (let (middle-region	; Region between the two given regions.
        links		; Store of regions between the two given regions.
        left-path	; Path from first given region to the middle-region.
        right-path	; Path from middle-region to the second region.
        (glide-path (region-union left-reg right-reg)) ; Region containing straight-forward paths between regions.
       )

    ;; Gather non-intersecting regions roughly between the two given regions.
    (loop for regx in (regionstore-regions path-options) do
      (if (and (not (region-intersects regx left-reg)) (not (region-intersects regx right-reg))
	       (region-intersects regx glide-path))
	(push regx links)
      )
    )
    (when links
      ;; Choose a region to split the problem in two.
      (setf middle-region (nth (random (length links)) links))
 
      (setf left-path (regionstore-find-path2 path-options left-reg middle-region))
      (if (null left-path)
        (return-from regionstore-find-path2 nil))

      (setf right-path (regionstore-find-path2 path-options middle-region right-reg))
      (if (null right-path)
        (return-from regionstore-find-path2 nil))

      (if (region-eq (regionstore-last-region left-path) (regionstore-first-region right-path))
        (return-from regionstore-find-path2 (path-append left-path (regionstore-cdr right-path))))

      (return-from regionstore-find-path2 (path-append left-path right-path))
    )
  )

  ;; Look for regions that intersect the left, or right, region.
  (let (next-region	; A region that intersects one of the given regions.
        links		; Store of next-regions.
        left-path	; Path from left region to the next-region.
        right-path	; Path from next-region to the right region.
       )

    (loop for regx in (regionstore-regions path-options) do

      ;; Find regions that intersect the left region, and at least one other region.
      (when (and (region-neq regx left-reg) (region-intersects regx left-reg))
	;; Check if the region intersects any other region, that left-reg does not intersect.
	(if (regionstore-other-intersections :store path-options :int-reg regx :not-reg left-reg)
	  (push regx links)
	)
      )
      ;; Find regions that intersect the right region, and at least one other region.
      (when (and (region-neq regx right-reg) (region-intersects regx right-reg))
	;; Check if the region intersects any other region, that left-reg does not intersect.
	(if (regionstore-other-intersections :store path-options :int-reg regx :not-reg right-reg)
	  (push regx links)
	)	
      )
    )
    (when links
      ;; Choose a region to continue with.
      (setf next-region (nth (random (length links)) links))

      ;; Process a region that intersects the left region.
      (when (region-intersects next-region left-reg)
        (setf right-path (regionstore-find-path2 path-options next-region right-reg))
        (if (path-is-empty right-path)
          (return-from regionstore-find-path2 right-path))

        (path-add-start right-path left-reg)
        (return-from regionstore-find-path2 right-path)
      )
      ;; Process a region that intersects the right region.
      (when (region-intersects next-region right-reg)
        (setf left-path (regionstore-find-path2 path-options left-reg next-region))
        (if (path-is-empty left-path)
          (return-from regionstore-find-path2 left-path))

        (path-add-end left-path right-reg)
        (return-from regionstore-find-path2 left-path)
      )
    )
  ) 
  ;; Default return.
  nil
)

