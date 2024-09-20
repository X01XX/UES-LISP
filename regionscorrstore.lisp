; Implement a store of regionscorr instances.

(defvar true t)
(defvar false nil)

; Implement a store of regions.
(defstruct (regionscorrstore (:print-function regionscorrstore-print))
  regionscorr-list  ; A list of zero, or more, regionscorr.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (regionscorrstore-<field name> <instance>) -> struct field.
;   (regionscorrstore-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> regionscorrstore
;   (typep <instance> 'regionscorrstore) -> bool
;
; Probably shouldn't use:
;   (make-regionscorrstore [:<field-name> <field-regionscorrstore>]*), use regionscorrstore-new instead.
;   (copy-regionscorrstore <instance>) copies a regionscorrstore instance.

;;; Return a new regionscorrstore instance, from a list of regions.
(defun regionscorrstore-new (regions) ; -> regionscorrstore.
  ;(format t "~&regions ~A" regions)
  (assert (regionscorr-list-p regions))

  (make-regionscorrstore :regionscorr-list regions)
)

;;; Print a regionscorrstore.
(defun regionscorrstore-print (instance stream depth)
  ;(assert (zerop depth))
  (format stream (regionscorrstore-str instance))
)

;;; Push region into a regionscorrstore.
(defun regionscorrstore-push (storex regx) ; -> nothing, side-effect regionscorrstore is changed.
  (assert (regionscorrstore-p storex))
  (assert (regionscorr-p regx))

  (push regx (regionscorrstore-regionscorr-list storex))
)

;;; Add region to the end of a regionscorrstore.
(defun regionscorrstore-add-end (storex regx) ; -> nothing, side-effect regionscorrstore changed.
  (assert (regionscorrstore-p storex))
  (assert (regionscorr-p regx))

  (setf (regionscorrstore-regionscorr-list storex) (append (regionscorrstore-regionscorr-list storex) (list regx)))
)

;;; Return a regionscorrstore, suppressing subsets.
;;; Preserve region order.
(defun regionscorrstore-push-nosubs (storex regx) ; -> bool, side-effect regionscorrstore is changed.
  ;(format t "~&regionscorrstore-push-nosubs ~A ~A" storex regx)
  (assert (regionscorrstore-p storex))
  (assert (regionscorr-p regx))

  ;; Check for region in store that is a superset (or dup) of the new region.
  (loop for regy in (regionscorrstore-regionscorr-list storex) do
    (if (regionscorr-superset-of :sup-regscorr regy :sub-regscorr regx)
      (return-from regionscorrstore-push-nosubs false))
  )

  ;; Check for regions that are a subset of the new region.
  (let (del-regs)
    ;; Find regions that are a subset of the new region.
    (loop for regy in (regionscorrstore-regionscorr-list storex) do
      (if (regionscorr-superset-of :sup-regscorr regx :sub-regscorr regy)
        (push regy del-regs)
      )
    )
    ;; Remove the subset regions.
    (loop for regy in del-regs do
      (setf (regionscorrstore-regionscorr-list storex) (remove regy (regionscorrstore-regionscorr-list storex) :test #'regionscorr-eq))
    )
  )

  ;; Add the region.
  (regionscorrstore-push storex regx)
  true
)

;;; Return the number of regions in a regionscorrstore.
(defun regionscorrstore-length (storex) ; -> number.
  (assert (regionscorrstore-p storex))

  (length (regionscorrstore-regionscorr-list storex))
)

;;; Return true if a regionscorrstore is empty.
(defun regionscorrstore-is-empty (storex) ; -> bool
  (assert (regionscorrstore-p storex))

  (zerop (regionscorrstore-length storex))
)

;;; Return true if a regionscorrstore is not empty.
(defun regionscorrstore-is-not-empty (storex) ; -> bool
  (assert (regionscorrstore-p storex))

  (plusp (regionscorrstore-length storex))
)

;;; Return a string representing a regionscorrstore.
(defun regionscorrstore-str (storex) ; -> string.
  (assert (regionscorrstore-p storex))

  (let ((ret "#S(REGIONCORRSTORE ") (start t))

    (loop for regx in (regionscorrstore-regionscorr-list storex) do
      (if start (setf start nil) (setf ret (concatenate 'string ret ", ")))    

      (setf ret (concatenate 'string ret (regionscorr-str regx)))
    )
    (if (zerop (regionscorrstore-length storex))
      (setf ret (concatenate 'string ret "NIL)"))
      (setf ret (concatenate 'string ret ")"))
    )
    ret
  )
)

;;; Return true if a regionscorrstore contains a given region.
(defun regionscorrstore-contains (storex regx) ; -> bool
  ;(format t "regionscorrstore-contains storex ~A regx ~A" storex regx)
  (assert (regionscorrstore-p storex))
  (assert (regionscorr-p regx))

  (if (member regx (regionscorrstore-regionscorr-list storex) :test #'regionscorr-eq) true false)
)

;;; Return the first region in a non-empty regionscorrstore.
(defun regionscorrstore-first-region (storex) ; -> region
  (assert (regionscorrstore-p storex))
  (assert (regionscorrstore-is-not-empty storex))

  (car (regionscorrstore-regionscorr-list storex))
)

;;; Return the last region in a non-empty regionscorrstore.
(defun regionscorrstore-last-region (storex) ; -> region
  (assert (regionscorrstore-p storex))
  (assert (regionscorrstore-is-not-empty storex))

  (car (last (regionscorrstore-regionscorr-list storex)))
)

;;; Return the cdr of a non-empty regionscorrstore.
(defun regionscorrstore-cdr (storex) ; -> regionscorrstore.
  (assert (regionscorrstore-p storex))
  (assert (regionscorrstore-is-not-empty storex))

  (make-regionscorrstore :regionscorr-list (cdr (regionscorrstore-regionscorr-list storex)))
)

;;; Return a regionscorrstore minus a region.
(defun regionscorrstore-subtract-regionscorr (storex regx) ; -> regionscorrstore.
  (assert (regionscorrstore-p storex))
  (assert (regionscorr-p regx))

  (let ((ret (regionscorrstore-new nil))
	tmpstore
       )

    (loop for regy in (regionscorrstore-regionscorr-list storex) do
        (cond ((regionscorr-superset-of :sup-regscorr regx :sub-regscorr regy) nil)
	      ((regionscorr-intersects regy regx)
	         (setf tmpstore (regionscorr-subtract :min-regscorr regy :sub-regscorr regx))
		 (loop for regz in (regionscorrstore-regionscorr-list tmpstore) do
		   (regionscorrstore-push-nosubs ret regz)
		 )
	       )
	      (t (regionscorrstore-push-nosubs ret regy))
	)
    )
    ret
  )
)

;;; Return true if there is any region in a regionscorrstore that intersects a given region.
(defun regionscorrstore-any-intersection (storex regx) ; -> bool.
  (assert (regionscorrstore-p storex))
  (assert (regionscorr-p regx))

  (loop for regy in (regionscorrstore-regionscorr-list storex) do
    (if (regionscorr-intersects regy regx)
      (return-from regionscorrstore-any-intersection true))
  )
  false 
)

;;; Append two regionscorrstores.
;;; Preserve order.
(defun regionscorrstore-append (store1 store2) ; -> regionscorrstore
  (assert (regionscorrstore-p store1))
  (assert (regionscorrstore-p store2))

  (let ((ret (make-regionscorrstore :regionscorr-list (regionscorrstore-regionscorr-list store1))))

    ;; Add store2 regions.
    (loop for regx in (regionscorrstore-regionscorr-list store2) do
      (regionscorrstore-add-end ret regx)
    )
    ret
  )
)

;;; Return true if a given region intersects regions that another region does not intersect.
(defun regionscorrstore-other-intersections (&key store int-reg not-reg) ; -> bool
  (assert (regionscorrstore-p store))
  (assert (regionscorr-p int-reg))
  (assert (regionscorr-p not-reg))

  (loop for regx in (regionscorrstore-regionscorr-list store) do
    (if (and (regionscorr-intersects regx int-reg) (not (regionscorr-intersects regx not-reg)))
      (return-from regionscorrstore-other-intersections true))
  )
  false
)

;;; Find a path of intersecting regions between two given regions.
;;; Return a regionscorrstore of reg1 + series-of-intersecting-regions + reg2.
;;; Failure to find a path returns nil.
;;; The strategy is to keep dividing the problem into two smaller problems.
;;; Later, a path can be calculated from intersection to intersection.
(defun regionscorrstore-find-path (pathcorr-options left-reg right-reg) ; -> path, or nil.
  ;(format t "~&regionscorrstore-find-path ~A and ~A" left-reg right-reg)
  (assert (regionscorrstore-p pathcorr-options))
  (assert (regionscorr-p left-reg))
  (assert (regionscorr-p right-reg))
  (assert (not (regionscorr-intersects left-reg right-reg)))

  ;; No point without at least one intersectionu of the left region.
  (if (not (regionscorrstore-any-intersection pathcorr-options left-reg))
    (return-from regionscorrstore-find-path nil))

  ;; No point without at least one intersection of the right region.
  (if (not (regionscorrstore-any-intersection pathcorr-options right-reg))
    (return-from regionscorrstore-find-path nil))

  ;; Regions should not intersect already.
  (if (regionscorr-intersects left-reg right-reg)
    (return-from regionscorrstore-find-path nil))

  ;; Try to find a path between the regions.
  (regionscorrstore-find-path2 pathcorr-options left-reg right-reg)
)
(defun regionscorrstore-find-path2 (pathcorr-options left-reg right-reg) ; -> path, or nil. Probably should not call this function directly.
  ;(format t "~&regionscorrstore-find-path2 ~A and ~A" left-reg right-reg)
  ;(assert (regionscorrstore-p pathcorr-options))
  ;(assert (regionscorr-p left-reg))
  ;(assert (regionscorr-p right-reg))

  ;; Check for the successful end of a search, or sub-search.
  ;; Look for one region that intersects both regions.
  (let (links ; Store of regions that intersect both given regions.
       )
    (loop for regx in (regionscorrstore-regionscorr-list pathcorr-options) do
      (if (and (and (regionscorr-neq regx left-reg) (regionscorr-intersects regx left-reg))
	       (and (regionscorr-neq regx right-reg) (regionscorr-intersects regx right-reg)))
	(push regx links)
      )
    )
    (if links
      (return-from regionscorrstore-find-path2 (pathcorr-new (list left-reg (nth (random (length links)) links) right-reg)))
    )
  )

  ;; Look for regions that do not intersect either region.
  ;; These split the search into two smaller searches.
  (let (middle-region	; Region between the two given regions.
        links		; Store of regions between the two given regions.
        left-path	; Path from first given region to the middle-region.
        right-path	; Path from middle-region to the second region.
        (glide-path (regionscorr-union left-reg right-reg)) ; Region containing straight-forward paths between regions.
       )

    ;; Gather non-intersecting regions roughly between the two given regions.
    (loop for regx in (regionscorrstore-regionscorr-list pathcorr-options) do
      (if (and (not (regionscorr-intersects regx left-reg)) (not (regionscorr-intersects regx right-reg))
	       (regionscorr-intersects regx glide-path))
	(push regx links)
      )
    )
    (when links
      ;; Choose a region to split the problem in two.
      (setf middle-region (nth (random (length links)) links))
 
      (setf left-path (regionscorrstore-find-path2 pathcorr-options left-reg middle-region))
      (if (null left-path)
        (return-from regionscorrstore-find-path2 nil))

      (setf right-path (regionscorrstore-find-path2 pathcorr-options middle-region right-reg))
      (if (null right-path)
        (return-from regionscorrstore-find-path2 nil))

      (if (regionscorr-eq (regionscorrstore-last-region left-path) (regionscorrstore-first-region right-path))
        (return-from regionscorrstore-find-path2 (pathcorr-append left-path (regionscorrstore-cdr right-path))))

      (return-from regionscorrstore-find-path2 (pathcorr-append left-path right-path))
    )
  )

  ;; Look for regions that intersect the left, or right, region.
  (let (next-region	; A region that intersects one of the given regions.
        links		; Store of next-regions.
        left-path	; Path from left region to the next-region.
        right-path	; Path from next-region to the right region.
       )

    (loop for regx in (regionscorrstore-regionscorr-list pathcorr-options) do

      ;; Find regions that intersect the left region, and at least one other region.
      (when (and (regionscorr-neq regx left-reg) (regionscorr-intersects regx left-reg))
	;; Check if the region intersects any other region, that left-reg does not intersect.
	(if (regionscorrstore-other-intersections :store pathcorr-options :int-reg regx :not-reg left-reg)
	  (push regx links)
	)
      )
      ;; Find regions that intersect the right region, and at least one other region.
      (when (and (regionscorr-neq regx right-reg) (regionscorr-intersects regx right-reg))
	;; Check if the region intersects any other region, that left-reg does not intersect.
	(if (regionscorrstore-other-intersections :store pathcorr-options :int-reg regx :not-reg right-reg)
	  (push regx links)
	)	
      )
    )
    (when links
      ;; Choose a region to continue with.
      (setf next-region (nth (random (length links)) links))

      ;; Process a region that intersects the left region.
      (when (regionscorr-intersects next-region left-reg)
        (setf right-path (regionscorrstore-find-path2 pathcorr-options next-region right-reg))
        (if (pathcorr-is-empty right-path)
          (return-from regionscorrstore-find-path2 right-path))

        (pathcorr-add-start right-path left-reg)
        (return-from regionscorrstore-find-path2 right-path)
      )
      ;; Process a region that intersects the right region.
      (when (regionscorr-intersects next-region right-reg)
        (setf left-path (regionscorrstore-find-path2 pathcorr-options left-reg next-region))
        (if (pathcorr-is-empty left-path)
          (return-from regionscorrstore-find-path2 left-path))

        (pathcorr-add-end left-path right-reg)
        (return-from regionscorrstore-find-path2 left-path)
      )
    )
  ) 
  ;; Default return.
  nil
)

;;; Return a regionscorrstore of intersections-of-intersection fragments.
(defun regionscorrstore-intersections-of-intersections(store1) ; -> regionscorrstore
  (assert (regionscorrstore-p store1))

  (let ((store2 (regionscorrstore-new nil)))

    ;; Remove dups, if any.
    (loop for regscorrx in (regionscorrstore-regionscorr-list store1) do
      (if (not (regionscorrstore-contains store2 regscorrx))
	(regionscorrstore-push store2 regscorrx)
      )
    )

    (if (< (regionscorrstore-length store2) 2)
      (return-from regionscorrstore-intersections-of-intersections store2))

    (let (ints-not-found (store3 (regionscorrstore-new nil)) tmpstore (any-change true) store4 (ret (regionscorrstore-new nil)))

      (while any-change
	(setf any-change false)

        (loop for regscorrx in (regionscorrstore-regionscorr-list store2) do
          (setf ints-not-found true)
  
	  (setf tmpstore (regionscorrstore-new (list regscorrx)))

          (loop for regscorry in (regionscorrstore-regionscorr-list store2) do
    
            (if (regionscorr-neq regscorrx regscorry)
  
              (when (regionscorrstore-any-intersection tmpstore regscorry)
  
                (setf ints-not-found false)
                (setf any-change true)
  
                (setf tmpstore (regionscorrstore-subtract-regionscorr tmpstore regscorry))
              )
    	    )
          ) ; next regscorry
          (loop for regscorrz in (regionscorrstore-regionscorr-list tmpstore) do
            (regionscorrstore-push-nosubs store3 regscorrz)
          )
        ) ; next regscorrx

	(setf store4 (regionscorrstore-subtract :min-store store2 :sub-store store3))

        (when any-change
 	  (setf store2 store4)
	  (setf ret (regionscorrstore-append ret store3))
 	  (setf store3 (regionscorrstore-new nil))
 	)
      ) ; end while
      (setf ret (regionscorrstore-append ret store3)) ; pick up last regions, with no intersections.
      ;; Return results.
      ret
    )
  )
)

;;; Return a regionscorrstore minus another.
(defun regionscorrstore-subtract (&key min-store sub-store) ; -> regionscorrstore.
  (assert (regionscorrstore-p min-store))
  (assert (regionscorrstore-p sub-store))

  (let ((ret min-store))
    (loop for regscorrx in (regionscorrstore-regionscorr-list sub-store) do
        (if (regionscorrstore-any-intersection ret regscorrx)
	  (setf ret (regionscorrstore-subtract-regionscorr ret regscorrx))
	)
    )
    ret
  )
)

