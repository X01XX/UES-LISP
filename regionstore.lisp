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
;;; Failure to find a path is an empty regionstore.
;;; The strategy is to keep dividing the problem into two smaller problems.
;;; Later, a path can be calculated from intersection to intersection.
(defun regionstore-find-links (storex reg1 reg2) ; -> regionstore
  ;(format t "~&regionstore-find-links ~A and ~A" reg1 reg2)
  (assert (regionstore-p storex))
  (assert (region-p reg1))
  (assert (region-p reg2))
  (assert (not (region-intersects reg1 reg2)))

  ;; No point without at least one intersection.
  (if (not (regionstore-any-intersection storex reg1))
    (return-from regionstore-find-links (regionstore-new nil)))

  ;; No point without at least one intersection.
  (if (not (regionstore-any-intersection storex reg2))
    (return-from regionstore-find-links (regionstore-new nil)))

  (regionstore-find-links2 storex reg1 reg2)
)
(defun regionstore-find-links2 (storex reg1 reg2) ; -> regionstore
  ;(format t "~&regionstore-find-links2 ~A and ~A" reg1 reg2)
  (assert (regionstore-p storex))
  (assert (region-p reg1))
  (assert (region-p reg2))
  (assert (not (region-intersects reg1 reg2)))

  ;; Check for the successful end of a search, or sub-search.
  ;; Look for one region that intersects both regions.
  (let (links store1)
    (loop for regx in (regionstore-regions storex) do
      (if (and (and (region-neq regx reg1) (region-intersects regx reg1))
	       (and (region-neq regx reg2) (region-intersects regx reg2)))
	(push regx links)
      )
    )
    (when links
      (setf store1 (regionstore-new (list reg1 (nth (random (length links)) links) reg2)))
      ;(format t "~&ret 1 ~A" store1)
      (return-from regionstore-find-links2 store1)
    )
  )

  ;; Look for regions that do not intersect either region.
  ;; These split the search into two smaller searches.
  (let (links middle-region store1 store2 store3
        (glide-path (region-union reg1 reg2))
       )

    (loop for regx in (regionstore-regions storex) do
      (if (and (not (region-intersects regx reg1)) (not (region-intersects regx reg2))
	       (region-intersects regx glide-path))
	(push regx links)
      )
    )
    (when links
      ;; Choose a region to split the ploblem in two.
      (setf middle-region
        (if (= 1 (length links))
  	  (car links)
  	  (nth (random (length links)) links))
      )
      ;(format t "~&recurse 1")
      (setf store1 (regionstore-find-links2 storex reg1 middle-region))
      ;(format t "~&rslt recurse 1 ~A" store1)
      ;(format t "~&recurse 2")
      (setf store2 (regionstore-find-links2 storex middle-region reg2))
      ;(format t "~&rslt recurse 2 ~A" store2)

      (when (and (regionstore-is-not-empty store1) (regionstore-is-not-empty store2))
        (if (region-eq (regionstore-last-region store1) (regionstore-first-region store2))
          (setf store3 (regionstore-append store1 (regionstore-cdr store2)))
          (setf store3 (regionstore-append store1 store2)))

        ;(format t "~&ret 2 ~A" store3)
        (return-from regionstore-find-links2 store3)
      )	
      ;(format t "~&ret 3")
      (return-from regionstore-find-links2 (regionstore-new nil))
    )
  )

  ;; Look for regions that intersect one, or the other, region.
  (let (links next-region store1)
    (loop for regx in (regionstore-regions storex) do
      (when (and (region-neq regx reg1) (region-intersects regx reg1))
	;; Check if the region intersects any other region, that reg1 does not intersect.
	(if (regionstore-other-intersections :store storex :int-reg regx :not-reg reg1)
	  (push regx links)
	)
      )
      (when (and (region-neq regx reg2) (region-intersects regx reg2))
	;; Check if the region intersects any other region, that reg1 does not intersect.
	(if (regionstore-other-intersections :store storex :int-reg regx :not-reg reg2)
	  (push regx links)
	)	
      )
    )
    (when links
      ;; Choose a region to continue with.
      (setf next-region (nth (random (length links)) links))

      (when (region-intersects next-region reg1)
        ;(format t "~&recurse 3")
        (setf store1 (regionstore-find-links2 storex next-region reg2))
        ;(format t "~&rslt recurse 3 ~A" store1)
        (regionstore-push store1 reg1)
        ;(format t "~&ret 4")
        (return-from regionstore-find-links2 store1)
      )
      (when (region-intersects next-region reg2)
        ;(format t "~&recurse 4")
        (setf store1 (regionstore-find-links2 storex reg1 next-region))
        ;(format t "~&rslt recurse 4 ~A" store1)
        (regionstore-add-end store1 reg2)
        ;(format t "~&ret 5")
        (return-from regionstore-find-links2 store1)
      )
    )
  ) 

  ;(format t "~&ret 6")
  (regionstore-new nil)
)
