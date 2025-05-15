;;;; Implement a store of regions.

; Implement a store of regions.
(defstruct regionstore
  regions  ; A list of zero, or more, regions.
)
; Automatically created by defstruct:
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
(defun regionstore-new (&rest regions) ; -> regionstore.
  (let (listx)
   ;; Check argument, convert list of list to list.
    (if (listp (car regions))
      (setf listx (car regions))
      (setf listx regions))

    ;; Check each item type.
    (loop for regx in listx do
      (assert (region-p regx))
    )   

    ;; Construct results.
    (make-regionstore :regions listx)
  )
)

;;; Push region into a regionstore.
(defun regionstore-push (storex regx) ; -> nothing, side-effect regionstore is changed.
  ;; Check arguments.
  (assert (regionstore-p storex))
  (assert (region-p regx))

  ;; Add region.
  (push regx (regionstore-regions storex))
)

;;; Add a region to a regionstore if there are no regions that are superset.
;;; Delete subsets of new region.
(defun regionstore-push-nosubs (storex regx) ; -> bool, true if regionstore is changed.
  ;; Check arguments.
  (assert (regionstore-p storex))
  (assert (region-p regx))
  (assert (or (regionstore-is-empty storex) (= (regionstore-num-bits storex) (region-num-bits regx))))

  ;; Check for region in store that is a superset (or dup) of the new region.
  (loop for regy in (regionstore-regions storex) do
    (if (region-superset-of :sup regy :sub regx)
      (return-from regionstore-push-nosubs false)) ;; Return negative result.
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
  ;; Return positive result.
  true
)

;;; Return true if any region in a store is a superset (or eq) of a given region.
(defun regionstore-any-superset-of (storex regx) ; -> bool
  ;; Check arguments.
  (assert (regionstore-p storex))
  (assert (region-p regx))
  (assert (or (regionstore-is-empty storex) (= (regionstore-num-bits storex) (region-num-bits regx))))

  (loop for regy in (regionstore-regions storex) do
    (if (region-superset-of :sup regy :sub regx)
      (return-from regionstore-any-superset-of true)) ; Return positive result.
  )
  ;; Return negative result.
  false
)

;;; Return true if any region in a store is a subset (or eq) of a given region.
(defun regionstore-any-subset-of (storex regx) ; -> bool
  ;; Check arguments.
  (assert (regionstore-p storex))
  (assert (region-p regx))
  (assert (or (regionstore-is-empty storex) (= (regionstore-num-bits storex) (region-num-bits regx))))

  (loop for regy in (regionstore-regions storex) do
    (if (region-superset-of :sub regy :sup regx)
      (return-from regionstore-any-subset-of true)) ; Return positive result.
  )
  ;; Return negative result.
  false
)

;;; Return true if any region in a store intersects a passed region.
(defun regionstore-any-intersection-of (storex regx) ; -> bool
  ;; Check arguments.
  (assert (regionstore-p storex))
  (assert (region-p regx))
  (assert (or (regionstore-is-empty storex) (= (regionstore-num-bits storex) (region-num-bits regx))))

  (loop for regy in (regionstore-regions storex) do
    (if (region-intersects regy regx)
      (return-from regionstore-any-intersection-of true)) ; Return positive result.
  )
  ;; Return negative result.
  false
)

;;; Add a region to a regionstore if there are no regions that are subset.
;;; Delete supersets of new region.
(defun regionstore-push-nosups (storex regx) ; -> bool, true if regionstore is changed.
  ;; Check arguments.
  (assert (regionstore-p storex))
  (assert (region-p regx))
  (assert (or (regionstore-is-empty storex) (= (regionstore-num-bits storex) (region-num-bits regx))))

  ;; Check if the new region is a superset of any store region.
  (loop for regy in (regionstore-regions storex) do
    (if (region-superset-of :sup regx :sub regy)
      (return-from regionstore-push-nosups false)) ; Return negative result.
  )

  ;; Check for regions that are a superset of the new region.
  (let (del-regs)
    ;; Find regions that are a subset of the new region.
    (loop for regy in (regionstore-regions storex) do
      (if (region-superset-of :sup regy :sub regx)
        (push regy del-regs)
      )
    )
    ;; Remove the superset regions.
    (loop for regy in del-regs do
      (setf (regionstore-regions storex) (remove regy (regionstore-regions storex) :test #'region-eq))
    )
  )

  ;; Add the region.
  (regionstore-push storex regx)

  ;; Return positive result.
  true
)

;;; Return the number of regions in a regionstore.
(defun regionstore-length (storex) ; -> number.
  ;; Check argument.
  (assert (regionstore-p storex))

  ;; Calc result.
  (length (regionstore-regions storex))
)

;;; Return true if a regionstore is empty.
(defun regionstore-is-empty (storex) ; -> bool
  ;; Check argument.
  (assert (regionstore-p storex))

  ;; Calc result.
  (zerop (regionstore-length storex))
)

;;; Return true if a regionstore is not empty.
(defun regionstore-is-not-empty (storex) ; -> bool
  ;; Check argument.
  (assert (regionstore-p storex))

  ;; Calc result.
  (plusp (regionstore-length storex))
)

;;; Return a string representing a regionstore.
(defun regionstore-str (storex) ; -> string.
  ;; Check argument.
  (assert (regionstore-p storex))

  (let ((ret "(") (start t))
    (loop for regx in (regionstore-regions storex) do
      (if start (setf start nil) (setf ret (concatenate 'string ret " ")))

      (setf ret (concatenate 'string ret (region-str regx)))
    )
    (setf ret (concatenate 'string ret ")"))

    ;; Return result.
    ret
  )
)

;;; Return true if a regionstore contains a given region.
(defun regionstore-member (storex regx) ; -> bool
  ;; Check arguments.
  (assert (regionstore-p storex))
  (assert (region-p regx))
  (assert (or (regionstore-is-empty storex) (= (regionstore-num-bits storex) (region-num-bits regx))))

  ;; Calc result.
  (member regx (regionstore-regions storex) :test #'region-eq)
)

;;; Return the first region in a non-empty regionstore.
(defun regionstore-first-region (storex) ; -> region
  ;; Check arguments.
  (assert (regionstore-p storex))
  (assert (regionstore-is-not-empty storex))

  ;; Calc result.
  (car (regionstore-regions storex))
)

;;; Return a regionstore minus a region.
(defun regionstore-subtract-region (storex regx) ; -> regionstore.
  ;; Check arguments.
  (assert (regionstore-p storex))
  (assert (region-p regx))
  (assert (or (regionstore-is-empty storex) (= (regionstore-num-bits storex) (region-num-bits regx))))

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
    ;; Return result.
    ret
  )
)

;;; Return a regionstore minus a state.
(defun regionstore-subtract-state (storex stax) ; -> regionstore.
  ;; Check arguments.
  (assert (regionstore-p storex))
  (assert (state-p stax))
  (assert (or (regionstore-is-empty storex) (= (regionstore-num-bits storex) (state-num-bits stax))))

  (let ((ret (regionstore-new nil)) tmpstore)

    (loop for regy in (regionstore-regions storex) do

      (cond ((region-superset-of-state regy stax)
             (setf tmpstore (region-subtract-state regy stax))

             (loop for regz in (regionstore-regions tmpstore) do
               (regionstore-push-nosubs ret regz)
             )
           )
           (t (regionstore-push-nosubs ret regy))
      )
    )
    ;; Return result.
    ret
  )
)

;;; Return a regionstore instance, given a list of symbols.
;;; Like (), (r1010), or (r101, r1000).
(defun regionstore-from (symbols) ; -> regionstore instance.
  ;; Check arguments.
  (assert (listp symbols))
  (assert (or (null symbols) (symbolp (car symbols))))

  (let (regions)
    (loop for tokx in symbols do
      (push (region-from tokx) regions)
    )
    ;; Return result.
    (regionstore-new (reverse regions))
  )
)

;;; Return a regionstore minus another.
(defun regionstore-subtract (&key min-store sub-store) ; -> regionstore.
  ;; Check arguments.
  (assert (regionstore-p min-store))
  (assert (regionstore-p sub-store))
  (assert (or (regionstore-is-empty min-store)
              (regionstore-is-empty sub-store)
              (= (regionstore-num-bits min-store) (regionstore-num-bits sub-store))))

  (let ((ret min-store))
    (loop for regx in (regionstore-regions sub-store) do
      (setf ret (regionstore-subtract-region ret regx))
    )
    ;; Return result.
    ret
  )
)

;;; Return the intersection of two regionstores.
(defun regionstore-intersection (storex storey) ; -> RegionStore.
  ;; Check arguments.
  (assert (regionstore-p storex))
  (assert (regionstore-p storey))
  (assert (or (regionstore-is-empty storex)
              (regionstore-is-empty storey)
              (= (regionstore-num-bits storex) (regionstore-num-bits storey))))

  (let ((ret (regionstore-new nil)))

    (loop for regx in (regionstore-regions storex) do

      (loop for regy in (regionstore-regions storey) do

        (if (region-intersects regx regy)
          (regionstore-push-nosubs ret (region-intersection regx regy))
        )
      )
    )
    ;; Return result.
    ret
  )
)

;;; Return the union of two regionstores.
(defun regionstore-union (storex storey) ; -> RegionStore.
  ;; Check arguments.
  (assert (regionstore-p storex))
  (assert (regionstore-p storey))
  (assert (or (regionstore-is-empty storex)
              (regionstore-is-empty storey)
              (= (regionstore-num-bits storex) (regionstore-num-bits storey))))

  (let ((ret (regionstore-new nil)))

    (loop for regx in (regionstore-regions storex) do
      (regionstore-push-nosubs ret regx)
    )

    (loop for regy in (regionstore-regions storey) do
      (regionstore-push-nosubs ret regy)
    )
    ;; Return result.
    ret
  )
)

;;; Append two regionstores.
(defun regionstore-append (storex storey) ; -> RegionStore.
  ;; Check arguments.
  (assert (regionstore-p storex))
  (assert (regionstore-p storey))
  (assert (or (regionstore-is-empty storex)
              (regionstore-is-empty storey)
              (= (regionstore-num-bits storex) (regionstore-num-bits storey))))

  (let ((ret (regionstore-new nil)))

    (loop for regx in (regionstore-regions storex) do
      (regionstore-push ret regx)
    )

    (loop for regy in (regionstore-regions storey) do
      (regionstore-push ret regy)
    )
    ;; Return result.
    ret
  )
)

;;; Return the number of bits used by regions in a non-empty regionstore.
(defun regionstore-num-bits (storex) ; -> number
  ;; Check arguments.
  (assert (regionstore-p storex))
  (assert (regionstore-is-not-empty storex))
  (assert (regionstore-same-num-bits storex))

  ;; Return result.
  (region-num-bits (regionstore-first-region storex))
)

;;; Return true if a state is in only one region.
(defun regionstore-state-in-exactly-one (storex stax) ; -> bool
  ;; Check arguments.
  (assert (regionstore-p storex))
  (assert (state-p stax))
  (assert (or (regionstore-is-empty storex) (= (regionstore-num-bits storex) (state-num-bits stax))))

  (let ((cnt 0))
    (loop for regx in (regionstore-regions storex) do
      (if (region-superset-of-state regx stax)
         (incf cnt))
    )
    ;; Calc result.
    (= cnt 1)
  )
)

;; Return true if a state is used in a regionstore.
(defun regionstore-state-needed (storex stax) ; -> bool
  ;; Check arguments.
  (assert (regionstore-p storex))
  (assert (state-p stax))
  (assert (or (regionstore-is-empty storex) (= (regionstore-num-bits storex) (state-num-bits stax))))

  (loop for regx in (regionstore-regions storex) do
    (if (region-state-needed regx stax)
      (return-from regionstore-state-needed true)) ; Return positive result.
  )
  ;; Return negative result.
  false
)

;;; Return true if a state is in only one region.
(defun regionstore-regions-state-in (storex stax) ; -> regionstore
  ;; Check arguments.
  (assert (regionstore-p storex))
  (assert (state-p stax))
  (assert (or (regionstore-is-empty storex) (= (regionstore-num-bits storex) (state-num-bits stax))))

  (let ((ret-store (regionstore-new nil)))
    (loop for regx in (regionstore-regions storex) do
      (if (region-superset-of-state regx stax)
         (regionstore-push ret-store regx))
    )
    ;; Return result.
    ret-store
  )
)

;;; Return a list of defining regions, if any,
;;; that is, any region that has something left from subtracting the other regions.
(defun regionstore-defining-regions (storex) ; -> regionstore
  ;; Check argument.
  (assert (regionstore-p storex))

  (let ((ret-store (regionstore-new nil)) tmp-store)
    ;; Test each region.
    (loop for regx in (regionstore-regions storex) do
      (setf tmp-store (regionstore-new (list regx)))
      ;; Subtract other regions, as needed.
      (loop for regy in (regionstore-regions storex) do

        (if (and (null (region-eq regy regx)) (regionstore-any-intersection-of tmp-store regy))
          (setf tmp-store (regionstore-subtract-region tmp-store regy)))
      )
      (if (regionstore-is-not-empty tmp-store)
        (regionstore-push-nosubs ret-store regx))
    )
    ;; Return result.
    ret-store
  )
)

;;; Return unique subregions of a given region, assuming the given region is in the
;;; store, which will be skipped.
(defun regionstore-unique-subregions (storex regx) ; -> regionstore
  ;; Check arguments.
  (assert (regionstore-p storex))
  (assert (region-p regx))

  (let ((ret (regionstore-new (list regx))))

    (loop for regy in (regionstore-regions storex) do
      (if (and (null (region-eq regy regx)) (regionstore-any-intersection-of ret regy))
        (setf ret (regionstore-subtract-region ret regy)))
    )
    ret
  )
)

;;; Return true if all regions in a regionstore use the same number of bits.
(defun regionstore-same-num-bits (storex) ; -> bool
  (assert (regionstore-p storex))

  (if (< (regionstore-length storex) 2)
    (return-from regionstore-same-num-bits true)) ; Return positive result.

  (let ((num-bits (region-num-bits (car (regionstore-regions storex)))))
    (loop for regx in (cdr (regionstore-regions storex)) do
      (if (/= (region-num-bits regx) num-bits)
        (return-from regionstore-same-num-bits false)) ; Return negative result.
    )   
    ;; Return positive result.
    true
  )
)

;;; Return true if a regionstore is congruent, by region number bits, with the domain list.
(defun regionstore-congruent (regionstore1) ; -> bool
  ;(format t "~&regionstore-congruent: rcx ~A dnbl: ~A" (regionstore-str regionstore1) *domain-num-bits-list*)
  (assert (regionstore-p regionstore1))

  (if (/= (regionstore-length regionstore1) (length *domain-num-bits-list*))
    (return-from regionstore-congruent false))

  (loop for regx in (regionstore-regions regionstore1)
        for numx in *domain-num-bits-list* do

    (if (/= (region-num-bits regx) numx)
      (return-from regionstore-congruent false))
  )
  true
)

;;; Return true if two regionstores have the same length and regions, in any order.
(defun regionstore-eq (storex storey) ; -> bool
  (assert (regionstore-p storex))
  (assert (regionstore-p storey))

  (if (/= (regionstore-length storex) (regionstore-length storey))
    (return-from regionstore-eq false))

  (loop for regx in (regionstore-regions storex) do
    (if (not (regionstore-member storey regx))
      (return-from regionstore-eq false))
  )
  true
)

;;; Return the nth element of a regionstore.
(defun regionstore-nth (storex inx) ; -> region instance, or nil.
  (assert (regionstore-p storex))
  (assert (integerp inx))

  (if (>= inx (regionstore-length storex))
    (return-from regionstore-nth nil))

  (nth inx (regionstore-regions storex))
)

;;; Return the largest intersections of regions within a regionstore.
(defun regionstore-largest-intersections (strx) ; -> regionstore
  (assert (regionstore-p strx))
  ;(format t "~&regionstore-largest-intersections: ~A" strx)

  ;; Check for regions that are a subset of the new region.
  (let ((int-regs (regionstore-new nil)) regx regy)
    ;; Find regions that are a subset of the new region.
    (loop for inx from 0 below (1- (regionstore-length strx)) do
      (setf regx (nth inx (regionstore-regions strx)))

      (loop for iny from (1+ inx) below (regionstore-length strx) do
        (setf regy (nth iny (regionstore-regions strx)))

        (if (region-intersects regx regy)
          (regionstore-push-nosubs int-regs (region-intersection regx regy))
        )
      )
    )
    ;(format t "~&regionstore-largest-intersections: returning ~A" int-regs)
    int-regs
  )
)
;;; Return self split by intersections.
;;; Each fragment returned will be a subset of any original item it intersects.
(defun regionstore-split-by-intersections (strx) ; -> regionstore
  (assert (regionstore-p strx))
  ;(format t "~&regionstore-split-by-intersections ~A" strx)

  (let ((fragments (regionstore-new nil)) (remaining (regionstore-new nil)) intersections intreg)

    ;; Remove duplicates, if any.
    (loop for regx in (regionstore-regions strx) do
        (if (not (regionstore-member remaining regx))
          (regionstore-push remaining regx))
    )

    (if (< (regionstore-length remaining) 2)
      (return-from regionstore-split-by-intersections remaining))

    (loop while (regionstore-is-not-empty remaining) do
      ;(format t "~&remaining ~A" remaining)

      (setf intersections (regionstore-largest-intersections remaining))
      ;(format t "~& ~&intersections ~A" intersections)

      (setf remaining (regionstore-subtract :min-store remaining :sub-store intersections))
      ;(format t "~&remaining ~A" remaining)

      (setf fragments (regionstore-append fragments remaining))
      ;(format t "~&fragments ~A" fragments)

      ;; Gather remaining regions from the original store.
      (setf remaining (regionstore-new nil))
      (loop for regx in (regionstore-regions strx) do
        (loop for regy in (regionstore-regions intersections) do
          (when (region-intersects regx regy)
            (setf intreg (region-intersection regx regy))
            (if (not (regionstore-member remaining intreg))
              (regionstore-push remaining intreg))
          )
        )
      )
    )
    ;(format t "~& ~&returning ~A" fragments)
    fragments
  )
)

;;; Return the number af superset regions in a regionstore of a given region.
(defun regionstore-num-superset (storex regx) ; -> integer GE 0.
  (assert (regionstore-p storex))
  (assert (region-p regx))
  (assert (or (regionstore-is-empty storex) (= (regionstore-num-bits storex) (region-num-bits regx))))

  (let ((cnt 0))
    (loop for regy in (regionstore-regions storex) do
        (if (region-superset-of :sup regy :sub regx)
          (incf cnt))
    )
    cnt
  )
)
