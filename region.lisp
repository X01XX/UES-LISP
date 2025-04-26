;;;; Implement the region struct and functions.
;;;;
;;;; Regions can also be used as a convenient way to store squares that are related in some way,
;;;; and regions can be stored and manipulated in a regionstore.

;;; The region struct.
;;; It represents a 2^x by 2^y region of squares on a K-Map.
(defstruct region
  states    ; A StateStore of one, or more, states, no state between two others.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (region-<field name> <instance>) -> struct field.
;   (region-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> region
;   (typep <instance> 'region) -> bool
;
; Probably shouldn't use:
;   (make-region [:<field-name> <field-region>]*), use region-new instead.
;   (copy-region <instance>) copies a region instance.

;;; Return a new region, made up of one, or more, states.
(defun region-new (states) ; -> region.
  (let (states2)
    ;; Allow a single state or a state list as argument.
    (cond ((listp states)
           (assert (not (null states)))
           (setf states2 (statestore-new states)))
          ((state-p states)
           (setf states2 (statestore-new (list states))))
          (t (error "region-new: invalid argumant passed")))

    (assert (statestore-same-num-bits states2))

    ;; Construct result.
    (make-region :states (statestore-remove-unneeded states2))
  )
)

;;; Return the list of states defining a region.
(defun region-state-list (regx) ; -> a list of states.
  ;; Check argument.
  (assert (region-p regx))

  ;; Return statestore field.
  (statestore-states (region-states regx))
)

;;; Return the highest state in a region.
(defun region-high-state (regx) ; -> state
  ;; Check argument.
  (assert (region-p regx))

  ;; OR subsequent states with the first state.
  (let ((ret (car (region-state-list regx))))
    (loop for stax in (cdr (region-state-list regx)) do
      (setf ret (state-new-or ret stax))
    )
    ;; Return result.
    ret
  )
)

;;; Return the lowest state in a region.
(defun region-low-state (regx) ; -> state
  ;; Check argument.
  (assert (region-p regx))

  ;; AND subsequent states with the first state.
  (let ((ret (car (region-state-list regx))))
    (loop for stax in (cdr (region-state-list regx)) do
      (setf ret (state-new-and ret stax))
    )
    ;; Return result.
    ret
  )
)

;;; Return the number of bits used by a region's states.
(defun region-num-bits (regx) ; -> number
  ;; Check argument.
  (assert (region-p regx))

  ;; Return result.
  (state-num-bits (statestore-first-state (region-states regx)))
)

;;; Return the first state in a region.
(defun region-first-state (regx) ; -> state
  ;; Check argument.
  (assert (region-p regx))

  ;; Return result.
  (statestore-first-state (region-states regx))
)

;;; Return the x mask of a region.
(defun region-x-mask (regx) ; -> mask
  ;; Check argument.
  (assert (region-p regx))

  ;; Construct result.
  (mask-new (state-xor (region-high-state regx) (region-low-state regx)))
)

;;; Return the edge 1s mask of a region.
(defun region-1-mask (regx) ; -> mask
  ;; Check argument.
  (assert (region-p regx))

  ;; Construct result.
  (mask-new (state-and (region-first-state regx) (region-second-state regx)))
)

;;; Return the edge 0s mask of a region.
(defun region-0-mask (regx) ; -> mask
  ;; Check argument.
  (assert (region-p regx))

  ;; Construct result.
  (mask-new (value-and (state-not (region-first-state regx)) (state-not (region-second-state regx))))
)

;;; Return the second state in a region, really the far state from the first state.
(defun region-second-state (regx) ; -> state
  ;; Check argument.
  (assert (region-p regx))

  (let ((len (statestore-length (region-states regx))))
    ;; Construct result.
    (cond ((= len 1) (region-first-state regx))
          ((= len 2) (statestore-last-state (region-states regx)))
          (t (state-new (state-xor (region-first-state regx) (region-x-mask regx)))))
  )
)

;;; Return the number of states that define a region.
(defun region-number-states (regx) ; -> integer, gt zero.
  ;; Check argument.
  (assert (region-p regx))

  ;; Return result.
  (statestore-length (region-states regx))
)

;;; Return a string for a region.
;;; The state making up a region with one state, is obvious.
;;; The states making up a region with two states, can be read from the string representation.
;;; X01x is made up of (1010, 0011).
;;; A region can have more than two states, typically three, where none of the states
;;; are between any other two states, and will be indicated by a trailing + sign.
;;; In X01x+ the first state is 1010, the following states cannot be inferred.
(defun region-str (regx)  ; -> string.
  ;; Check argument.
  (assert (region-p regx))

  (let ((strs "r") ; Region prefix.
       )
    ;; Add region bits.
    (setf strs (concatenate 'string strs (region-str-bits regx)))

    ;; Add suffix, if needed.
    (if (> (region-number-states regx) 2)
        (setf strs (concatenate 'string strs "+")))

    ;; Return result.
    strs
  )
)

;;; Return a string representing just region bit positions.
(defun region-str-bits (regx) ; -> string, like 010X.
  ;; Check argument.
  (assert (region-p regx))

  (let ((strs "") ; String to build up for result.
        (bit-pos (mask-msb (mask-new (state-value (region-first-state regx))))) ; msb, to successively shift to test bit positions.
        (first-state (region-first-state regx))
        (second-state (region-second-state regx))
        fval        ; First state bit-pos value.
        sval        ; Second state bit-pos value.
       )

    (loop while (not (mask-zerop bit-pos)) do

      (if (mask-zerop (mask-new-and bit-pos first-state))
        (setf fval 0)
        (setf fval 1))

      (if (mask-zerop (mask-new-and bit-pos second-state))
        (setf sval 0)
        (setf sval 1))

      (cond ((and (= fval 0) (= sval 0))
             (setf strs (concatenate 'string strs "0")))
            ((and (= fval 0) (= sval 1))
             (setf strs (concatenate 'string strs "x")))
            ((and (= fval 1) (= sval 0))
             (setf strs (concatenate 'string strs "X")))
            ((and (= fval 1) (= sval 1))
             (setf strs (concatenate 'string strs "1")))
      )
      (setf bit-pos (mask-shift-right bit-pos))
    ) ; end-while
    ;; Return result.
    (string-add-underscores strs)
  )
)

;;; Return a region instance from a symbol.
;;; Like r1010, r1X10x.
;;; A region can be made of a single state.
;;; A token with an X, or x, will be defined with two states.
;;; An X will cause a 1 in the first state, a zero in the second.
;;; An x will cause a 0 in the first state, a one in the second state.
;;; So the states making up a region can be specified by the symbol representation.
(defun region-from (symx) ; -> region.
  ;; Check argument.
  (assert (symbolp symx))

  (let ((strx (symbol-name symx)) ret)
    ;; Process symbol name.
    (setf ret (region-from-str strx))
    ;; Check type of result.
    (if (err-p ret)
      (error (err-str ret))
      ret) ; return value.
  )
)
;;; Return a region instance from a string.
(defun region-from-str (strx) ; -> region instance.
  ;; Check argument.
  (assert (stringp strx))

  (let (strx2 ; Work string.
       (state-first "s") (state-second "s")) ; Init string prefixs.

    ;; Trim spaces.
    (setf strx2 (string-left-trim '(#\Space #\Tab #\Newline) (string-right-trim '(#\Space #\Tab #\Newline) strx)))

    ;; Check prefix.
    (if (not (string-equal (subseq strx2 0 1) "r"))
      (return-from region-from-str (err-new (format nil "region-from-str: Region ~A Should begin with an r character" strx2))))

    (loop for chr across (subseq strx2 1) do
      (cond
        ((char= chr #\_) nil)
        ((char= chr #\0) (setf state-first (concatenate 'string state-first "0"))
                         (setf state-second  (concatenate 'string state-second  "0")))
        ((char= chr #\1) (setf state-first (concatenate 'string state-first "1"))
                         (setf state-second  (concatenate 'string state-second  "1")))
        ((char= chr #\X) (setf state-first (concatenate 'string state-first "1"))
                         (setf state-second  (concatenate 'string state-second  "0")))
        ((char= chr #\x) (setf state-first (concatenate 'string state-first "0"))
                         (setf state-second  (concatenate 'string state-second  "1")))
        (t (return-from region-from-str (err-new (format nil "region-from-str: Invalid character ~A in ~A" chr strx2)))))
    )
    (if (= (length state-first) 1)
      (return-from region-from-str (err-new (format nil "region-from-str: No valid bit character found in ~A" strx2))))

    ;; Construct return value.
    (region-new (list (state-from-str state-first)
                      (state-from-str state-second)))
  )
)

;;; Return true if two regions are equal.
(defun region-eq (reg1 reg2) ; -> bool
  ;; Check arguments.
  (assert (region-p reg1))
  (assert (region-p reg2))
  (assert (= (region-num-bits reg1) (region-num-bits reg2)))

  ;; Regions can be equal, even though defined by different states.
  ;; Check highest possible state.
  (if (not (state-eq (region-high-state reg1) (region-high-state reg2)))
    (return-from region-eq false))

  ;; Check lowest possible state.
  (if (not (state-eq (region-low-state reg1) (region-low-state reg2)))
    (return-from region-eq false))

  ;; Return a positive result.
  true
)

;;; Return true if two regions are not equal.
(defun region-ne (reg1 reg2) ; -> bool
  ;; Check arguments.
  (assert (region-p reg1))
  (assert (region-p reg2))
  (assert (= (region-num-bits reg1) (region-num-bits reg2)))

  ;; Return result.
  (not (region-eq reg1 reg2))
)

;;; Return true if a list is a list of regions.
;;; An empty list will return true.
(defun region-list-p (reglst) ; -> bool
  ;; Check argument.
  (if (not (listp reglst))
    (return-from region-list-p false))

  (loop for regx in reglst do
    (if (not (region-p regx))
      (return-from region-list-p false))
  )
  ;; Return a positive result.
  true
)

;;; Return the intersection of two regions.
(defun region-intersection (reg1 reg2) ; -> region, or nil.
  ;; Check arguments.
  (assert (region-p reg1))
  (assert (region-p reg2))
  (assert (= (region-num-bits reg1) (region-num-bits reg2)))

  ;; Check if regions intersect.  This is required, so it may be better to run this,
  ;; than running region-intersects followed by region-intersection.
  (if (not (region-intersects reg1 reg2))
    (return-from region-intersection nil))

  ;; Construct result.
  (region-new (list (state-new (state-and (region-high-state reg1) (region-high-state reg2)))
                    (state-new (state-or  (region-low-state reg1)  (region-low-state reg2)))))
)

;;; Return the union of two regions.
(defun region-union (reg1 reg2) ; -> region
  ;; Check arguments.
  (assert (region-p reg1))
  (assert (region-p reg2))
  (assert (= (region-num-bits reg1) (region-num-bits reg2)))

  ;; Construct result.
  (region-new (list (state-new (state-or  (region-high-state reg1) (region-high-state reg2)))
                    (state-new (state-and (region-low-state reg1) (region-low-state reg2)))))
)

;;; Return the union of two regions.
(defun region-union-state (reg1 stax) ; -> region
  ;; Check arguments.
  (assert (region-p reg1))
  (assert (state-p stax))
  (assert (= (region-num-bits reg1) (state-num-bits stax)))

  ;; Construct result.
  (region-new (list (state-new (state-or  (region-high-state reg1) stax))
                    (state-new (state-and (region-low-state reg1) stax))))
)

;;; Return a mask of edge bit positions.
(defun region-edge-mask (regx) ; -> mask
  ;; Check argument.
  (assert (region-p regx))

  ;; Construct result.
  (state-eqv (region-first-state regx) (region-second-state regx))
)

;;; Return a edge-difference mask of two regions.
(defun region-edge-dif-mask (reg1 reg2) ; -> mask
  ;; Check arguments.
  (assert (region-p reg1))
  (assert (region-p reg2))
  (assert (= (region-num-bits reg1) (region-num-bits reg2)))

  ;; Construct result.
  (mask-new-and (mask-new-and (region-edge-mask reg1) (region-edge-mask reg2))
                (mask-new (state-xor (region-first-state reg1) (region-first-state reg2))))
)

;;; Return the distance between two regions.
(defun region-distance (reg1 reg2) ; -> integer.
  ;; Check arguments.
  (assert (region-p reg1))
  (assert (region-p reg2))
  (assert (= (region-num-bits reg1) (region-num-bits reg2)))

  ;; Construct result.
  (mask-num-ones (region-edge-dif-mask reg1 reg2))
)

;;; Return true if a region intersects another.
(defun region-intersects (reg1 reg2) ; -> bool.
  ;; Check arguments.
  (assert (region-p reg1))
  (assert (region-p reg2))
  (assert (= (region-num-bits reg1) (region-num-bits reg2)))

  ;; Carc result.
  (= (region-distance reg1 reg2) 0)
)

;;; Return true if the first region is a superset of the second.
(defun region-superset-of (&key sub sup) ; -> bool.
  ;; Check arguments.
  (assert (region-p sub))
  (assert (region-p sup))
  (assert (= (region-num-bits sub) (region-num-bits sup)))

  ;; This is required, so it may be better to run this instead of running region-intersects followed by region-superset-of.
  (if (not (region-intersects sub sup))
    (return-from region-superset-of false))

  ;; Calc result.
  (let ((subx (region-x-mask sub))
        (supx (region-x-mask sup)))
    (mask-superset-of :sup-mask supx :sub-mask subx)
  )
)

;;; Return a region with edges of a mask set to ones.
(defun region-set-to-ones (regx mskx) ; -> region.
  ;; Check arguments.
  (assert (region-p regx))
  (assert (mask-p mskx))
  (assert (= (region-num-bits regx) (mask-num-bits mskx)))

  ;; Calc result.
  (region-new (list (state-new (mask-or mskx (region-high-state regx)))
                    (state-new (mask-or mskx (region-low-state regx)))))
)

;;; Return a region with edges of a mask set to zeros.
(defun region-set-to-zeros (regx mskx) ; -> region.
  ;; Check arguments.
  (assert (region-p regx))
  (assert (mask-p mskx))
  (assert (= (region-num-bits regx) (mask-num-bits mskx)))

  ;; Calc result.
  (let ((mskn (mask-new (mask-not mskx))))
    (region-new (list (state-new (mask-and mskn (region-high-state regx)))
                      (state-new (mask-and mskn (region-low-state regx)))))
  )
)

;;; Return a region with edges of a mask set to x.
(defun region-set-to-x (regx mskx) ; -> region.
  ;; Check arguments.
  (assert (region-p regx))
  (assert (mask-p mskx))
  (assert (= (region-num-bits regx) (mask-num-bits mskx)))

  ;; Calc result.
  (let ((mskn (mask-new (mask-not mskx))))
    (region-new (list (state-new (mask-or  mskx (region-high-state regx)))
                      (state-new (mask-and mskn (region-low-state regx)))))
  )
)

;;; Return the minuend region minus the subtrahend region.
(defun region-subtract (&key min-reg sub-reg) ; -> regionstore.
  ;; Check arguments.
  (assert (region-p min-reg))
  (assert (region-p sub-reg))
  (assert (= (region-num-bits min-reg) (region-num-bits sub-reg)))

  ;; This is required, so it may be better to run this instead of running region-intersects followed by region-subtract.
  (if (not (region-intersects min-reg sub-reg))
    (return-from region-subtract (regionstore-new (list min-reg))))

  ;; Check for nothing result.
  (if (region-superset-of :sup sub-reg :sub min-reg)
    (return-from region-subtract (regionstore-new nil)))

  (let ((ret (regionstore-new nil))
        (sub-bits (mask-split (mask-new-and (region-x-mask min-reg) (region-edge-mask sub-reg))))
       )
    ;; Calc result.
    ;; Copy and store the region, except, one position by one position, X over 0 becomes 1/0, X over 1 becomes 0/1.
    (loop for bitx in sub-bits do
      (if (mask-is-low (mask-new-and bitx (region-first-state sub-reg)))
        (regionstore-push-nosubs ret (region-set-to-ones min-reg bitx))
        (regionstore-push-nosubs ret (region-set-to-zeros min-reg bitx))
      )
    )
    ;; Return result.
    ret
  )
)

;;; Return a region minus a state.
(defun region-subtract-state (regx stax) ; -> regionstore.
  ;; Check arguments.
  (assert (region-p regx))
  (assert (state-p  stax))
  (assert (= (region-num-bits regx) (state-num-bits stax)))

  ;; This is required, so it may be better to run this instead of running region-superset-of followed by region-subtract-state.
  (if (not (region-superset-of-state regx stax))
    (return-from region-subtract-state (regionstore-new (list regx))))

  ;; Check for nothing result.
  (if (and (= 1 (region-number-states regx)) (state-eq (region-first-state regx) stax))
    (return-from region-subtract-state (regionstore-new nil)))

  (let ((ret (regionstore-new nil))
        (sub-bits (mask-split (region-x-mask regx))))

    ;; Calc result.
    ;; Copy and store the region, except, one position by one position, X over 0 becomes 1/0, X over 1 becomes 0/1.
    (loop for bitx in sub-bits do
      (if (mask-is-low (mask-new-and bitx stax))
        (regionstore-push-nosubs ret (region-set-to-ones regx bitx))
        (regionstore-push-nosubs ret (region-set-to-zeros regx bitx))
      )
    )
    ;; Return result.
    ret
  )
)

;;; Return the distance between a region and a state.
(defun region-distance-state (regx stax) ; -> integer.
  ;; Check arguments.
  (assert (region-p regx))
  (assert (state-p stax))
  (assert (= (region-num-bits regx) (state-num-bits stax)))

  ;; Construct result.
  (mask-num-ones (mask-new (state-and
                  (state-new-xor (region-first-state regx) stax)
                  (state-new-xor (region-second-state regx) stax))))
)

;;; Return true if a region intersects a state.
(defun region-intersects-state (regx stax) ; -> bool.
  ;; Check arguments.
  (assert (region-p regx))
  (assert (state-p stax))
  (assert (= (region-num-bits regx) (state-num-bits stax)))

  ;; Calc result.
  (= (region-distance-state regx stax) 0)
)


;;; Return true if the first region is a superset of a state.
(defun region-superset-of-state (regx stax) ; -> bool.
  ;; Check arguments.
  (assert (region-p regx))
  (assert (state-p stax))
  (assert (= (region-num-bits regx) (state-num-bits stax)))

  ;; Calc result.
  (= (region-distance-state regx stax) 0)
)

;;; Return the far state for, opposite a given state, in a region.
(defun region-far-state (regx stax) ; -> state
  ;; Check arguments.
  (assert (region-p regx))
  (assert (state-p stax))
  (assert (= (region-num-bits regx) (state-num-bits stax)))
  (assert (region-intersects-state regx stax))

  ;; Calc result.
  (state-new-xor stax (region-x-mask regx))
)

;;; Return the far region for, opposite a given subregion, in a region.
(defun region-far-region (regx subx) ; -> region
  ;; Check arguments.
  (assert (region-p regx))
  (assert (region-p subx))
  (assert (= (region-num-bits regx) (region-num-bits subx)))
  (assert (region-ne regx subx))
  (assert (region-superset-of :sup regx :sub subx))

  ;; Calc result.
  (let ((msk (mask-new-and (region-x-mask regx) (region-edge-mask subx))))
    (region-new (list (state-new-xor (region-first-state subx) msk)
                      (state-new-xor (region-second-state subx) msk)))
  )
)

;; Return true if a state is needed to define a region.
(defun region-state-needed (regx stax) ; -> bool
  ;; Check arguments.
  (assert (region-p regx))
  (assert (state-p stax))
  (assert (= (region-num-bits regx) (state-num-bits stax)))

  ;; Calc result.
  (statestore-member (region-states regx) stax)
)
